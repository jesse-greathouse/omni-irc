Param(
  [string]$Switch       = "omni-irc-dev",
  [string]$BuildProfile = "release",
  [string]$MSYS2        = "C:\msys64",
  [string]$OutDir       = ".dist\omni-irc-win64",
  [string]$ExeName      = "omni-irc-client.exe",

  # Version for naming and WiX define
  [string]$Version      = $null,

  # Naming controls
  [string]$Platform     = "win64",   # keep "win64" per your scheme
  [string]$Arch         = $null      # auto-detect if not provided
)

$ErrorActionPreference = "Stop"

# ---------- Path helpers ----------
$RepoRoot = (Split-Path -Parent $PSScriptRoot)
function Resolve-AbsPath([string]$p) {
  if ([System.IO.Path]::IsPathRooted($p)) { return (Resolve-Path $p).Path }
  return (Join-Path $RepoRoot $p)
}

# ---------- Version ----------
$version = $Version
if (-not $version) {
  $version = "0.0.0"
  $dp = Join-Path $RepoRoot "dune-project"
  if (Test-Path $dp) {
    $verLine = (Get-Content $dp) | Select-String '^\(version\s+([^)]+)\)'
    if ($verLine) { $version = ($verLine.Matches[0].Groups[1].Value.Trim()) }
  }
}
if (-not $version -or $version -eq "0.0.0") {
  throw "Cannot determine Version (pass -Version or ensure dune-project contains '(version ...)')."
}

# ---------- Robust arch resolution ----------
function Resolve-Arch([string]$a) {
  if ($a) { return $a.ToLower() }
  $cands = @($env:PROCESSOR_ARCHITECTURE, $env:PROCESSOR_ARCHITEW6432) | Where-Object { $_ -and $_.Trim() -ne "" }
  foreach ($c in $cands) {
    switch ($c.ToUpperInvariant()) {
      "ARM64" { return "arm64" }
      "AMD64" { return "x64"   }
      "X86"   { return "x86"   }
      "IA64"  { return "ia64"  }
    }
  }
  try {
    $osArch = [System.Runtime.InteropServices.RuntimeInformation]::OSArchitecture.ToString().ToLower()
    if ($osArch -match 'arm64') { return 'arm64' }
    if ($osArch -match 'x64')   { return 'x64'   }
    if ($osArch -match 'x86')   { return 'x86'   }
  } catch {}
  return "x64"
}
$Arch = Resolve-Arch $Arch

# Map to WiX arch token (WiX v4 accepts x64/arm64)
$WixArch = if ($Arch -eq 'arm64') { 'arm64' } elseif ($Arch -eq 'x86') { 'x86' } else { 'x64' }

# ---------- Paths ----------
$OutDirAbs     = Resolve-AbsPath $OutDir
$WixOutDirAbs  = Resolve-AbsPath ".dist\wix"
$FilesWxsAbs   = Join-Path $WixOutDirAbs "Files.wxs"
$ProductWxsAbs = Resolve-AbsPath "installer\wix\Product.wxs"

# Versioned, platform + arch ZIP name
$ZipAbs        = Resolve-AbsPath (".dist\omni-irc-client-{0}-{1}-{2}.zip" -f $version, $Platform, $Arch)

# ---------- WiX resolver ----------
function Resolve-WixExe {
  $cmd = Get-Command wix -ErrorAction SilentlyContinue
  if ($cmd) { return $cmd.Path }
  $candidates = @(
    "$env:USERPROFILE\.dotnet\tools\wix.exe",
    "C:\Program Files\WiX Toolset v4\bin\wix.exe",
    "C:\Program Files (x86)\WiX Toolset v4\bin\wix.exe"
  )
  if ($env:WIX) { $candidates += (Join-Path $env:WIX "bin\wix.exe") }
  foreach ($p in $candidates) { if (Test-Path $p) { return $p } }

  $dotnetToolDir = Join-Path $env:USERPROFILE ".dotnet\tools"
  if (Test-Path (Join-Path $dotnetToolDir "wix.exe")) {
    $global:env:Path = "$dotnetToolDir;$($global:env:Path)"
    $cmd = Get-Command wix -ErrorAction SilentlyContinue
    if ($cmd) { return $cmd.Path }
  }
  return $null
}

# ---------- Generate WiX fragment ----------
function New-WixFilesFragment {
  param(
    [Parameter(Mandatory)][string]$InstallDir,
    [Parameter(Mandatory)][string]$OutPath
  )
  if (-not (Test-Path $InstallDir)) { throw "InstallDir '$InstallDir' does not exist." }
  $rootFull = (Resolve-Path $InstallDir).Path

  $dirsSet = [System.Collections.Generic.HashSet[string]]::new()
  $dirsSet.Add("") | Out-Null
  $files = Get-ChildItem -Path $InstallDir -Recurse -File | Where-Object { $_.Name -notmatch '\.pdb$' }

  foreach ($f in $files) {
    $rel    = $f.FullName.Substring($rootFull.Length).TrimStart('\','/')
    $relDir = Split-Path $rel -Parent
    if ($relDir -and $relDir -ne ".") {
      $parts = $relDir -split '[\\\/]+'
      for ($i=0; $i -lt $parts.Length; $i++) {
        $seg = ($parts[0..$i] -join '\'); $dirsSet.Add($seg) | Out-Null
      }
    }
  }

  $dirsOrdered = $dirsSet | Sort-Object { ($_ -split '[\\\/]+').Length }, { $_ }
  $dirId = @{}; $dirId[""] = "INSTALLFOLDER"
  foreach ($d in $dirsOrdered) {
    if ($d -eq "") { continue }
    $dirId[$d] = "DIR_" + ($d -replace '[^A-Za-z0-9_]', '_')
  }

  $sb = New-Object System.Text.StringBuilder
  [void]$sb.AppendLine('<?xml version="1.0" encoding="UTF-8"?>')
  [void]$sb.AppendLine('<Wix xmlns="http://wixtoolset.org/schemas/v4/wxs">')
  [void]$sb.AppendLine('  <Fragment>')
  [void]$sb.AppendLine('    <DirectoryRef Id="INSTALLFOLDER">')
  foreach ($d in $dirsOrdered) {
    if ($d -eq "") { continue }
    $parent   = Split-Path $d -Parent; if ($parent -eq ".") { $parent = "" }
    $parentId = $dirId[$parent]; $thisId = $dirId[$d]; $name = Split-Path $d -Leaf
    [void]$sb.AppendLine("      <Directory Id=""$thisId"" Name=""$name"" Parent=""$parentId"" />")
  }
  [void]$sb.AppendLine('    </DirectoryRef>')
  [void]$sb.AppendLine('    <ComponentGroup Id="AppFiles">')
  foreach ($f in $files) {
    $rel     = $f.FullName.Substring($rootFull.Length).TrimStart('\','/')
    $relDir  = Split-Path $rel -Parent
    $dirKey  = if ($relDir -and $relDir -ne ".") { $relDir -replace '/', '\' } else { "" }
    $target  = $dirId[$dirKey]
    $cid     = 'CMP_' + ($rel -replace '[^A-Za-z0-9_]', '_')
    $src     = '$(var.InstallDir)\' + ($rel -replace '/', '\')
    [void]$sb.AppendLine("      <Component Id=""$cid"" Directory=""$target"" Guid=""*"">")
    [void]$sb.AppendLine("        <File Source=""$src"" KeyPath=""yes"" />")
    [void]$sb.AppendLine("      </Component>")
  }
  [void]$sb.AppendLine('    </ComponentGroup>')
  [void]$sb.AppendLine('  </Fragment>')
  [void]$sb.AppendLine('</Wix>')

  $outDir = Split-Path $OutPath -Parent
  if (-not (Test-Path $outDir)) { New-Item -ItemType Directory -Force $outDir | Out-Null }
  [System.IO.File]::WriteAllText($OutPath, $sb.ToString(), [System.Text.UTF8Encoding]::new($false))
}

# ---------- Build portable payload ----------
Write-Host ">> Activating opam switch: $Switch"
(& opam env --switch=$Switch --set-switch) -split '\r?\n' | ForEach-Object { Invoke-Expression $_ }

Write-Host ">> Building dune + deps"
$oldPath = $env:Path
try {
  $switchBin = (opam var bin).Trim()
  if (-not (Test-Path $switchBin)) { throw "opam var bin returned '$switchBin' which doesn't exist." }

  $env:Path = "$switchBin;$env:Path"
  Write-Host ">> PATH (temp prepend): $switchBin"

  $dune = @("dune.exe","dune.cmd","dune") |
          ForEach-Object { Join-Path $switchBin $_ } |
          Where-Object { Test-Path $_ } |
          Select-Object -First 1
  if (-not $dune) { throw "dune was not found in $switchBin even after PATH prepend." }
  Write-Host ">> dune resolved: $dune"

  Write-Host ">> Building ($BuildProfile)"
  & $dune build --profile $BuildProfile bin/omni.exe

  $built = Join-Path $RepoRoot "_build\default\bin\omni.exe"
  if (-not (Test-Path $built)) { throw "Expected exe not found at $built" }

  Write-Host ">> Staging output -> $OutDirAbs"
  Remove-Item -Recurse -Force $OutDirAbs -ErrorAction SilentlyContinue
  New-Item -ItemType Directory -Force $OutDirAbs | Out-Null
  Copy-Item $built (Join-Path $OutDirAbs $ExeName)

  # Dependency harvest via ntldd (MSYS2 UCRT64)
  $ntldd = Join-Path $MSYS2 "ucrt64\bin\ntldd.exe"
  if (Test-Path $ntldd) {
    Write-Host ">> Harvesting DLLs via ntldd"
    $deps = & $ntldd -R $built |
      Select-String -Pattern '=>\s+(.+?)\s+\(0x' |
      ForEach-Object { ($_ -replace '.*=>\s+','' -replace '\s+\(0x.*','').Trim() } |
      Where-Object { $_ -and (Test-Path $_) -and -not ($_.ToLower() -match 'windows\\system32|windows\\syswow64') } |
      Get-Unique
    foreach ($dll in $deps) { Copy-Item $dll $OutDirAbs -Force }
  } else {
    Write-Warning "ntldd not found at $ntldd; skipping DLL harvest."
  }

  # Optional docs
  Copy-Item (Join-Path $RepoRoot "README*")  $OutDirAbs -ErrorAction SilentlyContinue
  Copy-Item (Join-Path $RepoRoot "LICENSE*") $OutDirAbs -ErrorAction SilentlyContinue

  # Zip (versioned filename with platform+arch)
  $zipDirAbs = Split-Path $ZipAbs -Parent
  if (-not (Test-Path $zipDirAbs)) { New-Item -ItemType Directory -Force $zipDirAbs | Out-Null }
  if (Test-Path $ZipAbs) { Remove-Item $ZipAbs -Force }
  Compress-Archive -Path (Join-Path $OutDirAbs '*') -DestinationPath $ZipAbs

  Write-Host ">> ZIP created: $ZipAbs"
  Write-Host "   Unzip and run: $OutDirAbs\$ExeName"

  # ---------- MSI build ----------
  $wixExe = Resolve-WixExe
  if (-not $wixExe) {
    Write-Warning @"
WiX v4 CLI not found.
Install one of:
  dotnet tool install -g wix
  winget install WixToolset.WixToolset
  choco install wix-toolset --version=4.x
Skipping MSI build.
"@
    return
  }
  Write-Host ">> WiX: $wixExe"

  if (-not (Test-Path $WixOutDirAbs)) { New-Item -ItemType Directory -Force $WixOutDirAbs | Out-Null }
  Write-Host ">> Generating WiX fragment: $FilesWxsAbs"
  New-WixFilesFragment -InstallDir $OutDirAbs -OutPath $FilesWxsAbs

  if (-not (Test-Path $ProductWxsAbs)) {
    throw "Missing WiX authoring at $ProductWxsAbs (expected checked-in file)."
  }

  $msiAbs = Resolve-AbsPath (".dist\omni-irc-client-{0}-{1}-{2}.msi" -f $version, $Platform, $Arch)
  Write-Host ">> Building MSI -> $msiAbs"

  & $wixExe build `
      $ProductWxsAbs `
      $FilesWxsAbs `
      -o $msiAbs `
      -arch $Arch `
      -d ProductVersion=$version `
      -d InstallDir=$OutDirAbs |
      Write-Host

  Write-Host ">> MSI created: $msiAbs"
}
finally {
  $env:Path = $oldPath
  Write-Host ">> PATH restored"
}
