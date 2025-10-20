Param(
  # CLI overrides (highest precedence after --config)
  [string]$Switch       = $null,
  [string]$BuildProfile = $null,
  [string]$MSYS2        = $null,
  [string]$OutDir       = $null,
  [string]$ExeName      = $null,
  [string]$Version      = $null,
  [string]$Platform     = $null,
  [string]$Arch         = $null,
  [string]$Config       = $null,
  [switch]$PrintConfig
)

$ErrorActionPreference = "Stop"

# ───────────────────────── repo + config files ─────────────────────────
$RepoRoot   = (Split-Path -Parent $PSScriptRoot)
$ConfigDir  = Join-Path $RepoRoot "installer"
$ConfigDist = Join-Path $ConfigDir "build.dist.ini"
$ConfigLocal= Join-Path $ConfigDir "build.ini"

function Resolve-AbsPath([string]$p) {
  if ([string]::IsNullOrWhiteSpace($p)) { return $null }
  if ([System.IO.Path]::IsPathRooted($p)) { return (Resolve-Path $p).Path }
  return (Join-Path $RepoRoot $p)
}

# ───────────────────────── allowed config keys ─────────────────────────
# Keep this list explicit; only these keys load from INI/ENV.
$AllowedKeys = @(
  # build
  'SWITCH','BUILD_PROFILE','MSYS2','OUT_DIR','EXE_NAME','VERSION','PLATFORM','ARCH',
  # wix/output
  'WIX_EXE','ZIP_PATH','MSI_PATH',
  # signing (optional)
  'WIN_SIGN','WIN_CERT_SUBJECT','WIN_CERT_PFX','WIN_CERT_PFX_PASS','WIN_TIMESTAMP_URL'
)

# hard defaults (minimal; most defaults live in build.dist.ini)
$cfg = [ordered]@{
  SWITCH            = 'omni-irc-dev'
  BUILD_PROFILE     = 'release'
  MSYS2             = 'C:\msys64'
  OUT_DIR           = '.dist\omni-irc-win64'
  EXE_NAME          = 'omni-irc-client.exe'
  VERSION           = ''
  PLATFORM          = 'win64'
  ARCH              = ''              # auto-detect later
  WIX_EXE           = ''              # auto-resolve later if empty
  ZIP_PATH          = ''              # auto-name later if empty
  MSI_PATH          = ''              # auto-name later if empty

  # signing (off by default)
  WIN_SIGN          = '0'
  WIN_CERT_SUBJECT  = ''              # e.g. "Jesse Greathouse"
  WIN_CERT_PFX      = ''              # e.g. C:\path\code-signing.pfx
  WIN_CERT_PFX_PASS = ''              # secret (redacted in print)
  WIN_TIMESTAMP_URL = 'http://timestamp.digicert.com'
}

# ───────────────────────── INI loader (safe) ───────────────────────────
function Load-IniAllowed([string]$path, [hashtable]$into) {
  if (-not (Test-Path $path)) { return }
  Get-Content -Raw -LiteralPath $path -Encoding UTF8 `
  | Select-String -Pattern '^\s*([^#;].*?)$' -AllMatches `
  | ForEach-Object { $_.Matches.Value } `
  | Where-Object { $_ -match '^\s*[A-Z0-9_]+\s*=' } `
  | ForEach-Object {
      $line = $_.Trim()
      $eq   = $line.IndexOf('=')
      if ($eq -gt 0) {
        $k = $line.Substring(0,$eq).Trim()
        $v = $line.Substring($eq+1).Trim()
        if ($AllowedKeys -contains $k) {
          $into[$k] = $v
        }
      }
    }
}

# precedence: dist -> local -> --config -> ENV -> CLI args
Load-IniAllowed $ConfigDist $cfg
Load-IniAllowed $ConfigLocal $cfg
if ($Config) { Load-IniAllowed (Resolve-AbsPath $Config) $cfg }

# ENV snapshot
foreach ($k in $AllowedKeys) {
  $envVal = [Environment]::GetEnvironmentVariable($k, "Process")
  if ($envVal) { $cfg[$k] = $envVal }
}

# CLI overrides
if ($Switch)       { $cfg.SWITCH       = $Switch }
if ($BuildProfile) { $cfg.BUILD_PROFILE= $BuildProfile }
if ($MSYS2)        { $cfg.MSYS2        = $MSYS2 }
if ($OutDir)       { $cfg.OUT_DIR      = $OutDir }
if ($ExeName)      { $cfg.EXE_NAME     = $ExeName }
if ($Version)      { $cfg.VERSION      = $Version }
if ($Platform)     { $cfg.PLATFORM     = $Platform }
if ($Arch)         { $cfg.ARCH         = $Arch }

function Print-EffectiveConfig {
  Write-Host "---- effective config ----"
  foreach ($k in $AllowedKeys) {
    $v = $cfg[$k]
    if ($k -eq 'WIN_CERT_PFX_PASS') { $v = '****' }
    "{0,-20} = {1}" -f $k, ($v ?? '')
  }
  Write-Host "--------------------------"
}

if ($PrintConfig) { Print-EffectiveConfig; exit 0 }

# ───────────────────────── arch + version resolve ───────────────────────
function Resolve-Arch([string]$a) {
  if ($a) { return $a.ToLowerInvariant() }
  $cands = @($env:PROCESSOR_ARCHITECTURE, $env:PROCESSOR_ARCHITEW6432) | Where-Object { $_ -and $_.Trim() -ne "" }
  foreach ($c in $cands) {
    switch ($c.ToUpperInvariant()) {
      'ARM64' { return 'arm64' }
      'AMD64' { return 'x64'   }
      'X86'   { return 'x86'   }
    }
  }
  try {
    $osArch = [System.Runtime.InteropServices.RuntimeInformation]::OSArchitecture.ToString().ToLower()
    if ($osArch -match 'arm64') { return 'arm64' }
    if ($osArch -match 'x64')   { return 'x64' }
    if ($osArch -match 'x86')   { return 'x86' }
  } catch {}
  'x64'
}
$Arch = Resolve-Arch $cfg.ARCH
$cfg.ARCH = $Arch
$WixArch = if ($Arch -eq 'arm64') { 'arm64' } elseif ($Arch -eq 'x86') { 'x86' } else { 'x64' }

$version = $cfg.VERSION
if (-not $version) {
  $version = "0.0.0"
  $dp = Join-Path $RepoRoot "dune-project"
  if (Test-Path $dp) {
    $verLine = (Get-Content $dp) | Select-String '^\(version\s+([^)]+)\)'
    if ($verLine) { $version = ($verLine.Matches[0].Groups[1].Value.Trim()) }
  }
}
if (-not $version -or $version -eq "0.0.0") {
  throw "Cannot determine Version (set VERSION or ensure dune-project contains '(version ...)')."
}
$cfg.VERSION = $version

# ───────────────────────── path setup ───────────────────────────────────
$OutDirAbs     = Resolve-AbsPath $cfg.OUT_DIR
$WixOutDirAbs  = Resolve-AbsPath ".dist\wix"
$FilesWxsAbs   = Join-Path $WixOutDirAbs "Files.wxs"
$ProductWxsAbs = Resolve-AbsPath "installer\wix\Product.wxs"

if ([string]::IsNullOrEmpty($cfg.ZIP_PATH)) {
  $cfg.ZIP_PATH = ".dist\omni-irc-client-$version-$($cfg.PLATFORM)-$Arch.zip"
}
if ([string]::IsNullOrEmpty($cfg.MSI_PATH)) {
  $cfg.MSI_PATH = ".dist\omni-irc-client-$version-$($cfg.PLATFORM)-$Arch.msi"
}
$ZipAbs = Resolve-AbsPath $cfg.ZIP_PATH
$MsiAbs = Resolve-AbsPath $cfg.MSI_PATH

# ───────────────────────── signtool helpers ─────────────────────────────
function Resolve-SignTool {
  $cmd = Get-Command signtool.exe -ErrorAction SilentlyContinue
  if ($cmd) { return $cmd.Path }
  $candidates = @(
    "$env:ProgramFiles (x86)\Windows Kits\10\bin\x64\signtool.exe",
    "$env:ProgramFiles\Windows Kits\10\bin\x64\signtool.exe",
    "$env:ProgramFiles (x86)\Windows Kits\10\bin\10.0.22621.0\x64\signtool.exe",
    "$env:ProgramFiles (x86)\Windows Kits\10\bin\10.0.19041.0\x64\signtool.exe"
  )
  foreach ($p in $candidates) { if (Test-Path $p) { return $p } }
  return $null
}

function Sign-File([string]$path) {
  if ($cfg.WIN_SIGN -ne '1') { return }
  $signtool = Resolve-SignTool
  if (-not $signtool) { throw "WIN_SIGN=1 but signtool.exe not found. Install Windows SDK." }

  $ts = $cfg.WIN_TIMESTAMP_URL
  $args = @('sign','/fd','sha256','/tr', $ts, '/td','sha256')

  if ($cfg.WIN_CERT_PFX) {
    $pfx = Resolve-AbsPath $cfg.WIN_CERT_PFX
    if (-not (Test-Path $pfx)) { throw "PFX not found at $pfx" }
    $args += @('/f', $pfx)
    if ($cfg.WIN_CERT_PFX_PASS) { $args += @('/p', $cfg.WIN_CERT_PFX_PASS) }
  } elseif ($cfg.WIN_CERT_SUBJECT) {
    $args += @('/n', $cfg.WIN_CERT_SUBJECT)
  } else {
    throw "WIN_SIGN=1 but neither WIN_CERT_PFX nor WIN_CERT_SUBJECT is set."
  }

  & $signtool @args $path | Write-Host
}

# ───────────────────────── WiX resolver ─────────────────────────────────
function Resolve-WixExe {
  if ($cfg.WIX_EXE) {
    $p = Resolve-AbsPath $cfg.WIX_EXE
    if (Test-Path $p) { return $p }
  }
  $cmd = Get-Command wix -ErrorAction SilentlyContinue
  if ($cmd) { return $cmd.Path }
  $candidates = @(
    "$env:USERPROFILE\.dotnet\tools\wix.exe",
    "C:\Program Files\WiX Toolset v4\bin\wix.exe",
    "C:\Program Files (x86)\WiX Toolset v4\bin\wix.exe"
  )
  foreach ($p in $candidates) { if (Test-Path $p) { return $p } }
  # try PATH after dotnet tools hint
  $dotnetToolDir = Join-Path $env:USERPROFILE ".dotnet\tools"
  if (Test-Path (Join-Path $dotnetToolDir "wix.exe")) {
    $env:Path = "$dotnetToolDir;$env:Path"
    $cmd = Get-Command wix -ErrorAction SilentlyContinue
    if ($cmd) { return $cmd.Path }
  }
  return $null
}

# ───────────────────────── WiX files fragment ───────────────────────────
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

# ───────────────────────── Build portable payload ───────────────────────
Write-Host ">> Activating opam switch: $($cfg.SWITCH)"
(& opam env --switch=$($cfg.SWITCH) --set-switch) -split '\r?\n' | ForEach-Object { Invoke-Expression $_ }

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

  Write-Host ">> Building ($($cfg.BUILD_PROFILE))"
  & $dune build --profile $cfg.BUILD_PROFILE bin/omni.exe

  $built = Join-Path $RepoRoot "_build\default\bin\omni.exe"
  if (-not (Test-Path $built)) { throw "Expected exe not found at $built" }

  Write-Host ">> Staging output -> $(Resolve-AbsPath $cfg.OUT_DIR)"
  Remove-Item -Recurse -Force $OutDirAbs -ErrorAction SilentlyContinue
  New-Item -ItemType Directory -Force $OutDirAbs | Out-Null
  Copy-Item $built (Join-Path $OutDirAbs $cfg.EXE_NAME)

  # Dependency harvest via ntldd (MSYS2 UCRT64)
  $ntldd = Join-Path $cfg.MSYS2 "ucrt64\bin\ntldd.exe"
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

  # Sign EXE + harvested DLLs (optional)
  if ($cfg.WIN_SIGN -eq '1') {
    Write-Host ">> Code signing enabled; signing staged binaries"
    $targets = Get-ChildItem -Path $OutDirAbs -File -Include *.exe,*.dll -Recurse
    foreach ($t in $targets) { Sign-File $t.FullName }
  }

  # Zip (versioned filename with platform+arch)
  $zipDirAbs = Split-Path $ZipAbs -Parent
  if (-not (Test-Path $zipDirAbs)) { New-Item -ItemType Directory -Force $zipDirAbs | Out-Null }
  if (Test-Path $ZipAbs) { Remove-Item $ZipAbs -Force }
  Compress-Archive -Path (Join-Path $OutDirAbs '*') -DestinationPath $ZipAbs
  Write-Host ">> ZIP created: $ZipAbs"
  Write-Host "   Unzip and run: $OutDirAbs\$($cfg.EXE_NAME)"

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

  Write-Host ">> Building MSI -> $MsiAbs"
  & $wixExe build `
      $ProductWxsAbs `
      $FilesWxsAbs `
      -o $MsiAbs `
      -arch $WixArch `
      -d ProductVersion=$version `
      -d InstallDir=$OutDirAbs | Write-Host

  # Sign MSI (optional)
  if ($cfg.WIN_SIGN -eq '1') {
    Write-Host ">> Signing MSI"
    Sign-File $MsiAbs
  }

  Write-Host ">> MSI created: $MsiAbs"
}
finally {
  $env:Path = $oldPath
  Write-Host ">> PATH restored"
}
