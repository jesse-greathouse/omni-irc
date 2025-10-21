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

# --- OPAM helpers / env hygiene ---------------------------------------------
function Get-OpamExe {
  # Don’t touch $cfg here; it’s not defined yet.
  $cmd = Get-Command opam -CommandType Application -ErrorAction SilentlyContinue
  if ($cmd) { return $cmd.Source }

  $msys = if ($env:MSYS2 -and (Test-Path $env:MSYS2)) { $env:MSYS2 } else { 'C:\msys64' }
  $tryPaths = @(
    (Join-Path $msys 'ucrt64\bin\opam.exe'),
    (Join-Path $msys 'mingw64\bin\opam.exe')
  ) | Where-Object { Test-Path $_ }

  if ($tryPaths.Count -gt 0) { return $tryPaths[0] }
  return $null
}

function Ensure-OpamPackages {
  param(
    [Parameter(Mandatory)][string]$OpamExe,
    [Parameter(Mandatory)][string]$OpamRoot,
    [Parameter(Mandatory)][string]$Switch
  )
  # refresh repos
  & $OpamExe update --root "$OpamRoot" | Out-Host

  # ensure dune + common build deps
  $need = @('dune', 'ocamlfind', 'dune-configurator')
  foreach ($pkg in $need) {
    $installed = & $OpamExe list --root "$OpamRoot" --switch $Switch --installed --short $pkg 2>$null
    if (-not ($installed -match '^\s*' + [regex]::Escape($pkg) + '(\.|$)')) {
      Write-Host "  -> opam install $pkg"
      & $OpamExe install --root "$OpamRoot" --switch $Switch -y $pkg | Out-Host
    }
  }

  $hasOpamFiles = @(Get-ChildItem -Filter '*.opam' -Path . -ErrorAction SilentlyContinue).Count -gt 0
  $hasOpamDir   = Test-Path -LiteralPath '.\opam'
  if ($hasOpamFiles -or $hasOpamDir) {
    Write-Host "  -> opam install . --deps-only"
    & $OpamExe install --root "$OpamRoot" --switch $Switch -y . --deps-only | Out-Host
  }
}

function Ensure-OpamSwitchBinReady {
  param(
    [Parameter(Mandatory)][string]$OpamExe,
    [Parameter(Mandatory)][string]$OpamRoot,
    [Parameter(Mandatory)][string]$Switch
  )
  # Ask opam where this switch's bin lives
  $bin = (& $OpamExe var --root "$OpamRoot" --switch $Switch bin 2>$null).Trim()
  if (-not $bin) { throw "Could not resolve opam var 'bin' for switch '$Switch'." }

  # Create the directory tree if it's not there yet
  if (-not (Test-Path -LiteralPath $bin)) {
    New-Item -ItemType Directory -Force -Path $bin | Out-Null
  }

  # Make sure it's writable (ACLs sometimes get weird under %LOCALAPPDATA%)
  try {
    & icacls $bin /grant "$($env:USERNAME):(OI)(CI)M" | Out-Null
  } catch { }

  # Remove stale/locked shim targets if present (so shim package can overwrite)
  $shimNames = @(
    'x86_64-w64-mingw32-gcc.exe',
    'x86_64-w64-mingw32-g++.exe',
    'x86_64-w64-mingw32-ar.exe',
    'x86_64-w64-mingw32-ranlib.exe',
    'pkg-config.exe'
  )
  foreach ($n in $shimNames) {
    $p = Join-Path $bin $n
    if (Test-Path -LiteralPath $p) {
      try {
        attrib -r $p 2>$null
        Remove-Item -LiteralPath $p -Force
      } catch {
        # One more shove in case it's locked read-only
        try { & icacls $p /grant "$($env:USERNAME):F" | Out-Null } catch { }
        attrib -r $p 2>$null
        Remove-Item -LiteralPath $p -Force -ErrorAction SilentlyContinue
      }
    }
  }

  return $bin
}

# Clear inherited vars that often break builds
foreach ($v in 'OPAMSWITCH','OPAMREPOSITORY','OCAMLLIB','OCAMLPATH',
                'CAML_LD_LIBRARY_PATH','CAML_LD_LIBRARY_PATH__F',
                'OCAML_TOPLEVEL_PATH') {
  if (Test-Path Env:$v) { Remove-Item Env:$v -ErrorAction SilentlyContinue }
}

# Canonical Windows opam root for this packaging step
$OpamRoot = $env:OPAMROOT
if ([string]::IsNullOrWhiteSpace($OpamRoot)) {
  $OpamRoot = Join-Path $env:LOCALAPPDATA 'opam'
}
$env:OPAMROOT = $OpamRoot
$env:OPAMYES  = "1"

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
  if ($k -eq 'PLATFORM') { continue }
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
    $vOut = if ($null -ne $v -and "$v" -ne '') { "$v" } else { '' }
    "{0,-20} = {1}" -f $k, $vOut
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

# ───────────────────────── Build portable payload (robust) ───────────────────────
# Find opam
$opam = Get-OpamExe
if (-not $opam) {
  throw @"
opam was not found on PATH or in common MSYS2 locations.
Install opam (e.g. OCaml for Windows, or MSYS2 package) and re-run.
Searched PATH, C:\msys64\ucrt64\bin, and C:\msys64\mingw64\bin.
"@
}

# Ensure opam is initialized at this root
if (-not (Test-Path (Join-Path $OpamRoot 'config'))) {
  Write-Host ">> opam init @ $OpamRoot"
  & $opam init --root "$OpamRoot" --disable-sandboxing | Out-Host
}

Write-Host ">> Activating opam switch: $($cfg.SWITCH) (root: $OpamRoot)"
# Ensure both repos present on Windows
try {
  $reposShort = & $opam repo list --root "$OpamRoot" --short 2>$null
  $repos = $reposShort -split '\r?\n'
  if (-not ($repos -contains 'default')) {
    Write-Host "  -> adding opam repo 'default'"
    & $opam repo add --root "$OpamRoot" default https://opam.ocaml.org --set-default | Out-Host
  }
  if ($IsWindows -and -not ($repos -contains 'default-mingw')) {
    Write-Host "  -> adding opam repo 'default-mingw'"
    & $opam repo add --root "$OpamRoot" default-mingw https://github.com/ocaml-opam/opam-repository-mingw.git --set-default | Out-Host
  }
  Write-Host "  -> opam update"
  & $opam update --root "$OpamRoot" | Out-Host
} catch {
  Write-Warning "opam repo/update failed: $($_.Exception.Message)"
}

# Create switch if missing, with explicit package set
try {
  $have = & $opam switch list --root "$OpamRoot" --short 2>$null
  if (-not (($have -split '\r?\n') -contains $cfg.SWITCH)) {
    Write-Host "  -> creating opam switch $($cfg.SWITCH)"
    $candidates = @(
      'ocaml-compiler.5.3.0,system-mingw,ocaml-env-mingw64,mingw-w64-shims',
      'ocaml-compiler.5.2.1,system-mingw,ocaml-env-mingw64,mingw-w64-shims',
      'ocaml-base-compiler.5.2.1,system-mingw,ocaml-env-mingw64,mingw-w64-shims'
    )
    $created = $false
    foreach ($spec in $candidates) {
      Write-Host "     - trying: $spec"
      & $opam switch create --root "$OpamRoot" $cfg.SWITCH --packages $spec | Out-Host
      $envLines = & $opam env --root "$OpamRoot" --switch $cfg.SWITCH --set-switch
      $switchBin = Ensure-OpamSwitchBinReady -OpamExe $opam -OpamRoot $OpamRoot -Switch $cfg.SWITCH
      Write-Host ">> switch bin ready: $switchBin"
      if ($LASTEXITCODE -eq 0) { $created = $true; break }
    }
    if (-not $created) {
      throw "Could not create switch '$($cfg.SWITCH)' with any candidate package set."
    }
  }
} catch {
  throw "Switch creation failed: $($_.Exception.Message)"
}

# Apply env for THIS process only
$envLines = & $opam env --root "$OpamRoot" --switch $cfg.SWITCH --set-switch
$envLines -split '\r?\n' | ForEach-Object { if ($_ -match '\S') { Invoke-Expression $_ } }

# Sanity print
try {
  $oc = Get-Command ocamlc -ErrorAction Stop
  $ver = & $oc.Source -version
  $where = & $oc.Source -where
  Write-Host ("Using ocamlc {0} at {1} ({2})" -f $ver, $where, $oc.Source)
} catch {}

Write-Host ">> Building dune + deps"
$oldPath = $env:Path
try {
  $switchBin = (& $opam var --root "$OpamRoot" --switch $cfg.SWITCH bin).Trim()
  if (-not (Test-Path $switchBin)) { throw "opam var bin returned '$switchBin' which doesn't exist." }

  # Ensure dune + build deps are present in the switch
  Ensure-OpamPackages -OpamExe $opam -OpamRoot $OpamRoot -Switch $cfg.SWITCH

  $env:Path = "$switchBin;$env:Path"
  Write-Host ">> PATH (temp prepend): $switchBin"

  $dune = @("dune.exe","dune.cmd","dune") |
          ForEach-Object { Join-Path $switchBin $_ } |
          Where-Object { Test-Path $_ } |
          Select-Object -First 1
  if (-not $dune) { throw "dune was not found in $switchBin even after PATH prepend." }
  Write-Host ">> dune resolved: $dune"

  $built = $null
  # We build the app with dune; opam is used only for deps.
  Write-Host ">> Building with dune ($($cfg.BUILD_PROFILE))"
  & $dune build --profile $cfg.BUILD_PROFILE bin/omni.exe
  $candidate = Join-Path $RepoRoot "_build\default\bin\omni.exe"
  if (Test-Path $candidate) { $built = $candidate }

  if (-not $built) {
    Write-Host ">> Building with dune ($($cfg.BUILD_PROFILE))"
    & $dune build --profile $cfg.BUILD_PROFILE bin/omni.exe
    $candidate = Join-Path $RepoRoot "_build\default\bin\omni.exe"
    if (Test-Path $candidate) { $built = $candidate }
  }

  if (-not $built) {
    # last probe: anything resembling omni-irc-client in switch bin
    $probe = Get-ChildItem -LiteralPath $switchBin -Filter '*omni*irc*client*.exe' -ErrorAction SilentlyContinue | Select-Object -First 1
    if ($probe) { $built = $probe.FullName }
  }

  if (-not $built) {
    throw "Could not locate built executable. Ensure 'omni-irc-client' opam package exists or 'bin/omni.exe' dune target is present."
  }

  Write-Host ">> Build OK: $built"

  # ---------- Stage output ----------
  Write-Host ">> Staging output -> $(Resolve-AbsPath $cfg.OUT_DIR)"
  Remove-Item -Recurse -Force $OutDirAbs -ErrorAction SilentlyContinue
  New-Item -ItemType Directory -Force $OutDirAbs | Out-Null
  Copy-Item $built (Join-Path $OutDirAbs $cfg.EXE_NAME)

  # Dependency harvest via ntldd (MSYS2 UCRT64)
  $ntlddRoot = if ($cfg.MSYS2) { $cfg.MSYS2 } else { if ($env:MSYS2) { $env:MSYS2 } else { 'C:\msys64' } }
  $ntldd = Join-Path $ntlddRoot "ucrt64\bin\ntldd.exe"
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
