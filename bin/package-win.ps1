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
  # Don’t touch $script:cfg here; it’s not defined yet.
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
    [Parameter(Mandatory)][string]$Switch,
    [switch]$SkipUpdate
  )
  if (-not $SkipUpdate) {
    & $OpamExe update --root "$OpamRoot" | Out-Host
  }

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
$script:AllowedKeys = @(
  'SWITCH','BUILD_PROFILE','MSYS2','OUT_DIR','EXE_NAME','VERSION','PLATFORM','ARCH',
  'WIX_EXE','ZIP_PATH','MSI_PATH',
  'WIN_SIGN','WIN_CERT_SUBJECT','WIN_CERT_PFX','WIN_CERT_PFX_PASS','WIN_TIMESTAMP_URL',
  'MSIX_PATH','APPX_MANIFEST','APPX_PUBLISHER','APPX_IDENTITY','APPX_DISPLAY_NAME','APPX_PUBLISHER_DISPLAY_NAME',
  'APPINSTALLER_PATH','DIST_TARGET','APPX_URI_BASE','APPINSTALLER_NAME','RUN_APP_CERT_KIT'
)

# hard defaults (minimal; most defaults live in build.dist.ini)
$script:cfg = [ordered]@{
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

  # MSIX configs
  MSIX_PATH                 = ''       # auto-name later
  APPX_MANIFEST             = 'installer\msix\AppxManifest.xml'
  APPX_PUBLISHER            = 'CN=Jesse Greathouse'
  APPX_IDENTITY             = 'com.jesse.omni-irc-client'
  APPX_DISPLAY_NAME         = 'Omni IRC Client'
  APPX_PUBLISHER_DISPLAY_NAME = 'Jesse Greathouse'
  APPINSTALLER_PATH         = ''           # optional: emit .appinstaller
  DIST_TARGET               = 'store'      # 'store' | 'sideload'
  APPX_URI_BASE             = ''           # e.g. 'https://downloads.greathousetech.com/omni'
  APPINSTALLER_NAME         = 'omni-irc-client.appinstaller'
  RUN_APP_CERT_KIT          = '1'          # run App Certification Kit if found
}

# ───────────────────────── INI loader (safe) ───────────────────────────
function _Log([string]$msg) {
  $ts = (Get-Date).ToString('HH:mm:ss.fff')
  Write-Host "[$ts] $msg"
}

# Tracks where each key came from and what value it had when set.
if (-not $script:cfgMeta) { $script:cfgMeta = @{} }

# mode: 'overwrite' (always clobber), 'fill-only' (only set if empty)
function Load-IniAllowed {
  [CmdletBinding()]
  param(
    [Parameter(Mandatory)][string]$path,
    [Parameter(Mandatory)][System.Collections.IDictionary]$into
  )

  if (-not (Test-Path -LiteralPath $path)) { return }

  $allowed = $script:AllowedKeys
  foreach ($line in (Get-Content -LiteralPath $path -Encoding UTF8)) {
    if ($null -eq $line) { continue }
    $line = ($line -replace "^\uFEFF", '').Trim()
    if ($line -eq '' -or $line -match '^\s*[#;]') { continue }

    if ($line -match '^\s*([A-Z0-9_]+)\s*=\s*(.*)\s*$') {
      $k = $matches[1].Trim()
      $v = $matches[2].Trim()
      if ($allowed -contains $k) { $into[$k] = $v }
    }
  }
}

# ───────────────────────── load config (precedence) ─────────────────────
# dist -> local -> --config (path) -> ENV (fill-only) -> CLI
Load-IniAllowed $ConfigDist  $script:cfg
Load-IniAllowed $ConfigLocal $script:cfg
if ($Config) { Load-IniAllowed (Resolve-AbsPath $Config) $script:cfg }

# ENV snapshot (fill-only: do not clobber INI/CLI)
foreach ($k in $script:AllowedKeys) {
  if ($k -eq 'PLATFORM') { continue } # explicitly not taken from env
  $envVal = [Environment]::GetEnvironmentVariable($k, 'Process')
  if ($envVal -and [string]::IsNullOrWhiteSpace([string]$script:cfg[$k])) {
    $script:cfg[$k] = $envVal
  }
}

# CLI overrides (highest after --config)
if ($Switch)       { $script:cfg.SWITCH        = $Switch }
if ($BuildProfile) { $script:cfg.BUILD_PROFILE = $BuildProfile }
if ($MSYS2)        { $script:cfg.MSYS2         = $MSYS2 }
if ($OutDir)       { $script:cfg.OUT_DIR       = $OutDir }
if ($ExeName)      { $script:cfg.EXE_NAME      = $ExeName }
if ($Version)      { $script:cfg.VERSION       = $Version }
if ($Platform)     { $script:cfg.PLATFORM      = $Platform }
if ($Arch)         { $script:cfg.ARCH          = $Arch }

function Print-EffectiveConfig {
  Write-Host "---- effective config ----"
  foreach ($k in $script:AllowedKeys) {
    $v = $script:cfg[$k]
    if ($k -eq 'WIN_CERT_PFX_PASS' -and $null -ne $v -and "$v" -ne '') {
      $v = '****'
    }
    $val = if ($null -ne $v -and "$v" -ne '') { "$v" } else { '' }
    "{0,-24} = {1}" -f $k, $val
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
$Arch = Resolve-Arch $script:cfg.ARCH
$script:cfg.ARCH = $Arch
$WixArch = if ($Arch -eq 'arm64') { 'arm64' } elseif ($Arch -eq 'x86') { 'x86' } else { 'x64' }

$version = $script:cfg.VERSION
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
$script:cfg.VERSION = $version

# ───────────────────────── path setup ───────────────────────────────────
$OutDirAbs     = Resolve-AbsPath $script:cfg.OUT_DIR
$WixOutDirAbs  = Resolve-AbsPath ".dist\wix"
$FilesWxsAbs   = Join-Path $WixOutDirAbs "Files.wxs"
$ProductWxsAbs = Resolve-AbsPath "installer\wix\Product.wxs"

if ([string]::IsNullOrEmpty($script:cfg.ZIP_PATH)) {
  $script:cfg.ZIP_PATH = ".dist\omni-irc-client-$version-$($script:cfg.PLATFORM)-$Arch.zip"
}
if ([string]::IsNullOrEmpty($script:cfg.MSI_PATH)) {
  $script:cfg.MSI_PATH = ".dist\omni-irc-client-$version-$($script:cfg.PLATFORM)-$Arch.msi"
}
$ZipAbs = Resolve-AbsPath $script:cfg.ZIP_PATH
$MsiAbs = Resolve-AbsPath $script:cfg.MSI_PATH

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

function Test-SigningReady {
  # Returns $true if we have a usable signing identity, else $false (and prints one warning)
  if ($script:cfg.WIN_SIGN -ne '1') { return $false }

  $signtool = Resolve-SignTool
  if (-not $signtool) {
    Write-Warning "WIN_SIGN=1 but signtool.exe not found. Signing will be skipped."
    return $false
  }

  if ($script:cfg.WIN_CERT_PFX) {
    $pfx = (Resolve-Path $script:cfg.WIN_CERT_PFX -ErrorAction SilentlyContinue)?.Path
    if (-not $pfx -or -not (Test-Path $pfx)) {
      Write-Warning "WIN_SIGN=1 but WIN_CERT_PFX '$($script:cfg.WIN_CERT_PFX)' not found. Signing will be skipped."
      return $false
    }
    return $true
  }

  if ($script:cfg.WIN_CERT_SUBJECT) {
    try {
      $store = New-Object System.Security.Cryptography.X509Certificates.X509Store('My','CurrentUser')
      $store.Open('ReadOnly')
      $matches = $store.Certificates.Find(
        [System.Security.Cryptography.X509Certificates.X509FindType]::FindBySubjectDistinguishedName,
        $script:cfg.WIN_CERT_SUBJECT, $false
      )
      $store.Close()
      if ($matches.Count -gt 0) { return $true }
    } catch { }
    Write-Warning "WIN_SIGN=1 but no certificate with subject '$($script:cfg.WIN_CERT_SUBJECT)' was found in CurrentUser\My. Signing will be skipped."
    return $false
  }

  Write-Warning "WIN_SIGN=1 but neither WIN_CERT_PFX nor WIN_CERT_SUBJECT is set. Signing will be skipped."
  return $false
}

$script:_signingReady = $null  # cache
function Sign-File([string]$path) {
  if ($script:_signingReady -eq $null) { $script:_signingReady = (Test-SigningReady) }
  if (-not $script:_signingReady) { return }

  $signtool = Resolve-SignTool
  $ts = $script:cfg.WIN_TIMESTAMP_URL
  $args = @('sign','/fd','sha256','/tr', $ts, '/td','sha256')

  if ($script:cfg.WIN_CERT_PFX) {
    $pfx = (Resolve-Path $script:cfg.WIN_CERT_PFX).Path
    $args += @('/f', $pfx)
    if ($script:cfg.WIN_CERT_PFX_PASS) { $args += @('/p', $script:cfg.WIN_CERT_PFX_PASS) }
  } elseif ($script:cfg.WIN_CERT_SUBJECT) {
    $args += @('/n', $script:cfg.WIN_CERT_SUBJECT)
  }

  & $signtool @args $path | Out-Host
  if ($LASTEXITCODE -ne 0) {
    # Fail fast once to avoid spamming every file
    Write-Warning "Signing failed for '$path' (code $LASTEXITCODE). Further signing will be skipped."
    $script:_signingReady = $false
  }
}

function Sign-File([string]$path) {
  if ($script:cfg.WIN_SIGN -ne '1') { return }
  $signtool = Resolve-SignTool
  if (-not $signtool) { throw "WIN_SIGN=1 but signtool.exe not found. Install Windows SDK." }

  $ts = $script:cfg.WIN_TIMESTAMP_URL
  $args = @('sign','/fd','sha256','/tr', $ts, '/td','sha256')

  if ($script:cfg.WIN_CERT_PFX) {
    $pfx = Resolve-AbsPath $script:cfg.WIN_CERT_PFX
    if (-not (Test-Path $pfx)) { throw "PFX not found at $pfx" }
    $args += @('/f', $pfx)
    if ($script:cfg.WIN_CERT_PFX_PASS) { $args += @('/p', $script:cfg.WIN_CERT_PFX_PASS) }
  } elseif ($script:cfg.WIN_CERT_SUBJECT) {
    $args += @('/n', $script:cfg.WIN_CERT_SUBJECT)
  } else {
    throw "WIN_SIGN=1 but neither WIN_CERT_PFX nor WIN_CERT_SUBJECT is set."
  }

  & $signtool @args $path | Write-Host
}

function Resolve-MakeAppx {
  $candidates = @(
    "$env:ProgramFiles (x86)\Windows Kits\10\bin\x64\makeappx.exe",
    "$env:ProgramFiles\Windows Kits\10\bin\x64\makeappx.exe"
  )
  foreach ($p in $candidates) { if (Test-Path $p) { return $p } }
  $cmd = Get-Command makeappx.exe -ErrorAction SilentlyContinue
  if ($cmd) { return $cmd.Path }
  return $null
}

function Get-WinSdkVersionFromMakeAppx([string]$makeappxPath) {
  # e.g. ...\Windows Kits\10\bin\10.0.26100.0\x64\makeappx.exe -> 10.0.26100.0
  $m = $makeappxPath -replace '\\','/' -match '/bin/([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+)/'
  if ($m) { return $Matches[1] }
  return "10.0.0.0"
}

function Update-AppxManifest {
  param(
    [Parameter(Mandatory)] [string] $ManifestPath,
    [Parameter(Mandatory)] [string] $Version,
    [Parameter(Mandatory)] [string] $IdentityName,
    [Parameter(Mandatory)] [string] $Publisher,
    [Parameter(Mandatory)] [string] $DisplayName,
    [Parameter(Mandatory)] [string] $PublisherDisplayName,
    [string] $SdkVersion = "10.0.26100.0"
  )

  [xml]$xml = Get-Content -LiteralPath $ManifestPath

  # Always use the document element (root <Package/>)
  $pkg = [System.Xml.XmlElement]$xml.DocumentElement

  # Namespaces
  $nsFoundation = "http://schemas.microsoft.com/appx/manifest/foundation/windows10"
  $nsUap        = "http://schemas.microsoft.com/appx/manifest/uap/windows10"
  $nsRescap     = "http://schemas.microsoft.com/appx/manifest/foundation/windows10/restrictedcapabilities"

  # Ensure xmlns and IgnorableNamespaces
  if (-not $pkg.HasAttribute("xmlns:uap"))    { $pkg.SetAttribute("xmlns:uap",    $nsUap) }
  if (-not $pkg.HasAttribute("xmlns:rescap")) { $pkg.SetAttribute("xmlns:rescap", $nsRescap) }

  $ign = $pkg.GetAttribute("IgnorableNamespaces")
  $need = @("uap","rescap")
  foreach ($p in $need) {
    if (-not ($ign -split '\s+' | Where-Object { $_ -eq $p })) {
      $ign = (($ign, $p) -ne $null -and ($ign, $p) -ne "") -join ' '
      $ign = ($ign -split '\s+' | Where-Object { $_ -ne '' } | Select-Object -Unique) -join ' '
      $pkg.SetAttribute("IgnorableNamespaces", $ign)
    }
  }

  # Quick XPath helper with namespaces
  $nsmgr = New-Object System.Xml.XmlNamespaceManager($xml.NameTable)
  $nsmgr.AddNamespace("f", $nsFoundation)
  $nsmgr.AddNamespace("uap", $nsUap)
  $nsmgr.AddNamespace("rescap", $nsRescap)

  # Identity + Properties
  $id = $pkg.SelectSingleNode("f:Identity", $nsmgr)
  if (-not $id) {
    $id = $xml.CreateElement("Identity", $nsFoundation)
    $pkg.AppendChild($id) | Out-Null
  }
  $id.SetAttribute("Name",      $IdentityName)
  $id.SetAttribute("Publisher", $Publisher)
  $id.SetAttribute("Version",   "$Version.0")

  $props = $pkg.SelectSingleNode("f:Properties", $nsmgr)
  if (-not $props) {
    $props = $xml.CreateElement("Properties", $nsFoundation)
    $pkg.AppendChild($props) | Out-Null
  }
  # These are simple elements, not attributes:
  $disp = $props.SelectSingleNode("f:DisplayName", $nsmgr)
  if (-not $disp) { $disp = $xml.CreateElement("DisplayName", $nsFoundation); $props.AppendChild($disp) | Out-Null }
  $disp.InnerText = $DisplayName

  $pdisp = $props.SelectSingleNode("f:PublisherDisplayName", $nsmgr)
  if (-not $pdisp) { $pdisp = $xml.CreateElement("PublisherDisplayName", $nsFoundation); $props.AppendChild($pdisp) | Out-Null }
  $pdisp.InnerText = $PublisherDisplayName

  # Dependencies / TargetDeviceFamily (Desktop)
  $deps = $pkg.SelectSingleNode("f:Dependencies", $nsmgr)
  if (-not $deps) {
    $deps = $xml.CreateElement("Dependencies", $nsFoundation)
    $pkg.AppendChild($deps) | Out-Null
  }
  $tdf = $deps.SelectSingleNode("f:TargetDeviceFamily[@Name='Windows.Desktop']", $nsmgr)
  if (-not $tdf) {
    $tdf = $xml.CreateElement("TargetDeviceFamily", $nsFoundation)
    $tdf.SetAttribute("Name","Windows.Desktop")
    $deps.AppendChild($tdf) | Out-Null
  }
  if (-not $tdf.GetAttribute("MinVersion"))       { $tdf.SetAttribute("MinVersion","10.0.0.0") }
  if (-not $tdf.GetAttribute("MaxVersionTested")) { $tdf.SetAttribute("MaxVersionTested",$SdkVersion) }

  # Capabilities + runFullTrust
  $caps = $pkg.SelectSingleNode("f:Capabilities", $nsmgr)
  if (-not $caps) {
    $caps = $xml.CreateElement("Capabilities", $nsFoundation)
    $pkg.AppendChild($caps) | Out-Null
  }
  $runFT = $caps.SelectSingleNode("rescap:Capability[@Name='runFullTrust']", $nsmgr)
  if (-not $runFT) {
    $runFT = $xml.CreateElement("Capability", $nsRescap)
    # prefix for readability in the saved XML (optional)
    $runFT = [System.Xml.XmlElement]$caps.AppendChild($runFT)
    $runFT.SetAttribute("Name","runFullTrust") | Out-Null
  }

  $xml.Save($ManifestPath)
}

# ───────────────────────── WiX resolver ─────────────────────────────────
function Resolve-WixExe {
  if ($script:cfg.WIX_EXE) {
    $p = Resolve-AbsPath $script:cfg.WIX_EXE
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

# ------------------------- MSIX Helpers ------------------------------------------
function Ensure-FourPartVersion([string]$v) {
  # MSIX requires A.B.C.D ; you pass 0.1.18 -> produce 0.1.18.0
  $parts = $v.Split('.')
  while ($parts.Count -lt 4) { $parts += '0' }
  return ($parts[0..3] -join '.')
}

function Resolve-AppCertKit {
  $cands = @(
    "$env:ProgramFiles (x86)\Windows Kits\10\App Certification Kit\appcert.exe",
    "$env:ProgramFiles\Windows Kits\10\App Certification Kit\appcert.exe"
  )
  foreach ($p in $cands) { if (Test-Path $p) { return $p } }
  $cmd = Get-Command appcert.exe -ErrorAction SilentlyContinue
  if ($cmd) { return $cmd.Path }
  return $null
}

function Validate-MSIXSignature([string]$msixPath) {
  $signtool = Resolve-SignTool
  if (-not $signtool) { Write-Warning "signtool.exe not found; skipping signature verification."; return }
  Write-Host ">> Verifying MSIX signature"
  & $signtool verify /pa /v $msixPath | Write-Host
}

function Run-AppCertificationKit([string]$msixPath) {
  if ($script:cfg.RUN_APP_CERT_KIT -ne '1') { return }
  $ack = Resolve-AppCertKit
  if (-not $ack) { Write-Warning "App Certification Kit (appcert.exe) not found; skipping WACK tests."; return }
  $out = Join-Path ([System.IO.Path]::GetDirectoryName($msixPath)) "appcert-results.xml"
  Write-Host ">> Running Windows App Certification Kit"
  # /reportoutput wants a path; use XML for easy artifacting.
  & $ack test -packagepath $msixPath -reportoutput $out | Write-Host
  Write-Host ">> AppCertKit report: $out"
}

function New-AppInstallerFile([string]$appinstallerPath, [string]$version4, [string]$name, [string]$publisher, [string]$baseUri, [string]$msixFileName) {
  if ([string]::IsNullOrWhiteSpace($baseUri)) {
    Write-Warning "APPX_URI_BASE not set; skipping .appinstaller emission."
    return
  }
  $xml = @"
<?xml version="1.0" encoding="utf-8"?>
<AppInstaller
    xmlns="http://schemas.microsoft.com/appx/appinstaller/2018"
    Uri="$baseUri/$([System.IO.Path]::GetFileName($appinstallerPath))"
    Version="$version4">
  <MainPackage
      Name="$name"
      Publisher="$publisher"
      Version="$version4"
      Uri="$baseUri/$msixFileName" />
  <UpdateSettings>
    <OnLaunch HoursBetweenUpdateChecks="24"/>
  </UpdateSettings>
</AppInstaller>
"@
  $dir = Split-Path $appinstallerPath -Parent
  if (-not (Test-Path $dir)) { New-Item -ItemType Directory -Force $dir | Out-Null }
  Set-Content -LiteralPath $appinstallerPath -Encoding UTF8 -Value $xml
  Write-Host ">> AppInstaller created: $appinstallerPath"
}

function Ensure-Dir([string]$path) {
  if (-not (Test-Path -LiteralPath $path)) {
    New-Item -ItemType Directory -Force -Path $path | Out-Null
  }
}

# Minimal PNG generator for placeholders (solid bg + centered text)
function New-PlaceholderPng {
  param(
    [Parameter(Mandatory)][string]$Path,
    [Parameter(Mandatory)][object]$Width,
    [Parameter(Mandatory)][object]$Height,
    [string]$Bg = '#2D2D2D',
    [string]$Fg = '#FFFFFF',
    [string]$Text = 'Omni'
  )

  # Coerce to scalar ints (handles arrays/strings)
  $w = [int](@($Width)[0])
  $h = [int](@($Height)[0])
  if ($w -le 0 -or $h -le 0) { throw "Invalid image size: ${w}x${h}" }

  # Float math everywhere division happens
  $area     = [double]$w * [double]$h
  $fontSize = [int]([Math]::Max(([Math]::Sqrt($area) / 6.0), 10.0))

  Add-Type -AssemblyName System.Drawing
  $bmp = New-Object System.Drawing.Bitmap($w, $h)
  $g   = [System.Drawing.Graphics]::FromImage($bmp)
  try {
    $bgc = [System.Drawing.ColorTranslator]::FromHtml($Bg)
    $fgc = [System.Drawing.ColorTranslator]::FromHtml($Fg)
    $g.Clear($bgc)

    $font = New-Object System.Drawing.Font('Segoe UI', $fontSize, [System.Drawing.FontStyle]::Bold)
    $sz   = $g.MeasureString($Text, $font)

    # Force floats for the centers (avoid accidental array ops)
    $cx = [float]( ( [double]$w - [double]$sz.Width  ) / 2.0 )
    $cy = [float]( ( [double]$h - [double]$sz.Height ) / 2.0 )
    $pt = New-Object System.Drawing.PointF($cx, $cy)

    Ensure-Dir (Split-Path -Parent $Path)
    $brush = New-Object System.Drawing.SolidBrush($fgc)
    $g.DrawString($Text, $font, $brush, $pt)
    $bmp.Save($Path, [System.Drawing.Imaging.ImageFormat]::Png)
  }
  finally {
    $g.Dispose(); $bmp.Dispose()
  }
}

# Ensure the MSIX Assets folder + a sane set of default images.
function Ensure-MsixAssets([string]$assetsDir) {
  Ensure-Dir $assetsDir

  $required = @(
    @{ name='Square44x44Logo.png';    w=44;  h=44  },
    @{ name='Square150x150Logo.png';  w=150; h=150 },
    @{ name='Wide310x150Logo.png';    w=310; h=150 },
    @{ name='Square310x310Logo.png';  w=310; h=310 },
    @{ name='StoreLogo.png';          w=50;  h=50  },
    @{ name='SplashScreen.png';       w=620; h=300 }
  )

  foreach ($r in $required) {
    $p = Join-Path $assetsDir $r.name
    if (-not (Test-Path -LiteralPath $p)) {
      # Force scalars here
      $w = [int](@($r.w)[0])
      $h = [int](@($r.h)[0])
      New-PlaceholderPng -Path $p -Width $w -Height $h -Text 'Omni'
    }
  }
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

Write-Host ">> Activating opam switch: $($script:cfg.SWITCH) (root: $OpamRoot)"
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
  if (-not (($have -split '\r?\n') -contains $script:cfg.SWITCH)) {
    Write-Host "  -> creating opam switch $($script:cfg.SWITCH)"
    $candidates = @(
      'ocaml-compiler.5.3.0,system-mingw,ocaml-env-mingw64,mingw-w64-shims',
      'ocaml-compiler.5.2.1,system-mingw,ocaml-env-mingw64,mingw-w64-shims',
      'ocaml-base-compiler.5.2.1,system-mingw,ocaml-env-mingw64,mingw-w64-shims'
    )
    $created = $false
    foreach ($spec in $candidates) {
      Write-Host "     - trying: $spec"
      & $opam switch create --root "$OpamRoot" $script:cfg.SWITCH --packages $spec | Out-Host
      $envLines = & $opam env --root "$OpamRoot" --switch $script:cfg.SWITCH --set-switch
      $switchBin = Ensure-OpamSwitchBinReady -OpamExe $opam -OpamRoot $OpamRoot -Switch $script:cfg.SWITCH
      Write-Host ">> switch bin ready: $switchBin"
      if ($LASTEXITCODE -eq 0) { $created = $true; break }
    }
    if (-not $created) {
      throw "Could not create switch '$($script:cfg.SWITCH)' with any candidate package set."
    }
  }
} catch {
  throw "Switch creation failed: $($_.Exception.Message)"
}

# Apply env for THIS process only
$envLines = & $opam env --root "$OpamRoot" --switch $script:cfg.SWITCH --set-switch
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
  $switchBin = (& $opam var --root "$OpamRoot" --switch $script:cfg.SWITCH bin).Trim()
  if (-not (Test-Path $switchBin)) { throw "opam var bin returned '$switchBin' which doesn't exist." }

  # Ensure dune + build deps are present in the switch
  Ensure-OpamPackages -OpamExe $opam -OpamRoot $OpamRoot -Switch $script:cfg.SWITCH -SkipUpdate

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
  Write-Host ">> Building with dune ($($script:cfg.BUILD_PROFILE))"
  & $dune build --profile $script:cfg.BUILD_PROFILE bin/omni.exe

  $candidate = Join-Path $RepoRoot "_build\default\bin\omni.exe"
  if (-not (Test-Path $candidate)) {
    # Only retry if the first build failed
    & $dune build --profile $script:cfg.BUILD_PROFILE bin/omni.exe
  }
  $built = if (Test-Path $candidate) { $candidate } else { $null }

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
  Write-Host ">> Staging output -> $(Resolve-AbsPath $script:cfg.OUT_DIR)"
  Remove-Item -Recurse -Force $OutDirAbs -ErrorAction SilentlyContinue
  New-Item -ItemType Directory -Force $OutDirAbs | Out-Null
  Copy-Item $built (Join-Path $OutDirAbs $script:cfg.EXE_NAME)

  # Dependency harvest via ntldd (MSYS2 UCRT64)
  $ntlddRoot = if ($script:cfg.MSYS2) { $script:cfg.MSYS2 } else { if ($env:MSYS2) { $env:MSYS2 } else { 'C:\msys64' } }
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
  if ($script:cfg.WIN_SIGN -eq '1') {
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
  Write-Host "   Unzip and run: $OutDirAbs\$($script:cfg.EXE_NAME)"

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
  if ($script:cfg.WIN_SIGN -eq '1') {
    Write-Host ">> Signing MSI"
    Sign-File $MsiAbs
  }

  Write-Host ">> MSI created: $MsiAbs"
}
finally {
  $env:Path = $oldPath
  Write-Host ">> PATH restored"
}

# ---------- MSIX build ----------
if ([string]::IsNullOrEmpty($script:cfg.MSIX_PATH)) {
  $script:cfg.MSIX_PATH = ".dist\omni-irc-client-$version-$($script:cfg.PLATFORM)-$Arch.msix"
}
$MsixAbs  = Resolve-AbsPath $script:cfg.MSIX_PATH
$version4 = Ensure-FourPartVersion $version

function Step([string]$name, [scriptblock]$action) {
  $sw = [System.Diagnostics.Stopwatch]::StartNew()
  Write-Host ">> $name ..."
  try {
    & $action
    Write-Host ("   OK [{0:n1}s]" -f ($sw.Elapsed.TotalSeconds))
  } catch {
    Write-Host ("   FAIL [{0:n1}s]" -f ($sw.Elapsed.TotalSeconds))
    throw
  }
}

$makeappx = Resolve-MakeAppx
if (-not $makeappx) {
  Write-Warning @"
Windows SDK MakeAppx.exe not found.
Install 'Windows 10/11 SDK' and re-run.
Skipping MSIX build.
"@
} else {
  Write-Host ">> MakeAppx: $makeappx"

  # ---- MSIX build: safer copy that avoids recursion when OUT_DIR=.dist ----
  $pkgTemp     = Join-Path $RepoRoot ".dist\msix\payload"
  $manifestSrc = Resolve-AbsPath $script:cfg.APPX_MANIFEST
  $assetsSrc   = Join-Path (Split-Path $manifestSrc -Parent) "Assets"

  # Clean payload
  Remove-Item -LiteralPath $pkgTemp -Recurse -Force -ErrorAction SilentlyContinue
  New-Item -ItemType Directory -Force -Path $pkgTemp | Out-Null

  # Copy ONLY payload from OUT_DIR, excluding msix/wix + non-runtime files
  $excludeDirNames = @('msix','wix')
  $excludeExt      = @('.zip','.msi','.msix','.appinstaller','.wixpdb')

  Get-ChildItem -LiteralPath $OutDirAbs -Force | ForEach-Object {
    if ($_.PSIsContainer) {
      if ($excludeDirNames -contains $_.Name.ToLower()) { return }
      Copy-Item -LiteralPath $_.FullName `
                -Destination (Join-Path $pkgTemp $_.Name) `
                -Recurse -Force -ErrorAction Stop
    } else {
      if ($excludeExt -contains $_.Extension.ToLower()) { return }
      Copy-Item -LiteralPath $_.FullName -Destination $pkgTemp -Force -ErrorAction Stop
    }
  }

  $manifestDst = Join-Path $pkgTemp "AppxManifest.xml"
  $assetsDst   = Join-Path $pkgTemp "Assets"

  Step "Copy manifest" {
    Copy-Item -Path $manifestSrc -Destination $manifestDst -Force -ErrorAction Stop
  }

  Step "Create + copy Assets" {
    Ensure-Dir $assetsDst
    Copy-Item -Path (Join-Path $assetsSrc '*') -Destination $assetsDst -Recurse -Force -ErrorAction Stop
    $n = (Get-ChildItem -Path $assetsDst -File | Measure-Object).Count
    Write-Host "   Assets copied: $n"
  }

  Step "Stamp manifest identity + version" {
    $sdkVer = Get-WinSdkVersionFromMakeAppx $makeappx
    Update-AppxManifest -ManifestPath $manifestDst `
      -Version $version `
      -IdentityName $script:cfg.APPX_IDENTITY `
      -Publisher $script:cfg.APPX_PUBLISHER `
      -DisplayName $script:cfg.APPX_DISPLAY_NAME `
      -PublisherDisplayName $script:cfg.APPX_PUBLISHER_DISPLAY_NAME `
      -SdkVersion $sdkVer
  }

  Step "Prepare MSIX output path" {
    $msixDir = Split-Path $MsixAbs -Parent
    if (-not (Test-Path $msixDir)) { New-Item -ItemType Directory -Force $msixDir | Out-Null }
    if (Test-Path $MsixAbs) { Remove-Item $MsixAbs -Force -ErrorAction Stop }
  }

  Write-Host ">> Packing MSIX (verbose) -> $MsixAbs"
  & $makeappx pack /v /o /h SHA256 /d $pkgTemp /p $MsixAbs 2>&1 | Out-Host
  if ($LASTEXITCODE -ne 0 -or -not (Test-Path $MsixAbs)) {
    throw "MakeAppx failed with code $LASTEXITCODE. See log above."
  }
  Write-Host ">> MSIX packed: $MsixAbs"

  # Distribution flow (unchanged, just runs after pack/validate)
  $target = ($script:cfg.DIST_TARGET ?? 'store').ToLowerInvariant()
  switch ($target) {
    'store' {
      Write-Host ">> DIST_TARGET=store (no local signing required)."
      Run-AppCertificationKit -msixPath $MsixAbs
    }
    'sideload' {
      Write-Host ">> DIST_TARGET=sideload (will sign MSIX locally)."
      if ($script:cfg.WIN_SIGN -ne '1') {
        Write-Warning "WIN_SIGN=0; sideload installs will fail. Enable signing or switch DIST_TARGET to 'store'."
      } else {
        Write-Host ">> Signing MSIX for sideload"
        Sign-File $MsixAbs
        Validate-MSIXSignature $MsixAbs
      }
      if ($script:cfg.APPX_URI_BASE) {
        $appinstallerAbs = if ($script:cfg.APPINSTALLER_PATH) { Resolve-AbsPath $script:cfg.APPINSTALLER_PATH }
                           else { Join-Path (Split-Path $MsixAbs -Parent) $script:cfg.APPINSTALLER_NAME }
        New-AppInstallerFile -appinstallerPath $appinstallerAbs `
                             -version4 $version4 `
                             -name $script:cfg.APPX_IDENTITY `
                             -publisher $script:cfg.APPX_PUBLISHER `
                             -baseUri $script:cfg.APPX_URI_BASE `
                             -msixFileName (Split-Path $MsixAbs -Leaf)
      }
      Run-AppCertificationKit -msixPath $MsixAbs
    }
    default {
      Write-Warning "Unknown DIST_TARGET '$target'. Treating as 'store'."
      Run-AppCertificationKit -msixPath $MsixAbs
    }
  }

  Write-Host ">> MSIX created: $MsixAbs"
}
