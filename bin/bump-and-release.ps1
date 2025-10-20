Param(
  [Parameter(Mandatory)]
  [string]$Version,

  [string]$Switch       = "omni-irc-dev",
  [string]$BuildProfile = "release",
  [string]$MSYS2        = "C:\msys64",

  # Owner/Repo for gh release
  [string]$RepoSlug     = "jesse-greathouse/omni-irc"
)

$ErrorActionPreference = "Stop"

# ---------- Helpers ----------
function Get-RepoRoot {
  # This script lives in .../omni-irc/bin/bump-and-release.ps1
  return (Split-Path -Parent $PSScriptRoot)
}

function Update-DuneProjectVersion([string]$ver) {
  if (-not (Test-Path "dune-project")) { throw "dune-project not found." }
  $orig = Get-Content "dune-project" -Raw

  # Try to replace existing (version ...) first (multiline)
  $new  = [Regex]::Replace($orig, '(?m)^\(version\s+([^)]+)\)', "(version $ver)")

  if ($orig -ne $new) {
    Set-Content "dune-project" $new -Encoding UTF8
    return $true
  }

  # If there was no (version ...) stanza, insert after (name ...)
  $ins = $orig -replace '(?m)^(\(name\s+[^\)]+\)\s*)', "`$1(version $ver)`r`n`r`n"
  if ($ins -ne $orig) {
    Set-Content "dune-project" $ins -Encoding UTF8
    return $true
  }

  Write-Host "No '(version ...)' stanza found and no '(name ...)' anchor to insert under; leaving dune-project unchanged."
  return $false
}

function Read-DuneProjectVersion {
  $dp = Get-Content "dune-project" -Raw
  $m = [Regex]::Match($dp, '(?m)^\(version\s+([^)]+)\)')
  if (-not $m.Success) { throw "Unable to read version from dune-project after update." }
  return $m.Groups[1].Value.Trim()
}

function Ensure-GitHubCLI {
  if (-not (Get-Command gh -ErrorAction SilentlyContinue)) {
    throw "GitHub CLI (gh) not found. Install from https://cli.github.com/ and authenticate with `gh auth login`."
  }
  # Help users who installed WiX as dotnet tool reach wix.exe in child script
  $dotnetToolDir = Join-Path $env:USERPROFILE ".dotnet\tools"
  if (Test-Path (Join-Path $dotnetToolDir "wix.exe")) {
    if (-not ($env:Path -split ';' | Where-Object { $_ -eq $dotnetToolDir })) {
      $env:Path = "$dotnetToolDir;$env:Path"
    }
  }
}

# ---------- Main ----------
$repoRoot = Get-RepoRoot
Set-Location $repoRoot

Ensure-GitHubCLI

# 1) Bump version in dune-project (if needed)
Write-Host ">> Bumping to version $Version"
$changed = Update-DuneProjectVersion $Version
$dpVer   = Read-DuneProjectVersion
if ($dpVer -ne $Version) { throw "Version mismatch: dune-project has '$dpVer' but requested '$Version'." }

# 2) Commit (only if dune-project changed)
if ($changed) {
  git add dune-project
  git commit -m "Release v$Version" | Out-Null
} else {
  Write-Host "No dune-project changes to commit."
}

# 3) Tag & push
$branch = (& git rev-parse --abbrev-ref HEAD).Trim()
if (-not $branch) { $branch = "main" }

# Create/replace annotated tag (idempotent re-runs)
& git rev-parse -q --verify "refs/tags/v$Version" *> $null
$tagExists = ($LASTEXITCODE -eq 0)

if ($tagExists) {
  Write-Warning "Tag v$Version already exists; recreating."
  git tag -d "v$Version" | Out-Null
}
git tag -a "v$Version" -m "v$Version"

git push origin $branch
git push --force origin "v$Version"

# 4) Build artifacts (ZIP + MSI)
Write-Host ">> Building artifacts for v$Version"
$pkg = Join-Path $repoRoot "bin\package-win.ps1"
& $pkg `
  -Switch $Switch `
  -BuildProfile $BuildProfile `
  -MSYS2 $MSYS2 `
  -Version $Version

# 5) Collect artifacts (note: ZIP now includes version in filename)
$zip = Join-Path $repoRoot (".dist\omni-irc-win64-{0}.zip" -f $Version)
$msi = Join-Path $repoRoot (".dist\omni-irc-client-{0}-windows-x64.msi" -f $Version)

if (-not (Test-Path $zip)) { throw "Missing artifact: $zip (packager should have created it)" }

$assets = @($zip)
if (Test-Path $msi) {
  $assets += $msi
} else {
  Write-Warning "MSI not found at $msi. Will publish release with ZIP only."
}

# 6) Create or update GitHub release
$tag   = "v$Version"
$notes = "Release $tag"

# Try to pull the top section for this version from CHANGES.md (optional best effort)
$changes = Join-Path $repoRoot "CHANGES.md"
if (Test-Path $changes) {
  try {
    $raw = Get-Content $changes
    $start = ($raw | Select-String -Pattern "^\s*##\s*v?$([regex]::Escape($Version))\b").LineNumber
    if ($start) {
      $endIdx = ($raw[$start..($raw.Count-1)] | Select-String -Pattern "^\s*##\s*v?\d").LineNumber | Select-Object -Skip 1 -First 1
      if ($endIdx) {
        $slice = $raw[($start-1)..($endIdx-2)]
      } else {
        $slice = $raw[($start-1)..($raw.Count-1)]
      }
      $notes = ($slice -join "`n")
    }
  } catch { }
}

# Try create; if it exists, upload assets with clobber
$created = $true
try {
  gh release create $tag --repo $RepoSlug --title "omni-irc $tag" --notes $notes @assets
} catch {
  $created = $false
  Write-Warning "Release $tag may already exist. Attempting to upload assets only."
}

if (-not $created) {
  foreach ($a in $assets) {
    gh release upload $tag $a --repo $RepoSlug --clobber
  }
}

Write-Host ">> Release $tag published with asset(s):"
$assets | ForEach-Object { Write-Host "   - $_" }
