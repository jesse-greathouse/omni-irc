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

# 1) (No source edits) — build artifacts for the provided Version
Write-Host ">> Building artifacts for v$Version"
$pkg = Join-Path $repoRoot "bin\package-win.ps1"
& $pkg `
  -Switch $Switch `
  -BuildProfile $BuildProfile `
  -MSYS2 $MSYS2 `
  -Version $Version

# 2) Tag & push (idempotent)
$branch = (& git rev-parse --abbrev-ref HEAD).Trim()
if (-not $branch) { $branch = "main" }

# Create/replace annotated tag
& git rev-parse -q --verify "refs/tags/v$Version" *> $null
$tagExists = ($LASTEXITCODE -eq 0)

if ($tagExists) {
  Write-Warning "Tag v$Version already exists; recreating."
  git tag -d "v$Version" | Out-Null
}
git tag -a "v$Version" -m "v$Version"

# No code changes were made; pushing branch is harmless but can be skipped if desired
git push origin $branch
git push --force origin "v$Version"

# 3) Collect ALL .dist artifacts that match: omni-irc-client-{Version}-{Platform}-{Arch}.{ext}
$distDir = Join-Path $repoRoot ".dist"
if (-not (Test-Path $distDir)) {
  throw "Missing artifacts directory: $distDir"
}

# Build regex using the exact Version, anchor to start/end, accept any non-empty platform/arch and any extension
$verEsc = [Regex]::Escape($Version)
$artifactPattern = "^(?i:omni-irc-client)-$verEsc-[^-]+-[^.]+\..+$"

$assets = @()
Get-ChildItem -Path $distDir -File | ForEach-Object {
  if ($_.Name -match $artifactPattern) {
    $assets += $_.FullName
  }
}

if ($assets.Count -eq 0) {
  throw "No artifacts matched pattern '$artifactPattern' in $distDir"
}

Write-Host ">> Attaching the following artifact(s):"
$assets | ForEach-Object { Write-Host "   - $_" }

# 4) Create or update GitHub release
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
