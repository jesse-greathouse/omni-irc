Param(
  [Parameter(Mandatory=$true)]
  [string]$Version,                       # e.g. 0.1.16
  [string]$Branch = "main",               # branch to push
  [string]$Remote = "origin",             # remote to push
  [string]$NotesFile = "",                # optional: path to a notes file
  [switch]$Draft,                         # create as draft
  [switch]$Prerelease,                    # mark as prerelease
  [switch]$NoChangesEdit                  # don't touch CHANGES.md
)

$ErrorActionPreference = "Stop"

function Require-CleanGit {
  $status = git status --porcelain
  if ($status) {
    throw "Working tree is not clean. Commit or stash changes first."
  }
}

function Update-DuneProjectVersion([string]$ver) {
  if (-not (Test-Path "dune-project")) { throw "dune-project not found." }
  $orig = Get-Content "dune-project" -Raw
  $new  = $orig -replace '^\(version\s+([^)]+)\)', "(version $ver)"
  if ($orig -ne $new) {
    Set-Content "dune-project" $new -Encoding UTF8
  } else {
    Write-Host "dune-project already at $ver (or version stanza not found)."
  }
}

function Prepend-Changes([string]$ver) {
  if ($NoChangesEdit) { return }
  $file = "CHANGES.md"
  $today = (Get-Date).ToString("yyyy-MM-dd")
  $header = "## v$ver ($today)`n`n- Release $ver.`n"
  if (Test-Path $file) {
    $existing = Get-Content $file -Raw
    Set-Content $file ($header + "`n" + $existing) -Encoding UTF8
  } else {
    Set-Content $file $header -Encoding UTF8
  }
}

function Build-Artifacts([string]$ver) {
  # Uses your existing packager (makes .zip + MSI)
  powershell -ExecutionPolicy Bypass -File .\bin\package-win.ps1

  $zip = ".dist\omni-irc-win64.zip"
  $msi = ".dist\omni-irc-client-$ver-windows-x64.msi"

  if (-not (Test-Path $zip)) { throw "Missing artifact: $zip" }
  if (-not (Test-Path $msi)) { throw "Missing artifact: $msi" }

  return ,@($zip, $msi)
}

function Create-Release([string]$ver, [string[]]$assets) {
  # Build gh args
  $args = @("release","create","v$ver")
  $args += $assets
  $args += @("--title","omni-irc v$ver")

  if ($NotesFile -and (Test-Path $NotesFile)) {
    $args += @("--notes-file", $NotesFile)
  } else {
    # fallback: quick note
    $args += @("--notes", "Release v$ver — see CHANGES.md for details.")
  }
  if ($Draft)      { $args += "--draft" }
  if ($Prerelease) { $args += "--prerelease" }

  & gh @args
}

# --- main ---
Require-CleanGit

Write-Host ">> Bumping to version $Version"
Update-DuneProjectVersion $Version
Prepend-Changes $Version

git add -A
git commit -m "Release v$Version"

git tag -a "v$Version" -m "Release v$Version"
git push $Remote $Branch
git push $Remote "v$Version"

Write-Host ">> Building artifacts for v$Version"
$assets = Build-Artifacts $Version

Write-Host ">> Creating GitHub release v$Version"
Create-Release $Version $assets

Write-Host ">> Done. Published v$Version with assets:"
$assets | ForEach-Object { Write-Host "   - $_" }
