# WiX MSI

We use WiX Toolset v4 to wrap the portable Windows build into an MSI.

## Local build

1. Install WiX v4:
   - `winget install WixToolset.WixToolset`
   - or `choco install wix-toolset --version=4.0.4` (example)

2. Run the packager (produces ZIP + MSI):

   ```powershell
   powershell -ExecutionPolicy Bypass -File .\bin\package-win.ps1
