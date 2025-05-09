Write-Output "$(Get-ChildItem *_*.tar.gz -Name)"
if (-not $Env:R_RCPLOT_DEPLOY_PATH) {
    Write-Error "The required environment variable R_RCPLOT_DEPLOY_PATH has not been set"
    exit
}
Write-Output "$(Get-Location)\$(Get-ChildItem *_*.tar.gz -Name)"
Remove-Item -Path "$Env:R_RCPLOT_DEPLOY_PATH\$(Get-ChildItem *_*.tar.gz -Name)" -Force
Copy-Item -Path "$(Get-Location)\$(Get-ChildItem *_*.tar.gz -Name)" -Destination "$Env:R_RCPLOT_DEPLOY_PATH" -Force
$runscript = "$(Get-Location)\PsScripts\start_R_Process.ps1"
& $runscript
Write-Output "Deploy Complete!"
exit
