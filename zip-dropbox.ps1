$CONFIG = "Release"
$BUILD_DIR = "builds\Demeton.Console"

$zipName = "Demeton.zip"
$DEST_DIR = "D:\Dropbox\Public\XCTracer"

if (Test-Path -Path $BUILD_DIR)
{
    $filesToZip = Join-Path -Path $BUILD_DIR -ChildPath "*"

    Compress-Archive -Path $filesToZip -DestinationPath "$DEST_DIR\$zipName" -Force
    Write-Host "Successfully created archive: $DEST_DIR\$zipName"
}
else
{
    Write-Error "Error: Source directory '$BUILD_DIR' does not exist."
}