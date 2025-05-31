$CONFIG = "Release"
$BUILD_DIR = "builds\Demeton.Console"
$SRTM_CACHE = "$PWD\samples\cache"

Remove-Item -Recurse -Force "$BUILD_DIR"

Write-Host "`nBUILDING THE CODE..."

dotnet build --configuration $CONFIG --verbosity minimal --no-incremental
if ($LASTEXITCODE -ne 0)
{
    exit $LASTEXITCODE
}

Write-Host "`nRUNNING THE TESTS..."

dotnet test --configuration $CONFIG --verbosity minimal --filter "Category!=acceptance&Category!=slow"
if ($LASTEXITCODE -ne 0)
{
    exit $LASTEXITCODE
}

# commented out the linting since fsharplint reports assembly loading error for some reason
#Write-Host "RUNNING THE F# LINT..."
#
#dotnet dotnet-fsharplint.exe -sol Demeton.sln
#if ($LASTEXITCODE -ne 0)
#{
#    exit $LASTEXITCODE
#}

Write-Host "`nMAKING THE CONSOLE PACKAGE..."

dotnet publish Demeton.Console -c $CONFIG --output "$PWD\$BUILD_DIR"
if ($LASTEXITCODE -ne 0)
{
    exit $LASTEXITCODE
}

Write-Host "`nBUILD SUCCESSFUL"