$rootDirectory = "C:\code\advent2024"

1..24 | ForEach-Object {
    $dayFolderName = "day_$_"
    New-Item -Path $rootDirectory -Name "${dayFolderName}" -ItemType Directory
    $srcFolderPath = Join-Path -Path $rootDirectory -ChildPath $dayFolderName
    cd $srcFolderPath 
    dotnet new xunit -lang F#
    dotnet new sln    
    dotnet sln add $dayFolderName".fsproj"
    del Tests.fs
    copy ..\Program.fs .
    New-Item -Path $srcFolderPath -Name "input1.txt" -ItemType File
    New-Item -Path $srcFolderPath -Name "input2.txt" -ItemType File
    copy ..\template.v3.ncrunchsolution $dayFolderName".v3.ncrunchsolution"
} 
