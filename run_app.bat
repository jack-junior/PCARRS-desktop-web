@echo off
set "R_BIN=%~dp0R-Portable\bin\Rscript.exe"

:: On lance R en mode silencieux
start /b "" "%R_BIN%" "%~dp0launch.R"
exit