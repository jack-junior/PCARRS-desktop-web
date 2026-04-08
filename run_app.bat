@echo off
title GeneActiv Analysis Suite Launcher
echo Starting PCARRS Desktop App...
echo ------------------------------------------

:: Définit le chemin vers le R-Portable
set "R_BIN=%~dp0R-Portable\bin\Rscript.exe"

:: Vérifie si R-Portable existe
if not exist "%R_BIN%" (
    echo ERROR: R-Portable folder not found!
    echo Please make sure the R-Portable folder is in the root directory.
    pause
    exit
)

:: Lance le script launch.R
"%R_BIN%" "%~dp0launch.R"

if %ERRORLEVEL% neq 0 (
    echo.
    echo Application stopped with an error.
    pause
)