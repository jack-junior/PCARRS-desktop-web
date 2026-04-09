@echo off
setlocal enabledelayedexpansion

:: %~dp0 désigne le dossier contenant le fichier .bat
set "APP_DIR=%~dp0"
set "R_BIN=%APP_DIR%R-Portable\bin\Rscript.exe"

:: 1. VERIFICATION : Est-ce que l'app tourne déjà ?
:: On cherche si un processus Rscript.exe est actif
tasklist /FI "IMAGENAME eq Rscript.exe" 2>NUL | find /I /N "Rscript.exe">NUL
if "%ERRORLEVEL%"=="0" (
    echo [INFO] PCARRS Suite is already running.
    echo [INFO] Please check your browser or Task Manager.
    timeout /t 3
    exit
)

:: 2. VERIFICATION : Est-ce que R-Portable est là ?
if not exist "%R_BIN%" (
    echo ERREUR CRITIQUE : R-Portable introuvable.
    echo Chemin tente : "%R_BIN%"
    echo.
    echo Veuillez verifier que le dossier 'R-Portable' est bien present 
    echo dans le dossier d'installation.
    pause
    exit
)

:: 3. LANCEMENT
:: On lance en mode arriere-plan sans fenetre de commande persistante
start /b "" "%R_BIN%" "%APP_DIR%launch.R"

exit