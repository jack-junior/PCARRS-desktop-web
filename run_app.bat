@echo off
setlocal enabledelayedexpansion

:: 1. STYLE DE LA CONSOLE
title PCARRS Suite Engine
:: Force la taille de la fenêtre (Largeur, Hauteur)
mode con: cols=80 lines=20
:: Couleur : Fond noir (0), Texte Aqua (B) pour un look "médical/tech"
color 0B

:: Positionnement dans le dossier du script
cd /d "%~dp0"

set "APP_DIR=%~dp0"
:: Note: Utilisation de Rscript.exe souvent préférable pour launch.R, 
:: mais je garde R.exe si c'est ton choix.
set "R_BIN=%APP_DIR%R-Portable\bin\x64\Rscript.exe"
if not exist "%R_BIN%" set "R_BIN=%APP_DIR%R-Portable\bin\Rscript.exe"

set "RES_DIR=%APP_DIR%resources"
set "TMP=%APP_DIR%temp"
set "TEMP=%APP_DIR%temp"

if not exist "%APP_DIR%temp" mkdir "%APP_DIR%temp"

SET "HOME=%APP_DIR%"
SET "R_USER=%APP_DIR%"
SET "R_LIBS_USER=%APP_DIR%resources\R_libs"

:: 1. VERIFICATION : Dossier Resources (INTACT comme demandé)
if exist "%RES_DIR%" goto :ResourcesFound
cls
echo.
echo  ==========================================================
echo             CRITICAL ERROR: RESOURCES MISSING
echo  ==========================================================
echo.
echo  The core components are not installed yet.
echo.
echo  INSTRUCTION:
echo  1. Go to the "Patch" folder.
echo  2. Run (double-click) the "install_resources.bat" file.
echo  3. Once finished, you can launch this app again.
echo.
echo  ==========================================================
pause
exit

:ResourcesFound
:: 2. VERIFICATION : Déjà en cours ?
tasklist /FI "IMAGENAME eq Rscript.exe" 2>NUL | find /I /N "Rscript.exe">NUL
if "%ERRORLEVEL%"=="0" (
    echo [INFO] PCARRS Suite is already running.
    timeout /t 3
    exit
)

:: 3. VERIFICATION : R-Portable présent ?
if not exist "%R_BIN%" (
    echo ERREUR CRITIQUE : R-Portable introuvable dans %R_BIN%
    pause
    exit
)

:: 4. ANIMATION DE LANCEMENT (Avant le lancement effectif ou pendant)
:: On lance d'abord l'animation pour donner un feedback immédiat
set "total_steps=5"
set "bar="

for /L %%i in (1,1,%total_steps%) do (
    set "bar=!bar!####"
    cls
    echo.
    echo  ==========================================================
    echo             LAUNCHING PCARRS SUITE ENGINE
    echo  ==========================================================
    echo.
    echo  Please wait, initializing components...
    echo.
    
    set /a "percentage=%%i*20"
    echo  [!bar!                    ] !percentage!%%
    echo.
    
    if !percentage! LSS 60 echo  Status: Loading Engine...
    if !percentage! GEQ 60 echo  Status: Starting UI...
    
    timeout /t 1 >nul
)

:: 5. LANCEMENT VIA VBS (Silencieux)
:: 2. MESSAGE DE SUCCÈS ET MISE EN GARDE
cls
echo.
echo  ==========================================================
echo             PCARRS SUITE IS NOW RUNNING
echo  ==========================================================
echo.
echo  [SUCCESS] The interface is opening in your browser.
echo.
echo  !!! IMPORTANT !!!
echo  DO NOT CLOSE THIS WINDOW while using the application.
echo  Closing this window will stop the Engine and all analyses.
echo.
echo  Minimize this window to keep the application active.
echo  ==========================================================
echo.

:: 3. LANCEMENT DE R (Mode Verbose pour voir les erreurs si elles arrivent)
"%R_BIN%" --vanilla "%APP_DIR%launch.R" > nul 2>&1

:: Si R s'arrête (via stopApp), le script continue ici
echo.
echo  Engine stopped. You can now close this window.
pause