@echo off
setlocal enabledelayedexpansion

set "APP_DIR=%~dp0"
set "R_BIN=%APP_DIR%R-Portable\bin\Rscript.exe"

:: 1. VERIFICATION : Déjà en cours ?
tasklist /FI "IMAGENAME eq Rscript.exe" 2>NUL | find /I /N "Rscript.exe">NUL
if "%ERRORLEVEL%"=="0" (
    echo [INFO] PCARRS Suite is already running.
    timeout /t 3
    exit
)

:: 2. VERIFICATION : R-Portable présent ?
if not exist "%R_BIN%" (
    echo ERREUR CRITIQUE : R-Portable introuvable.
    pause
    exit
)

:: 3. LANCEMENT SILENCIEUX de R
start /b "" "%R_BIN%" "%APP_DIR%launch.R" >nul 2>&1

:: 4. ANIMATION RAPIDE (5 secondes)
set "total_steps=5"
set "bar="

for /L %%i in (1,1,%total_steps%) do (
    set "bar=!bar!■■■■"
    cls
    echo.
    echo  ==========================================================
    echo            LAUNCHING PCARRS SUITE ENGINE
    echo  ==========================================================
    echo.
    echo  Please wait, initializing components...
    echo.
    
    :: Calcul du pourcentage : %%i * 20
    set /a "percentage=%%i*20"
    
    echo  [!bar!                    ] !percentage!%%
    echo.
    
    if !percentage! LSS 60 echo  Status: Loading Engine...
    if !percentage! GEQ 60 echo  Status: Starting UI...
    
    timeout /t 1 >nul
)

echo.
echo  [SUCCESS] Opening Browser...
timeout /t 1 >nul
exit