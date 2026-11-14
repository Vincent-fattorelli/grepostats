@echo off
rem Double-clic : telecharge les donnees Grepolis, refait le site, puis ouvre le dossier
rem ou se trouve grepostats-<monde>.html (le fichier a envoyer sur Discord).
chcp 65001 >nul
cd /d "%~dp0"
set "RSCRIPT="
for /d %%D in ("%ProgramFiles%\R\R-*") do if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe"
if not defined RSCRIPT (
  echo R introuvable dans "%ProgramFiles%\R".
  pause
  exit /b 1
)
"%RSCRIPT%" collecte.R
if errorlevel 1 (
  echo.
  echo La mise a jour a echoue, voir le message ci-dessus.
  pause
  exit /b 1
)
explorer "%~dp0site"
