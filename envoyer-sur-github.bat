@echo off
rem Double-clic : envoie les modifications sur GitHub (une fenetre de connexion s'ouvre la premiere fois).
cd /d "%~dp0"
git push -u origin main
echo.
pause
