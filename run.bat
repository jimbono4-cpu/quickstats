@echo off
title Trial Randomizer - Balance Algorithm
echo Starting Trial Randomizer...
python "%~dp0trial_randomizer.py"
if %errorlevel% neq 0 (
    echo.
    echo ERROR: Python is required to run this application.
    echo Please install Python 3.6+ from https://www.python.org/downloads/
    echo Make sure to check "Add Python to PATH" during installation.
    pause
)
