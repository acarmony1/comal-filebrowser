@echo off
:menu
cls
echo.
echo          ���������������������������������ͻ
echo          �  CBM FileBrowser compile        �
echo          ���������������������������������͹
echo          �                                 � 
echo          � Select system:                  � 
echo          � -----------------               � 
echo          � 1 - C64                         � 
echo          � 2 - C64 Comal                   �   
echo          �                                 �
echo          � 0 - quit                        �
echo          �                                 � 
echo          ���������������������������������ͼ   
echo.

set /p system=           System: 

if "%system%"=="1" goto system1
if "%system%"=="2" goto system2

if "%system%"=="0" goto quit
goto menu



:system1        

rem **************************
rem * Commodore 64 (c64.asm) *
rem **************************

cls
echo.
echo Compiling CBM FileBrowser for: 
echo 1 - C64
echo.
echo acme --cpu 6502 -f cbm -o fb64.prg c64.asm
echo.

acme --cpu 6502 -f cbm -o fb64.prg c64.asm

if not %errorlevel%==0 goto :error

echo done, launching program (fb64.prg).
echo.
fb64.prg 

pause
exit






:system2 

rem *************************************
rem * Commodore 64 Comal (c64comal.asm) *
rem *************************************

cls
echo.
echo Compiling CBM FileBrowser for: 
echo 2 - C64 Comal
echo.
echo acme --cpu 6502 -f cbm -o sdtools-comal.prg c64comal.asm
echo.

acme --cpu 6502 -f cbm -o sdtools-comal.prg c64sdtools.asm

if not %errorlevel%==0 goto :error

echo done, launching program (.prg).
echo.
sdtools-comal.prg 

pause
exit

:error
echo.
echo WARNING, ERROR ENCOUNTERED !!!
echo.
pause
:quit
exit
