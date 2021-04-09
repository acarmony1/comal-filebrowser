# comal-filebrowser
Comal 80 SD2IEC filebrowser


This filebrowser is based on CBM-FileBrowser 1.6

The currect version of CBM-FileBrowser 1.6 does not work with Comal, and has to be slightly modified.

Most everything works as intended ...

BUGS: 
1)  The loader used is a crude hack to load Comal programs, Comal uses it own load system, until I find a way to properly load comal programs this will have to do.
2)  Drive #9 reads the directory, but will not load Comal programs this is due to the crude loader I had to implement see above.


To compile use place ACME compiler in the directory and run "CBM-FileBrowser.bat", option 2 for Comal, if eveything is ok fb64comal.prg should be produced. This is the 'rommed' package version for Comal, meaning once loaded and 'used' it cannot be discarded, simply 'use sdtools' to activate. This file cannot be used in Comal itself, as Comal needs an 'obj' file for linking. pkg.sdtools is that obj file, a progam for Comal will be added later. Also will be included is a standalone file, called sdtools, this uses the "un-rommed" version, meaning once it is discarded, overwritten or such else it is removed from memory and must be re-loaded.

Options for creating 'rommed' and 'un-rommed' versions are in c64comal.def file, under functions, ROM 0/1. 0 being not rommed, 1 being rommed.

USAGE

pkg.sdtools.seq

In Comal,

LINK "PKG.SDTOOLS"  <-- Loads the package into Comal

USE SDTOOLS         <-- Activates SDTOOLS

FB                  <-- Enters the filebrowser

As stated once loaded and activated, SDTOOLS stays in memory, to use again follow the above with no need to 'LINK' again.

sdtools.prg

load "sdtools"      <-- This is a standalone Comal program that have USE SDTOOLS and FB in Comal line format, has the package saved with it. Just load and run this program. note this package is NOT rommed, meaning it will be discarded after NEW, or DISCARD commands are used

makeobjfile.prg     <-- This file will create a SEQ package file (that you need if you compile this yourself EQ fb64comal >> pkg.sdtools), simply load and run from comal, enter the file (EQ fb64comal created from above) and output file >> whatever you want to call it. Traditionally all Comal Packages were formated with PKG. in front.

3/31/2021

Now FB does not show loading message (sta #$9d). There is an error in the c64def, it cannot calculate ROMMED and non ROMMED versions. All versions included in release (R) for rommed verions (NR) not rommed


3/09/2021

Added version, (currently V.03). Fixed the color  to normal colors (colorenable = 0)

Added "GO" command. This command will run a regular C64 Basic file and when finished on exit will return to Comal.  This Came from Dutch authors in the COmal Today Magazine (Comal Today #8, pg 75).. Uknown original authors.

USE:

go("basicfilename")  << will exit to BASIC and once finished will return to Comal.


03/08/2021

-- Deletes all non-C64 files, routines. Not needed since comal is not avaliable on these systems.




I still need to find a correct loader solution, I have tried all Comal file loader routines and none work? They work for machine language programs, but not actual Comal programs.







