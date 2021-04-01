; --------------------
; - Constants
; --------------------

; - joystick port address


joyport = $dc00         ; joystick port 2 ($dc01 for port 1)
ScreenCols = 40         ; Number of screen columns


; - max. length for stored entries
entrylen = 17
; - max. length for shown entries
namelen = 16

; - some PETSCII chars
quotechar = $22
leftarrowchar = 95

; --------------------
; - kernal functions
; --------------------

SETLFS = $ffba          ; a = filenr, x = unitnr, y = sec.addr/command (255|1)
SETNAM = $ffbd          ; a = namelen, x:y->name
OPEN = $ffc0            ; Open Vector [F40A] Open File
                        ; Preparing: SETLFS, SETNAM
CLOSE = $ffc3           ; x = filenr
LOAD = $ffd5            ; a = 0
SCNKEY = $ff9f
GETIN = $ffe4           ; ret: a = 0: no keys pressed, otherwise a = ASCII code
CHROUT = $ffd2          ; a = output char
PLOT = $fff0            ; x<->y



!if variant != 3 {
PRNINT  = $bdcd     ; print integer in A/X
prnstr  = $ab1e     ; print string in A/Y, 0 terminated
CLEARSCREEN = $e544
SHFLAG =  $28d      ; ($00 = None, $01 = Shift, $02 = CBM, $04 = CTRL or a sum of them). 

CINT1   = $e518     ; Initialize I/O
INITV   = $e45b     ; initv Initialize Vectors
INTTCZ  = $e3a4     ; initcz Initialize BASIC RAM
SCRTCHF = $a644     ; scrtch Perform [new] FORCED and return

}

!if variant = 3 {

;PRNINT  = $bdcd     ; print integer in A/X
;prnstr  = $ab1e     ; print string in A/Y, 0 terminated (NOT IN COMAL USE OUR OWN)
CLEARSCREEN = $e544
SHFLAG =  $28d      ; ($00 = None, $01 = Shift, $02 = CBM, $04 = CTRL or a sum of them). 

CINT1   = $e518     ; Initialize I/O
INITV   = $e45b     ; initv Initialize Vectors
INTTCZ  = $e3a4     ; initcz Initialize BASIC RAM
SCRTCHF = $a644     ; scrtch Perform [new] FORCED and return
CHKOUT 	= $ffc9

} 



LISTN   = $ffb1     ; Command Serial Bus LISTEN
UNLSN   = $ffae     ; Command Serial Bus UNLISTEN



; --------------------
; - hw addresses
; --------------------

screen = $0400          ; screen address
color = $d800           ; color ram address
vicborder = $d020       ; border color register
vicbackgnd = $d021      ; background color register
vicraster = $d012       ; raster compare register


;common for C64/Vic20/C16
basicstartaddress_lo = $2b ;Pointer: Start of Basic lo byte
basicstartaddress_hi = $2c ;Pointer: Start of Basic hi byte
variablesstartaddress_lo = $2d ;Pointer: Start of Basic lo byte
variablesstartaddress_hi = $2e ;Pointer: Start of Basic hi byte

status_word_st = $90    ; Status word ST 

keybuf = $0277          ; keyboard buffer
keybuflen = $00c6         ; keyboard buffer byte count
lastdevice = $ba        ; last used device number


; --------------------
; - Zero page variables
; --------------------


; current joystick state (1 = falling edge)
curjoy = $22

; temp variables
tmp  = $23
tmp2 = $24  ; it must be continuous with tmp Example $FD and $FE (used as pointer in some cases)
tmp3 = $25

; pointer to last of the table
tbllst  = $26
tbllsth = $27

; menustate, bits:
; 0 = top reached (disable up & page up)
; 1 = bottom reached
; 2 = last reached (disable down & page down)
; 7 = inside d64
menustate = $28

; length of diskname
disknamepetlen = $29

!if rememberpos = 1 {
; previous dir position
prevdirpos  = $57
prevdirposh = $58
}

; - pointer to selected entry
selected  = $fb
selectedh = $fc

; - temp pointer
tmpptr = $fd
tmpptrh = $fe


; --------------------
; - Miscellaneus
; --------------------

; --- Main

!ct pet     ; petscii

; start of program

!if variant != 3 {
*=$0801

entry:
; BASIC stub: "1 SYS 2061"
!by $0b,$08,$01,$00,$9e,$32,$30,$36,$31,$00,$00,$00

} else {

!if variant = 3 {

; comal start

;!if rom = 1 {

rommed = $0000

;} else {

;rommed = 0

;}



; comal zp

txtlo = $49
txthi = $4a

; comal system locations

comal =$ca30
dummy =$ca2f 
defpag =$0046 
endprc =$007e  
proc =$0070 
kbuf = $0277
link = 2
value    =  114
str      =  2

; start of program

*=$7000

entry:

;
;	init
;

map !byte defpag+rommed ;52kb ram memory map=(and rommed) cannot discard
 !word end ;modulee end state
 !word init ; signal-handler
 
;
;  package label
;
packlabel !tx 7,"sdtools" ;package header 'sdtools'
prochead !word sdtools ;procedure header
signal !word dummy ; signal
!byte 0 ;end table
 
;
;  procedure table
;
sdtools !tx 2,"fb" ;procedure 'fb'
 !word fb ;procedure location
 !tx 2,"go"    ;PROCEDURE NAME
 !word go
 !tx 4,"help"
 !word help
 !byte 0
 
;
;  proc fb (filebrowser)
;
fb !byte proc,<mlcodeentry,>mlcodeentry,0 
 !byte endprc


go   !byte proc,lo,hi,1
       !byte value+str,endprc
adres    =  *
lo       =  <adres
hi       =  >adres
       lda  #$01
       jsr  $c896
       ldy  #$02
       lda  ($45),y
       bne  argerr
       iny
       lda  ($45),y
       beq  argerr
       cmp  #$11
       bcc  ok
argerr ldx  #$01
       jmp  $c9fb
ok     sta  eind
       ldx  #$00
       iny
name   lda  ($45),y
       sta  eind+1,x
       inx
       iny
       cpx  eind
       bcc  name
       sei  
       lda  #$37
       sta  $01
       ldx  #$e0
       ldy  #$07
       lda  #$00
port   sta  $df00,y
       dey  
       bpl  port
       stx  $de00
       stx  $8008
       ldx  #$ff
       txs
       cld
       jsr  $fda3
       jsr  $fd50
       jsr  $fd15
       jsr  $ff5b
       cli
       jsr  $e453
       jsr  $e3bf
       jsr  $e422
       ldx  #$fb
       txs
       lda  #<nmi
       ldy  #>nmi
       sta  $0318
       sty  $0319
       lda  #<back
       ldy  #>back
       sta  $0302
       sty  $0303
       lda  #$80
       sta  $9d
       lda  eind
       ldx  #<na
       ldy  #>na
       jsr  SETNAM
       ldx  #$08
       ldy  #$ff
       jsr  SETLFS
       lda  #$00
       ldx  $2b
       ldy  $2c
       jsr  LOAD
       bcc  noerr
       jmp  $e0f9
noerr  lda  $90
       and  #$bf
       beq  nolerr
       jmp  $e19c
nolerr stx  $2d
       sty  $2e
       lda  #$02
       ldx  #<com
       ldy  #>com
       jsr  SETNAM
       lda  #$01
       ldx  #$08
       ldy  #$6f
       jsr  SETLFS
       jsr  OPEN
       jsr  $a533
       lda  #$00
       sta  $9d
       jsr  $a659
       lda  #$0d
       jsr  CHROUT
       jmp  $a7ae
nmi
       ldx  #$80
       ldy  #$00
wait   dey
       bne  wait
       dex
       bne  wait
back   lda  #$37
       sta  $01
       ldx  #$80
       ldy  #$07
       lda  #$00
labe   sta  $df00,y
       dey
       bpl  labe
       lda  #$8e
       sta  $033c
       lda  #$00
       sta  $033d
       lda  #$de
       sta  $033e
       lda  #$6c
       sta  $033f
       lda  #$fc
       sta  $0340
       lda  #$ff
       sta  $0341
       jmp  $033c
com    !byte 85,73
eind    =   *
na      =   eind+1
       !byte 0,0,0,0,0,0,0,0
       !byte 0,0,0,0,0,0,0,0
	   

help	!byte proc, <help1,>help1,0
		!byte endprc
help1	lda #<version
		ldy #>version
		jsr prnstr
	
		lda #<helpt
		ldy #>helpt
		jsr prnstr
		rts	   
	   
init cpy #link
bne init1
lda #<version
ldy #>version
jsr prnstr

init1 rts

}

}

mlcodeentry:

; **********************************************************************************************

!if forecolor>4 {
lda #forecolor
jsr CHROUT
}

; clear screen
!if clearscrena = 1 {
jsr CLEARSCREEN

}

; set colors 



!if bordercolor<16 {
lda #bordercolor
sta vicborder
}
!if backcolor<16 {
lda #backcolor
sta vicbackgnd
}




!if hwdevicenum = 0 & autodetectm = 0 {
; store last used device number
lda lastdevice
sta device

}


!if hwdevicenum = 0 & autodetectm = 1 {
; store last used device number
lda lastdevice
beq SearchDevice ; If current drive is zero search the first active drive

cmp #1
bne StoreDevice  ; If current drive is not zero and not 1 use it

SearchDevice:
jsr SearchActiveDrive ; else search the first active drive

bne StoreDevice ; drive present, store it, A=detected drive

lda #8 ; Set Drive 8 as Default if no drive found

StoreDevice:
sta device

}


!if printbars = 1 {		

ldx #listtopy-1    ; row
Jsr PrintLine      ; print the top line

ldx #listbottomy   ; row
Jsr PrintLine      ; print the bottom line


!if key_quit<255 {

;print quit
ldx #listbottomy+1 ;row to print

ldy #listx+17    ; col to print

clc              ; function: set cursor
jsr PLOT         ; set cursor position
 

ldy #>quit_text  ; string start point hi-byte
lda #<quit_text  ; string start point lo-byte
jsr prnstr       ; print string in A/Y, 0 terminated

}


!if sortdir = 1 {
jsr PrintSortMode
}

!if drivechange = 1 {

ldx #listbottomy+1 ; row to print
!if variant = 2 {
;C64-DTV
ldy #listx+14      ; col to print
} else{
ldy #listx-1       ; col to print
}
clc                ; function: set cursor
jsr PLOT           ; set cursor position

ldy #>drive_text   ; string start point hi-byte
lda #<drive_text   ; string start point lo-byte
jsr prnstr       ; print string in A/Y, 0 terminated

jsr PrintDriveNr

}


}



OpenNewDrive:


!if showcursors = 1 {
; draw arrows
!if arrowcolor>4 {
lda #arrowcolor
jsr CHROUT

}
!if listx > 0 {
ldy #listx-1
ldx #listtopy
clc
jsr PLOT
lda #'>'
jsr CHROUT
}
!if listx+namelen < ScreenCols {
ldy #listx+namelen
ldx #listtopy
clc
jsr PLOT
lda #'<'
jsr CHROUT
}
!if arrowcolor>4 {
!if forecolor>4 {
; set foreground color again
lda #forecolor
jsr CHROUT
} ; forecolor
} ; arrowcolor
} ; showcursors

!if rootonstart = 1 {
; exit to root: (Also from DNP - HD file images) "CD//", "CD:_", "CD//"    
ldx #<diskcmdroot
ldy #>diskcmdroot
lda #1
jsr openclose
ldx #<diskcmdexit
ldy #>diskcmdexit
lda #1
jsr openclose
ldx #<diskcmdroot
ldy #>diskcmdroot
lda #1
jsr openclose
}

!if rememberpos = 1 {
lda #0
sta prevdirposh
}


!if rootonstart = 0 {

lda #0        ;disknamepetlen - using this value the diskcmdexit command will be just "CD:" without left arrow.
beq loadprev2


; - previous directory load (if rootonstart = 0)
;
loadprev:
lda #1

loadprev2:
sta disknamepetlen

lda #0
sta menustate
sta tmp
} else {

; - previous directory load (if rootonstart = 1)
;
loadprev:
lda #0
sta menustate
sta tmp
lda #1
sta disknamepetlen

}

loadprev3:
lda #<tmp
sta selected
lda #>tmp
sta selectedh
lda #<diskcmdexit
sta tmpptr
lda #>diskcmdexit
sta tmpptrh
jsr loadlist
; set selected to tbl (or remembered pos)
!if rememberpos = 1 {
lda prevdirposh
beq +           ; skip if not available
sta selectedh
lda prevdirpos
sta selected
lda #0
sta prevdirposh ; disable next remembered pos
+
}


; - menu
;
menu:
jsr clearlist

!if sortdir = 1 {

lda sortflag
beq ++

jsr sortlist
++
}

jsr redrawlist  ; SLOW

menuchanged:
!if statusenabl = 1 {
; print file status
jsr printstatus
}

menuwait:

lda #rastercmp
- cmp vicraster
bne -           ; wait until raster = rastercmp

iny             ; y++
cpy #joyrepeat  ; check if y >= joyrepeat

bcc +           ; if not, skip
ldy #0          ; reset joyrepeat counter
lda #$ff        ; reset lastjoy for repeat
sta lastjoy


; read joystick
+ lda joyport   ; a = joy



;c64 joy bit switches structure:

;bit   switch
;0     Up
;1     Down
;2     Left
;3     Right
;4     Fire   
;5     --
;6     --     
;7     --     


;joystick read
tax             ; save to x
eor lastjoy     ; a = joy xor lastjoy
and lastjoy     ; a = a and lastjoy
stx lastjoy     ; update lastjoy


sta curjoy
and #$1f        ; test if anything is pressed
bne +           ; if is, process input


jsr GETIN       ; read keyboard
tax             ; x = pressed key (or 0)

beq menuwait    ; if no keys pressed, no input -> wait


; process input


+ lda curjoy



lsr             ; check if up
bcs +           ; jump if pressed
cpx #key_preve  ; check if key_preve
bne ++          ; jump if not
+ lda menustate ; a = menustate
and #1          ; check if top reached
bne menuwait    ; if it is, jump back
; prev entry
sec
lda selected    ; selected -= entrylen
sbc #entrylen
sta selected
bcs +
dec selectedh
+ 
!if fastscrolle = 1 {
jsr scrollup
jmp menuchanged
} else {
jmp menu
}

++ lsr          ; check if down
bcs +           ; jump if pressed
cpx #key_nexte  ; check if key_nexte
bne ++          ; jump if not
+ lda menustate ; a = menustate
and #4          ; check if last reached
bne menuwait    ; if it is, jump back
; next entry
clc
lda selected    ; selected += entrylen
adc #entrylen
sta selected
bcc +
inc selectedh
+ 
!if fastscrolle = 1 {
jsr scrolldown
jmp menuchanged
} else {
jmp menu
}


++ cpx #key_exit    ; check if key_exit
bne ++              ; jump if not
jmp loadprev        ; load prev dir

!if variant != 3 {
++ cpx #key_quit    ; check if key_quit
bne ++              ; jump if not

; quit to basic
;  rts              ; old method
	jmp $fe66	; Warm Start Basic [BRK]

}


!if variant = 3 {

++ cpx #key_quit    ; check if key_quit
bne ++

jsr CLEARSCREEN
jmp comal

}

!if variant != 3 {
++ cpx #key_reset
bne ++              ; jump if not
;jmp $FCE2           ; Reset the C64 SYS64738
jmp ($fffc)			;reset CPU
}

!if variant = 3 {

++ cpx #key_reset
bne ++  
jmp comal

}


++ cpx #key_top     ; check if key_top
bne ++              ; jump if not

movetop:
lda #<tbl
sta selected        ; selected = table
lda #>tbl
sta selectedh
jmp menu

++ cpx #key_bottom  ; check if key_bottom
bne ++              ; jump if not
lda tbllst
sta selected        ; selected = table
lda tbllsth
sta selectedh
jmp menu

++ lsr          ; check if left
bcs +           ; jump if pressed

!if key_prevp2=255 {

cpx #key_prevp  ; check if key_prevp (only)
bne ++          ; jump if not
+ jmp prevpage  ; previous page

} else {

cpx #key_prevp  ; check if key_prevp
beq prevpage20
cpx #key_prevp2 ; check if key_prevp2
bne ++          ; jump if not
prevpage20:
+ jmp prevpage  ; previous page
}


++ lsr          ; check if right
bcs +           ; jump if pressed

!if key_nextp2=255 {

cpx #key_nextp  ; check if key_nextp (only)
bne ++          ; jump if not

} else {

cpx #key_nextp  ; check if key_nextp
beq nextpage2
cpx #key_nextp2 ; check if key_nextp2
bne ++          ; jump if not
nextpage2:
}

+ jmp nextpage  ; next page

++ lsr          ; check if fire
bcs firepressed ; jump if pressed
cpx #key_fire   ; check if key_fire
beq firepressed ; jump if pressed


!if startmode = 1 {
cpx #key_fire2  ; check if key_fire2 (CBM+ENTER)
beq firepressed ; jump if pressed
}


!if sortdir = 1 { 
cpx #key_sort    ; check if key_sort
beq keysortpressed 
}


!if drivechange = 1 { 
cpx #key_drive  ; check if key_drive is pressed
bne ++          ; jump if not
jmp ChangeDrive ; change Drive
++
}


jmp menuwait


!if sortdir = 1 {


; - key_sort pressed
keysortpressed:

lda sortflag
eor #$ff
sta sortflag

!if printbars = 1 {

jsr PrintSortMode

}


lda #0
sta menustate
sta tmp
sta disknamepetlen

!if rememberpos = 1 {
sta prevdirposh       ;Reset previous dir position because it remembers the position of the list sorted in another way. 
}


lda sortflag
beq unSort


!if pleasewaite = 1 {
lda #<tmp
sta selected
lda #>tmp
sta selectedh

; print info...
jsr showpleasewait
}

jsr sortlist
jmp movetop

unSort:        ;Read again the current directory
jmp loadprev3




!if printbars = 1 {

PrintSortMode:

ldx #listbottomy+1 ; row
!if variant = 2 {
;C64-DTV
ldy #listx+8       ; col to print
} else{
ldy #listx+13      ; col to print
}
clc                ; function: set cursor
jsr PLOT

lda sortflag
beq sortOFF

lda #<sortON_text
ldy #>sortON_text
jmp PrintSortString 

sortOFF:
lda #<sortOFF_text
ldy #>sortOFF_text

PrintSortString:
jsr prnstr

rts
}


}

; - fire pressed
firepressed:

; - check if first file ("_")
lda selectedh
cmp #>tbl
bne ++          ; jump if not
lda selected
cmp #<tbl
bne ++          ; jump if not
jmp loadprev    ; was "_", load prev dir

!if rootentry = 1 {
; - check if 2nd entry ("//") exit to root
++
lda selectedh
cmp #>tbl+entrylen
bne +++          ; jump if not
lda selected
cmp #<tbl+entrylen
bne +++          ; jump if not

!if rootonstart = 0 {

;*** this routine is not present on OpenNewDrive if rootonstart = 0 ***
;*** so I replicate it here ****
;
;a JSR call is better but, at the moment, I leave it as is for old C64 version comparison

; exit to root: "CD:_", "CD//"
ldx #<diskcmdexit
ldy #>diskcmdexit
lda #1
jsr openclose
ldx #<diskcmdroot
ldy #>diskcmdroot
lda #1
jsr openclose
}

JMP OpenNewDrive  ; reload current drive dir from root
+++
} else {
++
}
; copy selected to disknamepet, save length of name
ldy #namelen
sty disknamepetlen
lda #<disknamepet
sta tmpptr
lda #>disknamepet
sta tmpptrh
- dey
php
lda (selected),y
bne +
sty disknamepetlen
+ sta (tmpptr),y
plp
bne -

; check if inside d64
bit menustate
bmi prselected  ; if in d64, load selected program

; check if selected is dir
ldy #(entrylen-1)
lda (selected),y
cmp #3          ; FIXME magic value for "dir"
bne search      ; jump if not dir

loaddir:
!if rememberpos = 1 {
lda selected
sta prevdirpos
lda selectedh
sta prevdirposh
}
lda #<diskcmdcd
sta tmpptr
lda #>diskcmdcd
sta tmpptrh
jsr loadlist
jmp menu

; determine if selected file is d64(/d71/d81/m2i/d41/dnp/tap)
;  - if d64 -> change dir & set d64 flag

; seach for last .
search:
!if tap_support = 1 {
lda #$0
sta tmp3            ; tmp3 used as flag for TAP detection (0=NO TAP)
}
+ ldy #namelen-1
- lda (tmpptr),y
cmp #'.'
beq +
dey
bne -

; . not found, assuming entry is program
beq prselected

; . found on y (>0), check known extensions
+ iny
lda #<extensions
sta extension_list
lda #>extensions
sta extension_list+1
lda #4               ; extension len
ldx #extensions_max  ; extension max
jsr parseext
cpx #0
beq prselected      ; jump not d64(/d71/d81/m2i/d41/dnp/tap)

lda #$0
sta menustate       ; Clear menustate bits
cpx #4
beq loaddir         ; inside dnp, just load directory, do not set flag "inside d64" to allow subfolders browsing  

!if tap_support = 1 {
cpx #8
beq tapselected     ; tap file selected
}

; d64(/d71/d81/m2i/d41) selected
lda #$80            ; set "inside d64" flag 
sta menustate
bne loaddir         ; load directory

!if tap_support = 1 {
tapselected:
lda #$1
sta tmp3            ; tmp3 used as flag for TAP detection (1=TAP file detected)
}

; - program selected, start loading
prselected:

!if pleasewaite = 1 {
; print info...
jsr showpleasewait
}

; setup load
setupload:

!if tap_support = 1 {

lda tmp3
beq +             ; No TAP file selected

; setup TAP name
ldx disknamepetlen
inx     ; x = filenamelen
ldy #00
lo_1:
lda disknamepet,y                 
sta tapname+3,y
iny
dex
bne lo_1

ldx #<tapname
ldy #>tapname
lda disknamepetlen
jsr openclose


ldx #$ff
sei
txs
cld
ldx #$0
stx $d016
JSR $FDA3         ; ioinit - Initialise I/O
;JSR $FD50         ; ramtas - Initialise System Constants (skip Memory check, Fast boot)
JSR $FD15         ; restor - Restore Kernal Vectors (at 0314)
JSR $FF5B         ; cint   - Initialize screen editor
cli

JSR $E453         ; initv  - Initialize Vectors
JSR $E3BF         ; initcz - Initialize BASIC RAM
jsr $e422         ; initms - Output Power-Up Message (DO NOT SKIP, otherwise there are problems with some loaders)
ldx #$fb
txs

; LOAD from Tape (Shift + Run Stop)
lda #131
sta $277
lda #1
sta $c6

JMP $A7AE    ; BASIC Warm Start (RUN)



+
}



!if variant != 3 {

!if startmode = 0 {
;old method,  LOAD"FILE",8,1 + (System auto RUN)
lda #1      ; filenr
ldx device  ; unitnr
ldy #1      ; sec.address (,1)
}


!if startmode = 1 {

LDA SHFLAG    ; 1 shift pressed, 2 CBM pressed, 4 CTRL pressed 
and #$02
cmp #$02      ; check cbm key  
beq +         ; start mode 2 (LOAD "FILE",8 + RUN) 
        
ldy #1        ; sec.address (,1) File start address LOAD
lda #0        ; start mode 0 (LOAD "FILE",8,1 + RUN) 
beq ++

+ ldy #0      ; sec.address (,0) Basic start address LOAD

++ sta tmpptr ; start mode (0 or <> 0)

lda #1        ; filenr
ldx device    ; unitnr
}
}

!if variant = 3 {

; comal load

jsr CLEARSCREEN

sei

ldy #$00

lda #<loadtext
ldy #>loadtext
jsr prnstr
;main lda loadtext,y	; print to screen "LOAD" + quote
;beq +
;jsr CHROUT
;iny
;cpy #loadtextlen
;bne main

;+ ldy #0

;lda #<selected
;ldy #>selected
;jsr prnstr

main1 lda (selected),y	; print to screen filename
beq +
jsr CHROUT		
iny
cpy #entrylen
bne main1

+ ldy #0

lda #<runtext
ldy #>runtext
jsr kbuffr

;main2 lda runtext,y	; store remainder in keyboard buffer
;sta kbuf,y			; send text to keyboard buffer
;iny
;cpy #runtextlen
;bne main2

cli

lda #runtextlen
sta keybuflen

jmp comal

}

!if variant != 3 {

jsr SETLFS
lda disknamepetlen  ; a = filenamelen
ldx #<disknamepet
ldy #>disknamepet   ; x:y->filename
jsr SETNAM

; copy LOAD & RUN code to loadrunpos
ldy #0
ldx #loadrunend-loadrunstart
- lda loadrunstart,y
sta loadrunpos,y
iny
dex
bne -
jmp loadrunpos  ; jump to loadrunpos



; (the following code is executed at loadrunpos)
loadrunstart    ; start of code to copy

; remember me

!pseudopc loadrunpos {
; load program
;
lda #0      ; a = 0: load
sta #$9d

!if startmode = 1 {
                          ;Load start address used only in RUN start mode (sec.address (,0) on SETLFS)
ldx basicstartaddress_lo  ;Pointer: Start of Basic lo byte
ldy basicstartaddress_hi  ;Pointer: Start of Basic hi byte
}

jsr LOAD
; error detection would be nice :)

; save end address
stx variablesstartaddress_lo
sty variablesstartaddress_hi
}

; clear screen
!if clearscrena = 1 {

jsr CLEARSCREEN
}

; restore default background/border/cursor colors
!if colorenable = 1 {
lda #14
sta vicborder           ;$d020 default border color (#14)

lda #6
sta vicbackgnd          ;$d021 default background color (#6)

lda #154
jsr CHROUT              ;default coursor color (#154 Light Blue)


}




!if startmode = 0 {
; autostart program
; (from comp.sys.cbm)
;
jsr $a659   ; reset pointers etc...
jmp $a7ae   ; BASIC warm start
}


!if startmode = 1 {

JSR $A659    ; CLR

lda tmpptr   ; read the RUN mode check byte (0 = ,8,1 + RUN)(<> 0 = ,8 + RUN)
beq StartMode0

JSR $A533    ; Relinks BASIC Program from and to any address... 

StartMode0:  
JMP $A7AE    ; BASIC Warm Start (RUN)

;ExitToBasic: ; Return to basic
;JMP $FE66    ; Warm Start Basic [BRK]

}

}


loadrunend  ; end of code to copy


; --- Subroutines

!if variant = 3 { 
; print integer & print string for COMAL

PRNINT lda device
ora #$30
jsr CHROUT

rts

prnstr
	sta txtlo
	sty txthi
	ldy #0
pstrlp
	lda (txtlo),y
	beq pstrex
	jsr CHROUT
	iny
	bne pstrlp
pstrex
	lda #0
	ldy #0
	ldx #0
	rts

kbuffr
	sta txtlo
	sty txthi
	ldy #0
kbuffrlp
	lda (txtlo),y
	beq kbuffrex
	sta kbuf,y
	iny
	bne kbuffrlp
kbuffrex
	lda #0
	ldy #0
	ldx #0
	rts
	

}

; - nextpage
;
nextpage:
lda #<(listbottomy-listtopy)*entrylen
sta tmp
lda #>(listbottomy-listtopy)*entrylen
sta tmp2
clc
lda selected    ; selected += page
adc tmp
sta selected
lda selectedh
adc tmp2
sta selectedh
lda tbllsth     ; check if table last < selected
cmp selectedh
bcc +
bne ++
lda tbllst
cmp selected
bcs ++
+ lda tbllst
sta selected    ; table last < selected -> selected = table last
lda tbllsth
sta selectedh
++ jmp menu 


; - prevpage
;
prevpage:
lda #<(listbottomy-listtopy)*entrylen
sta tmp
lda #>(listbottomy-listtopy)*entrylen
sta tmp2
sec
lda selected    ; selected -= page
sbc tmp
sta selected
lda selectedh
sbc tmp2
sta selectedh
lda #>tbl       ; check if selected < table
cmp selectedh
bcc ++
bne +
lda #<tbl
cmp selected
bcc ++
+ lda #<tbl
sta selected    ; selected < table -> selected = table
lda #>tbl
sta selectedh
++ jmp menu 


!if printbars = 1 {

; parameters:
;  x->the row where to print the bar
; returns:
;  menustate

PrintLine:

ldy #listx-1       ; col
clc                ; function: set cursor
jsr PLOT           ; set place to print
 
ldy #0
- lda #$c0     ; "-" char
jsr CHROUT     ; print char
iny
cpy #namelen+6
bne -
rts


}


!if drivechange = 1 { 
ChangeDrive:  

lda #0
sta status_word_st      ; set Status word ST as 0

ldx device
; #8-#14  (Drive accepted for increasing auto-detection)
cpx #8
bmi SearchActiveDriveSTD   ;auto detect next drive from drive #8
cpx #14  
bpl SearchActiveDriveSTD   ;auto detect next drive from drive #8

inx
txa
sta lastdevice

jsr SearchActiveDriveLoop  ;auto detect next drive from current drive +1

bne StoreNewDevice ; drive present, store it, A=detected drive

SearchActiveDriveSTD:
jsr SearchActiveDrive
bne StoreNewDevice ; drive present, store it, A=detected drive
lda #8 ; Set Drive 8 as Default if no drive found

StoreNewDevice:
sta device

!if printbars = 1 {
jsr PrintDriveNr
}

JMP OpenNewDrive

!if printbars = 1 {
PrintDriveNr:
ldx #listbottomy+1 ; row

!if variant = 2 {
;C64-DTV
ldy #listx+19       ; col
} else {
ldy #listx+4       ; col
}
clc                ; function: set cursor
jsr PLOT


!if buttoncolor>4 {
lda #buttoncolor
jsr CHROUT       ; print Red
}

lda device
cmp #128
bpl PrintDriveNumberText
cmp #10
bpl PrintDriveNumberText
lda #"0"
jsr CHROUT       ; print "0" char for device 8 and 9
PrintDriveNumberText:

ldx device
lda #0
jsr PRNINT       ; print integer in A/X remember

!if forecolor>4 {
lda #forecolor
jsr CHROUT       ; print white
}

lda device
cmp #128
bpl PrintDriveNrExit
cmp #100
bpl PrintDriveNrExit

lda #$20
jsr CHROUT       ; print space

PrintDriveNrExit:
rts
}

}



!if  autodetectm = 1 | drivechange = 1 {

SearchActiveDrive:     
	lda #0
	sta status_word_st      ; set Status word ST as 0

	lda #8                
	sta lastdevice          ; check for first existing drive from #8 to #15

SearchActiveDriveLoop:          ; from drive #8 to drive #15 
	jsr LISTN		; send LISTEN command ($FFB1) to the drive #A
	jsr UNLSN               ; send UNLISTEN command ($FFAE)
	lda status_word_st      ; read Status word ST
	beq SearchActiveDriveExit ; if a = 0 then drive exist so check for file now

	lda #0
	sta status_word_st      ; set Status word ST as 0

	inc lastdevice
	lda lastdevice
       
	cmp #16                   
	bne SearchActiveDriveLoop ; if a <> #16 loop  
	
	;else device from #8 to #15 is not present so exit reporting 0 as current device

	lda #0
	sta lastdevice

SearchActiveDriveExit:
	lda lastdevice          ; A report Current device, 0 = not found
	rts
}




!if disknameena = 1 & drivechange = 1 {

; - cleardiskname
;
cleardiskname:

lda #18
jsr CHROUT
ldx #disknamey
ldy #disknamex
clc            ; function: set cursor
jsr PLOT       ; set place to print
ldy #0
- lda #$20     ; space char
jsr CHROUT     ; print char
iny
cpy #namelen
bne -
lda #146
jsr CHROUT
rts
}




; - clearlist
;
clearlist:

lda #<screen+listtopy*ScreenCols+listx
sta clearlistsl
lda #>screen+listtopy*ScreenCols+listx
sta clearlistsh

ldx #listbottomy-listtopy
-- ldy #0
lda #listbackgnd
- 
clearlistsl = *+1
clearlistsh = *+2
sta $1234,y
iny
cpy #namelen
bne -
clc
lda clearlistsl
adc #ScreenCols
sta clearlistsl
lda clearlistsh
adc #0
sta clearlistsh
dex
bne --

rts


; - redrawlist (SLOW)
; parameters:
;  selected:h->top entry
; returns:
;  menustate
;
redrawlist:
; check if top is reached
lda menustate
and #$f0
tay
lda #>tbl
cmp selectedh
bne +
lda #<tbl
cmp selected
bne +
; selected is the first entry
iny                 ; y bit 0 = 1
+ sty menustate     ; menustate = y
; store selected:h
lda selected
sta tmpptr
lda selectedh
sta tmpptrh
ldx #listtopy       ; set list y location
; print loop
- ldy #0
lda (selected),y    ; check if end reached
beq ++              ; jump if end
; print entry
ldy #listx          ; set list x location
jsr printentry      ; print
cpx #listbottomy    ; is bottom reached
beq +++             ; jump if it is
clc
lda selected        ; selected += entrylen
adc #entrylen
sta selected
bcc -
inc selectedh
bne -               ; loop
; end reached
++ lda menustate
ora #2              ; end reached, bit 1 = 1
sta menustate
cpx #listtopy+1     ; check if last reached
bne +               ; skip if not
ora #4              ; last reached, bit 2 = 1
+ sta menustate     ; menustate |= a
+++ lda tmpptr      ; restore selected:h
sta selected
lda tmpptrh
sta selectedh
rts


; - printentry
; parameters:
;  x = y-coordinate (row)
;  y = x-coordinate (column)
;  selected:h->text to print
; returns:
;  x,y = coord. of next line
;
printentry:
txa
pha
tya
pha         ; store x,y
clc         ; function: set cursor
jsr PLOT    ; set place to print
ldy #0
- lda (selected),y  ; load char
beq +       ; jump if 0
jsr CHROUT  ; print char
iny
cpy #namelen
bne -       ; loop entrylen times
+ pla
tay
pla
tax     ; restore x,y
inx     ; x++ (next row)
rts


!if disknameena + fastscrolle != 0 {
; - printoverentry
; parameters:
;  x = y-coordinate (row)
;  y = x-coordinate (column)
;  tmpptr:h->text to print
; returns:
;  x,y = coord. of line end
;
printoverentry:
clc         ; function: set cursor
jsr PLOT    ; set place to print
ldy #0
- lda (tmpptr),y    ; load char
beq +       ; jump if 0
jsr CHROUT  ; print char
iny
cpy #namelen
bne -
rts
+ lda #listbackgnd
- jsr CHROUT
iny
cpy #namelen
bne -
sec
jsr PLOT
rts
} ; disknameena + fastscrolle != 0


!if statusenabl = 1 {
; - printstatus
;
printstatus:
ldy #statusx
ldx #statusy
clc         ; function: set cursor
jsr PLOT    ; set place to print
ldy #(entrylen-1)
lda (selected),y
tax
ldy #0
- lda filetypes_print,x
jsr CHROUT
inx
iny
cpy #3
bne -
rts
}


!if fastscrolle = 1 {
; - scrolldown
;
scrolldown:
lda menustate
and #$f0
sta menustate

lda #<screen+(listtopy+1)*ScreenCols+listx
sta scrolldownll
lda #>screen+(listtopy+1)*ScreenCols+listx
sta scrolldownlh
lda #<screen+(listtopy)*ScreenCols+listx
sta scrolldownsl
lda #>screen+(listtopy)*ScreenCols+listx
sta scrolldownsh
ldx #listbottomy-listtopy-1
-- ldy #namelen-1
- 
scrolldownll = *+1
scrolldownlh = *+2
lda $1234,y
scrolldownsl = *+1
scrolldownsh = *+2
sta $1234,y
dey
cpy #$ff
bne -
clc
lda scrolldownll
sta scrolldownsl
adc #ScreenCols
sta scrolldownll
lda scrolldownlh
sta scrolldownsh
adc #0
sta scrolldownlh
dex
bne --

jsr scrollcheckend      ; check if end
lda menustate
and #2
beq ++
lda #0
sta tmp
lda #<tmp
sta tmpptr
lda #>tmp
sta tmpptrh
++ lda tbllsth          ; check if selected = table last
cmp selectedh
bne +
lda tbllst
cmp selected
bne +
lda menustate
ora #4                  ; last reached, bit 2 = 1
sta menustate
bne +++                 ; if last, don't print
+ ldx #listbottomy-1
ldy #listx
jsr printoverentry
+++ ldy #0
rts


; - scrollup
;
scrollup:
lda menustate
and #$f0
sta menustate

lda #<screen+(listbottomy-2)*ScreenCols+listx
sta scrollupll
lda #>screen+(listbottomy-2)*ScreenCols+listx
sta scrolluplh
lda #<screen+(listbottomy-1)*ScreenCols+listx
sta scrollupsl
lda #>screen+(listbottomy-1)*ScreenCols+listx
sta scrollupsh
ldx #listbottomy-listtopy-1
-- ldy #namelen-1
- 
scrollupll = *+1
scrolluplh = *+2
lda $1234,y
scrollupsl = *+1
scrollupsh = *+2
sta $1234,y
dey
cpy #$ff
bne -
sec
lda scrollupll
sta scrollupsl
sbc #ScreenCols
sta scrollupll
lda scrolluplh
sta scrollupsh
sbc #0
sta scrolluplh
dex
bne --

jsr scrollcheckend      ; check if end
lda #>tbl               ; check if selected = table first
cmp selectedh
bne +
lda #<tbl
cmp selected
bne +
lda menustate
ora #1                  ; top reached, bit 0 = 1
sta menustate
+ lda selected
sta tmpptr
lda selectedh
sta tmpptrh
ldx #listtopy
ldy #listx
jsr printoverentry
+++ ldy #0
rts


; - scrollcheckend
; parameters:
;  selected:h
; returns:
;  menustate ( = a )
;  tmpptr
;
scrollcheckend:
lda #<(listbottomy-listtopy-1)*entrylen
sta tmp
lda #>(listbottomy-listtopy-1)*entrylen
sta tmp2
clc
lda selected
adc tmp
sta tmpptr
lda selectedh
adc tmp2
sta tmpptrh
lda tbllsth             ; check if table last >= tmpptr
cmp tmpptrh
bcc +
bne ++
lda tbllst
cmp tmpptr
bcs ++
+ lda menustate
ora #2               ; end reached, bit 1 = 1
sta menustate
++ rts
} ; fastscrolle


!if pleasewaite = 1 {
; - showpleasewait
; parameters:
;  selected:h->text to be shown
;
showpleasewait:
jsr clearlist
ldx #listtopy
ldy #listx
jsr printentry	; print filename
lda selected
pha
lda selectedh
pha
lda #<pleasewait
sta selected
lda #>pleasewait
sta selectedh
jsr printentry	; print "loading"
pla
sta selectedh
pla
sta selected
rts
}


; - parseext  
; parameters:
;  tmpptr:h+y->first char of extension
;  extension_list->list of extensions
;  a = extension length
;  x = extension max
; returns:
;  x = offset to next extension (or 0 if not found)
;
parseext:
sty tmp     ; store offset to first char
sta extension_len   ; store len
stx extension_max   ; store max
ldx #0
stx extension_limit ; store offset to extension
-- ldy tmp  ; y->first char of extension
lda extension_limit
tax         ; x->filetypes
clc
extension_len = *+1
adc #3      ; a->end of filetypes
sta extension_limit ; x->filetypes
extension_max = *+1
cpx #$12
beq +       ; extension not found
- lda (tmpptr),y
and #%01111111    ; compare shifted chars too, turn off bit 7 (Tap,TAP,tAP etc, all recognized) 
extension_list = *+1
cmp $1234,x
bne --
iny
inx
extension_limit = *+1
cpx #0
bne -
; match found (x->next extension)
rts
; no match found
+ ldx #0
rts


; - openclose
; parameters:
;  x:y->filename
;  a = filenamelen (or cmdlen - 3)
;
openclose:
clc
adc #3      ; add "cd:"
jsr SETNAM
lda #1      ; filenr
ldx device  ; unitnr
ldy #15     ; sec.address (,15)
jsr SETLFS
jsr OPEN    ; OPEN1,device,15,""
; error detection would be nice :)
lda #1
jsr CLOSE   ; CLOSE1
rts


; - loadlist
; parameters:
;  selected:h->text to be shown
;  tmpptr:h->dir/diskname as petscii
;  disknamepetlen = length of dir/diskname - 1
; returns:
;  tbl = the list
;  tbllst:h->end of list
;  selected->top of list (or remembered position)
;
loadlist:
!if disknameena = 1 & drivechange = 1 {
jsr cleardiskname
}
!if pleasewaite = 1 {
; clear list & show info
jsr showpleasewait
}

; prepare for extension parsing
lda #<filetypes
sta extension_list
lda #>filetypes
sta extension_list+1

; send open command
ldx tmpptr
ldy tmpptrh
lda disknamepetlen
jsr openclose

; load the list

lda #1              ; filenr
ldx device          ; unitnr
ldy #0              ; sec.address (0 = reloc)
jsr SETLFS
lda #1              ; filenamelen
ldx #<disklist
ldy #>disklist      ; x:y->filename ("$")
jsr SETNAM
lda #0              ; a = 0: load
!if rootentry = 0 {
ldx #<tbl 
} else {
ldx #<tbl+entrylen
}
stx selected
stx tmpptr
!if rootentry = 0 {
ldy #>tbl           ; load address = tbl (leave <- menu entries only)
} else {
ldy #>tbl+entrylen  ; load address = tbl + 17 (leave <- and // menu entries)
}
sty selectedh       ; selected:h->2nd entry in list
iny                 ; load address = tbl+256
sty tmpptrh         ; tmpptr:h->load address

!if drivechange = 1 {

;lda #0              ; clear dir entry (lda #0 for load is not required, A is 0 at this point)
sta $9d
ldy #128
cleardir_loop:
sta (tmpptr),y      ; clear dir entry
dey
bne cleardir_loop
ldy tmpptrh         ; tmpptr:h->load address (Restore Y value)
}

jsr LOAD

; - parse list
; skip first 6 bytes
clc
lda tmpptr
adc #6
sta tmpptr
bcc +
inc tmpptrh

+
!if disknameena = 1 {
; print disk name
lda #18
jsr CHROUT
ldx #disknamey
ldy #disknamex
jsr printoverentry
lda #146
jsr CHROUT
}

loadlist_loop: 
; search 0
ldy #0
- lda (tmpptr),y
beq +       ; if 0 -> next line
inc tmpptr
bne -
inc tmpptrh
bne -       ; loop until zero

; next entry (skips "_" on first time), set tbllst = current
+ clc
lda selected
sta tbllst
adc #<entrylen
sta selected
lda selectedh
sta tbllsth
adc #>entrylen
sta selectedh

; skip 5 bytes (0 + filelen&type)
+ clc
lda tmpptr
adc #5
sta tmpptr
bcc +
inc tmpptrh

; search for " (or 0 for last)
+
- lda (tmpptr),y
beq +++     ; if 0 -> "BLOCKS FREE" -> finished
cmp #quotechar
beq ++      ; if " -> start of filename found
inc tmpptr
bne -
inc tmpptrh
bne -       ; loop until zero

; error - no " or 0 found.

; finished
+++
; put end mark
sta (selected),y
lda #<tbl
sta selected
lda #>tbl
sta selectedh
rts

; copy name until last "
++ iny       ; y = 1, skip first "
ldx #0
- lda (tmpptr),y
cmp #quotechar
beq +       ; if ", ok
dey         ; y--, for selected
sta (selected),y
iny         ; restore y
iny         ; next y
bne -

; error - no " found.

; 0-pad to entrylen
+ lda #0
sty tmp2    ; tmp2 -> "
dey
- cpy #entrylen
beq +
sta (selected),y
iny
bne -

; skip ' ''s
+ ldy tmp2
iny         ; y++, skip last "
ldx #0
- lda (tmpptr),y
cmp #' '
bne +       ; jump if not ' '
iny         ; next y
bne -

; skip possible '*'
+ cmp #'*'
bne +
iny

; parse filetype
+ lda #3      ; extension len
ldx #filetypes_max  ; extension max
jsr parseext

; save extension to entry
ldy #(entrylen-1)
txa
sta (selected),y

jmp loadlist_loop


!if sortdir = 1 {

;
; Sort code by Mike from Denial Forum.
;

sortlist:

!if rootentry = 0 {

lda #<tbl+entrylen ; start list to sort (skip the first item <-)

sta tmpptr
lda #>tbl+entrylen ; starth list to sort
sta tmpptr+1

} else {

lda #<tbl+entrylen+entrylen ; start list to sort (skip the first 2 items <- and //)
sta tmpptr
lda #>tbl+entrylen+entrylen ; starth list to sort
sta tmpptr+1

}

lda #0
sta tmp3

sort_00:
clc
lda tmpptr
sta tmp
adc #entrylen   ;17
sta tmpptr
lda tmpptr+1
sta tmp+1
adc #0
sta tmpptr+1
lda tmp
cmp tbllst
lda tmp+1
sbc tbllsth
bcc sort_01
lda tmp3
bne sortlist
rts

sort_01:
ldy #0

sort_02:
lda (tmpptr),y
cmp (tmp),y
bcc sort_03
bne sort_00
iny
cpy #entrylen   ;17
bne sort_02
beq sort_00

sort_03:
lda #1
sta tmp3
ldy #0

sort_04:
lda (tmp),y
pha
lda (tmpptr),y
sta (tmp),y
pla
sta (tmpptr),y
iny
cpy #entrylen   ;17
bne sort_04
beq sort_00


}


; --- Variables

; device number (8,9,...)
device !by hwdevicenum

; last joystick state (for edge detection)
lastjoy !by $ff


!if sortdir = 1 {

; sort flag used for sortlist routine
sortflag !by $0

}




; --- Strings

;    |---------0---------0---------0--------|


!if printbars = 1 {


!if drivechange = 1 {
drive_text:
!if variant = 2 {

;C64-DTV (DTV_D) button

!tx $12,"d",$92,"rive",0
} else {

!tx $12,"d",$92,"rive",0
}

}


!if key_quit < 255 {
quit_text:
!tx $12,"q",$92,"uit",0
}

!if variant = 2 {

;C64-DTV 

cdback_text:
!tx $12,"rgbtn",$92,"cd",leftarrowchar,0

}


!if sortdir = 1 {

!if variant = 2 {

;C64-DTV (DTV_C) button

sortOFF_text:
!tx $12,"c",$92,"sort",0

sortON_text:
!tx $12,"c",$92
!if buttoncolor>4 {
!tx buttoncolor
}

!tx "sort"

!if forecolor>4 {
!tx forecolor
}


} else {

sortOFF_text:
!tx $12,"s",$92,"ort",0

sortON_text:
!tx $12,"s",$92
!if buttoncolor>4 {
!tx buttoncolor
}

!tx "ort"

!if forecolor>4 {
!tx forecolor
}

}
!tx 0

}

}


!if variant = 3 {

;inittxt !tx $0d, "DEFKEY(4,", $22, "USE SDTOOLS", $22, "13", $22, $22, ")", 0

version !tx $0d,"SDTOOLS by John Carmony, V.03", $0d,0

helpt	!tx $0d, $0d, "After 'linking' use 'USE sdtools'", $0d
		!tx "Then simply use 'fb' to enter",$0d 
		!tx "the filebrowser. You must use ",$0d
		!tx "'USE sdtools' after running or ",$0d
		!tx "using another package",$0d,$0d
		!tx "To use the 'go' feature, simply", $0d
		!tx "enter 'go (",$22,"basicfilename",$22,")'", $0d,$0d,0

setunit0 !tx "unit$ ", $22, "0:",0
setunit1 !tx "unit$ ", $22, "1:",0

loadtext !tx "load", $22,0
loadtextlen =*-loadtext

rettxt !tx " ", $0d,0
rettxtlen =*-rettxt

runtext !tx $22, $0d, "RUN", $0d,0
runtextlen =*-runtext

}

extensions
!tx "dnp",0   ; this extension must be the first for submenu logic of this file image
!if tap_support = 1 {
!tx "tap",0   ; this extension must be the second for submenu logic of this file image
}
!tx "d64",0
!tx "d71",0
!tx "d81",0
!tx "m2i",0
!tx "d41",0
extensions_max = * - extensions

filetypes_print
!tx "cd",leftarrowchar
filetypes
!tx "dir"
!tx "del"
!tx "seq"
!tx "prg"
!tx "usr"
!tx "rel"
!tx "cbm"
filetypes_max = * - filetypes

!if pleasewaite = 1 {
pleasewait
!tx "loading...",0
}

!if tap_support = 1 {
tapname     ; "xt:0123456789abcdef",0
!tx "xt:",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
}

disklist
!tx "$"

diskcmdroot
!tx "cd//",0

diskcmdcd   ; "cd:0123456789abcdef",0
!tx "cd:"
disknamepet ; "0123456789abcdef",0
!tx 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

diskcmdexit ; "cd:_"
!tx "cd:"

; --- Program table

tbl


; "prev dir" entry is always present
!tx leftarrowchar,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ; name
!by 0 ; type

!if rootentry = 1 {
; "exit to root" entry 
!tx "//",0,0,0,0,0,0,0,0,0,0,0,0,0,0 ; name
!by 0 ; type
}



tblbaseend


!if variant = 3 {


end !by 0

}


progsize = * - entry
