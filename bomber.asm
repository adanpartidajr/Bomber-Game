    processor 6502

; TODO:
; Add sound for missile
; Change color of terrain to desert
; Change sprite to have dual jets when not turning
; Add sound when p0 (jet) collides with p1 (bomber)
; Fix missile position to be center and top of jet nose
; Add 10 point to score when the timer reaches 100. The timer should restart from 0 and count up again.
; When score reaches 99, stop p0 and p1 onscreen, turn off jet engine noise and have score loop throuh colors to make a rainbow effect. If button is
; pressed, then restart the status of the game and jump back to beginning of rendering

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include the required files with the VCS register memory mapping and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    include "macro.h"
    include "vcs.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare the variables starting from memory address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    seg.u Variables
    org $80

JetXPos             byte      ; player 0 x-position
JetYPos             byte      ; player 0 x-position
BomberXPos          byte      ; player 1 y-position
BomberYPos          byte      ; player 1 y-position
MissileXPos         byte      ; x pos of the missile
MissileYPos         byte      ; y pos of the missile
Score               byte      ; score of the game. 2 digit score is stored as BCD
Timer               byte      ; timer for the game, timer is right after score in the memory. 2 digit time is stored as BCD
Temp                byte      ; variable to store the temporary score values
OnesDigitOffset     word      ; lookup table offset for the one's digit in the scoreboard
TensDigitOffset     word      ; lookup table offset for the ten's digit in the scoreboard
JetSpritePtr        word      ; points to the memory address of the player0 sprite lookup table
JetColorPtr         word      ; points to the memory address of the player0 sprite color lookup table
BomberSpritePtr     word      ; points to the memory address of the player1 sprite lookup table
BomberColorPtr      word      ; points to the memory address of the player1 sprite color lookup table
JetAnimationOffset  byte      ; p0 sprite frame offset for animation
Random              byte      ; random number generated to set enemy position
ScoreSprite         byte      ; store the sprite bit pattern for the score
TimerSprite         byte      ; store the sprite bit pattern for the score
TerrainColor        byte      ; store the color for the terrain
RiverColor          byte      ; store the color for the river


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

JET_HEIGHT = 9                ; player0 sprite height (# rows in the lookup table)
BOMBER_HEIGHT = 9             ; player1 sprite height (# rows in the lookup table)
DIGIT_HEIGHT = 5              ; scoreboard digit height (# rows in the lookup table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code at the memory address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    seg code
    org $F000

Reset:
    CLEAN_START               ; call macro to reset memory and registers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda #68
    sta JetXPos               ; jet x-pos = 60
    lda #10
    sta JetYPos               ; jet y-pos = 10
    lda #62
    sta BomberXPos            ; bomber x-pos = 54
    lda #83
    sta BomberYPos            ; bomber y-pos = 83
    lda #%11010100
    sta Random                ; Random = $D4
    lda #0                    ; start score and timer at 0
    sta Score
    sta Timer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare a MACRO to check if we should display the missile 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    MAC DRAW_MISSILE
        lda #%00000000
        cpx MissileYPos       ; compare X current scanline to missile y pos
        bne .SkipMissileDraw  ; if x != missile y pos, then skip draw
.DrawMissile:
        lda #%00000010            ; 2nd bit set to 1 means enable missile 0 display  
        inc MissileYPos
.SkipMissileDraw:
        sta ENAM0                 ; enables drawing missile 0
    ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize the pointers to the correct lookup table addresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda #<JetSprite
    sta JetSpritePtr          ; lo-byte pointer for JetSprite lookup table
    lda #>JetSprite
    sta JetSpritePtr+1        ; hi-byte pointer for the Jetsprite lookup table

    lda #<JetColor
    sta JetColorPtr           ; lo-byte pointer for JetColor lookup table
    lda #>JetColor
    sta JetColorPtr+1         ; hi-byte pointer for the JetColor lookup table

    lda #<BomberSprite
    sta BomberSpritePtr       ; lo-byte pointer for Bomber lookup table
    lda #>BomberSprite
    sta BomberSpritePtr+1     ; hi-byte pointer for the Bomber lookup table

    lda #<BomberColor
    sta BomberColorPtr        ; lo-byte pointer for Bomber color lookup table
    lda #>BomberColor
    sta BomberColorPtr+1      ; hi-byte pointer for the Bomber color lookup table
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

StartFrame:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda #2
    sta VBLANK                ; turn on VBLANK
    sta VSYNC                 ; turn on VSYNC

    REPEAT 3
        sta WSYNC             ; display 3 recommended lines of VSYNC
    REPEND

    lda #0
    sta VSYNC                 ; turn off VSYNC

    REPEAT 33
        sta WSYNC
    REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and tasks performed in the VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda JetXPos
    ldy #0
    jsr SetObjectXPos         ; set player0 horizontal position

    lda BomberXPos
    ldy #1
    jsr SetObjectXPos         ; set player1 horizontal position

    lda MissileXPos
    ldy #2
    jsr SetObjectXPos         ; set missile horizontal position

    jsr CalculateDigitOffset  ; calculate the scoreboard digit lookup table offset

    jsr GenerateJetSound      ; configure and enable our jet engine audio

    sta WSYNC
    sta HMOVE                 ; apply the horizontal offsets previously set

    lda #0
    sta VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the scoreboard lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda #0                    ; clear TIA registers before each frame
    sta COLUBK
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    sta CTRLPF
    
    lda #$0E                  ; set scoreboard color to white
    sta COLUPF

    ldx #DIGIT_HEIGHT         ; start the x counter with 5 (height of digits)

.ScoreDigitLoop
    ldy TensDigitOffset       ; get the tens digit offset for the Score
    lda Digits,y              ; load the bit pattern from lookup table
    and #$F0                  ; mask the graphics for the ones digit
    sta ScoreSprite           ; save the score tens digit in a variable

    ldy OnesDigitOffset       ; get the ones digit offset for the score
    lda Digits,y              ; load the bit pattern from the lookup table
    and #$0F                  ; mask the graphics for the tens digit
    ora ScoreSprite           ; merge it with the saved tens digit sprite
    sta ScoreSprite           ; and save it
    sta WSYNC                 ; wait for end of scanline
    sta PF1                   ; update the playfield for the score sprite

    ldy TensDigitOffset+1     ; get the left digit offset for timer
    lda Digits,y              ; load the digit from lookup table
    and #$F0                  ; mask the left digits to only get the tens digits
    sta TimerSprite           ; save the timer tens digit in a variable

    ldy OnesDigitOffset+1     ; get the left digit offset
    lda Digits,y              ; load the digit from the lookup
    and #$0F                  ; mask the left 4 bits to only get the 
    ora TimerSprite           ; merge it with the saved ones digit sprite
    sta TimerSprite           ; and save it

    jsr Sleep12Cycles         ; wastes cyles to give the beam extra time to go far enough to the right to display the timer in the correct pos.
    sta PF1                   ; update the playfield for Timer display

    ldy ScoreSprite           ; preload for the next scanline
    sta WSYNC                 ; wait for the next scanline

    sty PF1                   ; update playfield for the next score display
    inc TensDigitOffset
    inc TensDigitOffset+1
    inc OnesDigitOffset
    inc OnesDigitOffset+1

    jsr Sleep12Cycles


    dex                       ; x--
    sta PF1
    bne .ScoreDigitLoop       ; if dex !=0, then branch to ScoreDigitLoop

    sta WSYNC

    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC
    sta WSYNC
    sta WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the 84 visible scanlines of our main game (2 line kernal)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GameVisibleLine:
    lda TerrainColor
    sta COLUPF                 ; set the terrain background color
    lda RiverColor
    sta COLUBK
    lda #%00000001
    sta CTRLPF                 ; enable playfield reflection
    lda #$F0                   ; setting PF0 bit pattern
    sta PF0
    lda #$FC                   ; setting PF1 bit pattern
    sta PF1
    lda #$0                    ; setting PF2 bit pattern
    sta PF2
    ldx #85                    ; x counts the number of remaining scanlines
.GameLineLoop:
    DRAW_MISSILE               ;macro to check if we should draw the missile

.AreWeInsideTheJetSprite:
    txa                        ; transfer x to the a register
    sec                        ; make sure the carry flag is set for subtraction
    sbc JetYPos                ; subtract y sprite coordinate
    cmp #JET_HEIGHT             ; are we inside the sprite
    bcc .DrawSpriteP0          ; if the result is less than the sprite height, call the draw routine
    lda #0                     ; else, set lookup index to zero

.DrawSpriteP0
    clc                        ; clear carry flag before addition
    adc JetAnimationOffset     ; jump to the correct frame address in memory
    tay                        ; load y so we can work with the pointer
    lda (JetSpritePtr),y       ; load player0 bitmap data from lookup table
    sta WSYNC                  ; wait for scanline
    sta GRP0                   ; set graphics for player0
    lda (JetColorPtr),y        ; load color from lookup table
    sta COLUP0                 ; set color for player0

.AreWeInsideTheBomberSprite:
    txa                        ; transfer x to the a register
    sec                        ; make sure the carry flag is set for subtraction
    sbc BomberYPos             ; subtract y sprite coordinate
    cmp #BOMBER_HEIGHT          ; are we inside the sprite
    bcc .DrawSpriteP1          ; if the result is less than the sprite height, call the draw routine
    lda #0 

.DrawSpriteP1:
    tay                        ; load y so we can work with the pointer
    lda #%00000101
    sta NUSIZ1                 ; stretches the p1 sprite
    lda (BomberSpritePtr),y    ; load player1 bitmap data from lookup table
    sta WSYNC                  ; wait for scanline
    sta GRP1                   ; set graphics for player1
    lda (BomberColorPtr),y     ; load color from lookup table
    sta COLUP1                 ; set color for player1

    dex                        ; x--
    bne .GameLineLoop          ; if x != 0, loop back to top of gamelineloop

    lda #0
    sta JetAnimationOffset

    sta WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the VBLANK overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda #2
    sta VBLANK                 ; turn VBLANK on again
    REPEAT 30
        sta WSYNC
    REPEND
    lda #0                     ; turn VBLANK off again
    sta VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Joystick input test for P0 up/down/left/right
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CheckP0Up:
    lda #%00010000             ; loads the a register with the byte pattern for up
    bit SWCHA                  ; bit test (AND) for the byte pattern loaded in a
    bne CheckP0Down            ; if the bit pattern doesn't match, bypass up block and test down next
    lda JetYPos
    cmp #70
    bpl CheckP0Down
.P0UpPressed:
    inc JetYPos
    lda #0                     ; 0
    sta JetAnimationOffset     ; set animation offset to the second frame, no left/right movement so no tilt

CheckP0Down:
    lda #%00100000             ; p0 joystick down
    bit SWCHA
    bne CheckP0Left
    lda JetYPos
    cmp #5
    bmi CheckP0Left
.P0DownPressed:
    dec JetYPos
    lda #0
    sta JetAnimationOffset

CheckP0Left:
    lda #%01000000             ; p0 joystick left
    bit SWCHA
    bne CheckP0Right
    lda JetXPos
    cmp #35
    bmi CheckP0Right
.P0LeftPressed:
    dec JetXPos
    lda #JET_HEIGHT             ; 9
    sta JetAnimationOffset     ; set animation offset to the second frame

CheckP0Right:
    lda #%10000000             ; p0 joystick right
    bit SWCHA
    bne CheckButtonPressed
    lda JetXPos
    cmp #100
    bpl CheckButtonPressed
.P0RightPressed:
    inc JetXPos
    lda #JET_HEIGHT             ; 9 
    sta JetAnimationOffset     ; set animation offset to the second frame

CheckButtonPressed:
    lda #%10000000
    bit INPT4
    bne NoInput                ; if byte doesn't match, jmp to no input
.P0ButtonPressed:
    lda JetXPos                ; load jet x pos
    clc
    adc #4
    sta MissileXPos            ; and set it to the missile x pos
    lda JetYPos                ; load jet y pos
    clc
    adc #5
    sta MissileYPos            ; and set it to the missile y pos

NoInput:
    ; fallback when no input was performed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations to update position for next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateBomberPosition:
    lda BomberYPos
    clc
    cmp #0                     ; comparing bomber y pos with 0
    bmi .ResetBomberPos        ; if it is < 0, then reset y pos back to top of screen
    dec BomberYPos             ; else, decrement bomber y pos for next frame
    jmp EndPosUpdate

.ResetBomberPos
    jsr GetRandomBomberXPos    ; TODO set random bomber x pos

.SetScoreValues:
    sed                        ; set decimal mode for score and timer values
    lda Timer
    clc
    adc #1
    sta Timer
    cld                        ; deacticate BCD mode after updating the score and timer

EndPosUpdate:                  ; fallback for the position update code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations for the collision checks of objects on screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CheckCollisonP0P1:
    lda #%10000000             ; CXPPMM bit 7 detects p0 and p1 collision
    bit CXPPMM
    bne .P0P1Collided          ; if p0 and p1 collision, then game over
    jsr SetTerrainRiverColor   ; else, set pf color to green and blue
    jmp .CheckCollisionM0P1    ; skip to the bottom

.P0P1Collided:
    jsr GameOver               ; call GameOver subroutine

.CheckCollisionM0P1:
    lda #%10000000             ; bit 7 of CXMPO checks collision of m0 and p1
    bit CXM0P                  ; check bit 7 with the above pattern
    bne .M0P1Collided
    jmp EndCollisionCheck
.M0P1Collided:
    sed                         ; enable BCD mode
    lda Score
    clc                         ; enable carry flag for addition
    adc #1                      ; add one to score
    sta Score                   ; and store new value to Score
    cld                         ; disable BCD mode
    lda #0                      
    sta MissileYPos             ; reset the postion of the missile after collision

EndCollisionCheck:              ; fallback
    sta CXCLR                   ; clear all collision flags before next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to start of brand new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    jmp StartFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate audio for the jet engine sound based on the jet y pos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GenerateJetSound subroutine
    lda #3                     ; values 0-15, where 0 is quietest and 15 is loudest
    sta AUDV0                  ; stores value into volume control

;--- We change the pitch of the jet engine sound based on the jet's y pos on the visible playfield  ---;
;--- We divide the jet y pos by 8 to get the jet y pos to fall within the 32 value range of pitches ---;
    lda JetYPos                ; load jetypos to divide by 8
    lsr                        ; N/2
    lsr                        ; N/4
    lsr                        ; N/8
    sta Temp                   ; store temp variable with value inside accumulator
    lda #31                    ; load accumulator with 31. values 0-31, where 31 is lowest pitch and 0 is highest pitch
    sec                        ; set carry flag before subtraction
    sbc Temp                   ; subtract 31 from the value inside the Temp variable pitch = (N/8) - 31
    sta AUDF0                  ; stores value into frequency control

    lda #8                     ; values 0-15, use reference to determine tone type
    sta AUDC0                  ; stores value into ßtone control
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the colors for the terrain and river to green and blue respectively
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SetTerrainRiverColor subroutine
    lda #$C2
    sta TerrainColor
    lda #$84
    sta RiverColor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle object horizaontal position with fine effect
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A is the target x-coordinate position in pixels of our object
;; Y is the object type (0:player0, 1:player1, 2:missile0, 3:missle1,4:ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SetObjectXPos subroutine
    sta WSYNC                  ; start new scanline
    sec                        ; make sure carry flag is set for subtraction
.Div15Loop
    sbc #15                    ; subtract the literal decimal value 15 from the accumulator
    bcs .Div15Loop             ; loop until carry flag is set
    eor #7                     ; handle the offset range from -8 to 7
    asl                        ; shifts the bits because HMP0 only cares about the 4 leftmost bits
    asl
    asl
    asl
    sta HMP0,y                 ; store the fine offset to the correct HMxx
    sta RESP0,y                ; fix object position in 15-step increment
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Over subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GameOver subroutine
    lda #$30
    sta TerrainColor            ; set the terrain to red
    sta RiverColor              ; set the river to red
    lda #0
    sta Score                   ; score goes back to zero
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to generate random linear feedback shift register ranndom number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate a LSFR random number
;; Divide random number by 4 to limit the size of result to match the river bounds
;; Add 30 to compesate for the left green playfield
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GetRandomBomberXPos subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random                  ; performs a series of shifts and bit operations

    lsr
    lsr                         ; divide the random value by 4 by performing two right shifts
    sta BomberXPos
    lda #30                    ; set decimal flag for addition
    adc BomberXPos             ; add 30 to random bomber value
    sta BomberXPos             ; sets new value for bomber x pos

    lda #96
    sta BomberYPos             ; set the y-pos to the top of the screen 
    ;inc Score
    ;inc Timer   

    rts  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle scoreboard digits to be displayed on the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert the high and low nibbles of the variable Score and Time
;; into the offsets of digits lookup table so the values can be displayed
;; Each digit has a height of 5 bytes in the lookup table
;;
;; For the low nibble we need to multiply by 5
;;      - we can use left shifts to multiply by 5
;;      - for any number N, the value of N * 5 = (N*2*2) + N
;; For the upper nibble, since it is already 16 we need to divide it by 16 and divide by 5
;;      - we can use right shifts to divide by 2
;;      - for any number N, the value of (N/16) * 5 = (N/2/2/2/2) + (N/2/2)

CalculateDigitOffset subroutine
    ldx #1                     ; x register is the loop counter
.PrepareScoreLoop              ; this will loop twice. first x = 1, and then x = 0

    lda Score,x                ; load the accumulator with the Timer when the x = 1 or the Score when the x = 0
    and #$0F                   ; 00001111. Masks the last 4 bits of the bitmap for the digits. We only want the right 4 bits for the ones digit
    sta Temp                   ; save the value of the scoreboard in the temp variable. Temp is N in the above equations.
    asl                        ; shift left (N * 2)
    asl                        ; shift left (now N*2*2)
    adc Temp                   ; add N (now N*2*2) + N
    sta OnesDigitOffset,x      ; save A in OnesDigitOffset+1 or OnesDigitOffset

    lda Score,x                ; load A with Timer (x=1) or Score (x=0)
    and #$F0                   ; remove the ones digit by masking 4 bits 11110000
    lsr                        ; shift right (N/2)
    lsr                        ; shift right (N/4)
    sta Temp
    lsr                        ; shift right (N/8)
    lsr                        ; shift right (N/16)
    adc Temp                   ; (N/16+N/4)
    sta TensDigitOffset,x      ; save A in TensDigitsOffset+1 or TensDigitOffset





    dex                        ; x--
    bpl .PrepareScoreLoop      ; while x > 0, loop to pass a second time

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to waste 12 cycles
;; jsr takes 6 cyles and rts takes 6 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Sleep12Cycles subroutine
    rts 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set lookup tables for bitmaps and colors of sprites and scoreboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;---Bitmaps for the digits in the scoreboard---;

Digits:
;--- first nible is the 1st digit and the second nible is the 2nd digit---;
;---00---;
    .byte %01110111
    .byte %01010101
    .byte %01010101
    .byte %01010101
    .byte %01110111
;---11---;        
    .byte %00010001
    .byte %00010001
    .byte %00010001
    .byte %00010001        
    .byte %00010001
;---22---;        
    .byte %01110111
    .byte %00010001
    .byte %01110111
    .byte %01000100
    .byte %01110111
;---33---;        
    .byte %01110111
    .byte %00010001
    .byte %00110011
    .byte %00010001
    .byte %01110111
;---44---;        
    .byte %01010101
    .byte %01010101
    .byte %01110111
    .byte %00010001
    .byte %00010001
;---55---;        
    .byte %01110111
    .byte %01000100
    .byte %01110111
    .byte %00010001
    .byte %01110111
;---66---;           
    .byte %01110111
    .byte %01000100
    .byte %01110111
    .byte %01010101
    .byte %01110111
;--77---;        
    .byte %01110111
    .byte %00010001
    .byte %00010001
    .byte %00010001
    .byte %00010001
;---88---;        
    .byte %01110111
    .byte %01010101
    .byte %01110111
    .byte %01010101
    .byte %01110111
;---99---;        
    .byte %01110111
    .byte %01010101
    .byte %01110111
    .byte %00010001
    .byte %01110111
;---AA---;        
    .byte %00100010
    .byte %01010101
    .byte %01110111
    .byte %01010101
    .byte %01010101
;---BB---;         
    .byte %01100110
    .byte %01010101
    .byte %01100110
    .byte %01010101
    .byte %01100110
;---CC---;        
    .byte %00110011
    .byte %01000100
    .byte %01000100
    .byte %01000100
    .byte %00110011
;---DD---;        
    .byte %01100110
    .byte %01010101
    .byte %01010101
    .byte %01010101
    .byte %01100110
;---EE---;        
    .byte %01110111
    .byte %01000100
    .byte %01100110
    .byte %01000100
    .byte %01110111
;---FF---;        
    .byte %01110111
    .byte %01000100
    .byte %01100110
    .byte %01000100
    .byte %01000100

;---can also use hex values instead of the binary patterns, much smaller---;

;---End of digits data---;

;---Graphics Data from PlayerPal 2600---

JetSprite:
    .byte #%00000000               ; adds padding for y-pos calculations
    .byte #%00001000;$FE
    .byte #%01111111;$0C
    .byte #%00111110;$0E
    .byte #%00011100;$0E
    .byte #%00011100;$04
    .byte #%00001000;$BA
    .byte #%00001000;$0E
    .byte #%00001000;$08

JetSpriteTurn:
    .byte #%00000000               ; adds padding for y-pos calculations
    .byte #%00001000;$FE
    .byte #%00111110;$0C
    .byte #%00011100;$0E
    .byte #%00011100;$0E
    .byte #%00011100;$04
    .byte #%00001000;$BA
    .byte #%00001000;$0E
    .byte #%00001000;$08

BomberSprite:
    .byte #%00000000               ; adds padding for y-pos calculations
    .byte #%00001000;$32
    .byte #%00001000;$32
    .byte #%00101010;$0E
    .byte #%00111110;$40
    .byte #%01111111;$40
    .byte #%00101010;$40
    .byte #%00001000;$40
    .byte #%00011100;$40

;---Color Data from PlayerPal 2600---

JetColor:
    .byte #$00 
    .byte #$FE;
    .byte #$0C;
    .byte #$0E;
    .byte #$0E;
    .byte #$04;
    .byte #$BA;
    .byte #$0E;
    .byte #$08;

JetTurnColor:
    .byte #$00
    .byte #$FE;
    .byte #$0C;
    .byte #$0E;
    .byte #$0E;
    .byte #$04;
    .byte #$BA;
    .byte #$0E;
    .byte #$08;

BomberColor:
    .byte #$00
    .byte #$32;
    .byte #$32;
    .byte #$0E;
    .byte #$40;
    .byte #$40;
    .byte #$40;
    .byte #$40;
    .byte #$40;

;---End Color Data---

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Close cart by filling it with 4KB of ROM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    org $FFFC
    .word Reset
    .word Reset