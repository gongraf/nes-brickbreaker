;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (c) 2021 grafgonzalo@gmail.com ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
STATE_IN_TITLE    = $00
STATE_IN_GAME     = $01
STATE_GAME_OVER   = $02
PADDLE_Y          = $D8 ;fixed y position at 200px y
RIGHTWALL         = $F2 ;in pixels hex
TOPWALL           = $28
BOTTOMWALL        = $E0
LEFTWALL          = $10
RIGHT_PRESSED     = $01 ;%00000001
LEFT_PRESSED      = $02 ;%00000010
DOWN_PRESSED      = $04 ;%00000100
UP_PRESSED        = $08 ;%00001000
START_PRESSED     = $10 ;%00010000
SELECT_PRESSED    = $20 ;%00100000
A_PRESSED         = $40 ;%01000000
B_PRESSED         = $80 ;%10000000
SCORE_ONES_PPU    = $4B 
SCORE_TEN_PPU     = $4A
SCORE_HUNDRED_PPU = $49
SCORE_THOUSND_PPU = $48
PLAYER_RESETS_PPU = $54
NAMETABLE_PPU_1   = $20
PLAYER_RESETS     = $03


.segment "HEADER"
  .byte "NES"
  .byte $1a
  .byte $02       ;2*16kb prg rom
  .byte $01       ;1*8kb chr rom
  .byte %00000000 ;mapper and mirror set all 0 for now.
  .byte $00
  .byte $00
  .byte $00
  .byte $00
  .byte $00, $00, $00, $00 ;filler bytes.

.segment "ZEROPAGE"
  nmi_ready:      .res 1
  buttons1:       .res 1  ;controller 1 A B select start	up	down left right
  buttons2:       .res 1  ;controller 2
  gameState:      .res 1 
  ballX:          .res 1  ;ball position
  ballY:          .res 1
  ballSpeedx:     .res 1  ;ball speed
  ballSpeedy:     .res 1
  ballUp:         .res 1  ;ball current direction
  ballDown:       .res 1
  ballLeft:       .res 1
  ballRight:      .res 1
  ballcenterX:    .res 1
  ballWidth:      .res 1
  pointImpact:    .res 1
  paddleX:        .res 1
  paddleW:        .res 1
  paddleLeft:     .res 1
  paddleRight:    .res 1
  paddleSpeed:    .res 1
  world:          .res 2
  tileY:          .res 1
  tileX:          .res 1
  tileAddress:    .res 2
  isColission:    .res 1   ;might not be needed
  ballLaunched:   .res 1   ;flag to tell if ball is in play
  temp:           .res 1
  nmt_update_len: .res 1
  nmt_update:     .res 75  ;nametable update buffer. Handle up to 25 tile updates in a frame  
  tilePointer:    .res 2
  scoreOnes:      .res 1
  scoreTens:      .res 1
  scoreHunds:     .res 1
  scoreThounds:   .res 1
  resets:         .res 1

.segment "BSS"             ;set it to start at mem $0300 in the nes.cfg
  tileMap:        .res 960


.segment "STARTUP"
  Reset:
    sei         ;disable all interrupts
    cld         ;disable decimal mode 
    
    ldx #$40    
    stx $4017   ;disables sound

    ldx #$FF    ;init stack register pointer
    txs         ;
    
    inx         ; so it wraps to 0
    stx $2000   ;disable NMI
    stx $2001   ;disable rendering

    stx $4020   ;disable PCM samle

  :   ;wait for vblank
    bit $2002   ;see if bit number 7 is 1 means we are ready
    bpl :-      ;if bit 7 is 0 branch back until is 1

    txa         ; x is 0 at this point 
  :
    sta $0000, x ; start clearing ram
    sta $0100, x ; $0100 => $01FF and so on...
    sta $0300, x ;
    sta $0400, x ;
    sta $0500, x ;
    sta $0600, x ;
    sta $0700, x ;
    lda #$FF     ; init sprites with 255 so it will be offscreen
    sta $0200, x ; for sprites
    lda #$00     ; set acc back to 0
    inx          ;          
    bne :-       ;until x wraps to 0 keep looping

    
  :   ;wait for another vblank
    bit $2002   ;see if bit number 7 is 1 means we are ready
    bpl :-      ;if bit 7 is 0 branch back until is 1

          
    ;tell ppu we are loading palette
    ; $3F00 in ppu memory
    lda #$3F    ;give memory addres in ppu
    sta $2006   ;msb in ppu
    lda #$00
    sta $2006   ;lsb in ppu

    ldx #$00    ; x=0
  LoadPalettes:
    lda PaletteData, x
    sta $2007     ;writte to ppu
    inx  
    cpx #$20      ;32 dec; 16 for backfround and 16 bytes for sprites
    bne LoadPalettes


    ldx #$00
  LoadSprites:
    lda SpriteData, X
    sta $0200, X
    inx 
    cpx #$10   ;16 bytes only as I have 4 sprites
    bne LoadSprites 

;Sprites dma, this will move x bytes starting from $0200 from cpu to ppu
    lda #$00
    sta $2003
    lda #$02    ;tell ppu where the sprite bank is. This is for cpu address $0200
    sta $4014
    nop 

  LoadBackground:
    lda #<background
    sta world
    lda #>background
    sta world+1

    lda #<tileMap
    sta tilePointer
    lda #>tileMap
    sta tilePointer+1

    bit $2002             ; read PPU status to reset the high/low latch
    lda #$20
    sta $2006             ; write the high byte of $2000 address
    lda #$00
    sta $2006             ; write the low byte of $2000 address

    ldx #$00              ;to count 0 - $03 
    ldy #$00              ;to count from 0 - $c0  => 960 bytes = $03c0
  LoadBackgroundLoop:
    lda (world), y        ; load data 
    sta (tilePointer), y   
    sta $2007             ; write to PPU
    iny 
    cpx #$03
    bne :+
    cpy #$c0
    beq DoneLoadingWorld
  :
    cpy #$00               ; if y==0 means it wrapped 255+1
    bne LoadBackgroundLoop ; if not go back to loop
    inx                    ;if y wrapped, move to the next 255 batch of bytes
    inc world+1            ;update world pointer
    inc tilePointer+1
    jmp LoadBackgroundLoop

  DoneLoadingWorld:
    ldx #$00

    
  LoadAttribute:
    bit $2002             ; read PPU status to reset the high/low latch
    lda #$23
    sta $2006             ; write the high byte of $23C0 address
    lda #$C0
    sta $2006             ; write the low byte of $23C0 address
    ldx #$00              ; start out at 0
  LoadAttributeLoop:
    lda attribute, x      ; load data from address (attribute + the value in x)
    sta $2007             ; write to PPU
    inx                   ; X = X + 1
    cpx #$40              ; Compare X to hex $40, decimal 64 - copying 64 bytes
    bne LoadAttributeLoop


  InitGame:
        
    lda #$00
    sta scoreOnes
    sta scoreTens
    sta scoreHunds
    sta scoreThounds
    
    lda #$02
    sta paddleSpeed
    lda #$18
    sta paddleW
    lda #$70
    sta paddleX

    jsr ResetBall
    
    ;load world pointer with current level tilemap
    lda #<tileMap    ;we have only one nametable for all levels currently
    sta world
    lda #>tileMap
    sta world+1

    lda #PLAYER_RESETS
    sta resets

    lda #STATE_IN_GAME
    sta gameState


  cli              ;after all enable interrupts again
  lda #%10000000   ;left 1 enable nmi, second 1 tells ppu to use 2 chr of tiles ($1000)
  sta $2000

  lda #%00011000   ;enable drawing again
  sta $2001

  ;;main loop
  Loop:
    lda nmi_ready 
    bne Loop      ;keep looping while 1
                  ;if 0 means vblank is done and continue...
    
    jsr ReadController1
    
    jsr GameEngine
    
    lda #1
    sta nmi_ready
    
    jmp Loop ;;forever loop

  NMI:
    lda #$00
    sta $2003
    lda #$02 ;update sprite data in ppu
    sta $4014
    
    jsr DrawScores
    jsr UpdateNametable

    lda #$00
    sta $2005 ;keep x scrooll in 0
    sta $2005 ;keep y scroll in 0
    
    lda #0    ;clear vblank flag so main loop can go on
    sta nmi_ready

    rti;  


.segment "CODE"

pointscontact: 
   .byte $00,$04,$08,$0c,$10,$14,$18

getNewSpeed:
   .byte $03,$02,$01,$01,$01,$01,$02,$03,$03 ;adding extra entry as default 

getNewSpeedY:
   .byte $01,$01,$02,$01,$01,$02,$01,$01,$01 ;adding extra entry as default

PaletteData:
  ;background palette data
  .byte $0f,$0f,$10,$00, $0f,$0c,$18,$27, $0f,$0f,$1c,$2b, $0f,$0f,$1c,$20
  ;sprite palette data
  .byte $0f,$0F,$10,$30, $0f,$02,$38,$3C, $0f,$1C,$15,$14, $0f,$02,$38,$3C

SpriteData:
  .byte $08,$01,$00,$12 ;ball
  .byte $c8,$02,$00,$80 ;paddle left
  .byte $c8,$03,$00,$88 ;paddle center
  .byte $c8,$04,$00,$96 ;paddle right
;        y   t   f   x

background:
  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$00,$53,$43,$4f,$52,$45,$3a,$30,$30,$30,$30,$30,$00,$54,$52
  .byte $49,$45,$53,$3a,$30,$00,$4c,$45,$56,$45,$4c,$3a,$30,$20,$20,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$07,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08
  .byte $08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$09,$00
  .byte $00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$09,$00
  .byte $00,$07,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$09,$00
  .byte $00,$07,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$09,$00
  .byte $00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$95,$95,$a5,$a5,$a5,$18
  .byte $a6,$a5,$95,$95,$95,$95,$00,$00,$95,$95,$95,$96,$97,$98,$09,$00
  .byte $00,$07,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$09,$00
  .byte $00,$07,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$09,$00
  .byte $00,$07,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$09,$00
  .byte $00,$07,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$09,$00
  .byte $00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$09,$00
  .byte $00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$70,$71,$72,$09,$00
  .byte $00,$07,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$09,$00
  .byte $00,$07,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$09,$00
  .byte $00,$07,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$09,$00
  .byte $00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$10,$11
  .byte $10,$11,$10,$11,$00,$97,$00,$00,$00,$00,$9c,$b0,$b1,$b2,$09,$00
  .byte $00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$6c,$7c,$a0,$a1
  .byte $a2,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$09,$00
  .byte $00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$b3,$b4,$b5,$b6,$00,$00,$00,$00,$00,$00,$00,$00,$00,$09,$00
  .byte $00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$8a,$8b,$8c,$9c,$c0,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$09,$00
  .byte $00,$07,$10,$00,$00,$00,$10,$11,$98,$00,$00,$00,$00,$00,$00,$d1
  .byte $d2,$d3,$d4,$d5,$d6,$d7,$00,$00,$11,$00,$00,$10,$00,$00,$09,$00
  .byte $00,$07,$ca,$ca,$ca,$ca,$ca,$00,$00,$00,$aa,$ab,$ac,$bc,$e0,$e1
  .byte $e2,$e3,$e4,$e5,$e6,$e7,$00,$00,$ea,$eb,$ec,$00,$00,$00,$09,$00
  .byte $00,$07,$ca,$ca,$ca,$ca,$00,$00,$00,$00,$00,$bb,$bc,$cc,$f0,$f1
  .byte $f2,$f3,$f4,$f5,$f6,$f7,$00,$00,$fa,$00,$00,$00,$00,$00,$09,$00
  .byte $00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$09,$00
  .byte $00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$09,$00
  .byte $00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$09,$00
  .byte $00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$09,$00
  .byte $00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$09,$00
  .byte $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa
  .byte $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa
  .byte $aa,$aa,$aa,$2a,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$0a,$8a,$aa,$aa
  .byte $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a


attribute:
  .byte %01010000, %01010000, %01010000, %01010000, %01010000, %01010000, %01010000, %01010000
  .byte %10000000, %10100000, %10100000, %10100000, %10100000, %10100000, %10100000, %00100000
  .byte %11001100, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %00110011
  .byte %11001100, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %00110011
  .byte %00001100, %11011111, %11111111, %11111111, %11111111, %11111111, %11111111, %00000011
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

LatchController:
  lda #$01
  sta $4016
  lda #$00
  sta $4016
  rts

ReadController1:
  jsr LatchController
  ldx #$08        ; 8 buttons to read
  :               ; read controller loop
  lda $4016
  lsr a           ; bit0 -> Carry
  rol buttons1    ; bit0 <- Carry
  dex             ; 1 button less to read
  bne :-          ; not done so iterate again
  rts

ReadController2:
  jsr LatchController
  ldx #$08
  :
  lda $4017
  lsr a
  rol buttons2
  dex
  bne :-
  rts

GameEngine:
    lda gameState
    cmp #STATE_IN_TITLE
    beq EngineTitle

    lda gameState
    cmp #STATE_GAME_OVER
    beq EngineGameOver

    lda gameState
    cmp #STATE_IN_GAME
    beq EnginePlaying

    rts;

EngineTitle:
    rts;


EngineGameOver:
    rts;


EnginePlaying:
    
    LaunchBall:
      lda buttons1
      and #B_PRESSED
      beq LaunchBallDone     ;if B not pressed skip
      lda ballLaunched
      bne LaunchBallDone     ;if B was pressed but ball is already launched ingore B press
      
      lda #1                 ;at this point B was pressed and ball is not launched.
      sta ballLaunched       ;set ball launched

    LaunchBallDone:
    ;

    MoveBallRight:
        lda ballRight
        beq MoveBallRightDone  ;if ballRight == 0 branch

        lda ballX
        clc
        adc ballSpeedx   ;ballx = ballx+ballspeed
        sta ballX

        lda ballX     ;add ball width for accurate colision
        clc
        adc #$08
        cmp #RIGHTWALL
        bcc MoveBallRightDone ; carry will be clear if ballx < rightwall skip next

        lda #$00
        sta ballRight
        lda #$01
        sta ballLeft
    MoveBallRightDone:
        ;

    MoveBallLeft:
        lda ballLeft
        beq MoveBallLeftDone ;if ballleft 0 skip

        lda ballX
        sec
        sbc ballSpeedx
        sta ballX

        lda ballX
        cmp #LEFTWALL
        bcs MoveBallLeftDone ;if ball x > left wall no need to change direction so skip next

        lda #$00
        sta ballLeft
        lda #$01
        sta ballRight
        
    MoveBallLeftDone:
        ;

    MoveBallUp:
        lda ballUp              ;are going up?
        beq MoveBallUpDone      ;no so  skip

        lda ballY               ;yes so get current y
        sec          
        sbc ballSpeedy          ;y= y-vely
        sta ballY               ;save new y

        lda ballY
        cmp #TOPWALL
        bcs MoveBallUpDone      ;if y > top wall no need to do nothing

        lda #$00
        sta ballUp
        lda #$01
        sta ballDown

    MoveBallUpDone:
        ;    

    MoveBallDown:
        lda ballDown
        beq MoveBallDownDone

        lda ballY
        clc
        adc ballSpeedy
        sta ballY

        lda ballY
        cmp #BOTTOMWALL
        bcc MoveBallDownDone

        jsr ResetBall
        ;substract player lives here also check for game over

    MoveBallDownDone:
        ;    

    MovePaddleRight:
    lda buttons1
    and #RIGHT_PRESSED
    beq MovePaddleRightDone

    lda #$00
    sta paddleLeft
    lda #$01
    sta paddleRight

    lda paddleX
    clc
    adc paddleSpeed
    sta paddleX

    lda paddleX
    clc 
    adc paddleW
    cmp #RIGHTWALL
    bcc MovePaddleRightDone
    
    ;;handle colission here
    lda paddleX
    sec
    sbc paddleSpeed
    sta paddleX

    MovePaddleRightDone:
        ;

    MovePaddleLeft:
        lda buttons1
        and #LEFT_PRESSED
        beq MovePaddleLeftDone

        lda #$00
        sta paddleRight
        lda #$01
        sta paddleLeft

        lda paddleX
        sec
        sbc paddleSpeed
        sta paddleX

        lda paddleX
        cmp #LEFTWALL
        bcs MovePaddleLeftDone

        ;handle colission here
        lda paddleX
        clc
        adc paddleSpeed
        sta paddleX

    MovePaddleLeftDone:
        ;    

    CheckCollisions:

        lda ballLaunched            ;if ball launched branch to paddle collision logic
        bne :+
                                  
        lda #PADDLE_Y-6             ;if not stick ball on top of paddle
        sta ballY

        lda paddleX
        clc
        adc #8
        sta ballX                            
        jmp CheckCollisionsDone

        :
        ;bally+ballw < paddle y no collision
        lda ballY
        clc
        adc #$08
        cmp #PADDLE_Y
        bcc CheckCollisionsDone 

        ;if ball y > paddle y +1 no collision
        lda #PADDLE_Y 
        clc
        adc #$01
        cmp ballY
        bcc CheckCollisionsDone 

        ;if ball x + ballw  < paddle x no colision
        lda ballX
        clc
        adc #$08
        cmp paddleX
        bcc CheckCollisionsDone

        ;if ballx > paddlex+paddle width no colission
        lda paddleX
        clc
        adc #$1C
        cmp ballX
        bcc CheckCollisionsDone
        
        ;colission so put ball were it was exactly before colission
        lda ballY
        sec
        sbc ballSpeedy
        sta ballY

        ;we have a colission so now handle ball direction
        lda ballX
        clc
        adc #$04
        sta ballcenterX ;get the center of the ball


        ldx #$00
        @loop:
            lda pointscontact,x
            clc
            adc paddleX
            cmp ballcenterX
            bcs @break      ;carry set means we found point of contact.
            inx             ;not found so keep iterating
            cpx #$07        ;did we check all points of contact?
            bne @loop ;no so keep looping
                            ;yes so end loop
        @break:             ;point of contact is in x if found
        stx pointImpact     ;save it for later use
     
        lda #$00
        sta ballSpeedx      ;reset ball speed
        lda getNewSpeed,x   ;point of impact still in x, use it in look up table
        sta ballSpeedx      ;just overwrite ball speed x with new value

        lda #$00
        sta ballSpeedy
        lda getNewSpeedY,x
        sta ballSpeedy
  
        ldx pointImpact                    
        clc                 ;point of impcat is still in x
        cpx #$04            ;less than 04 means left paddle side
        bcc ComputeLeft     ;carry not set so left paddle side speed negative
        
        lda #$00
        sta ballLeft
        lda #$01
        sta ballRight
        jmp ComputeSpeedDone ;all done with x direction
        
        ComputeLeft:
        lda #$01
        sta ballLeft
        lda #$00
        sta ballRight

        ComputeSpeedDone:
            lda #$00
            sta ballDown
            lda #$01
            sta ballUp
        ;

    CheckCollisionsDone:
        ;

    CheckToTileMap:

      lda ballRight     ;are moving right
      beq @notRight     ;not now
                        ;yes
                        ;calculates the X coordinate of the right edge
      clc
      lda ballX         ;get y position in "pixels coordinates"
      adc #$08          ;add width
      lsr               ;divide it by 8 to get the tiled offset position 
      lsr
      lsr
      sta tileX
      jmp @checkUp 

      @notRight:        ;aka moving left
        clc
        lda ballX
        lsr
        lsr
        lsr
        sta tileX

      @checkUp:
        lda ballUp
        beq @notUp
        ;calculates the top Y coordinate
        clc
        lda ballY     ;get y position in "pixels coordinates"
        lsr           ;divide it by 8 to get the tiled offset position 
        lsr
        lsr
        sta tileY
        jmp @getTile

      @notUp:      ;aka moving down
        clc
        lda ballY
        adc #$08
        lsr
        lsr
        lsr
        sta tileY
      
      @getTile:
        jsr GetTile
        sta pointImpact          ;tile is in A, so save it for later
        cmp #$05                 ;is it 5?
        beq :+                   ;yes
        cmp #$06                 ;no, is it 6?
        beq :+                   ;yes
        jmp CheckToTileMapDone   ;No colission
        :
        jsr IncrementScore       ;we have a collision so update score...
        ldx tileX                ;update tile actually collided
        ldy tileY                ;y position
        lda #$00                 ;just put 0 so brick is gone
        jsr ppu_update_tile
        jsr tileMap_update_tile      

        lda pointImpact
        cmp #$05
        beq @update5          
        jmp @update6             ;we know if not 5 it must be 6             

      @update5:                  ;tile 5 so need to also update right tile to it
        lda tileX
        clc
        adc #$01
        sta tileX
        ldx tileX
        ldy tileY
        lda #$00
        jsr ppu_update_tile
        jsr tileMap_update_tile  ;do the update
        jmp @ballDir
      
      @update6:
        lda tileX                ;tile 6 so need to also update left tile to it
        sec 
        sbc #$01
        sta tileX
        ldx tileX
        ldy tileY
        lda #$00
        jsr ppu_update_tile
        jsr tileMap_update_tile  ;do the update
        
      
      @ballDir:
        ;now reflect collision in ball direction                       
        lda ballUp
        beq :+                   ;branch if not moving up
        ;put ball were it was before colisisin
        lda ballY
        clc
        adc ballSpeedy
        sta ballY
        ;invert y dir
        lda #$01
        sta ballDown
        lda #$00
        sta ballUp
        jmp @checkRight

      :  ;handle moving down
      ;put ball were it was before colission
        lda ballY
        sec
        sbc ballSpeedy
        sta ballY
        ;invert y dir
        lda #$00
        sta ballDown
        lda #$01
        sta ballUp
      
      @checkRight:
        lda ballLeft
        beq :+
        lda ballX
        sec 
        sbc ballSpeedx
        sta ballX
        jmp CheckToTileMapDone

      :
        lda ballX
        clc
        adc ballSpeedx
        sta ballX
      
    CheckToTileMapDone:
        ;    
    
    jsr UpdateSprites        
    rts;


GetTile:
  ;does Address = TileY * 32
  lda #$00
  sta tileAddress+0 ;clears the low byte
  lda tileY
  lsr
  ror tileAddress+0
  lsr
  ror tileAddress+0
  lsr
  ror tileAddress+0
  sta tileAddress+1 ;shifting the high byte right 3x is faster than shifting the low byte left 5x

  ;does Address = Address + MapAddress
  clc
  lda tileAddress+0
  adc world+0         ;world points to the current tilemap rendered
  sta tileAddress+0
  lda tileAddress+1
  adc world+1         ;world points to the current tilemap rendered
  sta tileAddress+1

  ;uses indexing instead of doing Address = Address + TileX
  ldy tileX

  ;reads the tile index
  lda (tileAddress), y
  rts;

;it will just replace tile at tileX,tileY with 0
;TODO custom tile to replace
tileMap_update_tile:
  ;does Address = TileY * 32
  lda #$00
  sta tileAddress+0 ;clears the low byte
  lda tileY
  lsr
  ror tileAddress+0
  lsr
  ror tileAddress+0
  lsr
  ror tileAddress+0
  sta tileAddress+1 ;shifting the high byte right 3x is faster than shifting the low byte left 5x

  ;does Address = Address + MapAddress
  clc
  lda tileAddress+0
  adc world+0         ;world points to the current tilemap rendered
  sta tileAddress+0
  lda tileAddress+1
  adc world+1         ;world points to the current tilemap rendered
  sta tileAddress+1

  ;uses indexing instead of doing Address = Address + TileX
  ldy tileX

  ;replace the tile index with 0 from A
  lda #$00
  sta (tileAddress), y
  rts

DrawScores:
    
    bit $2002
    
    lda #NAMETABLE_PPU_1
    sta $2006
    lda #SCORE_ONES_PPU
    sta $2006       
    lda scoreOnes
    clc
    adc #$30                
    sta $2007      
    
    lda #NAMETABLE_PPU_1
    sta $2006
    lda #SCORE_TEN_PPU
    sta $2006          
    lda scoreTens
    clc
    adc #$30             
    sta $2007          

    lda #NAMETABLE_PPU_1
    sta $2006
    lda #SCORE_HUNDRED_PPU
    sta $2006          
    lda scoreHunds
    clc
    adc #$30             
    sta $2007          

    lda #NAMETABLE_PPU_1
    sta $2006
    lda #SCORE_THOUSND_PPU
    sta $2006          
    lda scoreThounds
    clc
    adc #$30             
    sta $2007          

    lda #NAMETABLE_PPU_1
    sta $2006
    lda #PLAYER_RESETS_PPU
    sta $2006
    lda resets
    clc
    adc #$30
    sta $2007

    rts

UpdateSprites:
    lda ballY
    sta $0200

    lda #$01
    sta $0201

    lda #$00
    sta $0202

    lda ballX
    sta $0203

    ;;paddle left side
    lda #PADDLE_Y
    sta $0204

    lda #$02
    sta $0205

    lda #$00
    sta $0206

    lda paddleX
    sta $0207

    ;;paddle mid side
    lda #PADDLE_Y
    sta $0208

    lda #$03
    sta $0209

    lda #$00
    sta $020a

    lda paddleX
    clc 
    adc #$08
    sta $020b

    ;;paddle right side
    lda #PADDLE_Y
    sta $020C

    lda #$04
    sta $020D

    lda #$00
    sta $020E

    lda paddleX
    clc 
    adc #$10
    sta $020F

    rts;

UpdateNametable:
  ldx #0
  cpx nmt_update_len
  bcs @return
  @loop:
    lda nmt_update, X
    sta $2006
    inx
    lda nmt_update, X
    sta $2006
    inx
    lda nmt_update, X
    sta $2007
    inx
    cpx nmt_update_len
    bcc @loop
    lda #0
    sta nmt_update_len
  @return:  
    rts

ppu_update_tile:
  pha ; temporarily store A on stack
  txa 
  pha ; temporarily store X on stack
  ldx nmt_update_len
  tya 
  lsr 
  lsr 
  lsr 
  ora #$20 ; high bits of Y + $20
  sta nmt_update, X
  inx 
  tya 
  asl 
  asl 
  asl 
  asl 
  asl 
  sta temp
  pla ; recover X value (but put in A)
  ora temp
  sta nmt_update, X
  inx
  pla ; recover A value (tile)
  sta nmt_update, X
  inx
  stx nmt_update_len
  rts

IncrementScore:
  
  @IncOnes:
    lda scoreOnes
    clc 
    adc #$01
    sta scoreOnes
    cmp #$0A 
    bne @IncDone
  
  @IncTens:  
    lda #$00
    sta scoreOnes
    lda scoreTens
    clc
    adc #$01
    sta scoreTens
    cmp #$0A 
    bne @IncDone

  @IncHundres:
    lda #$00
    sta scoreTens
    lda scoreHunds
    clc
    adc #$01
    sta scoreHunds
    cmp #$0A
    bne @IncDone
  
  @IncThouns:
    lda #$00
    sta scoreHunds
    lda scoreThounds
    clc
    adc #$01
    sta scoreThounds
  
  @IncDone:
    rts;

ResetBall:
  lda #$00
  sta ballUp
  sta ballDown
  sta ballUp
  sta ballDown

  lda #$01
  sta ballSpeedx
  sta ballSpeedy

  lda #$08
  sta ballWidth
  
  lda #PADDLE_Y-6   ;put ball on top of paddle
  sta ballY

  lda paddleX
  sta ballX
  
  lda #$00
  sta ballLaunched

  rts    

.segment "VECTORS"
  .word NMI
  .word Reset
  ;

.segment "CHARS"
  .incbin "tileset.chr"

