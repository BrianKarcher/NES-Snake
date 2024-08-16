; main game Loop
.include "constants.asm"

.import init, load_palette, draw_board, place_food, place_header_food, print_level_end_message
.export zp_temp_1, zp_temp_2, zp_temp_3, screen, current_low, current_high, end_low, end_high, current_low_2, current_high_2
.export random_index, random, tile_to_screen_space_xy, ppu_update_tile, ppu_update_tile_temp, screen_space_to_ppu_space, temp_a, temp_x, temp_y, current_level
.export food_count
; .segment "HEADER"
; 	.byte "NES",26, 2,1, 0,0

.segment "HEADER"

INES_MAPPER = 1 ; 0 = NROM, 1 = MMC 1
INES_MIRROR = 1 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 0 ; 1 = battery backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A ; ID
.byte $02 ; 16k PRG chunk count
.byte $01 ; 8k CHR chunk count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding

; .segment "HEADER"
; .byte 'N', 'E', 'S', $1A  ; NES header magic number
; .byte 2                   ; Number of 16KB PRG ROM banks
; .byte 1                   ; Number of 8KB CHR ROM banks
; .byte $01                   ; Mapper, mirroring, battery, trainer
; .byte $00                   ; Mapper, VS/Playchoice, NES 2.0
; .byte 0, 0, 0, 0, 0, 0, 0, 0 ; Padding bytes

; .segment "HEADER"
; .byte "NES", $1A       ; iNES file identifier
; .byte 2                ; Number of 16 KB PRG-ROM banks
; .byte 1                ; Number of 8 KB CHR-ROM banks
; ;.byte %00000010        ; Flags 6: Mapper, mirroring, battery, trainer
; ;.byte 41
; .byte 40
; .byte %00000000        ; Flags 7: Mapper, VS/Playchoice, NES 2.0
; .byte 0                ; Flags 8: PRG-RAM size (rarely used)
; .byte 0                ; Flags 9: TV system (rarely used)
; .byte 0, 0, 0, 0, 0, 0    ; Unused padding bytes

.segment "ZEROPAGE"
nmi_lock:       .res 1 ; prevents NMI re-entry
nmi_ready:      .res 1 ; set to 1 to push a PPU frame update, 2 to turn rendering off next NMI
buttons:        .res 2
nmi_count:      .res 1
tick_count:     .res 1
head_x:         .res 2 ; Enable two-player
head_y:         .res 2
new_x:          .res 2
new_y:          .res 2
tail_x:         .res 2
tail_y:         .res 2
snake_update:   .res 1
next_dir:       .res 2
cur_dir:        .res 2
target_size:    .res 2
size:           .res 2
head_index:     .res 2
tail_index:     .res 2 ; 0x17
zp_temp_1:      .res 1
zp_temp_2:      .res 1
zp_temp_3:      .res 1
nmt_update_len: .res 1 ; number of bytes in nmt_update buffer
;start_low:      .res 1  ; Low byte of start address
;start_high:     .res 1  ; High byte of start address
end_low:        .res 1  ; Low byte of end address
end_high:       .res 1  ; High byte of end address
current_low:    .res 1  ; Low byte of current address, this is a memory pointer for indirect indexing
current_high:   .res 1  ; High byte of current address
;start_low_2:    .res 1
;start_high_2:   .res 1
current_low_2:  .res 1
current_high_2: .res 1
snake_speed:    .res 1
random_index:   .res 1
food_count:     .res 2
temp_a:         .res 1
temp_x:         .res 1
temp_y:         .res 1
player_count:   .res 1
current_level:  .res 1
level_complete: .res 1

;nmt_update = $6ff
.segment "BSS"          ; This is the 8k SRAM memory (can be used for work or saves)
nmt_update: .res 256 ; nametable update entry buffer for PPU update
screen:     .res 960 ; Mirror of what is in the PPU. The snake can get quite large so we store it in this mirror.
                        ; We sacrifice memory for speed, it takes a while to check collisions on a 100-size snake if not in screen mirror memory.
                     ; The snake is mutable background and collides with itself.


.segment "STARTUP" ; avoids warning
dirs:
    .byte BUTTON_UP, BUTTON_DOWN, BUTTON_LEFT, BUTTON_RIGHT
o_dirs: ; opposite directions - to test if player pressed in opposite of current direction
    .byte BUTTON_DOWN, BUTTON_UP, BUTTON_RIGHT, BUTTON_LEFT

snakes_hi:
    .byte >SNAKE, >SNAKE2
snakes_lo:
    .byte <SNAKE, <SNAKE2

reset:
    sei        ; ignore IRQs
    cld        ; disable decimal mode
    ldx #$40
    stx $4017  ; disable APU frame IRQ
    ldx #$ff
    txs        ; Set up stack
    stx $8000  ; reset the mapper
    ; Set up memory mapper to enable SRAM
    ;LDA #$80       ; Enable SRAM at $6000-$7FFF
    ;STA $A001
    ; Initialize MMC1
    ; lda #BANK_SWITCH_LOW
    ; sta MMC1_CTRL  ; Select low 8KB PRG-ROM bank
    ; lda #$00
    ; sta MMC1_DATA  ; Write to MMC1 Data Register to set initial state
    ; Initialize MMC1
    ; LDA #$80
    ; STA $8000           ; Write to MMC1 control register to set PRG ROM mode
    ; LDA #$80            ; Enable SRAM at $6000-$7FFF
    ; STA $A001
    inx        ; now X = 0
    stx $2000  ; disable NMI
    stx $2001  ; disable rendering
    stx $4010  ; disable DMC IRQs

    ; Optional (omitted):
    ; Set up mapper and jmp to further init code here.

    ; The vblank flag is in an unknown state after reset,
    ; so it is cleared here to make sure that @vblankwait1
    ; does not exit immediately.
    bit $2002

    ; First of two waits for vertical blank to make sure that the
    ; PPU has stabilized
@vblankwait1:  
    bit $2002
    bpl @vblankwait1

    ; We now have about 30,000 cycles to burn before the PPU stabilizes.
    ; One thing we can do with this time is put RAM in a known state.
    ; Here we fill it with $00, which matches what (say) a C compiler
    ; expects for BSS.  Conveniently, X is still 0.
    txa
@clrmem:
    sta $000,x
    sta $100,x
    sta $200,x
    sta $300,x
    sta $400,x
    sta $500,x
    sta $600,x
    sta $700,x
    inx
    bne @clrmem

    ; Other things you can do between vblank waits are set up audio
    ; or set up other mapper registers.
   
@vblankwait2:
    bit $2002
    bpl @vblankwait2

jsr load_palette

lda #$c0
sta $0200
lda #$43
sta $0201
lda #%00000110
sta $0202
lda #$05
sta $0203

; Set up OAMADDR
lda #<OAM   ; Low byte of sprite_data address
sta OAM_ADDRESS           ; Store low byte into OAMADDR

lda #$20            ; High byte of sprite_data address
sta OAM_ADDRESS           ; Store high byte into OAMADDR

jsr init_game

; Trigger OAMDMA transfer
lda #$80
sta $2000  ; enable NMI
lda #$1e
sta $2001  ; enable rendering
lda #$ff
sta $4010  ; enable DMC IRQs

.proc game
    @loop:
        inc random_index
        ldx #$00
        jsr readjoy2_safe
        jsr process_input
		jsr process_snake
		jsr ppu_update
	jmp @loop
.endproc

.proc init_game
    lda #$01
    sta current_level
    jsr draw_board
    jsr place_food
    ;jsr place_food
    ;jsr place_food
    jsr initialize_variables
    jsr place_header_food
    jsr setup_game_variables
    rts
.endproc

random:
    .byte $4d,$41,$bb,$56,$86,$2f,$ae,$f3,$93,$56,$f3,$89,$4c,$e7,$d6,$f5,$60,$ac,$29,$fc,$3f,$0d,$16,$fd,$c5,$c5,$40,$bc,$95,$a7,$d8,$4f,$e2,$f0,$77,$ed,$f3
    .byte $1f,$5b,$df,$2c,$46,$44,$f6,$40,$c4,$f5,$7f,$1a,$59,$51,$06,$bb,$14,$cf,$91,$76,$c6,$92,$1b,$d7,$a1,$5e,$96,$2c,$06,$81,$30,$3b,$57,$47,$d0,$40,$31
    .byte $86,$c9,$01,$66,$90,$85,$85,$da,$c6,$12,$a8,$8e,$d7,$7a,$7d,$5f,$f7,$6c,$7a,$97,$53,$c5,$10,$ad,$d7,$80,$cf,$51,$86,$2b,$dc,$91,$e0,$0f,$95,$93,$85
    .byte $40,$8d,$79,$1e,$3c,$69,$b7,$ab,$06,$d8,$1e,$13,$97,$55,$3d,$c0,$b4,$a2,$8d,$b0,$5b,$9f,$52,$d5,$6b,$00,$e9,$2b,$76,$c4,$8b,$57,$23,$1c,$5d,$68
    .byte $53,$ae,$21,$db,$08,$67,$85,$87,$2d,$2c,$49,$ef,$3c,$51,$c5,$40,$86,$c5,$16,$00,$77,$a1,$4d,$1b,$e9,$86,$42,$45,$6c,$0d,$39,$e7,$e3,$4c,$97,$b0
    .byte $c8,$ff,$31,$7e,$b6,$0f,$cd,$74,$76,$ce,$05,$7d,$67,$2a,$ec,$b0,$98,$6d,$c6,$d1,$16,$f9,$f7,$84,$f1,$0c,$43,$c4,$c1,$65,$fd,$13,$bd,$1d,$b8,$47
    .byte $92,$7f,$a3,$57,$5b,$69,$0a,$74,$90,$d7,$3b,$b1,$48,$03,$86,$e0,$eb,$4b,$ef,$17,$e5,$9c,$f1,$2e,$b2,$03,$86,$2b,$0f,$36,$11,$c0,$53,$a9,$2c,$dd,$01

.proc initialize_variables
    lda #$1
    sta player_count
    lda #$10
    sta snake_speed

    ; TODO - Move these to a board tile.
    lda #$09
    ldy #$0
    sta START_X, y
    lda #$0f
    sta START_Y, y
    lda #$10
    ldy #$01
    sta START_X, y
    lda #$11
    sta START_Y, y

    ldy #$00
    init_snake:
        lda #BUTTON_RIGHT
        sta cur_dir, y
        sta next_dir, y
        lda #$07
        sta target_size, y
        lda START_X, y
        sta head_x, y
        sta tail_x, y
        sta new_x, y
        lda START_Y, y
        sta head_y, y
        sta tail_y, y
        sta new_y, y
        lda #$00
        sta head_index, y
        sta tail_index, y
        sta food_count, y
        iny
    cpy #$02
    bne init_snake
    rts

.endproc

.proc setup_game_variables
    ldx #$0
    @loop:

    lda new_x, x
    sta temp_x
    lda new_y, x
    sta temp_y

    jsr tile_to_screen_space_temp

    jsr store_head

    lda snakes_hi, x
    sta current_high
    lda snakes_lo, x
    sta current_low

    lda #$68 ; h
    sta temp_a
    ldy #$0
    sta (current_low), y
    ; place head on nmi queue
    lda new_x, x
    sta temp_x
    lda new_y, x
    sta temp_y
    jsr screen_space_to_ppu_space
    jsr ppu_update_tile_temp
    inc size, x

    inx
    cpx player_count
    bne @loop
    rts
.endproc

.proc process_snake
	lda tick_count
    cmp snake_speed ; snake speed - how many ticks to skip between updates.
	bne @end
    lda #$00
	sta tick_count ; reset frame counter
    jsr move_snake
    lda level_complete
    beq @end
        jsr process_level_change
    @end:
    rts
.endproc

.proc move_snake_on_input
    lda next_dir, x
    sta cur_dir, x

    cmp #BUTTON_UP
    bne @notUp
    dec new_y, x
    jmp @buttonEnd

    @notUp:
    cmp #BUTTON_DOWN
    bne @notDown
    inc new_y, x
    jmp @buttonEnd

    @notDown:
    cmp #BUTTON_LEFT
    bne @notLeft
    dec new_x, x
    jmp @buttonEnd

    @notLeft:
    cmp #BUTTON_RIGHT
    bne @buttonEnd
    inc new_x, x

    @buttonEnd:
    rts
.endproc

.proc move_snake
    ldx #$0
    @loop:
    jsr move_snake_on_input
    jsr move_snakex
    inx
    cpx player_count
    bne @loop
    rts
.endproc

; Moves one snake while checking for collision
; IN
; X = snake number (0 = snake 1, 1 = snake 2)
; Using temp variables and the stack - when possible, instead of obliterating the x register, to simplify code and reduce bugs.
.proc move_snakex
    ;txa ; This code is a mess.
    ;pha ; store x on stack
    ; Check tile ran into
    ;ldy new_y, x
    ;tya ; a is temp storage
    ;txy
    ;stx zp_temp_1
    ;ldy zp_temp_1
    ;ldx new_x, y
    ;tay

    ;jsr tile_to_screen_space_xy
    ; Using the temp variable version of the routine so X remains intact
    ; This has the unintended side effect of making indexing cleaner, further simplifying code.
    lda new_y, x
    sta temp_y
    lda new_x, x
    sta temp_x
    jsr tile_to_screen_space_temp

    ; IN
; Stack 0 = x
; Stack 1 = y
; OUT
; Stack 0 = LOW BYTE of screen space
; Stack 1 = HIGH BYTE of screen space
    ; lda new_y
    ; pha
    ; lda new_x
    ; pha
    ; jsr tile_to_screen_space_stack
    ; Feed the outgoing stack into the next function.
    ; pla
    ; sta current_low_2
    ; pla
    ; sta current_high_2



    ;stx current_high_2
    ;sty current_low_2
    ;pla
    ;tax ; restore x from stack
    ; FIX AND RESTORE THIS
    ; jsr process_collision_detection




    jsr process_collision_detection
    jsr store_head
    jsr move_head

    jsr process_tail
	rts
.endproc

; Store the head in screen space memory
.proc store_head
    lda #$68 ; h
    ldy #$00 ; current_low_2 points to the exact location, no offset
    sta (current_low_2), y
    rts
.endproc

.proc move_head
    ; Record the movement to the "linked list"
    ; I call it a linked list but it's more of a sliding window array. Uses less memory. Need to be careful for page reset however.
    ; Mark the current head with the direction to the new head

    lda snakes_hi, x
    sta current_high
    lda snakes_lo, x
    sta current_low

    lda cur_dir, x ; We just store directions so the tail can follow along
    ldy head_index, x
    sta (current_low), y
    ; Increment to new head
    iny
    ; Store 'h' at the new head since we don't know the direction to its next head yet
    lda #$68 ; h
    sta (current_low), y
    ;sta SNAKE, y
    sty head_index, x

    ;txa
    ;pha ; store x (snake index) on stack
    ; Move head, place on nmi queue
    ;ldx new_x
    ;stx head_x
    ;ldy new_y
    ;sty head_y
    lda new_x, x
    sta temp_x
    lda new_y, x
    sta temp_y
    lda #$68 ; h
    sta temp_a
    jsr screen_space_to_ppu_space
    jsr ppu_update_tile_temp
    ;pla
    ;tax ; restore x (snake index) from stack
    rts
.endproc

.proc process_tail
    ; Move tail?
    lda size, x
    cmp target_size, x
    bne grow
        ; Remove the current tail via nmi queue
        ;lda #$00 ; h
        ;ldx tail_x
        ;ldy tail_y
        lda tail_x, x
        sta temp_x
        lda tail_y, x
        sta temp_y
        lda #$00 ; empty
        sta temp_a
        jsr screen_space_to_ppu_space
        jsr ppu_update_tile_temp

        ;ldx tail_x
        ;ldy tail_y
        lda tail_x, x
        sta temp_x
        lda tail_y, x
        sta temp_y
        jsr tile_to_screen_space_temp
        ;stx current_high_2
        ;sty current_low_2
        ldy #$00
        ; Erase the tail
        ; This is optional, more for debugging
        lda #$00
        sta (current_low_2), y
        ; We are at target size, move the tail along

        ; Move the tail
        lda snakes_hi, x
        sta current_high
        lda snakes_lo, x
        sta current_low
        lda #$0

        ldy tail_index, x
        inc tail_index, x
        lda (current_low), Y
        cmp #BUTTON_UP
        bne @not_up
            dec tail_y, x
            jmp tail_done
        @not_up:
        cmp #BUTTON_DOWN
        bne @not_down
            inc tail_y, x
            jmp tail_done
        @not_down:
        cmp #BUTTON_LEFT
        bne @not_left
            dec tail_x, x
            jmp tail_done
        @not_left:
            inc tail_x, x
            jmp tail_done
        jmp tail_done
    grow:
        inc size, x
        jmp tail_end

    tail_done:
    tail_end:
    rts
.endproc

; Processes collision detection for the new head position
; IN
; current_high_2 and current_low_2 are the head position in screen space
; The function doesn't directly speficy current_high_2, but it uses it. Trust me.
; Currently it goes into an infinite loop on a collision with a wall or self, update
; this later to have a proper death cycle
; Snake grows bigger when eating food

.proc process_collision_detection
    ldy #$00
    ; Check tile ran into
    lda (current_low_2), y
    cmp #$68
    bne no_self
        jmp inf_loop
    no_self:
    cmp #$58
    bne no_wall
        jmp inf_loop
    no_wall:
    cmp #$69 ; food!
    bne @no_coll
        ;jmp inf_loop
        lda target_size, x
        clc
        adc #$07
        sta target_size, x
        txa
        pha ; store x on stack
        jsr place_food ; this is destructive to the x and y registers
        pla
        tax ; restore x from stack
        inc food_count, x
        jsr place_header_food
        jsr check_level_change
    @no_coll:
    rts
.endproc

check_level_change:
    lda food_count, X
    cmp #LEVEL_CHANGE
    bne rtn
        lda #$01
        sta level_complete
        ;jsr process_level_change
    rtn:
rts

process_level_change:
    jsr print_level_end_message
    jsr ppu_update
    jsr inf_loop
rts

; Same as above but uses stack. Useful if the caller uses the index registers.
; .proc process_collision_detection_stack
;     pla ; HIGH Byte
;     sta current_high_2
;     pla ; LOW Byte
;     sta current_low_2
;     ldy #$00
;     ; Check tile ran into
;     lda (current_low_2), y
;     cmp #$68
;     bne no_self
;         jmp inf_loop
;     no_self:
;     cmp #$58
;     bne no_wall
;         jmp inf_loop
;     no_wall:
;     cmp #$69 ; food!
;     bne @no_coll
;         ;jmp inf_loop
;         lda target_size
;         clc
;         adc #$07
;         sta target_size
;         jsr place_food
;     @no_coll:
;     rts
; .endproc

.proc inf_loop
    @forever:
    jmp @forever
.endproc

process_input:
    ldx #$0
    @loop:
    jsr process_inputx
    inx
    cpx player_count
    bne @loop
    rts

process_inputx: ; X register = 0 for controller 1, 1 for controller 2
    ; short circuit if not pressing anything
    lda buttons, x
    beq @end
    ldy #$0
    @repeat:
        lda buttons, x
        and dirs, Y
        beq @not_it
            lda cur_dir, x
            cmp o_dirs, Y
            beq @end ; Don't set next_dir if you pressed in the opposite of the current direction.
            lda dirs, y
            sta next_dir, x
            jmp @end
        @not_it:
        iny
        cpy #$4
        bne @repeat
    @end:
    rts

.proc nmi
	; save registers
	pha
	txa
	pha
	tya
	pha
	; prevent NMI re-entry
	lda nmi_lock
	beq :+
		jmp nmi_end
	:
	lda #1
	sta nmi_lock

    ; increment frame counter
	inc nmi_count
    ; nmi_count is the global game tick (timer). It is for game control. It should NEVER be changed aside from the line above.
    ; tick_count tracks the snake's speed and is reset to zero when the snake updates.
    inc tick_count
	;
	lda nmi_ready
	bne :+ ; nmi_ready == 0 not ready to update PPU
		jmp ppu_update_end
	:
	cmp #2 ; nmi_ready == 2 turns rendering off
	bne :+
		lda #%00000000
		sta $2001
		ldx #0
		stx nmi_ready
		jmp ppu_update_end
	:

    ; Define RGB values for a sprite palette (example)
    ; palette:

	;sta $00, x
	

    ; Load palette into PPU registers
    ;ldx #$0              ; Start with color 0
    ;lda #$55       ; Load first color (RGB values)
    ;sta $3F00,x         ; Store in PPU palette register
    ;inx                 ; Increment index
    ;lda $0000,x       ; Load second color (RGB values)
    ;sta $3F00,x         ; Store in PPU palette register
    ;inx                 ; Increment index
    ;lda $0000,x       ; Load third color (RGB values)
    ;sta $3F00,x         ; Store in PPU palette register
    ;inx                 ; Increment index
    ;lda $0000,x       ; Load fourth color (RGB values)
    ;sta $3F00,x         ; Store in PPU palette register

    ; Example of setting palette for sprites (PPU address $3F10-$3F1F)
    ;ldx #$0              ; Start with color 0
    ;lda $0000,x       ; Load first color (RGB values)
    ;sta $3F10,x         ; Store in PPU palette register (sprite palette)
    ;inx                 ; Increment index
    ;lda $0000,x       ; Load second color (RGB values)
    ;sta $3F10,x         ; Store in PPU palette register (sprite palette)
    ;inx                 ; Increment index
    ;lda $0000,x       ; Load third color (RGB values)
    ;sta $3F10,x         ; Store in PPU palette register (sprite palette)
    ;inx                 ; Increment index
    ;lda $0000,x       ; Load fourth color (RGB values)
    ;sta $3F10,x         ; Store in PPU palette register (sprite palette)

    ; Set up OAMADDR
    ;lda #<OAM   ; Low byte of sprite_data address
    ;sta OAM_ADDRESS           ; Store low byte into OAMADDR
    ;.hiby

    ; nametable update
	ldx #0
	cpx nmt_update_len
	bcs @scroll
	@nmt_update_loop:
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
		bcc @nmt_update_loop
	lda #0
	sta nmt_update_len
    
    @scroll:
    ; reset scroll location to top-left of screen
    lda #$00
    sta PPU_SCROLL
    sta PPU_SCROLL

    lda snake_update
    bne oam
    lda #$00
    sta snake_update

    oam:
    lda #$0                     ; Start writing at OAM_ADDRESS 0 in the PPU
    sta OAM_ADDRESS           ; Store high byte into OAMADDR
    lda #>OAM                   ; Store high byte into OAM_DMA
    sta OAM_DMA

    ; flag PPU update complete
	ldx #0
	stx nmi_ready

ppu_update_end:
	; if this engine had music/sound, this would be a good place to play it
	; unlock re-entry flag
	lda #0
	sta nmi_lock
nmi_end:
	; restore registers and return
	pla
	tay
	pla
	tax
	pla

    ; Wait for DMA transfer to complete (optional)
    ; wait_dma:
        ;bit $2002       ; Check if DMA transfer is still in progress
        ;bpl wait_dma    ; Wait until DMA transfer completes
	rti
.endproc

; ppu_update: waits until next NMI, turns rendering on (if not already), uploads OAM, palette, and nametable update to PPU
ppu_update:
	lda #1
	sta nmi_ready
	:
        ldx #00
        ;jsr readjoyx_safe
        ;jsr process_input
		lda nmi_ready
		bne :-
	rts

; ppu_skip: waits until next NMI, does not update PPU
ppu_skip:
	lda nmi_count
	:
		cmp nmi_count
		beq :-
	rts

; ppu_off: waits until next NMI, turns rendering off (now safe to write PPU directly via $2007)
ppu_off:
	lda #2
	sta nmi_ready
	:
		lda nmi_ready
		bne :-
	rts

; ppu_address_tile: use with rendering off, sets memory address to tile at X/Y, ready for a $2007 write
;   Y =  0- 31 nametable $2000
;   Y = 32- 63 nametable $2400
;   Y = 64- 95 nametable $2800
;   Y = 96-127 nametable $2C00
ppu_address_tile:
	lda $2002 ; reset latch
	tya
	lsr
	lsr
	lsr
	ora #$20 ; high bits of Y + $20
	sta $2006
	tya
	asl
	asl
	asl
	asl
	asl
	sta zp_temp_1
	txa
	ora zp_temp_1
	sta $2006 ; low bits of Y + X
	rts

tile_to_screen_space_temp:
    tya ; store Y on stack
    pha
    txa ; store X on stack
    pha
    ldx temp_x
    ldy temp_y
    jsr tile_to_screen_space_xy
    stx current_high_2
    sty current_low_2
    pla ; get X from stack
    tax
    pla ; get Y from stack
    tay
	rts

; Converts tile space (x,y from top-left of screen) to Nametable space (single memory span).
; IN
; x = x
; y = y
; OUT
; x = HIGH BYTE of screen space
; y = LOW BYTE of screen space
tile_to_screen_space_xy:
	;lda $2002 ; reset latch
	tya
	lsr
	lsr
	lsr
	;ora #$20 ; high bits of Y + $20
    pha ; Store High Byte on stack
	;sta zp_temp_1
	tya
	asl
	asl
	asl
	asl
	asl
	sta zp_temp_3
	txa
	ora zp_temp_3
    tay ; Store low byte into Y
    pla ; Pull High Byte from stack
    tax
    ;ldx zp_temp_1
    ; Falls through to next routine
    ;jsr convert_screen_space_to_screen_memory
	;rts

; Screen Space starts at 0. This moves the pointers so they start at the memory location for the screen in CPU memory
convert_screen_space_to_screen_memory:
    ;txa
    tya ; Low byte
    clc
    adc #<screen
    tay
    ;sta current_low_2
    txa ; High byte
    ;clc
    adc #>screen ; adds carry flag if needed
    ;adc x
    ;sta current_high_2
    tax
    ;ora #$20 ; high bits of Y + $20
    ;sta current_high
    ;sty current_low
	;sta zp_temp_2 ; low bits of Y + X
    rts

; Same as above but uses the stack instead to pass variables. This makes it easier for the caller to use the x and y registers as indexes.
; This function restores the x and y registers to their original value.
; IN
; Stack 0 = y
; Stack 1 = x
; OUT
; Stack 0 = LOW BYTE of screen space
; Stack 1 = HIGH BYTE of screen space
tile_to_screen_space_stack:
	;lda $2002 ; reset latch
    ; Need to restore x and y registers when existing the routine
    ;txa
    ;pha ; Store x on stack for restore at the end
	;tya
    ;pha ; Store y on stack for restore at the end
    stx zp_temp_1
    sty zp_temp_2
    pla ; pull x off stack (from coord system)
    tax
    pla ; pull y off stack (from coord system)
	lsr
	lsr
	lsr
	;ora #$20 ; high bits of Y + $20
    pha ; Store High Byte on stack
	;sta zp_temp_1
	tya
	asl
	asl
	asl
	asl
	asl
	sta zp_temp_3
	txa
	ora zp_temp_3
    tay ; Store low byte into Y
    pla ; Pull High Byte from stack
    tax
    ;ldx zp_temp_1
    ; Falls through to next routine
    ;jsr convert_screen_space_to_screen_memory
	;rts

; Screen Space starts at 0. This moves the pointers so they start at the memory location for the screen in CPU memory
convert_screen_space_to_screen_memory_stack:
    ;txa
    tya ; Low byte
    clc
    adc #<screen
    ;tay ; record the low byte to return
    pha ; record the LOW byte to return on the stack
    ;sta current_low_2
    txa ; High byte
    ;clc
    adc #>screen ; adds carry flag if needed
    ;adc x
    ;sta current_high_2
    ;tax ; record high byte to return
    pha ; record HIGH byte to return on the stack
    ;ora #$20 ; high bits of Y + $20
    ;sta current_high
    ;sty current_low
	;sta zp_temp_2 ; low bits of Y + X
    ldx zp_temp_1 ; Restore original x and y registers
    ldy zp_temp_2
    rts

screen_space_to_ppu_space:
    pha ; store A on stack
    lda temp_y
    clc
    ; Adjust Y down to account for the header
    adc #$02
    sta temp_y
    pla ; Get A from stack
rts

; ppu_update_tile: can be used with rendering on, sets the tile at temp vars X/Y to temp var A next time you call ppu_update
ppu_update_tile_temp:
    pha ; store A on stack
    txa
    pha ; store X on stack
    tya
    pha ; store Y on stack

    lda temp_a
    ldx temp_x
    ldy temp_y
    jsr ppu_update_tile

    pla ; Get Y from stack
    tay
    pla ; Get X from stack
    tax
    pla ; Get A from stack
	rts

; ppu_update_tile: can be used with rendering on, sets the tile at X/Y to tile A next time you call ppu_update
ppu_update_tile:
	pha ; temporarily store A on stack
	txa
	pha ; temporarily store X on stack
	ldx nmt_update_len
	tya
	lsr
	lsr ; every 8 rows is $100 hex in memory. - 32 tiles wide = 20 hex * 8 = 100 hex (remember hex is base 16)
	lsr ; the 100 hex is important because we need the high byte in the next sta
	ora #$20 ; high bits of Y + $20. The bits for 20 do not overlap with 1, 2, or 3. So OR-ing them gives us 20, 21, 22, or 23.
	sta nmt_update, X
	inx
	tya
	asl
	asl
	asl
	asl
	asl ; 2 ^ 5 = 32. This multiplies Y by 32 (the width of the screen). Y * 32 + x = screen location in memory.
	sta zp_temp_1
	pla ; recover X value (but put in A)
	ora zp_temp_1 ; No bits overlap so OR is a very fast ADC
	sta nmt_update, X
	inx
	pla ; recover A value (tile)
	sta nmt_update, X
	inx
	stx nmt_update_len
	rts

; ppu_update_byte: like ppu_update_tile, but X/Y makes the high/low bytes of the PPU address to write
;    this may be useful for updating attribute tiles
ppu_update_byte:
	pha ; temporarily store A on stack
	tya
	pha ; temporarily store Y on stack
	ldy nmt_update_len
	txa
	sta nmt_update, Y
	iny
	pla ; recover Y value (but put in Y)
	sta nmt_update, Y
	iny
	pla ; recover A value (byte)
	sta nmt_update, Y
	iny
	sty nmt_update_len
	rts

; At the same time that we strobe bit 0, we initialize the ring counter
; so we're hitting two birds with one stone here
readjoyx2:
    ldx #$00
    jsr readjoyx    ; X=0: read controller 1
    inx
    ; fall through to readjoyx below, X=1: read controller 2

readjoyx:           ; X register = 0 for controller 1, 1 for controller 2
    lda #$01
    sta JOYPAD1
    sta buttons, x
    lsr a
    sta JOYPAD1
button_loop:
    lda JOYPAD1, x
    and #%00000011  ; ignore bits other than controller
    cmp #$01        ; Set carry if and only if nonzero
    rol buttons, x  ; Carry -> bit 0; but 7 -> Carry
    bcc button_loop
    rts

readjoy2_safe:
    ldx #$00
    jsr readjoyx_safe  ; X=0: safe read controller 1
    inx
    ; fall through to readjoyx_safe, X=1: safe read controller 2

readjoyx_safe:
    jsr readjoyx
reread:
    lda buttons, x
    pha
    jsr readjoyx
    pla
    cmp buttons, x
    bne reread
    rts

.segment "VECTORS"
	.addr nmi, reset, 0

.segment "CHARS"
    .incbin "sprites.chr"