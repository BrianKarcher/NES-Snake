; main game Loop
.include "constants.asm"

.import init, load_palette, draw_board
.export zp_temp_1, zp_temp_2, screen, start_low, start_high, current_low, current_high, end_low, end_high, start_low_2, start_high_2, current_low_2, current_high_2

Message:
.byte "Hello World!", $00

; The start of each row in the name table
NT_Y:
    .word   $2000, $2020, $2040, $2060, $2080, $20A0, $20C0, $20E0, $2100, $2120, $2140, $2160, $2180, $21A0, $21C0, $21E0, $2200, $2220, $2240, $2260, $2280, $22A0, $22C0, $22E0, $2300, $2320, $2340, $2360, $2380, $23A0

.segment "HEADER"
	.byte "NES",26, 2,1, 0,0

.segment "ZEROPAGE"
nmi_lock:       .res 1 ; prevents NMI re-entry
nmi_ready:      .res 1 ; set to 1 to push a PPU frame update, 2 to turn rendering off next NMI
buttons:        .res 1
nmi_count:      .res 1
tick_count:     .res 1
head_x:         .res 1
head_y:         .res 1
tail_x:         .res 1
tail_y:         .res 1
snake_update:   .res 1
next_dir:       .res 1
cur_dir:        .res 1
size:           .res 1
dir:            .res 1
zp_temp_1:      .res 1
zp_temp_2:      .res 1
zp_temp_3:      .res 1
nmt_update_len: .res 1 ; number of bytes in nmt_update buffer
start_low:      .res 1  ; Low byte of start address
start_high:     .res 1  ; High byte of start address
end_low:        .res 1  ; Low byte of end address
end_high:       .res 1  ; High byte of end address
current_low:    .res 1  ; Low byte of current address, this is a memory pointer for indirect indexing
current_high:   .res 1  ; High byte of current address
start_low_2:    .res 1
start_high_2:   .res 1
current_low_2:  .res 1
current_high_2: .res 1

.segment "BSS"          ; This is the 8k SRAM memory (can be used for work or saves)
nmt_update: .res 256 ; nametable update entry buffer for PPU update
screen:     .res 960 ; Mirror of what is in the PPU. The snake can get quite large so we store it in this mirror.
                        ; We sacrifice memory for speed, it takes a while to check collisions on a 100-size snake if not in screen mirror memory.
                     ; The snake is mutable background and collides with itself.


.segment "STARTUP" ; avoids warning

reset:
    sei        ; ignore IRQs
    cld        ; disable decimal mode
    ldx #$40
    stx $4017  ; disable APU frame IRQ
    ldx #$ff
    txs        ; Set up stack
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

; Trigger OAMDMA transfer
jsr draw_board

lda #$80
sta $2000  ; enable NMI
lda #$1e
sta $2001  ; enable rendering
lda #$ff
sta $4010  ; enable DMC IRQs
lda RIGHT
sta cur_dir
sta next_dir
jsr game

.proc game
	lda #$0f
	sta head_x
	lda #$0e
	sta head_y
    @loop:
        ldx #$00
        jsr readjoyx_safe
        jsr process_input
		jsr snake
		jsr ppu_update
	jmp @loop
.endproc

.proc snake
	lda tick_count
	; cmp #$3c ; snake speed, move every 60 frames, otherwise exit
    cmp #$10 ; snake speed
	bne @end
	lda #$00
	sta tick_count ; reset frame counter
    lda next_dir
    sta cur_dir

    cmp #UP
    bne @notUp

    dec head_y
    jmp @buttonEnd
    @notUp:
    cmp #DOWN
    bne @notDown
    inc head_y

    jmp @buttonEnd
    @notDown:
    cmp #LEFT
    bne @notLeft
    dec head_x

    jmp @buttonEnd
    @notLeft:
    cmp #RIGHT
    bne @buttonEnd
    inc head_x

    @buttonEnd:
    @no_overflow:
    lda #$68 ; h
    ldx head_x
    ldy head_y
    jsr ppu_update_tile

    ; Check tile ran into
    ldx head_x
    ldy head_y

    jsr tile_to_nt_space_xy

	@end:
	rts
.endproc

.proc process_input
    lda buttons
    ;bit BUTTON_UP
    and #BUTTON_UP
    beq @notButtonUp
    lda #UP
    sta next_dir
    jmp @end
    @notButtonUp:
    lda buttons
    and #BUTTON_DOWN
    beq @notButtonDown
    lda #DOWN
    sta next_dir
    jmp @end
    @notButtonDown:
    lda buttons
    and #BUTTON_LEFT
    beq @notButtonLeft
    lda #LEFT
    sta next_dir
    jmp @end
    @notButtonLeft:
    lda buttons
    and #BUTTON_RIGHT
    beq @end
    lda #RIGHT
    sta next_dir
    @end:
    rts
.endproc

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
loop:
    lda JOYPAD1, x
    and #%00000011  ; ignore bits other than controller
    cmp #$01        ; Set carry if and only if nonzero
    rol buttons, x  ; Carry -> bit 0; but 7 -> Carry
    bcc loop
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

; Converts tile space (x,y from top-left of screen) to Nametable space (single memory span).
; IN
; x = x
; y = y
; OUT
; x = HIGH BYTE of nametable
; y = LOW BYTE of nametable
tile_to_nt_space_xy:
	;lda $2002 ; reset latch
	tya
	lsr
	lsr
	lsr
	ora #$20 ; high bits of Y + $20
	sta zp_temp_1
	tya
	asl
	asl
	asl
	asl
	asl
	sta zp_temp_3
	txa
	ora zp_temp_3
    ldx zp_temp_1
    tay
	;sta zp_temp_2 ; low bits of Y + X
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

.segment "VECTORS"
	.addr nmi, reset, 0

.segment "CHARS"
    .incbin "sprites.chr"