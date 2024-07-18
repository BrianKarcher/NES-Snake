; main game Loop
.include "constants.asm"

.export game
.export nmi
.importzp frame_done
.importzp frame_count
.importzp head_index_hi, head_index_lo, tail_index, snake_update
; .importzp head_x, head_y, size, tail_x, tail_y, dir
; nt_head_x, nt_head_y, nt_tail_x, nt_tail_y

.proc game
	; lda #$0f
	; sta head_x
	; lda #$0e
	; sta head_y
	lda 2000 + (32*16 + 15) ; The center of the board
    lda #$22
    sta head_index_hi
    lda #$0f
    sta head_index_lo
    @loop:
		jsr snake
		;ldx #$0
		; @loop2:
			; lda #$cf
			;txa
			;sta $000, x
			;inx
			;cpx #$a
			;bne @loop2
		; inc $00
		jsr wait_frame
		; inc $00
		inc $0203
	jmp @loop
	rts
.endproc

.proc snake
	lda frame_count
	cmp #$3c ; snake speed, move every 60 frames, otherwise exit
	bne @end
	lda #$00
	sta frame_count ; reset frame counter
	lda head_index_lo
	clc
	adc #$01
	sta head_index_lo   ; Move head to the right one tile
    ;sta head_index_hi   ; carry bit

    lda #$01
    sta snake_update
	@end:
	rts

.endproc

.proc wait_frame
    inc frame_done          ; Make it non-zero
    @loop:
        lda frame_done
        bne @loop           ; Wait for frame_done to become zero again
        rts
.endproc

.proc nmi
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

    ;lda #$22
    lda head_index_hi
    sta PPU_ADDRESS
    lda head_index_lo
    ;lda #$0f
    sta PPU_ADDRESS

    lda #$68 ; h
    sta PPU_DATA

    ; reset scroll location to top-left of screen
    lda #$00
    sta PPU_SCROLL
    sta PPU_SCROLL

    lda snake_update
    bne oam
    lda #$00
    sta snake_update
    ;lda #>head_index
    ;sta PPU_ADDRESS
    ;lda #<head_index
    ;sta PPU_ADDRESS


    ;lda #$20            ; High byte of sprite_data address
    oam:
    lda #$0                     ; Start writing at OAM_ADDRESS 0 in the PPU
    sta OAM_ADDRESS           ; Store high byte into OAMADDR
    lda #>OAM                   ; Store high byte into OAM_DMA
    sta OAM_DMA

    lda #$0
    sta frame_done      ; Ding fries are done
	inc frame_count

    ; Wait for DMA transfer to complete (optional)
    ; wait_dma:
        ;bit $2002       ; Check if DMA transfer is still in progress
        ;bpl wait_dma    ; Wait until DMA transfer completes
	rti
.endproc