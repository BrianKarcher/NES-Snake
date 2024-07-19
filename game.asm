; main game Loop
.include "constants.asm"

;.export game
;.export nmi
;.importzp frame_done
;.importzp frame_count
;.importzp head_index_hi, head_index_lo, tail_index, snake_update
.import init, load_palette

Message:
.byte "Hello World!", $00

.segment "HEADER"
	.byte "NES",26, 2,1, 0,0

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

; initialize PPU OAM
;ldx #$00
;stx OAM_ADDRESS ; $00
;lda #$02 ; use page $0200-$02ff
;sta OAM_DMA

jsr load_palette

; lda #$21
; sta PPU_ADDRESS
; lda #$ca
; sta PPU_ADDRESS

    ;lda 2000 + (32*16 + 15)
    ;lda <$#09E0

; Loop:
;     LDA Message,X     ; Load the byte at Message + X into A
;     BEQ Done          ; If it's the null terminator, jump to Done
;     STA PPU_DATA       ; Store the byte in memory at $0200 + Y
;     INX               ; Increment X to point to the next character in the string
    ;INY               ; Increment Y to point to the next memory location
;     JMP Loop          ; Repeat the loop

; Done:

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
;lda #%00000001      ; Any non-zero value will initiate DMA transfer
;sta $4014           ; Start DMA transfer to OAM (this transfers all sprite data - 256 bytes - from CPU memory to PPU memory)

lda #$80
;lda #%10000000
;inx        ; now X = 1
sta $2000  ; enable NMI
lda #$1e
sta $2001  ; enable rendering
lda #$ff
sta $4010  ; enable DMC IRQs

jsr game

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
	;rts
.endproc

.proc snake
	lda frame_count
	; cmp #$3c ; snake speed, move every 60 frames, otherwise exit
    cmp #$10 ; snake speed
	bne @end
	lda #$00
	sta frame_count ; reset frame counter
	lda head_index_lo
	clc
	adc #$01
	sta head_index_lo   ; Move head to the right one tile
    bcc @no_overflow
    ;sta head_index_hi   ; carry bit
    inc head_index_hi
    @no_overflow:
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

.segment "VECTORS"
	.addr nmi, reset, 0

.segment "CHARS"
    .incbin "sprites.chr"

.segment "ZEROPAGE"
buttons:        .res 1
frame_done:     .res 1
frame_count:    .res 1
head_index_hi:  .res 1 ; The index into the memory space. We don't use x or y coords.
head_index_lo:  .res 1
tail_index:     .res 1
snake_update:   .res 1
;head_x:         .res 1
;head_y:         .res 1
;nt_head_x:      .res 1
;nt_head_y:      .res 1
;tail_x:         .res 1
;tail_y:         .res 1
;nt_tail_x:      .res 1
;nt_tail_y:      .res 1
size:           .res 1
dir:            .res 1