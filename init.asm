.include "constants.asm"
;.include "game.asm"

.import game
.export frame_done, frame_count, head_index_hi, head_index_lo, tail_index, snake_update ;head_x, head_y, tail_x, tail_y, size, dir
.import nmi

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

;palette = $0000

.proc load_palette
    lda #$3f
    sta PPU_ADDRESS
    lda #$00
    sta PPU_ADDRESS
    ; load 8 palettes (4 bg, 4 sprite)
    ldx 8
    palette_loop:
        ;ldx 0
        lda #$0f ; black
        sta PPU_DATA
        lda #$20 ; white
        sta PPU_DATA
        sta PPU_DATA
        sta PPU_DATA
        dex
        bne palette_loop
    rts
.endproc

.segment "VECTORS"
	.addr nmi, reset, 0

.segment "CHARS"
    .incbin "sprites.chr"

.segment "ZEROPAGE"
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