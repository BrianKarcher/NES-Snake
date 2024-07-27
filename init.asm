.include "constants.asm"

.export init, load_palette

.proc init

.endproc

.proc load_palette
    lda #$3f
    sta PPU_ADDRESS
    lda #$00
    sta PPU_ADDRESS
    ; load 8 palettes (4 bg, 4 sprite)
    ldx 8
    palette_loop:
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