.include "constants.inc"

.importzp palette_ptr, temp_a
.export load_palette

; A: Total number of colors to copy. Sometimes we only do four palettes instead of all 8.
.proc load_palette
    sta temp_a
    lda #$3f
    sta PPU_ADDRESS
    lda #$00
    sta PPU_ADDRESS
    ; load 8 palettes (4 bg, 4 sprite)
    ldy #$0
    palette_loop:
        lda (palette_ptr), y
        sta PPU_DATA
        iny
        cpy temp_a
        bne palette_loop
    rts
.endproc