.include "constants.inc"

.importzp palette_ptr, temp_a
.export load_palette

forest_palette_0:
    .byte $2a, $0f, $15, $30 ; header ; background
    .byte $0f, $2a, $27, $26 ; grass
    .byte $0f, $0f, $3d, $2d ; grays
    .byte $0f, $1c, $27, $30 ; snake
    .byte $2a, $1c, $27, $30 ; snake ; sprites
    .byte $0f, $25, $1a, $39 ; snake toungue
    .byte $0f, $1c, $27, $30 ; snake
    .byte $0f, $1c, $27, $30 ; snake

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