.include "constants.asm"

.export load_palette

forest_palette_0:
    .byte $2a, $0f, $15, $30 ; header ; background
    .byte $0f, $2a, $27, $26 ; grass
    .byte $0f, $0f, $3d, $2d ; grays
    .byte $0f, $1c, $27, $30 ; snake
    .byte $2a, $1c, $27, $30 ; snake ; sprites
    .byte $0f, $1c, $27, $30 ; snake
    .byte $0f, $1c, $27, $30 ; snake
    .byte $0f, $1c, $27, $30 ; snake

.proc load_palette
    lda #$3f
    sta PPU_ADDRESS
    lda #$00
    sta PPU_ADDRESS
    ; load 8 palettes (4 bg, 4 sprite)
    ldx #$0
    palette_loop:
        lda forest_palette_0, x
        sta PPU_DATA
        inx
        cpx #$20
        bne palette_loop
    rts
.endproc