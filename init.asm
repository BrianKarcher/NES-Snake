.include "constants.asm"

.export load_palette

forest_palette_0:
    .byte $0f, $1c, $0f, $30 ; snake
    .byte $0f, $0f, $0f, $30 ; header
    .byte $0f, $3d, $2d, $28 ; grass
    .byte $0f, $3d, $2d, $28

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
        cpx #$10
        bne palette_loop
    rts
.endproc