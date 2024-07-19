.include "constants.asm"
;.include "game.asm"

;.import game
;.export frame_done, frame_count, head_index_hi, head_index_lo, tail_index, snake_update ;head_x, head_y, tail_x, tail_y, size, dir
;.import nmi

.export init, load_palette

.proc init

.endproc

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