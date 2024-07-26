.include "constants.asm"
; Drawing the various game boards or levels

.importzp zp_temp_1, zp_temp_2
.export draw_board

board0:
    .byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $69, 0
    .byte 0, 0, 0, 0, $69, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

.proc draw_board
    ;rts
    ;lda #$20
    ;sta zp_temp_1
    ;lda #$00
    ;sta zp_temp_2
    ;ldx #$20
    ;stx #$20 ; 32 (width of screen)

    ;jsr draw_horizontal

    lda #>NT0
    sta PPU_ADDRESS
    lda #<NT0
    ;lda #$0f
    ;lda #$10
    sta PPU_ADDRESS

    lda #$69 ; i
    ldx #$00 ; 32
    @loop:
        lda board0, x
        sta PPU_DATA
        inx
        cpx #$40
        bne @loop
    rts
.endproc

.proc draw_horizontal


    rts
.endproc