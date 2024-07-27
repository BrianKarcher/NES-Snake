.include "constants.asm"
; Drawing the various game boards or levels

.importzp zp_temp_1, zp_temp_2, start_low, start_high, current_low, current_high, end_low, end_high
.import screen
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
    sta PPU_ADDRESS
    jsr load_board_to_nt
    ;ldx #$00 ; 32
    ; @loop:
    ;    lda board0, x
    ;    sta PPU_DATA
    ;    sta screen, x
    ;    inx
    ;    cpx #$40
    ;    bne @loop
    rts
.endproc

load_board_to_nt:
    lda #<board0
    sta start_low
    sta current_low
    lda #>board0
    sta start_high
    sta current_high
    lda #<board0
    clc
    adc #$40
    sta end_low
    lda #>board0
    adc #$00 ; add carry flag, if needed
    sta end_high

    ldy #$0
    loop:
    ; Transfer the current memory location to the PPU
    lda (current_low), y
    sta PPU_DATA
    ; Increment the address
    ;iny
    inc current_low
    bne @no_high_increment
        inc current_high
        lda #$00
        sta current_low
    @no_high_increment:
        ; Check if we've reached the end
        lda current_low
        cmp end_low ; Check low first since it is more likely to be different
        bne loop
        lda current_high
        cmp end_high
        bne loop
        ; Done transfering
        rts

.proc draw_horizontal


    rts
.endproc