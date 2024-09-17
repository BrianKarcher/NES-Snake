; main game Loop
.include "constants.asm"

.import init, load_palette, draw_board, place_food, place_header_food, print_level_end_message, generate_attribute_byte, generate_attribute_byte_header
.import readjoy2_safe
.export zp_temp_1, zp_temp_2, zp_temp_3, screen, screen_rows, current_low, current_high, end_low, end_high, current_low_2, current_high_2
.export random_index, random, ppu_update_tile, ppu_update_tile_temp, screen_space_to_ppu_space, temp_a, temp_x, temp_y, current_level, xy_meta_tile_offset
.export food_count, ppu_update, xy_meta_tile_offset, temp_offset, buttons, place_shape
; .segment "HEADER"
; 	.byte "NES",26, 2,1, 0,0

.segment "HEADER"

INES_MAPPER = 1 ; 0 = NROM, 1 = MMC 1
INES_MIRROR = 1 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 0 ; 1 = battery backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A ; ID
.byte $02 ; 16k PRG chunk count
.byte $01 ; 8k CHR chunk count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding

; .segment "HEADER"
; .byte 'N', 'E', 'S', $1A  ; NES header magic number
; .byte 2                   ; Number of 16KB PRG ROM banks
; .byte 1                   ; Number of 8KB CHR ROM banks
; .byte $01                   ; Mapper, mirroring, battery, trainer
; .byte $00                   ; Mapper, VS/Playchoice, NES 2.0
; .byte 0, 0, 0, 0, 0, 0, 0, 0 ; Padding bytes

; .segment "HEADER"
; .byte "NES", $1A       ; iNES file identifier
; .byte 2                ; Number of 16 KB PRG-ROM banks
; .byte 1                ; Number of 8 KB CHR-ROM banks
; ;.byte %00000010        ; Flags 6: Mapper, mirroring, battery, trainer
; ;.byte 41
; .byte 40
; .byte %00000000        ; Flags 7: Mapper, VS/Playchoice, NES 2.0
; .byte 0                ; Flags 8: PRG-RAM size (rarely used)
; .byte 0                ; Flags 9: TV system (rarely used)
; .byte 0, 0, 0, 0, 0, 0    ; Unused padding bytes

; I've commented the memory addresses to make debugging easier. It is a nightmare without these.
.segment "ZEROPAGE"
nmi_lock:       .res 1 ; 00 prevents NMI re-entry
nmi_ready:      .res 1 ; 01 set to 1 to push a PPU frame update, 2 to turn rendering off next NMI
buttons:        .res 2 ; 02, 03
nmi_count:      .res 1 ; 04
tick_count:     .res 1 ; 05
head_x:         .res 2 ; 06, 07 Enable two-player
head_y:         .res 2 ; 08, 09
new_x:          .res 2 ; 0a, 0b
new_y:          .res 2 ; 0c, 0d
tail_x:         .res 2 ; 0e, 0f
tail_y:         .res 2 ; 10, 11
snake_update:   .res 1 ; 12
next_dir:       .res 2 ; 13, 14
cur_dir:        .res 2 ; 15, 16
target_size:    .res 2 ; 17, 18
size:           .res 2 ; 19, 1a
head_index:     .res 2 ; 1b, 1c
tail_index:     .res 2 ; 1d, 1e 0x17
zp_temp_1:      .res 1 ; 1f
zp_temp_2:      .res 1 ; 20
zp_temp_3:      .res 1 ; 21
nmt_update_len: .res 1 ; 22 number of bytes in nmt_update buffer
end_low:        .res 1 ; 23 Low byte of end address
end_high:       .res 1 ; 24 High byte of end address
current_low:    .res 1 ; 25 Low byte of current address, this is a memory pointer for indirect indexing
current_high:   .res 1 ; 26 High byte of current address
current_low_2:  .res 1 ; 27
current_high_2: .res 1 ; 28
snake_speed:    .res 1 ; 29 UNUSED
random_index:   .res 1 ; 2a
food_count:     .res 2 ; 2b, 2c
temp_a:         .res 1 ; 2d
temp_x:         .res 1 ; 2e
temp_y:         .res 1 ; 2f
player_count:   .res 1 ; 30
current_level:  .res 1 ; 31
level_complete: .res 1 ; 32
temp_offset:    .res 1 ; 33
prev_dir:       .res 2 ; 34, 35
return:         .res 2 ; 36, 37 sub return value
; Postions are an 8.4 fixed point structure. This requires two bytes.
x_px:           .res 2 ; 38, 39
x_sub_px:       .res 2 ; 3a, 3b
y_px:           .res 2 ; 3c, 3d
y_sub_px:       .res 2 ; 3e, 3f
; x_vel:          .res 2
; Velocities are a 4.4 fixed point structure. This requires one byte. The significant byte stores the sign. 2's compliment is the signed notation.
; 4.4 = 1 (sign) + 3 bits for the pixel + 4 bits for the subpixel.
x_vel_px:       .res 2 ; 40, 41
;x_vel_sub_px:   .res 2 ; 40, 41
; y_vel:          .res 2
y_vel_px:       .res 2 ; 42, 43
;y_vel_sub_px:   .res 2 ; 43, 44
; screen_lo       .res 1 ; 36
; screen_hi       .res 1 ; 37
zp_temp_4:      .res 1 ; 44

.segment "BSS"          ; This is the 8k SRAM memory (can be used for work or saves)
nmt_update: .res 256 ; nametable update entry buffer for PPU update
screen:     .res 240 ; Mirror of what is in the PPU. The snake can get quite large so we store it in this mirror.
                        ; We sacrifice memory for speed, it takes a while to check collisions on a 100-size snake if not in screen mirror memory.
                     ; The snake is mutable background and collides with itself.

.segment "STARTUP" ; avoids warning
screen_rows: ; screen offset to start of each row
    .byte $0, $10, $20, $30, $40, $50, $60, $70, $80, $90, $a0, $b0, $c0, $d0, $e0
dirs:
    .byte UP, DOWN, LEFT, RIGHT

; The speed in UP, DOWN, LEFT, RIGHT, using two's compliment for UP and LEFT.
speed_x_dir:
    .byte $0, $0, $EA, $16

speed_y_dir:
    .byte $EA, $16, $0, $0

; x_speed:
;     .byte $16 ; TODO: Turn this into an array so player can choose game speed
; y_speed:
;     .byte $16

button_dirs:
    .byte BUTTON_UP, BUTTON_DOWN, BUTTON_LEFT, BUTTON_RIGHT

o_dirs: ; opposite directions - to test if player pressed in opposite of current direction
    .byte DOWN, UP, RIGHT, LEFT

snakes_hi:
    .byte >SNAKE, >SNAKE2
snakes_lo:
    .byte <SNAKE, <SNAKE2

; head_up_entity:
;     .byte $02

; head_down_entity:
;     .byte $06

; head_left_entity:
;     .byte $04

; head_right_entity:
;     .byte $00

; head_up_shape:
;     .byte $08, $09, $18, $19
; head_down_shape:
;     .byte $0c, $0d, $1c, $1d
; head_left_shape:
;     .byte $0a, $0b, $1a, $1b
; head_right_shape:
;     .byte $06, $07, $16, $17
; This 8-bit processor drives me NUTS with the high and low bytes.
; head_hi:
;     .byte >head_up_shape, >head_down_shape, >head_left_shape, >head_right_shape
; head_lo:
;     .byte <head_up_shape, <head_down_shape, <head_left_shape, <head_right_shape
; head_hi:
;     .byte >head_up_entity, >head_down_entity, >head_left_entity, >head_right_entity
; head_lo:
;     .byte <head_up_entity, <head_down_entity, <head_left_entity, <head_right_entity

; I'm lazy so assuming that all sprites are stored in a perfect rectangle in CHR-ROM
; Store the top-left tile for each entity
head_dir:
    .byte $02, $06, $04, $00

body_hor_shape:
    .byte $05, $05, $15, $15

body_vert_shape:
    .byte $80, $81, $80, $81

blank_shape:
    .byte $00, $00, $00, $00

; same as left down
body_up_right_shape:
    .byte $02, $05, $80, $00

body_up_left_shape:
    .byte $05, $03, $01, $81

; same as down_left
body_right_up_shape:
    .byte $11, $81, $15, $13

body_right_down_shape:
    .byte $05, $03, $01, $81

; same as left up
body_down_right_shape:
    .byte $80, $10, $12, $15

; This is the start of a double-array. The first array index is the previous direction. The second index is the current direction.
; This determines which body shape to pick.
; TODO - Consider splitting the high and low bytes into separate arrays to remove the two bit shifts.
body_lo:
    .byte <body_up_lo, >body_up_lo, <body_down_lo, >body_down_lo, <body_left_lo, >body_left_lo, <body_right_lo, >body_right_lo ;, <body_down_hi ;, LEFT, RIGHT

body_up_lo:
    .byte <body_vert_shape, >body_vert_shape
    .byte <body_vert_shape, >body_vert_shape
    .byte <body_up_left_shape, >body_up_left_shape
    .byte <body_up_right_shape, >body_up_right_shape

body_down_lo:
    .byte <body_vert_shape, >body_vert_shape
    .byte <body_vert_shape, >body_vert_shape
    .byte <body_right_up_shape, >body_right_up_shape
    .byte <body_down_right_shape, >body_down_right_shape

body_left_lo:
    .byte <body_down_right_shape, >body_down_right_shape
    .byte <body_up_right_shape, >body_up_right_shape
    .byte <body_hor_shape, >body_hor_shape
    .byte <body_hor_shape, >body_hor_shape

body_right_lo:
    .byte <body_right_up_shape, >body_right_up_shape
    .byte <body_right_down_shape, >body_right_down_shape
    .byte <body_hor_shape, >body_hor_shape
    .byte <body_hor_shape, >body_hor_shape
    
; body_hi:
;     .byte <body_up_hi, >body_up_hi ;, <body_down_hi ;, LEFT, RIGHT

; body_up_hi:
;     .byte >body_vert_shape, >body_vert_shape, >body_up_left_shape, >body_up_right_shape

; body_down_hi:
;     .byte $00

; body_lo:
;     .byte <body_up_lo, >body_up_lo

; body_up_lo:
;     .byte <body_vert_shape, <body_vert_shape, >body_up_left_shape, >body_up_right_shape




; body_down_left_shape:
;     .byte 

reset:
    sei        ; ignore IRQs
    cld        ; disable decimal mode
    ldx #$40
    stx $4017  ; disable APU frame IRQ
    ldx #$ff
    txs        ; Set up stack
    stx $8000  ; reset the mapper
    ; Set up memory mapper to enable SRAM
    ;LDA #$80       ; Enable SRAM at $6000-$7FFF
    ;STA $A001
    ; Initialize MMC1
    ; lda #BANK_SWITCH_LOW
    ; sta MMC1_CTRL  ; Select low 8KB PRG-ROM bank
    ; lda #$00
    ; sta MMC1_DATA  ; Write to MMC1 Data Register to set initial state
    ; Initialize MMC1
    ; LDA #$80
    ; STA $8000           ; Write to MMC1 control register to set PRG ROM mode
    ; LDA #$80            ; Enable SRAM at $6000-$7FFF
    ; STA $A001
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

jsr load_palette

; lda #$c0
; sta $0200
; lda #$43
; sta $0201
; lda #%00000110
; sta $0202
; lda #$05
; sta $0203

; Set up OAMADDR
lda #<OAM   ; Low byte of sprite_data address
sta OAM_ADDRESS           ; Store low byte into OAMADDR

lda #$20            ; High byte of sprite_data address
sta OAM_ADDRESS           ; Store high byte into OAMADDR

jsr init_game
jsr init_level

; Trigger OAMDMA transfer
lda #%10001000
sta $2000  ; enable NMI, sprite pattern table starts at $1000
lda #$1e
sta $2001  ; enable rendering
lda #$ff
sta $4010  ; enable DMC IRQs

.proc game
    @loop:
        inc random_index
        ldx #$00
        jsr readjoy2_safe
        jsr process_input
		jsr process_snake
		jsr ppu_update
	jmp @loop
.endproc

.proc init_game
    lda #$01
    sta current_level
    ; TODO player_count and snake_speed to be in-game user-selected values
    lda #$1
    sta player_count
    ; lda #$10
    ; sta snake_speed
    rts
.endproc

.proc init_level
    lda #$00
    sta level_complete
    jsr draw_board
    jsr place_food
    jsr init_level_variables
    ;jsr place_header_food
    jsr init_place_snake
rts
.endproc

random:
    .byte $4d,$41,$bb,$56,$86,$2f,$ae,$f3,$93,$56,$f3,$89,$4c,$e7,$d6,$f5,$60,$ac,$29,$fc,$3f,$0d,$16,$fd,$c5,$c5,$40,$bc,$95,$a7,$d8,$4f,$e2,$f0,$77,$ed,$f3
    .byte $1f,$5b,$df,$2c,$46,$44,$f6,$40,$c4,$f5,$7f,$1a,$59,$51,$06,$bb,$14,$cf,$91,$76,$c6,$92,$1b,$d7,$a1,$5e,$96,$2c,$06,$81,$30,$3b,$57,$47,$d0,$40,$31
    .byte $86,$c9,$01,$66,$90,$85,$85,$da,$c6,$12,$a8,$8e,$d7,$7a,$7d,$5f,$f7,$6c,$7a,$97,$53,$c5,$10,$ad,$d7,$80,$cf,$51,$86,$2b,$dc,$91,$e0,$0f,$95,$93,$85
    .byte $40,$8d,$79,$1e,$3c,$69,$b7,$ab,$06,$d8,$1e,$13,$97,$55,$3d,$c0,$b4,$a2,$8d,$b0,$5b,$9f,$52,$d5,$6b,$00,$e9,$2b,$76,$c4,$8b,$57,$23,$1c,$5d,$68
    .byte $53,$ae,$21,$db,$08,$67,$85,$87,$2d,$2c,$49,$ef,$3c,$51,$c5,$40,$86,$c5,$16,$00,$77,$a1,$4d,$1b,$e9,$86,$42,$45,$6c,$0d,$39,$e7,$e3,$4c,$97,$b0
    .byte $c8,$ff,$31,$7e,$b6,$0f,$cd,$74,$76,$ce,$05,$7d,$67,$2a,$ec,$b0,$98,$6d,$c6,$d1,$16,$f9,$f7,$84,$f1,$0c,$43,$c4,$c1,$65,$fd,$13,$bd,$1d,$b8,$47
    .byte $92,$7f,$a3,$57,$5b,$69,$0a,$74,$90,$d7,$3b,$b1,$48,$03,$86,$e0,$eb,$4b,$ef,$17,$e5,$9c,$f1,$2e,$b2,$03,$86,$2b,$0f,$36,$11,$c0,$53,$a9,$2c,$dd,$01

.proc init_level_variables
    ldy #$00
    init_snake:
        lda #RIGHT
        sta cur_dir, y
        sta prev_dir, y
        sta next_dir, y
        lda #$07
        sta target_size, y
        lda START_X, y
        sta head_x, y
        sta tail_x, y
        sta new_x, y
        asl ; Multiply by 16 to get x in pixels
        asl
        asl
        asl
        jsr convert_unsigned_to_signed_fixed_point ; convert to fixed point
        lda return
        sta x_px, y
        lda return + 1
        sta x_sub_px
        lda START_Y, y
        sta head_y, y
        sta tail_y, y
        sta new_y, y
        clc
        adc #$1 ; header adjustment
        asl ; Multiply by 16 to get y in pixels
        asl
        asl
        asl
        jsr convert_unsigned_to_signed_fixed_point ; convert to fixed point
        lda return
        sta y_px, y
        lda return + 1
        sta y_sub_px
        lda #$00
        sta head_index, y
        sta tail_index, y
        sta food_count, y
        sta size, y
        iny
    cpy #$02
    bne init_snake
    rts
.endproc

.proc init_place_snake
    ldx #$0
    @loop:

    lda new_x, x
    sta temp_x
    lda new_y, x
    sta temp_y

    jsr xy_meta_tile_offset
    ;jsr tile_to_screen_space_temp

    jsr store_head

    lda snakes_hi, x
    sta current_high
    lda snakes_lo, x
    sta current_low

    lda #$a ; h
    sta temp_a
    ldy #$0
    sta (current_low), y
    ; place head on nmi queue
    lda #$05
    sta temp_a
    lda new_x, x
    sta head_x, x
    sta temp_x
    lda new_y, x
    sta head_y, x
    sta temp_y
    ; ldy cur_dir, x
    ; lda head_lo, y
    ; sta current_low
    ; lda head_hi, y
    ; sta current_high
    ; jsr screen_space_to_ppu_space
    ; ;jsr ppu_update_tile_temp
    ; jsr place_shape
    inc size, x

    ldy #RIGHT
    lda speed_x_dir, y
    sta x_vel_px, x ; TODO - Change it so that the snake doesn't move at the start for a time so the player can view the level.

    inx
    cpx player_count
    bne @loop
    rts
.endproc

.proc process_snake
    jsr move_head_px

    ; Determine if the head has reached a new tile
    jsr check_tile_change
    cmp #$0
    beq @end
    ;     lda #$00
    ;     sta tick_count ; reset frame counter
    jsr align_head_if_turning
    jsr process_snake_new_tile
    ;     lda level_complete
    ;     beq @end
    ;         jsr process_level_change
    @end:
    jsr draw_head
    rts
.endproc

; Place the head directly onto a tile's coords. Used when changing directions so the head always stays exactly on a tile.
.proc align_head_if_turning
    ; Only align the head when changing directions.
    lda cur_dir
    cmp next_dir
    beq @end
        ; When moving LEFT and the x changes, x + 1 is closer so we align to that instead.
        ; example: x changes from 1 to 0 (moving left). Upon the change from 1 to 0, 1 is closer so align x at 1 and set as the new head.
        ; There's probably a better way to fix this but this seems efficent CPU cycle-wise.
        cmp #LEFT
        bne @not_left
            inc head_x
            jmp @not_up
        @not_left:
        ; Likewise for UP and Y
        cmp #UP
        bne @not_up
            inc head_y
        @not_up:
        lda head_x
        asl
        asl
        asl
        asl ; multiply by 16

        jsr convert_unsigned_to_signed_fixed_point
        lda return
        sta x_px
        lda return + 1
        sta x_sub_px

        lda head_y
        asl
        asl
        asl
        asl ; multiply by 16
        jsr convert_unsigned_to_signed_fixed_point
        lda return
        sta y_px
        lda return + 1
        sta y_sub_px
    @end:
rts
.endproc

; OUT A (1 = changed, 0 = unchanged)
.proc check_tile_change
    ; lda cur_dir
    ; cmp #UP
    ; bne @not_up
    ;     jsr @check_y
    ;     jsr @check_x
    ;     rts
    ; @not_up:
    ; cmp #DOWN
    ; bne @not_down
    ;     jmp @check_y
    ; @not_down:
    ; cmp #LEFT
    ; bne @not_left
    ;     jmp @check_x
    ; @not_left:
    ; cmp #RIGHT
    ; bne @end
    ;     jmp @check_x
    lda #$0
    sta return
    @check_y:
        ; check if y is in a new tile
        jsr convert_fixed_point_y_to_unsigned
        lsr
        lsr
        lsr
        lsr ; divide by 16
        cmp head_y
        beq @check_x
        ; y tile changed
        sta head_y
        lda #$1
        sta return

    @check_x:
        ; check if y is in a new tile
        jsr convert_fixed_point_x_to_unsigned
        lsr
        lsr
        lsr
        lsr ; divide by 16
        cmp head_x
        beq @end
        ; x tile changed
        sta head_x
        lda #$1
        sta return

	@end:
        lda return
        rts
.endproc

.proc move_head_px
    lda x_px
    sta zp_temp_1
    lda x_sub_px
    sta zp_temp_2
    lda x_vel_px
    sta zp_temp_3
    jsr add_signed_unsigned_8_16_bit
    lda return
    sta x_px
    lda return + 1
    sta x_sub_px

    lda y_px
    sta zp_temp_1
    lda y_sub_px
    sta zp_temp_2
    lda y_vel_px
    sta zp_temp_3
    jsr add_signed_unsigned_8_16_bit
    lda return
    sta y_px
    lda return + 1
    sta y_sub_px
rts
.endproc

.proc draw_head
    ldx cur_dir
    ; y = current sprite position
    ldy head_dir, x
    ; x = sprite number
    ldx #$0

    jsr convert_fixed_point_y_to_unsigned
    sta temp_y
    jsr convert_fixed_point_x_to_unsigned
    sta temp_x
    ;clc
    ;adc #$f ; header adjustment
    lda temp_y
    sta OAM, x
    inx
    ;lda #$00 ; tile number
    tya
    sta OAM, x
    inx
    lda #$0 ; attributes
    sta OAM, X
    inx
    lda temp_x
    sta OAM, x




    ; clear the other 63 sprites
    @forx:
        inx
        lda #$ff ; don't render the sprite, ff is an out of bounds Y value
        sta OAM, X
        inx
        lda #$00
        sta OAM, X
        inx
        lda #$00
        sta OAM, X
        inx
        lda #$00
        sta OAM, x
        cpx #$ff
    bne @forx
rts
.endproc

.proc draw_sprite

rts
.endproc

.proc process_snake_new_tile
    ldx #$0
    @loop:
    jsr adjust_direction
    jsr calc_velocity
    jsr move_body_on_input
    ;jsr move_snakex
    inx
    cpx player_count
    bne @loop
    rts
.endproc

.proc adjust_direction
    lda cur_dir, x
    sta prev_dir, x
    lda next_dir, x
    sta cur_dir, x
.endproc

.proc calc_velocity
    ldy cur_dir
    lda cur_dir, X

    lda speed_x_dir, y
    sta x_vel_px
    lda speed_y_dir, y
    sta y_vel_px

    rts
.endproc

.proc move_body_on_input

    lda cur_dir, x

    cmp #UP
    bne @notUp
    dec new_y, x
    jmp @buttonEnd

    @notUp:
    cmp #DOWN
    bne @notDown
    inc new_y, x
    jmp @buttonEnd

    @notDown:
    cmp #LEFT
    bne @notLeft
    dec new_x, x
    jmp @buttonEnd

    @notLeft:
    cmp #RIGHT
    bne @buttonEnd
    inc new_x, x

    @buttonEnd:
    rts
.endproc

; Moves one snake while checking for collision
; IN
; X = snake number (0 = snake 1, 1 = snake 2)
; Using temp variables and the stack - when possible, instead of obliterating the x register, to simplify code and reduce bugs.
.proc move_snakex
    ;txa ; This code is a mess.
    ;pha ; store x on stack
    ; Check tile ran into
    ;ldy new_y, x
    ;tya ; a is temp storage
    ;txy
    ;stx zp_temp_1
    ;ldy zp_temp_1
    ;ldx new_x, y
    ;tay

    ;jsr tile_to_screen_space_xy
    ; Using the temp variable version of the routine so X remains intact
    ; This has the unintended side effect of making indexing cleaner, further simplifying code.
    lda new_y, x
    sta temp_y
    lda new_x, x
    sta temp_x
    ;jsr tile_to_screen_space_temp
    jsr xy_meta_tile_offset

    ; lda #<screen
    ; sta current_low_2
    ; lda #>screen
    ; sta current_high_2
    ; IN
; Stack 0 = x
; Stack 1 = y
; OUT
; Stack 0 = LOW BYTE of screen space
; Stack 1 = HIGH BYTE of screen space
    ; lda new_y
    ; pha
    ; lda new_x
    ; pha
    ; jsr tile_to_screen_space_stack
    ; Feed the outgoing stack into the next function.
    ; pla
    ; sta current_low_2
    ; pla
    ; sta current_high_2



    ;stx current_high_2
    ;sty current_low_2
    ;pla
    ;tax ; restore x from stack
    ; FIX AND RESTORE THIS
    ; jsr process_collision_detection




    jsr process_collision_detection
    jsr store_head
    jsr move_head

    jsr process_tail
	rts
.endproc

; Store the head in screen space memory
.proc store_head
    lda #$a ; h
    ldy temp_offset
    sta screen, y
    rts
.endproc

.proc move_head
    ; Record the movement to the "linked list"
    ; I call it a linked list but it's more of a sliding window array. Uses less memory. Need to be careful for page reset however.
    ; Mark the current head with the direction to the new head

    lda snakes_hi, x
    sta current_high
    lda snakes_lo, x
    sta current_low

    lda cur_dir, x ; We just store directions so the tail can follow along
    ldy head_index, x
    sta (current_low), y
    ; Increment to new head
    iny
    ; Store 'h' at the new head since we don't know the direction to its next head yet
    lda #$a ; h
    sta (current_low), y
    ;sta SNAKE, y
    sty head_index, x

    ; Draw over old head with body
    lda head_x, x
    sta temp_x
    lda head_y, x
    sta temp_y
    jsr choose_body_metatile
    ; lda #<body_hor_shape
    ; sta current_low
    ; lda #>body_hor_shape
    ; sta current_high
    jsr screen_space_to_ppu_space
    ;jsr ppu_update_tile_temp
    jsr place_shape

    ;txa
    ;pha ; store x (snake index) on stack
    ; Move head, place on nmi queue
    ;ldx new_x
    ;stx head_x
    ;ldy new_y
    ;sty head_y
    ; Record new head
    lda new_x, x
    sta head_x, x
    sta temp_x
    lda new_y, x
    sta head_y, x
    sta temp_y
    ; lda #$05 ; head
    ; sta temp_a
    ; Draw new head
    ; ldy cur_dir, x
    ; lda head_lo, y
    ; sta current_low
    ; lda head_hi, y
    ; sta current_high
    ; jsr screen_space_to_ppu_space
    ; ;jsr ppu_update_tile_temp
    ; jsr place_shape
    ;pla
    ;tax ; restore x (snake index) from stack
    rts
.endproc

.proc choose_body_metatile

    ; For each direction (up,down,left,right), we store the array as UP_LO, UP_HI, DOWN_LO, DOWN_HI, etc.
    lda prev_dir, X
    asl ; double the value to find the correct index in the array
    tay
    lda body_lo, Y
    sta current_low_2
    iny
    lda body_lo, Y
    sta current_high_2

    lda cur_dir, x
    asl
    tay
    lda (current_low_2), Y
    sta current_low
    iny
    lda (current_low_2), Y
    sta current_high


    ; Repeat for the double-array to find the prev_dir -> curr_dir combo


; body_lo:
;     .byte <body_up_lo, >body_up_lo ;, <body_down_hi ;, LEFT, RIGHT

; body_2_lo:
;     .byte <body_vert_shape, >body_vert_shape
;     .byte <body_vert_shape, >body_vert_shape
;     .byte <body_up_left_shape, >body_up_left_shape
;     .byte <body_up_right_shape, >body_up_right_shape
    ; lda prev_dir, X
    ; cmp #UP
    ; ; This is easy to do in other languages with a double array. We can hack that if needed, but for now I'm using a bunch of if statements
    ; ; And honestly, the "double array" would probably be slower than what I am doing here.
    ; bne @not_up
    ;     lda cur_dir, X
    ;     cmp #DOWN
    ;     bne :+
    ;         lda #<body_vert_shape
    ;         sta current_low
    ;         lda #>body_vert_shape
    ;         sta current_high
    ;         jmp @exit
    ;     :
    ;     cmp #UP
    ;     bne :+
    ;         lda #<body_vert_shape
    ;         sta current_low
    ;         lda #>body_vert_shape
    ;         sta current_high
    ;         jmp @exit
    ;     :
    ;     cmp #RIGHT
    ;     bne :+
    ;         lda #<body_up_right_shape
    ;         sta current_low
    ;         lda #>body_up_right_shape
    ;         sta current_high
    ;         jmp @exit
    ;     :
    ;     cmp #LEFT
    ;     bne @not_up
    ;         lda #<body_up_left_shape
    ;         sta current_low
    ;         lda #>body_up_left_shape
    ;         sta current_high
    ;         jmp @exit

    ; @not_up:
    ; cmp #RIGHT
    ; bne @not_right
    ;     lda cur_dir, x
    ;     cmp #LEFT
    ;     bne :+
    ;         lda #<body_hor_shape
    ;         sta current_low
    ;         lda #>body_hor_shape
    ;         sta current_high
    ;         jmp @exit
    ;     :
    ;     cmp #RIGHT
    ;     bne :+
    ;         lda #<body_hor_shape
    ;         sta current_low
    ;         lda #>body_hor_shape
    ;         sta current_high
    ;         jmp @exit
    ;     :
    ;     cmp #UP
    ;     bne :+
    ;         lda #<body_right_up_shape
    ;         sta current_low
    ;         lda #>body_right_up_shape
    ;         sta current_high
    ;         jmp @exit
    ;     :
    ;     cmp #DOWN
    ;     bne @not_right
    ;         lda #<body_right_down_shape
    ;         sta current_low
    ;         lda #>body_right_down_shape
    ;         sta current_high
    ;         jmp @exit

    ; @not_right:
    ; cmp #DOWN
    ; bne @not_down
    ;     lda cur_dir, x
    ;     cmp #LEFT
    ;     bne :+
    ;         lda #<body_right_up_shape
    ;         sta current_low
    ;         lda #>body_right_up_shape
    ;         sta current_high
    ;         jmp @exit
    ;     :
    ;     cmp #RIGHT
    ;     bne :+
    ;         lda #<body_down_right_shape
    ;         sta current_low
    ;         lda #>body_down_right_shape
    ;         sta current_high
    ;         jmp @exit
    ;     :
    ;     cmp #UP
    ;     bne :+
    ;         lda #<body_vert_shape
    ;         sta current_low
    ;         lda #>body_vert_shape
    ;         sta current_high
    ;         jmp @exit
    ;     :

    ; @not_down:
    ; @exit:
rts
.endproc

; IN
; temp_x, temp_y
; current_low, current_high stores the shape address
.proc place_shape ; places a 2x2 shape
    txa ; store x
    pha
    lda temp_x
    asl
    sta temp_x
    lda temp_y
    asl
    sta temp_y
    ; This might be easier than doing a nested loop
    ; 1,1
    ldy #$0 ; shape offset
    lda (current_low), y
    sta temp_a
    jsr ppu_update_tile_temp
    ; 2,1
    iny
    lda (current_low), y
    sta temp_a
    inc temp_x
    jsr ppu_update_tile_temp
    ; 1,2
    iny
    lda (current_low), y
    sta temp_a
    dec temp_x
    inc temp_y
    jsr ppu_update_tile_temp
    ; 2,2
    iny
    lda (current_low), y
    sta temp_a
    inc temp_x
    jsr ppu_update_tile_temp

    ; txa
    ; pha ; store x
    ldx temp_x
    ldy temp_y
    ; lda temp_x ; convert x and y back into metatile-coords
    ; lsr
    ; tax
    ; lda temp_y
    ; lsr
    ; tay
    ; Create the attribute byte replacement
    ; convert from nmi-space to attribute space (divide x and y by 2)
    jsr coord_quarter
    stx zp_temp_1
    jsr generate_attribute_byte ; byte gets stored in zp_temp_2
    
    lda #$23
    tax ; high byte
    ;sta PPU_ADDRESS
    txa
    ;sta temp_x
    tya
    asl ; find the low byte memory space, y*8 + x + c0
    asl
    asl
    clc
    ;sty temp_x
    adc zp_temp_1
    clc
    adc #$c0
    ;ora #$c0
    tay
    lda zp_temp_2
    jsr ppu_update_byte
    
    pla ; restore x
    tax
    rts
.endproc

.proc process_tail
    ; Move tail?
    lda size, x
    cmp target_size, x
    bne grow

        ;ldx tail_x
        ;ldy tail_y
        lda tail_x, x
        sta temp_x
        lda tail_y, x
        sta temp_y
        jsr xy_meta_tile_offset
        ;jsr tile_to_screen_space_temp
        ;stx current_high_2
        ;sty screen
        ldy temp_offset
        ; Erase the tail from the screen copy
        lda #$00 ; TODO - Reload the background at this level's position instead
        sta screen, y


        ; Remove the current tail via nmi queue
        ;lda #$00 ; h
        ;ldx tail_x
        ;ldy tail_y
        lda tail_x, x
        sta temp_x
        lda tail_y, x
        sta temp_y
        lda #<blank_shape
        sta current_low
        lda #>blank_shape
        sta current_high
        ; lda #$00 ; empty
        ; sta temp_a
        jsr screen_space_to_ppu_space
        ;jsr ppu_update_tile_temp
        jsr place_shape

        ; We are at target size, move the tail along

        ; Move the tail
        lda snakes_hi, x
        sta current_high
        lda snakes_lo, x
        sta current_low
        lda #$0

        ldy tail_index, x
        inc tail_index, x
        lda (current_low), Y
        cmp #UP
        bne @not_up
            dec tail_y, x
            jmp tail_done
        @not_up:
        cmp #DOWN
        bne @not_down
            inc tail_y, x
            jmp tail_done
        @not_down:
        cmp #LEFT
        bne @not_left
            dec tail_x, x
            jmp tail_done
        @not_left:
            inc tail_x, x
            jmp tail_done
        jmp tail_done
    grow:
        inc size, x
        jmp tail_end

    tail_done:
    tail_end:
    rts
.endproc

; Processes collision detection for the new head position
; IN
; current_high_2 and current_low_2 are the head position in screen space
; The function doesn't directly speficy current_high_2, but it uses it. Trust me.
; Currently it goes into an infinite loop on a collision with a wall or self, update
; this later to have a proper death cycle
; Snake grows bigger when eating food

.proc process_collision_detection
    ldy #$00
    ; Check tile ran into
    ldy temp_offset
    lda screen, y
    cmp #$a
    bne no_self
        jmp inf_loop
    no_self:
    cmp #$58
    bne no_wall
        jmp inf_loop
    no_wall:
    cmp #$b ; food!
    bne @no_coll
        ;jmp inf_loop
        lda target_size, x
        clc
        adc #$07
        sta target_size, x
        txa
        pha ; store x on stack
        jsr place_food ; this is destructive to the x and y registers
        pla
        tax ; restore x from stack
        inc food_count, x
        jsr place_header_food
        jsr check_level_change
    @no_coll:
    rts
.endproc

check_level_change:
    lda food_count, X
    cmp #LEVEL_CHANGE
    bne rtn
        lda #$01
        sta level_complete
        ;jsr process_level_change
    rtn:
rts

process_level_change:
    jsr print_level_end_message
    jsr ppu_update
    jsr inf_loop
    ; loop:

    ; jmp loop
    @end:
    inc current_level
    jsr ppu_off
    jsr init_level
    ; Wait for next frame and turns PPU rendering back on
    jsr ppu_on
    jsr ppu_update
    ;jsr inf_loop
rts

;delete_snake:

;rts

; Same as above but uses stack. Useful if the caller uses the index registers.
; .proc process_collision_detection_stack
;     pla ; HIGH Byte
;     sta current_high_2
;     pla ; LOW Byte
;     sta screen
;     ldy #$00
;     ; Check tile ran into
;     lda screen, y
;     cmp #$a
;     bne no_self
;         jmp inf_loop
;     no_self:
;     cmp #$58
;     bne no_wall
;         jmp inf_loop
;     no_wall:
;     cmp #$69 ; food!
;     bne @no_coll
;         ;jmp inf_loop
;         lda target_size
;         clc
;         adc #$07
;         sta target_size
;         jsr place_food
;     @no_coll:
;     rts
; .endproc

.proc inf_loop
    @forever:
    jmp @forever
.endproc

process_input:
    ldx #$0
    @loop:
    jsr process_inputx
    inx
    cpx player_count
    bne @loop
    rts

process_inputx: ; X register = 0 for controller 1, 1 for controller 2
    ; short circuit if not pressing anything
    lda buttons, x
    beq @end
    ldy #$0
    @repeat:
        lda buttons, x
        and button_dirs, Y
        beq @not_it
            lda cur_dir, x
            cmp o_dirs, Y
            beq @end ; Don't set next_dir if you pressed in the opposite of the current direction.
            lda dirs, y
            sta next_dir, x
            jmp @end
        @not_it:
        iny
        cpy #$4
        bne @repeat
    @end:
    rts

.proc nmi
	; save registers
	pha
	txa
	pha
	tya
	pha

	; prevent NMI re-entry
	lda nmi_lock
	beq :+
		jmp nmi_end
	:
	lda #1
	sta nmi_lock

    ; increment frame counter
	inc nmi_count
    ; nmi_count is the global game tick (timer). It is for game control. It should NEVER be changed aside from the line above.
    ; tick_count tracks the snake's speed and is reset to zero when the snake updates.
    inc tick_count
	;
	lda nmi_ready
	bne :+ ; nmi_ready == 0 not ready to update PPU
		jmp ppu_update_end
	:
	cmp #2 ; nmi_ready == 2 turns rendering off
	bne :+
		lda #%00000000
		sta $2001
		ldx #0
		stx nmi_ready
		jmp ppu_update_end
	:

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
    ;.hiby

    ; nametable update
	ldx #0
	cpx nmt_update_len
	bcs @scroll
	@nmt_update_loop:
		lda nmt_update, X
        sta current_high
		sta $2006
		inx
		lda nmt_update, X
        sta current_low
		sta $2006
		inx
		lda nmt_update, X
		sta $2007
		inx
        
		cpx nmt_update_len
		bcc @nmt_update_loop

	lda #0
	sta nmt_update_len
    
    @scroll:
    ; reset scroll location to top-left of screen
    lda PPU_SCROLL
    lda #$00
    sta PPU_SCROLL
    sta PPU_SCROLL

    lda snake_update
    bne oam
    lda #$00
    sta snake_update

    oam:
    lda #$0                     ; Start writing at OAM_ADDRESS 0 in the PPU
    sta OAM_ADDRESS           ; Store high byte into OAMADDR
    lda #>OAM                   ; Store high byte into OAM_DMA
    sta OAM_DMA

    ; flag PPU update complete
	ldx #0
	stx nmi_ready

ppu_update_end:
	; if this engine had music/sound, this would be a good place to play it
	; unlock re-entry flag
	lda #0
	sta nmi_lock
nmi_end:
	; restore registers and return
	pla
	tay
	pla
	tax
	pla

    ; Wait for DMA transfer to complete (optional)
    ; wait_dma:
        ;bit $2002       ; Check if DMA transfer is still in progress
        ;bpl wait_dma    ; Wait until DMA transfer completes
	rti
.endproc

; ppu_update: waits until next NMI, turns rendering on (if not already), uploads OAM, palette, and nametable update to PPU
ppu_update:
	lda #1
	sta nmi_ready
	:
        ldx #00
        ;jsr readjoyx_safe
        ;jsr process_input
		lda nmi_ready
		bne :-
	rts

; ppu_skip: waits until next NMI, does not update PPU
ppu_skip:
	lda nmi_count
	:
		cmp nmi_count
		beq :-
	rts

; ppu_off: waits until next NMI, turns rendering off (now safe to write PPU directly via $2007)
ppu_off:
	lda #2
	sta nmi_ready
	:
		lda nmi_ready
		bne :-
	rts

ppu_on:
    lda #$1e
    sta $2001  ; enable rendering
rts

; ppu_address_tile: use with rendering off, sets memory address to tile at X/Y, ready for a $2007 write
;   Y =  0- 31 nametable $2000
;   Y = 32- 63 nametable $2400
;   Y = 64- 95 nametable $2800
;   Y = 96-127 nametable $2C00
ppu_address_tile:
	lda $2002 ; reset latch
	tya
	lsr
	lsr
	lsr
	ora #$20 ; high bits of Y + $20
	sta $2006
	tya
	asl
	asl
	asl
	asl
	asl
	sta zp_temp_1
	txa
	ora zp_temp_1
	sta $2006 ; low bits of Y + X
	rts

; Works ONLY on Metatiles (2x2 tiles)
.proc xy_meta_tile_offset
    ; Perform calculation y * width(16) + x
    lda temp_y
    clc
    asl
    asl
    asl
    asl
    adc temp_x
    sta temp_offset
    rts
.endproc

; tile_to_screen_space_temp:
;     tya ; store Y on stack
;     pha
;     txa ; store X on stack
;     pha
;     ldx temp_x
;     ldy temp_y
;     jsr tile_to_screen_space_xy
;     stx current_high_2
;     sty current_low_2
;     pla ; get X from stack
;     tax
;     pla ; get Y from stack
;     tay
; 	rts

; Converts tile space (x,y from top-left of screen) to Screen space (single memory span).
; IN
; x = x
; y = y
; OUT
; x = HIGH BYTE of screen space
; y = LOW BYTE of screen space
; tile_to_screen_space_xy:
; 	;lda $2002 ; reset latch
; 	tya
; 	lsr
; 	lsr
; 	lsr
; 	;ora #$20 ; high bits of Y + $20
;     pha ; Store High Byte on stack
; 	;sta zp_temp_1
; 	tya
; 	asl
; 	asl
; 	asl
; 	asl
; 	asl
; 	sta zp_temp_3
; 	txa
; 	ora zp_temp_3
;     tay ; Store low byte into Y
;     pla ; Pull High Byte from stack
;     tax
;     ;ldx zp_temp_1
;     ; Falls through to next routine
;     ;jsr convert_screen_space_to_screen_memory
; 	;rts

; Screen Space starts at 0. This moves the pointers so they start at the memory location for the screen in CPU memory
convert_screen_space_to_screen_memory:
    ;txa
    tya ; Low byte
    clc
    adc #<screen
    tay
    ;sta screen
    txa ; High byte
    ;clc
    adc #>screen ; adds carry flag if needed
    ;adc x
    ;sta current_high_2
    tax
    ;ora #$20 ; high bits of Y + $20
    ;sta current_high
    ;sty current_low
	;sta zp_temp_2 ; low bits of Y + X
    rts

; Same as above but uses the stack instead to pass variables. This makes it easier for the caller to use the x and y registers as indexes.
; This function restores the x and y registers to their original value.
; IN
; Stack 0 = y
; Stack 1 = x
; OUT
; Stack 0 = LOW BYTE of screen space
; Stack 1 = HIGH BYTE of screen space
tile_to_screen_space_stack:
	;lda $2002 ; reset latch
    ; Need to restore x and y registers when existing the routine
    ;txa
    ;pha ; Store x on stack for restore at the end
	;tya
    ;pha ; Store y on stack for restore at the end
    stx zp_temp_1
    sty zp_temp_2
    pla ; pull x off stack (from coord system)
    tax
    pla ; pull y off stack (from coord system)
	lsr
	lsr
	lsr
	;ora #$20 ; high bits of Y + $20
    pha ; Store High Byte on stack
	;sta zp_temp_1
	tya
	asl
	asl
	asl
	asl
	asl
	sta zp_temp_3
	txa
	ora zp_temp_3
    tay ; Store low byte into Y
    pla ; Pull High Byte from stack
    tax
    ;ldx zp_temp_1
    ; Falls through to next routine
    ;jsr convert_screen_space_to_screen_memory
	;rts

; Screen Space starts at 0. This moves the pointers so they start at the memory location for the screen in CPU memory
convert_screen_space_to_screen_memory_stack:
    ;txa
    tya ; Low byte
    clc
    adc #<screen
    ;tay ; record the low byte to return
    pha ; record the LOW byte to return on the stack
    ;sta screen
    txa ; High byte
    ;clc
    adc #>screen ; adds carry flag if needed
    ;adc x
    ;sta current_high_2
    ;tax ; record high byte to return
    pha ; record HIGH byte to return on the stack
    ;ora #$20 ; high bits of Y + $20
    ;sta current_high
    ;sty current_low
	;sta zp_temp_2 ; low bits of Y + X
    ldx zp_temp_1 ; Restore original x and y registers
    ldy zp_temp_2
    rts

screen_space_to_ppu_space:
    inc temp_y
    ; pha ; store A on stack
    ; lda temp_y
    ; clc
    ; ; Adjust Y down to account for the header
    ; adc #$02
    ; sta temp_y
    ; pla ; Get A from stack
rts

; ppu_update_tile: can be used with rendering on, sets the tile at temp vars X/Y to temp var A next time you call ppu_update
ppu_update_tile_temp:
    pha ; store A on stack
    txa
    pha ; store X on stack
    tya
    pha ; store Y on stack

    lda temp_a
    ldx temp_x
    ldy temp_y
    jsr ppu_update_tile

    pla ; Get Y from stack
    tay
    pla ; Get X from stack
    tax
    pla ; Get A from stack
	rts

; ppu_update_tile: can be used with rendering on, sets the tile at X/Y to tile A next time you call ppu_update
ppu_update_tile:
	pha ; temporarily store A on stack
	txa
	pha ; temporarily store X on stack
	ldx nmt_update_len
	tya
	lsr
	lsr ; every 8 rows is $100 hex in memory. - 32 tiles wide = 20 hex * 8 = 100 hex (remember hex is base 16)
	lsr ; the 100 hex is important because we need the high byte in the next sta
	ora #$20 ; high bits of Y + $20. The bits for 20 do not overlap with 1, 2, or 3. So OR-ing them gives us 20, 21, 22, or 23.
	sta nmt_update, X
	inx
	tya
	asl
	asl
	asl
	asl
	asl ; 2 ^ 5 = 32. This multiplies Y by 32 (the width of the screen). Y * 32 + x = screen location in memory.
	sta zp_temp_1
	pla ; recover X value (but put in A)
	ora zp_temp_1 ; No bits overlap so OR is a very fast ADC
	sta nmt_update, X
	inx
	pla ; recover A value (tile)
	sta nmt_update, X
	inx
	stx nmt_update_len
	rts

; ppu_update_byte: like ppu_update_tile, but X/Y makes the high/low bytes of the PPU address to write
;    this may be useful for updating attribute tiles
ppu_update_byte:
	pha ; temporarily store A on stack
	tya
	pha ; temporarily store Y on stack
	ldy nmt_update_len
	txa
	sta nmt_update, Y
	iny
	pla ; recover Y value (but put in Y)
	sta nmt_update, Y
	iny
	pla ; recover A value (byte)
	sta nmt_update, Y
	iny
	sty nmt_update_len
	rts

; get x by just getting the lower 5 bits (AND x11111)
; Every bit above the 5th one is Y. This is because the width is 32. Matters are complicated slightly
; because addresses are 16-bit, but our data bus is 8-bit.
; We solve this by getting ONLY the first two bits of the high byte (AND #3) - this is nametable agnostic
; Then shift the high byte to the LEFT 3 times
; Shift the low byte to the right 5 times
; then AND both of them together.
; IN current_low, current_high
; OUT x, y
ppu_getxy_from_address:
    ; get X
    lda current_low
    and #$1f ; Mask the lower 5 bits (1f is 0x11111)
    ;sta temp_x
    tax
    lda current_high
    asl
    asl
    asl ; shift 3 times left
    sta current_high
    lda current_low
    lsr
    lsr
    lsr
    lsr
    lsr ; shift 5 to the right
    ora current_high ; combine low and high to get Y
    ;sta temp_y
    tay
rts

; Converts an x and y to a coord system with half the width (Tile to Metatile, or Metatile to Attribute Table)
; IN (x,y)
; OUT (x,y), modified
coord_half:
    txa
    ;lda temp_x
    ;asl ; Multiply by 2
    lsr ; Divide by 2
    tax
    ;sta temp_x
    ;lda temp_y
    tya
    ;asl
    lsr ; Divide by 2
    tay
    ;sta temp_y
    rts

; Converts an x and y to a coord system with one quarter the width (ex: Tile to Attribute Table)
; IN (x,y)
; OUT (x,y), modified
coord_quarter:
    txa
    ;lda temp_x
    ;asl ; Multiply by 2
    lsr
    lsr ; Divide by 4
    tax
    ;sta temp_x
    ;lda temp_y
    tya
    ;asl
    lsr
    lsr ; Divide by 4
    tay
    ;sta temp_y
    rts

; IN A in positive
; OUT A in negative notation via two's compliment.
; Example: #02, xor'd turns into #FD, add 1 gives the result FE.
; If we take #03 and add #FE from above, we get %0001 0000 0001 - HOWEVER, since we are in 8-bit the resulting value is 1! Which is 3 - 2.
; We can now add or subtract signed integers without the need for a condition check on any of the signs.
; It's also compatible with one of the values being unsinged.
; Refer to https://stackoverflow.com/questions/1049722/what-is-twos-complement
.proc toTwosCompliment
    eor #$ff
    adc #$1
rts
.endproc

; Reverse the above function to reverse two's compliment.
; This turns a negative number into a positive number but cannot be used on a number that is already positive.
; This should be called by an absolute value function when needed.
; IN A in two's compliment
; OUT A as its positive value
.proc exitTwosCompliment
    sec
    sbc #$1
    eor #$ff
rts
.endproc

; Input: Unsigned number in memory at $00 (e.g., 5).
;        Signed number in memory at $01 (e.g., -3).
; Output: Result stored at $02 (treated as signed).
.proc add_signed_unsigned_8_bit
    CLC              ; Clear carry flag
    LDA $01          ; Load the signed number from $01

    ; Check if the signed number is negative
    BMI @convert_neg  ; If the number is negative (bit 7 set), jump to convert_neg

    ; If positive, just add directly as an unsigned number
    ADC $00          ; Add unsigned number
    BVC @store_result ; If no overflow, store result
    ; Handle overflow (optional)
    JMP @store_result ; Skip to store result

@convert_neg:
    ; Convert negative signed number to its unsigned equivalent
    EOR #$FF         ; Invert bits (2's complement step 1)
    CLC              ; Clear carry for addition
    ADC #$01         ; Add 1 (2's complement step 2)

    ; Now the signed number is converted to positive. Add unsigned number.
    SEC              ; Set carry for subtraction
    SBC $00          ; Subtract unsigned number from the converted signed number

@store_result:
    STA $02          ; Store the result in $02 (interpreted as signed)
rts
.endproc

; Input: Unsigned number in memory at zp_temp_1 (HI),zp_temp_2 (LO) (e.g., 5).
;        Signed number in memory at zp_temp_3 (HI),zp_temp_4 (LO) (e.g., -3).
; Output: Result stored at return HI,LO (treated as unsigned).
.proc add_signed_unsigned_16_bit
    CLC              ; Clear carry flag
    LDA zp_temp_3    ; Load the high byte signed number from zp_temp_3

    ; Check if the signed number is negative
    BMI @convert_neg  ; If the number is negative (bit 7 set), jump to convert_neg

    ; If positive, just add directly as an unsigned number
    lda zp_temp_4    ; start with signed LO
    clc
    ADC zp_temp_2    ; Add unsigned number lo
    sta return + 1
    lda zp_temp_3    ; and now do HI
    adc zp_temp_1
    sta return
    BVC @store_result ; If no overflow, store result
    ; Handle overflow (optional)
    JMP @store_result ; Skip to store result

@convert_neg:
    ; Convert negative signed number to its unsigned equivalent
    ; Start with low byte
    ; We do LO then HI to conserve the carry flag
    lda zp_temp_4
    EOR #$FF         ; Invert bits (2's complement step 1)
    CLC              ; Clear carry for addition
    ADC #$01         ; Add 1 (2's complement step 2)
    sta zp_temp_4    ; store back for later use

    ; now invert the HI byte, same as above
    lda zp_temp_3
    eor #$FF
    adc #$01
    sta zp_temp_3    ; store back

    lda zp_temp_4
    ; Now the signed number is converted to positive. Add unsigned number.
    SEC              ; Set carry for subtraction
    SBC zp_temp_2    ; Subtract unsigned number from the converted signed number
    sta return + 1   ; store the lo byte
    lda zp_temp_3
    sbc zp_temp_1
    sta return       ; store the hi byte
    @store_result:
rts
.endproc

; Input: Unsigned number in memory at zp_temp_1 (HI),zp_temp_2 (LO) (e.g., 5).
;        Signed number in memory at zp_temp_3 (8-bit) (e.g., -3).
; Output: Result stored at return HI,LO (treated as unsigned).
.proc add_signed_unsigned_8_16_bit
    CLC              ; Clear carry flag
    LDA zp_temp_3    ; Load the signed number

    ; Check if the signed number is negative
    BMI @convert_neg  ; If the number is negative (bit 7 set), jump to convert_neg

    ; If positive, just add directly as an unsigned number
    clc
    ADC zp_temp_2    ; Add unsigned number lo
    sta return + 1
    lda zp_temp_1    ; Now do HI
    adc #$00         ; carry
    sta return
    BVC @store_result ; If no overflow, store result
    ; Handle overflow (optional)
    JMP @store_result ; Skip to store result

@convert_neg:
    ; Convert negative signed number to its unsigned equivalent
    ; Start with low byte
    EOR #$FF         ; Invert bits (2's complement step 1)
    CLC              ; Clear carry for addition
    ADC #$01         ; Add 1 (2's complement step 2)
    sta zp_temp_3

    ; Now the signed number is converted to positive. Add unsigned number.
    lda zp_temp_2
    SEC              ; Set carry for subtraction
    SBC zp_temp_3    ; Subtract unsigned number LO from the converted signed number
    sta return + 1   ; store the lo byte
    lda zp_temp_1
    sbc #$00          ; add carry
    ;sta zp_temp_1    ; retain carry
    sta return       ; store the hi byte

@store_result:
    ;STA $02          ; Store the result in $02 (interpreted as signed)
rts
.endproc

; Converts an unsigned 8-bit integer to a 16-bit signed fixed point (8.4).
; IN A - the 8-bit int
; OUT return (HI), return + 1 (LO)
.proc convert_unsigned_to_signed_fixed_point
    ; Take the 4 most significant bits, move them to the high byte's low 4 bits
    pha ; store A on the stack
    and #$f0 ; Get the four high bytes
    lsr
    lsr
    lsr
    lsr ; Shift those bytes right
    sta return ; store HI
    pla ; restore A
    asl
    asl
    asl
    asl ; Shift least sig. 4 bits to most sig. Subpixels are zero.
    sta return + 1 ; store LO
rts
.endproc

.proc convert_fixed_point_y_to_unsigned
    lda y_px
    sta zp_temp_1
    lda y_sub_px
    sta zp_temp_2
    jsr convert_fixed_point_to_unsigned
    rts
.endproc

.proc convert_fixed_point_x_to_unsigned
    lda x_px
    sta zp_temp_1
    lda x_sub_px
    sta zp_temp_2
    jsr convert_fixed_point_to_unsigned
    rts
.endproc

; Converts a 16-bit fixed point number (8.4) to an 8-bit unsigned number.
; The fixed point number is assumed to be positive. I use this for pixel conversion.
; Subpixels get stripped away
; IN zp_temp_1 (HI), zp_temp_2 (LO)
; OUT A
.proc convert_fixed_point_to_unsigned
    lda zp_temp_1
    asl
    asl
    asl
    asl ; shift left 4 times
    ;pha ; Toss on the stack
    sta zp_temp_1
    lda zp_temp_2
    lsr
    lsr
    lsr
    lsr ; shift right 4 times
    ora zp_temp_1
rts
.endproc

.segment "VECTORS"
	.addr nmi, reset, 0

.segment "CHARS"
    .incbin "sprites.chr"