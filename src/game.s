; main game Loop
.include "constants.inc"

.import init, load_palette, draw_board, place_food, place_header_food, print_level_end_message, generate_attribute_byte, generate_attribute_byte_header
.import readjoy2_safe, restore_board_meta_tile, ppu_place_board_meta_tile, attributes, copy_nametable, forest_palette_0
.export zp_temp_1, zp_temp_2, zp_temp_3, screen, screen_rows, current_low, current_high, end_low, end_high, current_low_2, current_high_2
.export random_index, random, ppu_update_tile, ppu_update_tile_temp, temp_a, temp_x, temp_y, current_level, xy_meta_tile_offset
.export food_count, ppu_update, xy_meta_tile_offset, temp_offset, buttons, ppu_update_byte, coord_quarter, print_ptr
.export temp_i, tile, ppu_update_tile_reg, nametable_ptr, titlescreen_pal, palette_ptr

.segment "HEADER"

INES_MAPPER = 1 ; 0 = NROM, 1 = MMC 1
INES_MIRROR = 1 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 0 ; 1 = battery backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A ; ID
.byte $02 ; 16k PRG chunk count
.byte $02 ; 8k CHR chunk count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding

; I've commented the memory addresses to make debugging easier. It is a nightmare without these.
.segment "ZEROPAGE"
nmi_lock:       .res 1 ; 00 prevents NMI re-entry
nmi_ready:      .res 1 ; 01 set to 1 to push a PPU frame update, 2 to turn rendering off next NMI
buttons:        .res 2 ; 02, 03
nmi_count:      .res 1 ; 04
tick_count:     .res 1 ; 05
head_x:         .res 2 ; 06, 07 Enable two-player
head_y:         .res 2 ; 08, 09
new_x:          .res 2 ; 0a, 0b UNUSED
new_y:          .res 2 ; 0c, 0d UNUSED
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
; Velocities are a 4.4 fixed point structure. This requires one byte. The significant byte stores the sign. 2's compliment is the signed notation.
; 4.4 = 1 (sign) + 3 bits for the pixel + 4 bits for the subpixel.
x_vel_px:       .res 2 ; 40, 41
y_vel_px:       .res 2 ; 42, 43
zp_temp_4:      .res 1 ; 44
temp_tile:      .res 1
prev_head_x:    .res 2
prev_head_y:    .res 2
snake_head_offset: .res 1
snake_ll_lo:     .res 1
snake_ll_hi:     .res 1
timer:           .res 1
print_ptr:       .res 2
window_right:    .res 1
window_bottom:   .res 1
window_left:     .res 1
window_top:      .res 1
meta_x:         .res 1
meta_y:         .res 1
meta_i:         .res 1
tile:           .res 4 ; Used when we find the four tiles in a metatile
temp_i:         .res 1
nametable_ptr:  .res 2 ; Pointer to nametable data in RAM
palette_ptr:    .res 2 ; Pointer to a palette in RAM

.segment "BSS"          ; This is the 8k SRAM memory (can be used for work or saves)
nmt_update: .res 256 ; nametable update entry buffer for PPU update
screen:     .res 256 ; Mirror of what is in the PPU. The snake can get quite large so we store it in this mirror.
                        ; We sacrifice memory for speed, it takes a while to check collisions on a 100-size snake if not in screen mirror memory.
                     ; The snake is mutable background and collides with itself.

.segment "STARTUP" ; avoids warning
screen_rows: ; screen offset to start of each row
    .byte $0, $10, $20, $30, $40, $50, $60, $70, $80, $90, $a0, $b0, $c0, $d0, $e0
dirs:
    .byte UP, DOWN, LEFT, RIGHT

; The speed in UP, DOWN, LEFT, RIGHT, using two's compliment for UP and LEFT.

speed_x_dir:
    .byte $0, $0, $F8, $08

speed_y_dir:
    .byte $F8, $08, $0, $0

button_dirs:
    .byte BUTTON_UP, BUTTON_DOWN, BUTTON_LEFT, BUTTON_RIGHT

o_dirs: ; opposite directions - to test if player pressed in opposite of current direction
    .byte DOWN, UP, RIGHT, LEFT

snakes_hi:
    .byte >SNAKE, >SNAKE2
snakes_lo:
    .byte <SNAKE, <SNAKE2

; I'm lazy so assuming that all sprites are stored in a perfect rectangle in CHR-ROM
; Store the top-left tile for each entity
; The beginning tile for each head dir
head_dir:
    .byte $02, $06, $04, $00

blank_shape:
    .byte $00, $00, $00, $00

; This is the start of a double-array. The first array index is the previous direction. The second index is the current direction.
; This determines which body shape to pick.
; TODO - Consider splitting the high and low bytes into separate arrays to remove the bit shifts.
body_lo:
    .byte <body_up_lo, >body_up_lo, <body_down_lo, >body_down_lo, <body_left_lo, >body_left_lo, <body_right_lo, >body_right_lo ;, <body_down_hi ;, LEFT, RIGHT

body_up_lo:
    .byte BODY_VERT_SHAPE
    .byte BODY_VERT_SHAPE
    .byte BODY_UP_LEFT_SHAPE
    .byte BODY_UP_RIGHT_SHAPE

body_down_lo:
    .byte BODY_VERT_SHAPE
    .byte BODY_VERT_SHAPE
    .byte BODY_RIGHT_UP_SHAPE
    .byte BODY_DOWN_RIGHT_SHAPE

body_left_lo:
    .byte BODY_DOWN_RIGHT_SHAPE
    .byte BODY_UP_RIGHT_SHAPE
    .byte BODY_HOR_SHAPE
    .byte BODY_HOR_SHAPE

body_right_lo:
    .byte BODY_RIGHT_UP_SHAPE
    .byte BODY_RIGHT_DOWN_SHAPE
    .byte BODY_HOR_SHAPE
    .byte BODY_HOR_SHAPE

reset:
    sei        ; ignore IRQs
    cld        ; disable decimal mode
    ldx #$40
    stx $4017  ; disable APU frame IRQ
    ldx #$ff
    txs        ; Set up stack
    jsr reset_mapper
    lda #%00011110     ; 4KB CHR mode (bit 4 = 0), vertical mirroring, 16KB PRG mode
    jsr init_mapper
    lda #2             ; CHR bank 3 (12KB-16KB in CHR ROM)
    ; lda #$1             ; CHR bank 1 (4KB-8KB in CHR ROM)
    jsr set_mapper_chr0

    ; stx $8000  ; reset the mapper

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

lda #<titlescreen_pal
sta palette_ptr
lda #>titlescreen_pal
sta palette_ptr + 1
lda #$0f
jsr load_palette

; Set up OAMADDR
lda #<OAM   ; Low byte of sprite_data address
sta OAM_ADDRESS             ; Store low byte into OAMADDR

lda #$20                    ; High byte of sprite_data address
sta OAM_ADDRESS             ; Store high byte into OAMADDR

lda #<nametable_data
sta nametable_ptr
lda #>nametable_data
sta nametable_ptr + 1
; Copy to the first nametable ($2000)
lda #$20
jsr copy_nametable

; Trigger OAMDMA transfer
lda #%10001000
sta $2000  ; enable NMI, sprite pattern table starts at $1000
lda #$1e
sta $2001  ; enable rendering
lda #$ff
sta $4010  ; enable DMC IRQs
jsr wait_until_any_button_press

jsr ppu_off
jsr init_game
jsr init_level
jsr draw_head
jsr ppu_on
jsr ppu_update
jsr level_start_wait

.proc game
    @loop:
        inc random_index
        ldx #$00
        jsr readjoy2_safe
        jsr process_input
		jsr process_snake
        lda level_complete
        beq @no_level_change
            jsr process_level_change
        @no_level_change:
		jsr ppu_update
	jmp @loop
.endproc

reset_mapper:
    ; inc resetInc
    lda #$80
    sta MMC1_CTRL ; Resets MMC1, sets the PRG-ROM bank mode to 3 (fixing the last bank at $C000 and allowing the 16 KB bank at $8000 to be switched)
    rts

init_mapper:
    ; Step 1: Set Control Register to 4KB CHR mode
    sta MMC1_ANY       ; Bit 0
    lsr
    sta MMC1_ANY       ; Bit 1
    lsr
    sta MMC1_ANY       ; Bit 2
    lsr
    sta MMC1_ANY       ; Bit 3
    lsr
    sta MMC1_CTRL      ; Bit 4 (to $8000-$9FFF for Control Register)
    rts

; CHR bank 3 (12KB-16KB in CHR ROM)
; Store the contents of A into MMC_CHR1
; A is simply the bank to use. 1 = bank 1, etc. in 4kb gaps.
; We feed the contents of A as a stream bit by bit.
set_mapper_chr0:
    ; lda #3             ; CHR bank 3 (12KB-16KB in CHR ROM)
    sta MMC1_ANY       ; Bit 0
    lsr
    sta MMC1_ANY       ; Bit 1
    lsr
    sta MMC1_ANY       ; Bit 2
    lsr
    sta MMC1_ANY       ; Bit 3
    lsr
    sta MMC1_CHR0      ; Bit 4 (to $A000-$BFFF for CHR Bank 1)
    rts

set_mapper_chr1:
    ; lda #3             ; CHR bank 3 (12KB-16KB in CHR ROM)
    sta MMC1_ANY       ; Bit 0
    lsr
    sta MMC1_ANY       ; Bit 1
    lsr
    sta MMC1_ANY       ; Bit 2
    lsr
    sta MMC1_ANY       ; Bit 3
    lsr
    sta MMC1_CHR1      ; Bit 4 (to $C000-$DFFF for CHR Bank 1)
    rts

.proc init_game
    lda #0             ; CHR bank 0 (0KB-4KB in CHR ROM)
    ; lda #$1             ; CHR bank 1 (4KB-8KB in CHR ROM)
    jsr set_mapper_chr0
    lda #1
    jsr set_mapper_chr1
    lda #START_LEVEL
    sta current_level
    ; TODO player_count and snake_speed to be in-game user-selected values
    lda #PLAYER_COUNT
    sta player_count
    rts
.endproc

.proc init_level
    lda #$00
    sta level_complete

    lda #<forest_palette_0
    sta palette_ptr
    lda #>forest_palette_0
    sta palette_ptr + 1
    lda #$20
    jsr load_palette

    jsr draw_board
    jsr place_food
    jsr init_level_variables
    jsr init_place_snake
rts
.endproc

level_start_wait:
    lda #$0
    sta timer
    @loop:
        jsr ppu_update
        bne @loop
        inc timer
        lda timer
        cmp #$50
        bne @loop
rts

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
        sta prev_head_x, y
        sta tail_x, y
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
        sta prev_head_y, y
        sta tail_y, y
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

; x = player count index
.proc init_place_snake
    ldx #$0
    @loop:

    lda START_X, x
    sta temp_x
    lda START_Y, x
    sta temp_y

    jsr xy_meta_tile_offset
    sta snake_head_offset

    jsr store_head

    lda snakes_hi, x
    sta snake_ll_hi
    lda snakes_lo, x
    sta snake_ll_lo

    lda #$4 ; h
    sta temp_a
    ldy #$0
    sta (snake_ll_lo), y
    ; The body exists on the current head tile
    ; place head on nmi queue
    lda START_X, x
    sta temp_x
    lda START_Y, x
    sta temp_y
    
    lda #BODY_HOR_SHAPE
    sta temp_i
    jsr ppu_place_board_meta_tile

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
    lda return
    cmp #$0
    beq @end

    jsr align_head_if_turning
    jsr process_snake_new_tile
    lda head_x
    sta prev_head_x
    lda head_y
    sta prev_head_y
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
    lda #$0
    sta return
    ; offset the point to check based on direction
    ; The pixel to check is in the opposite direction you are moving
    ; example: If moving left, use the rightmost point of the sprite to check when player has fully crossed into the new tile
    lda cur_dir
    cmp #UP
    bne @not_up
        ; check if y is in a new tile
        jsr convert_fixed_point_y_to_unsigned
        clc
        adc #$f ; 15, check bottom pixel
        jmp @check_y
     @not_up:
    cmp #DOWN
    bne @not_down
        jsr convert_fixed_point_y_to_unsigned
        jmp @check_y
    @not_down:
    cmp #LEFT
    bne @not_left
        jsr convert_fixed_point_x_to_unsigned
        clc
        adc #$f ; 15, check right pixel
        jmp @check_x
    @not_left:
    cmp #RIGHT
    bne @end
        jsr convert_fixed_point_x_to_unsigned
        jmp @check_x

    @check_y:
        lsr
        lsr
        lsr
        lsr ; divide by 16
        cmp head_y
        beq @end
        ; y tile changed
        sta head_y
        lda #$1
        sta return
        rts

    @check_x:
        ; check if x is in a new tile
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
        rts

	@end:
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
    lda head_dir, x
    sta temp_tile
    ; x = sprite number
    ldx #$0

    jsr convert_fixed_point_y_to_unsigned
    clc
    adc #$ff ; subtract one to correct an NES PPU issue where all sprites are drawn on the next scanline
    sta temp_y
    jsr convert_fixed_point_x_to_unsigned
    sta temp_x
    jsr draw_sprite

    inc temp_tile
    lda temp_x
    clc
    adc #$08
    sta temp_x
    jsr draw_sprite

    lda temp_tile
    clc
    adc #$f
    sta temp_tile
    lda temp_x
    sec
    sbc #$08
    sta temp_x
    lda temp_y
    clc
    adc #$08
    sta temp_y
    jsr draw_sprite

    inc temp_tile
    lda temp_x
    clc
    adc #$08
    sta temp_x
    jsr draw_sprite

    ; clear the other 63 sprites
    @forx:
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
        inx
    bne @forx
rts
.endproc

; Loads an 8x8 sprite to OAM memory
; IN
; (temp_x, temp_y) coords
; temp_a tile
.proc draw_sprite
    lda temp_y
    sta OAM, x
    inx
    lda temp_tile
    sta OAM, x
    inx
    lda #$0 ; attributes
    sta OAM, X
    inx
    lda temp_x
    sta OAM, x
    inx
rts
.endproc

.proc process_snake_new_tile
    ldx #$0
    @loop:
    jsr adjust_direction
    jsr calc_velocity
    jsr move_snakex
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

; Moves one snake while checking for collision
; IN
; X = snake number (0 = snake 1, 1 = snake 2)
; Using temp variables and the stack - when possible, instead of obliterating the x register, to simplify code and reduce bugs.
.proc move_snakex
    ; Using the temp variable version of the routine so X remains intact
    ; This has the unintended side effect of making indexing cleaner, further simplifying code.
    lda head_y, x
    sta temp_y
    lda head_x, x
    sta temp_x
    jsr xy_meta_tile_offset
    sta snake_head_offset

    jsr process_collision_detection

    jsr store_head
    jsr move_head

    jsr process_tail
	rts
.endproc

; Store the head in screen space memory
.proc store_head
    lda #SCREEN_SNAKE ; the snake index
    ldy snake_head_offset
    sta screen, y
    rts
.endproc

.proc move_head
    ; Record the movement to the "linked list"
    ; I call it a linked list but it's more of a sliding window array. Uses less memory. Need to be careful for page reset however.
    ; Mark the current head with the direction to the new head

    lda snakes_hi, x
    sta snake_ll_hi
    lda snakes_lo, x
    sta snake_ll_lo

    lda prev_dir, x ; We just store directions so the tail can follow along
    ldy head_index, x
    sta (snake_ll_lo), y
    ; Increment to new head
    iny
    ; Store 'h' at the new head since we don't know the direction to its next head yet
    lda cur_dir
    sta (snake_ll_lo), y
    sty head_index, x

    ; Draw over old head with body
    lda head_x, x
    sta temp_x
    lda head_y, x
    sta temp_y
    jsr choose_body_metatile
    tay
    jsr ppu_place_board_meta_tile

    rts
.endproc

; Chooses the correct metatile for the "neck" of the snake.
; The neck is one below the head. We need the direction of the tile before the neck, as well as the neck, to determine
; the metatile.
; Ex: If the direction before the neck was up, and the neck's dir is right, the tile we need for the neck is up-right
; OUT
; A = tile index
.proc choose_body_metatile
    ldy head_index, x
    dey

    ; For each direction (up,down,left,right), we store the array as UP_LO, UP_HI, DOWN_LO, DOWN_HI, etc.
    lda (snake_ll_lo), y
    asl ; double the value to find the correct index in the array
    tay
    lda body_lo, Y
    sta current_low_2
    iny
    lda body_lo, Y
    sta current_high_2

    ldy head_index, x
    lda (snake_ll_lo), y

    tay
    lda (current_low_2), Y
    sta temp_i
rts
.endproc

.proc process_tail
    ; Move tail?
    lda size, x
    cmp target_size, x
    bne grow
        lda tail_x, x
        sta temp_x
        lda tail_y, x
        sta temp_y
        jsr restore_board_meta_tile

        lda #$0

        ldy tail_index, x
        inc tail_index, x
        lda (snake_ll_lo), Y
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
    ldy snake_head_offset
    lda screen, y
    tay
    lda attributes, Y

    cmp #NONE
    bne @not_none
        rts
    @not_none:
    cmp #DEAD
    bne no_self
        jmp inf_loop
    no_self:
    cmp #FOOD ; food!
    bne @no_coll
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
    rtn:
rts

process_level_change:
    jsr print_level_end_message
    jsr ppu_update
    jsr wait_until_a_b_button_press

    @end:
    inc current_level
    jsr ppu_off
    jsr init_level
    ; Wait for next frame and turns PPU rendering back on
    jsr ppu_on
    jsr ppu_update
rts

wait_until_a_b_button_press:
    :
        jsr ppu_skip
        jsr readjoy2_safe
        ldx #$0
        lda buttons, x
        and #BUTTON_A
        bne @rtn
        lda buttons, x
        and #BUTTON_B
        bne @rtn
        jmp :-
    @rtn:
rts

wait_until_any_button_press:
    :
        jsr readjoy2_safe
        ldx #$0
        lda buttons, x
        and #ACTION_BUTTONS
        bne @rtn
        jsr ppu_update
        jmp :-
    @rtn:
rts

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

    ; nametable update
	ldx #0
	cpx nmt_update_len
	bcs @scroll
	@nmt_update_loop:
		lda nmt_update, X ; PPU High Byte address to store data
        sta current_high
		sta $2006
		inx
		lda nmt_update, X ; Low byte
        sta current_low
		sta $2006
		inx
		lda nmt_update, X ; data
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

	rti
.endproc

; ppu_update: waits until next NMI, turns rendering on (if not already), uploads OAM, palette, and nametable update to PPU
ppu_update:
	lda #1
	sta nmi_ready
	:
        ldx #00
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

; IN
; temp_y, temp_x
; proc converts an x,y position for a Metatile to the screen space's offset.
; OUT
; A = meta tile offset
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

; Screen Space starts at 0. This moves the pointers so they start at the memory location for the screen in CPU memory
convert_screen_space_to_screen_memory:
    tya ; Low byte
    clc
    adc #<screen
    tay
    txa ; High byte
    adc #>screen ; adds carry flag if needed
    tax
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
    stx zp_temp_1
    sty zp_temp_2
    pla ; pull x off stack (from coord system)
    tax
    pla ; pull y off stack (from coord system)
	lsr
	lsr
	lsr
    pha ; Store High Byte on stack
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
    ; Falls through to next routine

; Screen Space starts at 0. This moves the pointers so they start at the memory location for the screen in CPU memory
convert_screen_space_to_screen_memory_stack:
    tya ; Low byte
    clc
    adc #<screen
    pha ; record the LOW byte to return on the stack
    txa ; High byte
    adc #>screen ; adds carry flag if needed
    pha ; record HIGH byte to return on the stack
    ldx zp_temp_1 ; Restore original x and y registers
    ldy zp_temp_2
    rts

; ppu_update_tile_reg: can be used with rendering on, sets the tile at temp vars X/Y to temp var A next time you call ppu_update
; This is useful because ppu_update_tile destroys register values while this subroutine retains them.
; IN
; A = tile
; X = x
; Y = y
ppu_update_tile_reg:
    pha ; store A on stack
    txa
    pha ; store X on stack
    tya
    pha ; store Y on stack

    jsr ppu_update_tile

    pla ; Get Y from stack
    tay
    pla ; Get X from stack
    tax
    pla ; Get A from stack
rts

; ppu_update_tile: can be used with rendering on, sets the tile at temp vars X/Y to temp var A next time you call ppu_update
; This is useful because ppu_update_tile destroys register values while this subroutine retains them.
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

; Converts an x,y tile coordinate to the location in the nametable 1 array
; Future enhancement - support other nametables. This game only ever uses NT 1 so no need.
; IN X and Y registers
; OUT return (hi), return + 1 (lo) bytes of nametable address
tile_space_to_ppu_space:
	pha ; temporarily store A on stack
	tya
	lsr
	lsr ; every 8 rows is $100 hex in memory. - 32 tiles wide = 20 hex * 8 = 100 hex (remember hex is base 16)
	lsr ; the 100 hex is important because we need the high byte in the next sta
	ora #$20 ; high bits of Y + $20. The bits for 20 do not overlap with 1, 2, or 3. So OR-ing them gives us 20, 21, 22, or 23.
	sta return
	tya
	asl
	asl
	asl
	asl
	asl ; 2 ^ 5 = 32. This multiplies Y by 32 (the width of the screen). Y * 32 + x = screen location in memory.
	sta zp_temp_1
	txa
	ora zp_temp_1 ; No bits overlap so OR is a very fast ADC
    sta return + 1
    pla ; restore A from stack
    rts

; ppu_update_tile: can be used with rendering on, sets the tile at X/Y to tile A next time you call ppu_update
ppu_update_tile:
    pha ; store A
    jsr tile_space_to_ppu_space
    ldx nmt_update_len
    lda return
    sta nmt_update, X ; hi byte
    inx
    lda return + 1
    sta nmt_update, X ; lo byte
    inx
    pla ; recover A value (tile)
	sta nmt_update, X ; tile
	inx
	stx nmt_update_len
	rts

; ppu_update_byte: like ppu_update_tile, but X/Y makes the high/low bytes of the PPU address to write
;    this may be useful for updating attribute tiles or when you need to make a bunch of updates at once.
ppu_update_byte:
	pha ; temporarily store A on stack
	tya
	pha ; temporarily store Y on stack
	ldy nmt_update_len
	txa
	sta nmt_update, Y
	iny
	pla ; recover Y value (but put in A)
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
    tay
rts

; Converts an x and y to a coord system with half the width (Tile to Metatile, or Metatile to Attribute Table)
; IN (x,y)
; OUT (x,y), modified
coord_half:
    txa
    lsr ; Divide by 2
    tax
    tya
    lsr ; Divide by 2
    tay
    rts

; Converts an x and y to a coord system with one quarter the width (ex: Tile to Attribute Table)
; IN (x,y)
; OUT (x,y), modified
coord_quarter:
    txa
    lsr
    lsr ; Divide by 4
    tax
    tya
    lsr
    lsr ; Divide by 4
    tay
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
    sta return       ; store the hi byte

@store_result:
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

.segment "CHR_BANK_0"
    .incbin "./assets/sprites.chr"

.segment "CHR_BANK_1"
    .incbin "./assets/titlescreen.chr"

.segment "RODATA"
nametable_data:
    .incbin "./assets/titlescreen.nam"
titlescreen_pal:
    .incbin "./assets/titlescreen.pal"