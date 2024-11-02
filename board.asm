.include "constants.asm"
; Drawing the various game boards or levels

.importzp zp_temp_1, zp_temp_2, zp_temp_3, start_low, start_high, current_low, current_high, end_low, end_high
.importzp random_index, temp_a, temp_x, temp_y, current_level, food_count, temp_offset, print_ptr, window_x, window_y
; .importzp meta_x, meta_y, meta_i
.import screen, random, tile_to_screen_space_xy, ppu_update_tile, ppu_update_tile_temp, screen_space_to_ppu_space, ppu_update
.import xy_meta_tile_offset, screen_rows, ppu_update_byte, coord_quarter
.export draw_board, place_food, place_header_food, print_level_end_message, generate_attribute_byte, get_board_tile
.export restore_board_meta_tile, ppu_place_board_meta_tile, attributes

; 2x2 tiles. indexes into the tile table below
board0:
.byte 2, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 3
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 5, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 4

board1:
.byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.byte 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0
.byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

board2:
.byte 2, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 3
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 7
.byte 9, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 7
.byte 9, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 7
.byte 9, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 7
.byte 9, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 7
.byte 9, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 7
.byte 9, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 7
.byte 9, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 5, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 4

boards:
.word board0, board1, board2

startxs1:
    .byte $0f, $03, $09
startys1:
    .byte $03, $03, $0f
startxs2:
    .byte $10, $1a, $10
startys2:
    .byte $11, $1a, $11


header:
    .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, "S", "N", "A", "K", "E", $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
    .byte $ff, "F", "o", "o", "d", $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, "F", "o", "o", "d", $ff, $ff, $ff, "L", "e", "v", "e", "l", $ff, $ff, $ff, $ff, $ff

level_complete_message:
    .byte "@@@@@@@@@@@@@@@@@@@@@", 1
;level_complete_message1:
    .byte "@                   @", 1
;level_complete_message2:
    .byte "@  Level Complete!  @", 1
;level_complete_message3:
    .byte "@                   @", 1
;level_complete_message4:
    .byte "@@@@@@@@@@@@@@@@@@@@@", 0

;level:
;    .byte "Level", $1A
;food:
;    .byte "Food", $1A

; Tile tables
; Storing our levels in the full 32x30 screen size wastes far too much space - 960 bytes per level!!! We are 
; dividing our tiles into 2x2 segments similar to how Megaman 2 and M.C. Kids works.

; Each segment is represented by a series of arrays instead of a struct because this is easier for the 6502 to work with.
; Reference https://games.greggman.com/game/programming_m_c__kids/ section Tilesets
; This allows me to create 4x the number of levels.

; Each index in the following arrays represent one factor of the metatile. From each of the four tiles to the color in the palette.
; Naturally, this caps our metatiles at 256

top_left:
.byte $90, $92, $b0, $ff, $05, $80, $02, $05, $11, $05, $80, $00

top_right:
.byte $91, $93, $b1, $ff, $05, $81, $05, $03, $81, $03, $10, $15

bottom_left:
.byte $a0, $a2, $c0, $ff, $15, $80, $80, $01, $15, $01, $12, $81

bottom_right:
.byte $a1, $a3, $c1, $ff, $15, $81, $00, $81, $13, $81, $15, $ff

color:
.byte 1, 2, 0, 0, 3, 3, 3, 3, 3, 3, 3, 2

attributes:
.byte $00, $01, $02, $00, $01, $01, $01, $01, $01, $01, $01

food_header_offset:
    .byte 6, 20

; Make sure PPU rendering is OFF before calling this
draw_board:
    jsr load_board_to_nt
    ; Load the starting locations for both snakes
    ldy current_level
    ldx #$0
    lda startxs1, y
    sta START_X, x
    lda startys1, y
    sta START_Y, x
    inx
    lda startxs2, y
    sta START_X, x
    lda startys2, y
    sta START_Y, x
rts

load_board_to_nt:
    ; locate board
    lda current_level
    asl
    tax

    lda boards, x
    sta current_low
    inx
    lda boards, x
    sta current_high

    ; It's easier to load it into the ppu memory first
    ;jsr ppu_load
    jsr header_to_ppu_load
    jsr board_to_ppu_load

    ;jsr ppu_to_screen_space
    ldy #$f0
    jsr copy_current_low_to_screen
    jsr attribute_table_load
rts

; Refer to https://www.nesdev.org/wiki/PPU_attribute_tables to find out how attributes are stored.
.proc attribute_table_load
    lda #$23
    sta PPU_ADDRESS
    lda #$c0
    sta PPU_ADDRESS

    ; ldx #$0 ; Do header, which includes header and the top row of the screen
    ; ldy #$0
    ; forx_header:
    ;     jsr generate_attribute_byte_header
    ;     sta PPU_DATA
    ;     inx
    ;     cpx #$8
    ; bne forx_header

    ldy #$0
    ; Loop through the attribute table, which is 8x8
    fory:
        ldx #$0
        forx:
            lda #$0
            jsr generate_attribute_byte
            sta PPU_DATA
            inx
            cpx #$8
        bne forx
        iny
        cpy #$8
    bne fory

    ; We do the last 8 bytes here
    rts
.endproc

; .proc attribute_table_load
;     jsr clear_attribute_table

;     lda #$23
;     sta PPU_ADDRESS
;     lda #$c0
;     sta PPU_ADDRESS 
;     ; We do the first 60 bytes here
;     lda #$0
;     sta temp_y
;     fory:
;         ldy temp_y
;         lda screen_rows, y
;         sta zp_temp_1
;         iny
;         sta screen_rows, y
;         sta zp_temp_2
;         ldx screen_rows, y
;         ldy #$0 ; metatile offset (this is the X-coord!)
;         forx:
;             lda #$0
;             ; One attribute byte is made up of four metatiles. Each metatile color is two bits.

;             cpy #$3c
;             bne forx

;     ; We do the last 8 bytes here
;     rts
; .endproc

; TODO: THIS IS TOO SLOW, FIX NOW!
.proc generate_attribute_byte
    txa
    pha ; store x
    tya
    pha ; store y

    txa; double x and y to find screen coords
    asl
    sta temp_x
    tya
    asl
    sta temp_y
    jsr generate_attribute_byte_body
    pla ; restore y
    tay
    pla ; restore x
    tax
    lda zp_temp_2
rts
.endproc

; IN x, y (attribute coords)
; OUT a (the byte), also stored in zp_temp_2
; x and y are not modified
; One attribute byte is made up of four metatiles. Each metatile color is two bits.
; value = (bottomright << 6) | (bottomleft << 4) | (topright << 2) | (topleft << 0)
; TODO: Optimize this
; One optimization I can do is to start at the bottom-right. This would require less bit-shifts as the bottom-right gradually gets shifted left.
.proc generate_attribute_byte_body
    jsr xy_meta_tile_offset ; get screen offset for the top-left metatile
    
    ;ldy temp_offset
    tay ; transfer offset to Y
    ldx screen, y ; top-left
    lda color, x
    sta zp_temp_2

    iny
    ldx screen, y ; top-right
    lda color, x
    asl
    asl
    ora zp_temp_2
    sta zp_temp_2

    ; The last row of the attribute table does not have bottom metatiles. https://www.nesdev.org/wiki/PPU_attribute_tables
    cpy #$e ; 14
    beq exit

    tya
    clc
    adc #$f ; 15, gets us to the bottom-left metatile
    tay
    ldx screen, y ; bottom-left
    lda color, x
    asl
    asl
    asl
    asl
    ora zp_temp_2
    sta zp_temp_2

    iny ; bottom-right
    ldx screen, y ; bottom-left
    lda color, x
    asl
    asl
    asl
    asl
    asl
    asl
    ora zp_temp_2
    sta zp_temp_2

    exit:
    rts
.endproc

; .proc clear_attribute_table
;     lda #$23
;     sta PPU_ADDRESS
;     lda #$c0
;     sta PPU_ADDRESS

;     ; clear the attribute table
;     ldy #$0
;     lda #$0
;     fory:
;         sta PPU_DATA
;         iny
;         cpy #$40 ; 64
;         bne fory
;     rts
; .endproc

; tile_map_to_screen:
;     ldx #$0
;     ldy #$0

;     ; This transfers from ppu memory to screen memory
;     ; x and y are flip flopped because indirect indexing forces us to use y :(
;     fory:
;     ldy #$0
;     forx:
;     ; loop:
;     ; Transfer the current memory location to the PPU
;     lda (current_low), y
;     sta screen, y ; CPU screen memory
;     sta PPU_DATA ; PPU memory
;     ; Increment the address
;     iny
;     jmp checkexit
;     noexit:
;     cpy #$00 ; page flip?
;     bne forx

;     inc current_high
;     inc current_high_2
;     inx
;     cpx #$1e ; 30
;     bne fory

;     checkexit:
;     cpy #$c0
;     bne noexit
;     cpx #$03
;     bne noexit
; rts

header_to_ppu_load:
    lda #>NT0
    sta PPU_ADDRESS
    lda #<NT0
    sta PPU_ADDRESS
    ;ldx #$0
    ldy #$0
    @fory:
        ; This is just a memory span, no need to loop x
        lda header, y
        sta PPU_DATA ; PPU memory
        iny
    cpy #$40 ; 64, two full lines
    bne @fory
rts

place_header_food:
    txa
    pha ; place x on stack
    ldx #$0
    jsr place_header_foodx
    inx
    jsr place_header_foodx
    pla ; remove x from stack
    tax
rts

place_header_foodx:
    lda #$1
    sta temp_y
    lda food_header_offset, X
    sta temp_x
    lda food_count, x
    clc
    adc #$30 ; number to CHR offset
    sta temp_a
    jsr ppu_update_tile_temp
rts

copy_current_low_to_screen:
    ldx #$00
    lda #HEADER
    @header_loop:
        sta screen, x
        inx
        cpx #$10
    bne @header_loop

    ldy #$0
    @loop:
        lda (current_low), y
        sta screen, x
        iny
        inx
        ; cpy #$00 ;0
    bne @loop
rts

restore_board_meta_tile:
    txa ; store x
    pha
    ; TODO Replace ppu_update_tile_temp calls to ppu_update_tile to improve performance and decrease stack depth
    ;dec temp_y
    jsr xy_meta_tile_offset
    sta temp_offset ; store offset in temp_offset
    ;inc temp_y
    ; clc
    ; tya
    ; adc #$20
    ; tay
    ; TODO Find a better way to do this, it's kind of hacky
    ; lda temp_offset
    ; sec
    ; sbc #$20
    ; sta temp_offset
    jsr get_board_tile
    ldy temp_offset
    ; Restore the original metatile onto the screen in RAM
    sta screen, y
    ;tay
    sta temp_i
    jsr ppu_place_board_meta_tile

    pla ; restore x
    tax
rts

; Stores and draws a 2x2 metatile for display from the board in ROM, sets attributes
; Use when rendering is ON, it will update on the next nmi
; Used to reset a metatile to their original contents
; IN
; temp_x (metatile location)
; temp_y
; temp_i = tile index
; This is typically called inside of a loop, so the pass-in params are variables instead of registers
; Also retaining the x and y registers because of the loop scenario
ppu_place_board_meta_tile:
    tya
    pha ; store Y on stack
    txa
    pha ; store X on stack
    ; Convert from metatile-space to tile space
    lda temp_x
    asl ; multiply by 2
    sta temp_x
    ;tax
    lda temp_y
    asl
    ;tay
    sta temp_y
    ldy temp_i

    lda top_left, y
    sta temp_a
    jsr ppu_update_tile_temp
    inc temp_x
    ;inx
    lda top_right, y
    sta temp_a
    jsr ppu_update_tile_temp
    dec temp_x
    inc temp_y
    lda bottom_left, y
    sta temp_a
    jsr ppu_update_tile_temp
    inc temp_x
    lda bottom_right, y
    sta temp_a
    jsr ppu_update_tile_temp

    ; Update the attribute byte
    
    ldx temp_x
    ldy temp_y
    ; lda temp_x ; convert x and y back into metatile-coords
    ; lsr
    ; tax
    ; lda temp_y
    ; lsr
    ; tay
    ; Create the attribute byte replacement
    ; convert from tile-space to attribute space (divide x and y by 4)
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
    adc #$c0 ; attribute table offset
    ;ora #$c0
    tay
    lda zp_temp_2
    jsr ppu_update_byte

    pla
    tax ; restore X from stack
    pla
    tay ; restore Y from stack
rts

; Gets the tile for the currently loaded board by temp_offset
; IN temp_offset
; OUT a = tile index
get_board_tile:
    ;jsr xy_meta_tile_offset
    ;ldy temp_offset
    ; locate board

    ; screen to board offset - we are working in metatiles so subtract $10 (16)
    lda temp_offset
    sec
    sbc #$10
    sta zp_temp_1

    lda current_level
    asl
    tax

    lda boards, x
    sta current_low
    inx
    lda boards, x
    sta current_high
    ldy zp_temp_1
    lda (current_low), y


rts

board_to_ppu_load:
    lda #>NT0
    sta PPU_ADDRESS
    lda #<NT0
    clc
    adc #$40 ; skip the title area
    sta PPU_ADDRESS
    ldx #$0
    ldy #$0
    ; PPU_DATA requires a stream of memory. We are using 2x2 tiles. Act accordingly.

    ldy #$0 ; 15 x 16 = 240, we don't ever need to carry to the high byte.
    lda #$0
    sta zp_temp_1 ; start of row
    lda #$10 ; 16
    sta zp_temp_2 ; end of row
    lda #$0
    sta zp_temp_3 ; row count
    @fory:
        ldy zp_temp_1
        ; Loop through the top row
        @forx:
            ; Transfer the current memory location to the PPU
            ; The board stores indexes into a series of arrays to represent the 2x2 tile
            lda (current_low), y
            tax
            lda top_left, x
            ;jsr tile_convert
            sta PPU_DATA ; PPU memory
            lda top_right, x
            ;jsr tile_convert
            sta PPU_DATA ; PPU memory
            
            ; Increment the address
            iny
            cpy zp_temp_2 ; 16
        bne @forx
        ; Loop through the same row on the board again, this time to do the bottom row
        ldy zp_temp_1
        @forx2:
            ; Transfer the current memory location to the PPU
            ; The board stores indexes into a series of arrays to represent the 2x2 tile
            lda (current_low), y
            tax
            lda bottom_left, x
            ;jsr tile_convert
            sta PPU_DATA ; PPU memory
            lda bottom_right, x
            ;jsr tile_convert
            sta PPU_DATA ; PPU memory
            
            ; Increment the address
            iny
            cpy zp_temp_2 ; 16
        bne @forx2

        ;inx
        lda zp_temp_1
        clc
        adc #$10
        sta zp_temp_1
        lda zp_temp_2
        clc
        adc #$10
        sta zp_temp_2
        inc zp_temp_3
        lda zp_temp_3 ; row count
        cmp #$e ; 14
    bne @fory
    ; reset scroll location to top-left of screen
    lda #$00
    sta PPU_SCROLL
    sta PPU_SCROLL
rts

; Convert tiles from the ppu to screen space
; ppu_to_screen_space:
;     lda #>NT0
;     sta PPU_ADDRESS
;     lda #<NT0
;     clc
;     adc #$40 ; skip the title area
;     sta PPU_ADDRESS

;     ; Load the buffer
;     lda PPU_DATA

;     ldx #$0
;     ldy #$0

;     ;This transfers from ppu memory to screen memory
;     ;x and y are flip flopped because indirect indexing forces us to use y :(
;     fory:
;         ldy #$0
;         forx:
;             ; loop:
;             ; Transfer the current memory from the PPU to the screen space
;             lda PPU_DATA ; PPU memory
;             sta (current_low), y
;             ;sta screen, y ; CPU screen memory

;             ; Increment the address
;             iny
;             jmp checkexit
;         noexit:
;         cpy #$00 ; page flip?
;         bne forx

;         inc current_high
;         ;inc current_high_2
;     inx
;     cpx #$1e ; 30
;     bne fory

;     checkexit:
;     cpy #$c0
;     bne noexit
;     cpx #$03
;     bne noexit
;     ; Done transfering
; rts

; Convert a tile from an abstracted value to a themed 
tile_convert:
    cmp #$1
    bne @no_wall
    lda #$58
    jmp @end
    @no_wall:
    lda #$0
    @end:
rts

place_food:
    jsr find_blank_tile
    lda #FOOD
    sta temp_a
    sta screen, y
    ; txa ; temporarily store X on stack
    ; pha
    ; tya ; temporarily store Y on stack
    ; pha
    ; lda #<food_shape
    ; sta current_low
    ; lda #>food_shape
    ; sta current_high
    ; tya
    ; ; clc
    ; ; adc #$02 ; align with screen
    ; sta temp_y
    ; txa
    ; sta temp_x
    ;jsr screen_space_to_ppu_space

    ;ldy temp_y
    ldy #FOOD
    jsr ppu_place_board_meta_tile
    ;jsr ppu_update_tile_temp ; Place in PPU queue before we start destroying the y register
    
    ; pla ; Pull Y off the stack
    ; tay
    ; pla ; Pull X off the stack
    ; tax

    ;jsr tile_to_screen_space_xy
    ;jsr xy_meta_tile_offset
    ;ldy temp_offset
    
    rts

; Find a blank tile
; IN
; None
; OUT
; current_x_2 points to the tile in screen space
; y = tile offset
; a = tile id
find_blank_tile:
    jsr get_random_axis
    ;pha ; Store X onto stack
    ;sta zp_temp_1 ; store x
    ;pha ; store x in stack
    ;tax
    sta temp_x
    find_y:
    jsr get_random_axis
    cmp #$e ; 14
    bpl find_y ; Find another y if > 15
    ; cmp #$00 ; Title area? Find a new y
    ; beq find_y
    ; cmp #$01 ; Title area
    ; beq find_y
    ;sta zp_temp_2 ; store y
    ;pha ; Store y onto stack

    ; header adjustment
    clc
    adc #$01
    sta temp_y
    ;tay
    ;jsr tile_to_screen_space_xy
    jsr xy_meta_tile_offset

    ; Check if the tile is free
    ;ldy temp_offset
    tay ; transfer offset to Y
    lda screen, Y
    ; TODO Use tile types instead of tile id's
    ; cmp #$c ; header area
    ; beq fail
    cmp #$58
    beq fail
    cmp #$b
    beq fail
    cmp #$a ; Cannot place on top of snake
    beq fail
    ;pla ; pull Y from stack
    ;tay
    ;ldx zp_temp_1 ; transfer x to x register
    ;tax
    ;pla ; pull X from stack
    ;tax
    ;ldy zp_temp_2 ; transfer y to y register
    ;tay
    rts
    fail:
    ;pla ; Pull X from stack
    ;pla ; Pull Y from stack
    jmp find_blank_tile

; Get a random tile on an axis (x or y)
; IN
; None
; OUT
; A = random number
get_random_axis:
    inc random_index
    ldy random_index
    ;iny ; There are 256 random values, random_index can just loop in circles via overflow
    ;sty random_index
    lda random, Y
    lsr
    lsr
    lsr
    lsr ; Shift right four times to convert 0-256 to 0-16
rts

; The message will display on the next nmi update.
print_level_end_message:
    lda #$7 ; 7
    sta temp_x
    lda #$d ; 13
    sta temp_y
    lda #<level_complete_message
    sta print_ptr
    lda #>level_complete_message
    sta print_ptr + 1
    ;jsr ppu_off
    jsr print_text
    ;jsr ppu_on
rts

; IN
; temp_x
; temp_y
; window_width
; window_height
; MUST BE CALLED WHEN PPU RENDERING IS OFF!
print_window:


    ; window top
    ldx window_x
    ldy window_y

    stx temp_x
    sty temp_y
    lda #WINDOW_TOPLEFT
    sta temp_a
    jsr ppu_place_board_meta_tile
    inx
    @forx:

        inx
        cpx window_width
        bne @forx
        
    ; window middle

    ; window bottom

rts

; Prints text to the screen. Text will appear on the next NMI update.
; Each line gets an NMI update to flush the cache. Also gives an interesting visual effect.
; Consider adding a delay after a newline to make the effect more pronounced - like a scroll.
; IN
; temp_x = x position of text
; temp_y = y position of text
; current_low = low byte of text address
; current_high = high byte of text address
print_text:
    lda temp_x
    sta zp_temp_3 ; Record x for carriage return
    ldy #$0
    @loop:
        lda (print_ptr), y
        sta temp_a
        beq @done ; 0 = null character, end
        cmp #$01
        bne @skip_new_line
            jsr ppu_update
            iny ; don't print the new line character
            ; Do carriage return
            inc temp_y
            lda zp_temp_3
            sta temp_x
            lda (print_ptr), y
            sta temp_a
        @skip_new_line:
        jsr ppu_update_tile_temp
        iny
        inc temp_x
        jmp @loop
    @done:
rts