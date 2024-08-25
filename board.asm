.include "constants.asm"
; Drawing the various game boards or levels

.importzp zp_temp_1, zp_temp_2, zp_temp_3, start_low, start_high, current_low, current_high, end_low, end_high, current_low_2, current_high_2
.importzp random_index, temp_a, temp_x, temp_y, current_level, food_count, temp_offset
.import screen, random, tile_to_screen_space_xy, ppu_update_tile, ppu_update_tile_temp, screen_space_to_ppu_space, ppu_update
.import xy_meta_tile_offset, screen_rows
.export draw_board, place_food, place_header_food, print_level_end_message

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
.byte 2, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 3
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7
.byte 5, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 4

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
    .byte 0, 0, 0, 0, 0, 0, 0, 0, "S", "N", "A", "K", "E", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    .byte 0, "F", "o", "o", "d", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "F", "o", "o", "d", 0, 0, 0, "L", "e", "v", "e", "l", 0, 0, 0, 0, 0

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
; I should build a level editor for this...
; 0 = empty
; 1 = wall
;     0, 1, 2, 3, 4, 5, 6, 7, 8, 9
top_left:
.byte 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0

top_right:
.byte 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0

bottom_left:
.byte 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0

bottom_right:
.byte 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0

color:
.byte 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1

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

    lda #<screen
    sta current_low_2
    lda #>screen
    sta current_high_2
    ;jsr ppu_to_screen_space
    ldy #$f0
    jsr copy_current_low_to_2
    jsr attribute_table_load
rts

; Refer to https://www.nesdev.org/wiki/PPU_attribute_tables to find out how attributes are stored.
.proc attribute_table_load
    lda #$23
    sta PPU_ADDRESS
    lda #$c0
    sta PPU_ADDRESS

    ldx #$0 ; Do header, which includes header and the top row of the screen
    ldy #$0
    forx_header:
        jsr generate_attribute_byte_header
        sta PPU_DATA
        inx
        cpx #$8
    bne forx_header

    ldy #$1
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
        cpy #$6
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

; IN x, y
; OUT a (the byte)
; x and y are not modified
; One attribute byte is made up of four metatiles. Each metatile color is two bits.
; value = (bottomright << 6) | (bottomleft << 4) | (topright << 2) | (topleft << 0)
; TODO: Optimize this
; One optimization I can do is to start at the bottom-right. This would require less bit-shifts as the bottom-right gradually gets shifted left.
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
    jsr xy_meta_tile_offset ; get screen offset for the top-left metatile
    
    ldy temp_offset
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
    pla ; restore y
    tay
    pla ; restore x
    tax
    lda zp_temp_2

    rts
.endproc

; Top two tile rows are the header, next two rows are in-game
.proc generate_attribute_byte_header
    txa
    pha ; store x
    tya
    pha ; store y

    txa; double x and y to find screen coords
    asl
    sta temp_x
    lda #$0
    sta temp_y
    jsr xy_meta_tile_offset ; get screen offset for the top-left metatile

    lda #HEADER_PALETTE ; top-left
    sta zp_temp_2

    lda #HEADER_PALETTE
    asl
    asl
    ora zp_temp_2 ; top-right
    sta zp_temp_2

    ldy temp_offset

    ldx screen, y ; top-left
    lda color, x
    asl
    asl
    asl
    asl
    ora zp_temp_2
    sta zp_temp_2

    iny
    ldx screen, y ; top-right of screen goes into bottom-right of attribute byte
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
    pla ; restore y
    tay
    pla ; restore x
    tax
    lda zp_temp_2

    rts
.endproc

.proc clear_attribute_table
    lda #$23
    sta PPU_ADDRESS
    lda #$c0
    sta PPU_ADDRESS

    ; clear the attribute table
    ldy #$0
    lda #$0
    fory:
        sta PPU_DATA
        iny
        cpy #$40 ; 64
        bne fory
    rts
.endproc

; tile_map_to_screen:
;     ldx #$0
;     ldy #$0
;     lda #<screen
;     sta current_low_2
;     lda #>screen
;     sta current_high_2

;     ; This transfers from ppu memory to screen memory
;     ; x and y are flip flopped because indirect indexing forces us to use y :(
;     fory:
;     ldy #$0
;     forx:
;     ; loop:
;     ; Transfer the current memory location to the PPU
;     lda (current_low), y
;     sta (current_low_2), y ; CPU screen memory
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

copy_current_low_to_2:
    @loop:
        dey
        lda (current_low), y
        sta (current_low_2), y
        cpy #$00 ;0
    bne @loop
rts

; board_to_screen_space:
;     ldy #$0
;     @loop:
;         lda (current_low), y
;         sta (current_low_2), y
;         iny
;         cpy #$f0 ;240
;     bne @loop
; rts

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
            jsr tile_convert
            sta PPU_DATA ; PPU memory
            lda top_right, x
            jsr tile_convert
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
            jsr tile_convert
            sta PPU_DATA ; PPU memory
            lda bottom_right, x
            jsr tile_convert
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
;             ;sta (current_low_2), y ; CPU screen memory

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
    ; Restore current_x_2 after we overwrite them.
    lda current_high_2
    pha
    lda current_low_2
    pha
    jsr find_blank_tile
    ; txa ; temporarily store X on stack
    ; pha
    ; tya ; temporarily store Y on stack
    ; pha
    lda #$69
    sta temp_a
    tya
    ; clc
    ; adc #$02 ; align with screen
    sta temp_y
    txa
    sta temp_x
    jsr screen_space_to_ppu_space
    jsr ppu_update_tile_temp ; Place in PPU queue before we start destroying the y register
    
    ; pla ; Pull Y off the stack
    ; tay
    ; pla ; Pull X off the stack
    ; tax

    ;jsr tile_to_screen_space_xy
    jsr xy_meta_tile_offset
    lda #<screen
    sta current_low_2
    lda #>screen
    sty current_high_2
    ldy temp_offset

    lda #$69
    sta (current_low_2), y
    pla ; pull low byte from stack
    sta current_low_2
    pla ; pull high byte from stack
    sta current_high_2
    ;ldy #$0
    ;sta (current_low_2), y
    
    rts

; Find a blank tile
; IN
; None
; OUT
; current_x_2 points to the tile in screen space
; x, y in their registers
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
    bpl find_y ; Find another y if > 30
    ; cmp #$00 ; Title area? Find a new y
    ; beq find_y
    ; cmp #$01 ; Title area
    ; beq find_y
    ;sta zp_temp_2 ; store y
    ;pha ; Store y onto stack
    sta temp_y
    ;tay
    ;jsr tile_to_screen_space_xy
    jsr xy_meta_tile_offset
    lda #<screen
    sta current_low_2
    lda #>screen
    sta current_high_2
    ;stx current_high_2
    ;sty current_low_2
    ; Check if the tile is free
    ldy temp_offset
    lda (current_low_2), Y
    ; TODO Use tile types instead of tile id's
    cmp #$58
    beq fail
    cmp #$69
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
    sta current_low
    lda #>level_complete_message
    sta current_high
    ;jsr ppu_off
    jsr print_text
    ;jsr ppu_on
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
    loop:
        lda (current_low), y
        beq done ; 0 = null character, end
        cmp #$01
        bne skip_new_line
            jsr ppu_update
            iny ; don't print the new line character
            ; Do carriage return
            inc temp_y
            lda zp_temp_3
            sta temp_x
            lda (current_low), y
        skip_new_line:
        sta temp_a
        jsr ppu_update_tile_temp
        iny
        inc temp_x
        jmp loop
    done:
rts