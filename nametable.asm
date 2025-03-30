.include "constants.inc"

.importzp nametable_ptr

.export copy_nametable

; -----------------------
; copy_nametable routine
; IN
; nametable_ptr: points to the nametable data in RAM (960x64 bytes)
; A: High byte of PPU address (usually $20, $20, $28, or $2C for the 4 nametables)
; -------------------

copy_nametable:
    ; store PPU destination address (high byte from A)
    pha

    ; Wait for VBLANK to safely access PPU
    wait_vblank:
        bit PPU_STATUS
        bpl wait_vblank
    
    ; Disable rendering
    lda #$00
    sta PPU_MASK

    ; Set PPU address to start of nametable (passed in A)
    pla                 ; Retrieve high byte of PPU address
    sta PPU_ADDRESS        ; Set high byte ($20, $24, etc.)
    lda #$00            ; Low byte = $00
    sta PPU_ADDRESS
    
    ; Copy the 960 bytes of tile data
    ldx #0              ; X will be our high byte counter (0-3)
    ldy #0              ; Y will be our low byte counter (0-255)
    
    ; Main copy loop for the 960 bytes of tile data
TileDataLoop:
    lda (nametable_ptr), y   ; Load tile data
    sta PPU_DATA             ; Write to PPU
    iny                      ; Next byte
    bne :+                   ; If Y didn't wrap (not 0), continue
    inc nametable_ptr+1      ; Increment high byte of pointer
    inx                      ; Increment high byte counter
:   
    ; Check if we've copied all 960 bytes (3*256 + 192)
    cpx #3                   ; Have we completed 3 full pages?
    bcc TileDataLoop         ; If X < 3, continue loop
    cpy #192                 ; Have we copied the extra 192 bytes?
    bcc TileDataLoop         ; If Y < 192, continue loop
    
    ; Now copy the 64 bytes of attribute data
    ; PPU address is automatically continuing from where tile data ended
    
    ; Reset Y for attribute data loop
    ldy #0
AttrDataLoop:
    lda (nametable_ptr), y   ; Load attribute data
    sta PPU_DATA             ; Write to PPU
    iny                      ; Next byte
    cpy #64                  ; Have we copied all 64 bytes?
    bcc AttrDataLoop         ; If Y < 64, continue loop
    
    ; Reset scrolling
    lda PPU_STATUS           ; Reset PPU address latch
    lda #$00
    sta PPU_ADDRESS
    sta PPU_ADDRESS
    sta $2005                ; Reset horizontal scroll
    sta $2005                ; Reset vertical scroll
    
    rts