; ; write_to_sram_mmc1.asm

; .segment "HEADER"

; INES_MAPPER = 1 ; 0 = NROM, 1 = MMC 1
; INES_MIRROR = 1 ; 0 = horizontal mirroring, 1 = vertical mirroring
; INES_SRAM   = 1 ; 1 = battery backed SRAM at $6000-7FFF

; .byte 'N', 'E', 'S', $1A ; ID
; .byte $02 ; 16k PRG chunk count
; .byte $01 ; 8k CHR chunk count
; .byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
; .byte (INES_MAPPER & %11110000)
; .byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding

;     ; .segment "HEADER"
;     ; .byte 'N', 'E', 'S', $1A  ; NES header magic number
;     ; .byte 1                   ; Number of 16KB PRG ROM banks
;     ; .byte 1                   ; Number of 8KB CHR ROM banks
;     ; .byte $C0                 ; Mapper 1, no battery, no trainer, vertical mirroring
;     ; .byte $00                 ; Mapper 1, no VS/Playchoice, not NES 2.0
;     ; .byte 0, 0, 0, 0, 0, 0, 0 ; Padding bytes

;     .segment "CODE"

;     ; Define memory locations for MMC1 registers
;     MMC1_CTRL    = $8000  ; MMC1 Control Register
;     MMC1_DATA    = $8001  ; MMC1 Data Register
;     ; Define bankswitching values for MMC1
;     BANK_SWITCH_LOW   = $80  ; Low 8KB PRG-ROM bank
;     BANK_SWITCH_HIGH  = $C0  ; High 8KB PRG-ROM bank

;     .org $8000
;     RESET:
;     SEI                 ; Disable interrupts
;     CLD                 ; Clear decimal mode

;     LDX #$40            ; Initialize stack pointer
;     TXS

;     ; Initialize MMC1
;     lda #BANK_SWITCH_LOW
;     sta MMC1_CTRL  ; Select low 8KB PRG-ROM bank
;     lda #$00
;     sta MMC1_DATA  ; Write to MMC1 Data Register to set initial state

;     ; Initialize MMC1
;     ;LDA #$80
;     ;STA $8000           ; Write to MMC1 control register to set PRG ROM mode

;     ; Set up memory mapper to enable SRAM
;     ;LDA #$80            ; Enable SRAM at $6000-$7FFF
;     ;STA $A001

;     ; Write data to SRAM
;     LDA #$42            ; Data to write
;     STA $6000           ; Write to SRAM address $6000

;     ; Infinite loop to end the program
;     forever:
;     JMP forever

;     ; .org $FFFA
;     ; .dw NMI             ; NMI vector
;     ; .dw RESET           ; Reset vector
;     ; .dw IRQBRK          ; IRQ/BRK vector


;     ;.org $FFFC
; ;RESET: .dw $8000        ; Reset vector address

; NMI:
;     RTI                 ; Return from NMI

; IRQBRK:
;     RTI                 ; Return from IRQ/BRK

; .segment "VECTORS"
; .addr NMI, RESET, 0