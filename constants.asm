OAM_ADDRESS     = $2003
OAM_DMA         = $4014
PPU_ADDRESS     = $2006
PPU_DATA        = $2007
PPU_SCROLL      = $2005
;sprite_data     = $0200
OAM = $0200
NT0             = $2000 ; Nametable Zero
SNAKE           = $0300
SNAKE2          = $0400
START_X         = $0500
START_Y         = $0502
;COLOR_BLACK     = #$0f
;COLOR_WHITE     = #$20
LEVEL_CHANGE    = $01

GRASS = 0
MOUNTAIN = 1
FOOD = 2
HEADER = 3
BODY_HOR_SHAPE = 4
BODY_VERT_SHAPE = 5
; same as left down
BODY_UP_RIGHT_SHAPE = 6
BODY_UP_LEFT_SHAPE = 7
; same as down_left
BODY_RIGHT_UP_SHAPE = 8
BODY_RIGHT_DOWN_SHAPE = 9
; same as left up
BODY_DOWN_RIGHT_SHAPE = 10

UP = 0
DOWN = 1
LEFT = 2
RIGHT = 3

BUTTON_A      = 1 << 7
BUTTON_B      = 1 << 6
BUTTON_SELECT = 1 << 5
BUTTON_START  = 1 << 4
BUTTON_UP     = 1 << 3
BUTTON_DOWN   = 1 << 2
BUTTON_LEFT   = 1 << 1
BUTTON_RIGHT  = 1 << 0

; Define memory locations for MMC1 registers
MMC1_CTRL    = $8000  ; MMC1 Control Register
MMC1_DATA    = $8001  ; MMC1 Data Register
; Define bankswitching values for MMC1
BANK_SWITCH_LOW   = $80  ; Low 8KB PRG-ROM bank
BANK_SWITCH_HIGH  = $C0  ; High 8KB PRG-ROM bank
HEADER_PALETTE    = $01