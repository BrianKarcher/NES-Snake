RAM Layout

Work RAM (2k)
$0000-$00FF - Zero Page
$0100-$01FF - Stack (I may shorten this as the stack may never get close to 256 bytes)
$0200-02FF  - OAM space that gets DMA'd to the PPU every frame. This stores sprite information for all 64 possible sprites.
#0300-03FF  - Snake 1 array. The purpose of this is so the tail can follow along. I may shorten this by compression (this only needs U,D,L,R - so 2 bits).
            - Each snake may also grow above 256 tiles? Maybe?
#0400-04FF  - Snake 2 array. See above.
$0500-0501  - Start x (two bytes, one for each snake). Consider removing by having board.asm call init_snake directly.
$0502-0503  - Start y (two bytes, one for each snake). ^

SRAM (8k)   - this RAM can get saved on the cartridge as save data, or used as work RAM. SNAKE uses it as work RAM. This spans from $6000-7FFF
$6000-60FF  - Nametable update buffer (I may shorten this, not much needs to be updated while the PPU is on)
$6100-64C0  - Screen copy in CPU memory. I have a copy of the screen in CPU-addressable memory because the snake(s)
            - can get quite large. It is much faster to check for collisions in x,y memory than to loop through hundreds of bytes of an array or linked list.
            - The screen copy is the only reason I use SRAM at all. Otherwise this program would only use Work RAM.
