## How it was made ##

# Tile Size #

The NES has a screen resolution of 256 pixels by 240 pixels. Tiles are 8x8. If we were to keep this tile size, the maximum possible size of the game board would be 32 pixels by 30 pixels. This is what I originally programmed the game for.

However, when play testing, I found that the detail an 8x8 tile allowed was too small for the snake. I wanted snakes with expression and a good level of detail. That required the use of a concept called Metatiles. Which are simply 
2x2 tiles. The max possible board size using Metatiles is 16x15. When looking at similar games on the market this seemed reasonable.

Another reason for using Metatiles is that palettes are shared among metatiles, not tiles. There wasn't a good way to get a good-looking snake that had to share a palette with any possible other tile in the 2x2 grid.

For an example of a Metatile, refer to the question mark block in Super Mario Brothers.

An example of where I use regular sized tiles are text.

# The Snake #

There are two ways to represent a snake on screen. The first is a sprite. Sprites are nice because they move pixel by pixel. They can also animate easier, flip and rotate, etc. They also offer us a different series of palettes than the background. The NES's graphics chip, however, has a critical issue. It can only display 8 sprites per scanline. If the snake goes over that amount it will start flickering. Bad!

The second approach is to represent the the snake as background tiles. This allows for a snake of unlimited size.

The approach I took is a mixture of both. The head and tail are sprites while the body are background tiles. This gives me the benefits of both approaches with zero sacrifices.

A max 16x15 board allows for a snake of length 240. This is perfect since it falls within the 8-bit limit of 256! This allowed for easier development since all I need now is a page of data to represent a single snake. We can simply roll the snake data over and not have to worry about a conditional check. A page of data is 256 bytes.

The snake's memory simply rotates through this page of data. Old data remains, no need to clean it up.

To move a snake we just need to adjust three bytes of data plus the head and tail pointers.

Here's an example of how the memory works. Let's say we currently have a snake like this:

0 . . . . . . 256
    T--------H
	^ (tp)   ^ (hp)
T = tail, H = head, tp = tail pointer, hp = head pointer
On the adjustment, we simply move the snake forward by one byte. It is now,
0 . . . . . . 256
    TT--------H
	 ^ (tp)   ^ (hp)
	
No need to clean the old tail. Just move the pointer. But oh no! The head is at 256! Now what? Next movement now looks like this:
0 . . . . . . 256
H   TTT--------
^(hp) ^(tp)

If we were to enumerate the array from the tail, we would simply hit the 256 barrier and it will overflow to zero. Same if we went from the Head backwards.

If the snake grows from eating food we move the head and keep the tail where it is.

This limits us to a snake size of 256 but I think it is a fair compromise. We are working on the NES and there are very limited CPU cycles to spare.

You might be wondering why we need to keep the snake data in memory at all if we're just adjusting the head and tail. Why can't we just use data in the graphics buffer? The issue there is two fold. One, we cannot access the graphics buffer while a frame is being rendered. Second, we need the tail to accurately follow the snake. The snake data we store are directions the snake went from that tile to the next. Up, Down, Left, or Right. Using these directions makes the tail following the head trivial. The directions also dictate the graphics to display on screen if and when we need to render that tile.

If space becomes critical, one possible improvement would be to represent directions as two bits instead of a byte. This would reduce the memory needed by a factor of four but would incur some extra CPU cycles. I am using (abusing?) the SRAM chip so CPU cycles take priority over memory. The SRAM chip gives us an extra 8 KB of RAM to work with. That's 10 KB total. Yes, the NES's built-in memory is only 2KB.

# Screen Buffer/Collision Detection #

On a faster system, performing collision detection by iterating through the array would be a good approach. Iterating through up to 256 bytes on the NES is a show stopper however. That would cause us to miss a frame. There is also the issue of collisions on the board to worry about.
To solve this issue I am using a copy of the screen data in memory. This is represented as a long array indexed by (y * screen_width) + x. This makes collision detection easy and quick.

Since the NES doesn't do multiplication I add or subtract screen_width from the head or tail index when going up or down. As a result I have to record three bytes for the head - x, y, and the index into the screen buffer. Likewise for the tail.

# Memory #
Recall that we have 10KB of memory. Counting every byte is a necessity. The screen copy costs us 240 bytes. Head and tail variables, 6 bytes. 256 bytes for one snake. I wanted two-player action so 512 bytes for two snakes. 256 bytes for the CPU stack - I may be able to adjust this downward but I want to wait until the game is near completion. It is common for NES games to give the stack less space but if it overflows the game will crash. The 6502 is no built-in protections for the stack. It will just overwrite whatever is there if you go over.

Frequently accessed variables are stored on the Zero Page to reduce CPU cycles. The Zero Page is the first 256 bytes of RAM and can be accessed by just one byte (say, 0xff), instead of two (say, 0xffff).

There are many other variables in use but those are the major ones.

# Space #

I am using a 40KB cart size to store the program ROM data. I forgot the size of the CHR ROM, I'll add that later.


The graphics were created in yychr.

## Compile and build ##

This game uses CC65 as its compiler, so make sure that is installed. You will need the PATH environment variable set up and point it to cc65/bin.

GNU's Make is used to build the game. To build, go to the root and run the commmand:
make

The executable will be called snake.nes. Simply run it in an NES emulator to play! Mesen is recommended if you want to debug.
I haven't tried but it should also be functional in a real NES using Everdrive.

## RAM Layout ##

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
