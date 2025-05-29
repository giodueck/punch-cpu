# Picture Processing Unit
Coprocessor responsible for handling the display, which receives commands from the CPU through some CPU opcodes.

## Hardware
Registers:
- Color register: 24-bit color (RGB-888)
- Point register: 2 integers for a (x,y) (col,row) position in the screen
    (0,0) is top left, with +y going down and +x going to the right
- VRAM: 64x32 (2048) screen buffer with one 24-bit (stored in a 32-bit address) color per pixel
- VROM: 1024 words for storage of 8x8 sprites. Fits 16 of these sprites at 24 bits per pixel
    Idea: support 16bpp colors too, so we can have 32 sprites instead, or some mix of both

Screen resolution of 48x24 is possible to build without losing pixels to substations, and is only 1152 words long.

## Screen

## Instructions
Opcodes: 0x14-0x1b (20-27)

Summary:
- Set color register
- Set point register
- Draw pixel
- Draw sprite
- Draw line
- Draw box
- Fill box
- Fill columns/rows between lines (?)
- Push frame/clear screen

### Set color
Set the color register using a CPU register or a 16-bit immediate in RGB-565 format, which is then converted to RGB-888.

Formats: R, A. In R-type, use e bytes to tell if wanting to use a 16 or 24 bit color

### Set point register
Set the point registers using two operand registers or one operand and an immediate. Op1 is row (y) and op2 is column (x).

Formats: R, I.

### Draw pixel
Use color register and internal point if destination is x0. Use op1 as row (y) and op2 as column (x) if not.

Formats: all

### Draw sprite
Draw 8x8 sprite at the point in the point register. Use op2 to tell which sprite to use in the graphics ROM.

Sprites are stored flattened with the top row and left column first.

Idea: if destination is not x0, use 16bpp, which allows double the sprites to be stored.

If a 1-tick per pixel write can be achieved, completes in 64 ticks or 1.066667s

Formats: all

### Draw line
Draw line starting at point register to the point described by (y,x) = (op1,op2) using the color register as color.

Use Bresenham's line drawing algorithm. Maximum time should be 64 ticks for lines from one side to the other.

Formats: R, I
