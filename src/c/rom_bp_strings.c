// Taken from https://github.com/giodueck/FC-tools, irrelevant code deleted
#include <stdint.h>

#include "rom_bp_strings.h"

const int rom_12_capacity = 4096;
const char *rom_12_name = "64-value ROM";
const char *rom_12_desc = "64-value ROM with decimal(0xFF000000 + i) as a placeholder value for the ith word. 4096 words. For programming with outside tools.";

// Returns the index of the placeholder if valid, or -1 of not
int64_t is_rom_12_placeholder(int placeholder)
{
    if ((placeholder & 0xFF000000) == 0)
        return -1;

    if ((placeholder & 0x00FFFFFF) < rom_12_capacity)
        return placeholder & 0x00FFFFFF;
    return -1;
}
