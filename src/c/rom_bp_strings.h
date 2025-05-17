// Taken from https://github.com/giodueck/FC-tools, irrelevant code deleted
#ifndef ROM_BP_STRINGS_H
#define ROM_BP_STRINGS_H

#include <stdint.h>

extern const int rom_12_capacity;
extern const char *rom_12_name;
extern const char *rom_12_desc;

typedef int (*placeholder_func_t)(char *dest, int i);
typedef int64_t (*placeholder_index_func_t)(int placeholder);

// Returns the index of the placeholder if valid, or -1 of not
int64_t is_rom_12_placeholder(int placeholder);

// Returns the index of the placeholder if valid, or -1 of not
int64_t is_data_rom_12_placeholder(int placeholder);

#endif // ROM_BP_STRINGS_H
