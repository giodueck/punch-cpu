# Lexer

State managed in a struct, functions scoped to this struct.

Functions:
- pop: consume next token and return it (in a slice)
- peek: get next token without consuming it and return

Return a struct with additional information:
- what kind of token? directive, instruction, newline?
- in case of instruction: which, which flags and conditions?
- in case of argument: register or immediate? how many bits of immediate?

Tokens to recognize:
- newline, *or split into lines in the first place*
- *split by whitespace and trim comments*
- directive: @directive
    - const declaration
    - variable allocation
    - array allocation
- label: .label
- instruction
    - s or l flag
    - condition suffix
- argument
    - register
    - immediate: #-prefixed literals

Operators are special characters that are their own tokens and prefix other tokens or delimit groups of tokens. E.g. '.', '#' and '$' are prefix operators which condition the following token: the '.' defines a label, the '#' precedes an immediate operand and the '$' precedes a macro parameter inside the macro definition.

# Parser

Error reporting: report errors immediately, skip rest of line to check for errors in rest of code

For each line, keep track of
- if directive was used
- if label has been declared
- if and which instruction was used
- arguments used and how many
    - to decide on instruction format, together with instruction used

For macros, also keep track of macro declaration active across lines.

Constants and labels only usable as immediate arguments.

Macros usable as if they were instructions.

Several passes:
1. parse directives: constants, allocations and macros. constants and macros may be nested if the nested identifier was declared before being used. Set data memory locations for allocations.
2. parse code: replace constants, allocation address identifiers and macros, get label addresses.
3. replace labels in code.
4. machine code generation: replace istructions with binary instructions, verify formats and immediate bit-widths.
5. compile machine code into ROM blueprint.
