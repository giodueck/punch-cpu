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

## Macros
macro_start: @macro ident literal
macro_end:   @endm
macro_call:  ident arg_list
macro_content: instruction
             | macro_call
(macro_content in a list)

Store as a list of instructions and argument count.

Store instructions with their destination, op1 and op2 arguments, and optional argument indices, which if not null override that argument with the argument passed to the macro call.

Parse macro definitions by parsing instructions and storing in this format into an arraylist. Resolve macro calls by saving the instructions called in the nested macro. This way, every macro is just a list of parameterized instructions and not a possibly nested structure.

Replace macro calls with the list of instructions they store, replacing the parameters given. Determine at this point whether the instructions use an immediate format, if op2 is parameterized.

## Progress
- [ ] First pass
    - [x] `@const ident literal`
    - [x] `@var ident literal`
    - [x] `@array ident literal '[' list_of_literals ']'`
    - [x] `.label ident`
    - [x] `@macro ident literal`
    - [x] (in macro) `instruction macro_arglist`
    - [ ] (in macro) `ident macro_arglist` (skipped for now, is it necessary?)
    - [x] `@endm`
    - [x] Const resolution in first pass, after they have been defined, even inside directives
- [x] Second pass
    - [x] instructions: parse arguments, replace idents with values as immediate arguments
    - [x] branch: calculate offset if immediate
    - [x] replace macros
- [x] Third pass
    - [x] machine code gen
- [x] Fourth pass
    - [x] BP gen
        - [x] Program
        - [x] Data

Additional:
- Second class instructions: implement translations in the assembler
    This enables using flags and condition codes, which is not supported for macros. E.g. `movseq`
- Save program output into files
- PPU
