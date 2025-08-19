# KagC Compiler

This folder contains the core compiler implementation for **Kagati** language. The compiler is written in Rust and organized into several modules, each handling a specific stage of the compilation process.

## Modules

- **kagc_ast** – Defines the Abstract Syntax Tree (AST) structures for representing parsed code.  
- **kagc_comp_unit** – Handles compilation units, managing multiple source files and their interactions.  
- **kagc_parser** – Parses token streams into AST nodes.  
- **kagc_lexer** – Converts raw source code into a stream of tokens.  
- **kagc_const** – Stores constants used throughout the compiler.  
- **kagc_ctx** – Maintains the compiler context, including state shared across compilation stages.  
- **kagc_errors** – Manages error reporting and diagnostic messages.  
- **kagc_ir** – Represents intermediate representations of the code for analysis and transformation.  
- **kagc_lowering** – Converts high-level AST constructs into lower-level IR suitable for code generation.  
- **kagc_scope** – Handles scoping rules and symbol visibility.  
- **kagc_sema** – Performs semantic analysis, including type checking and validation.  
- **kagc_span** – Tracks source code spans for diagnostics and error reporting.  
- **kagc_symbol** – Manages symbols, names, and identifiers within the compiler.  
- **kagc_target** – Handles target-specific details, such as register allocation and calling conventions.  
- **kagc_token** – Defines token types and related utilities.  
- **kagc_types** – Defines type representations and type system utilities.  
- **kagc_utils** – General utilities and helper functions used across the compiler.