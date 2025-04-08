# Bugs to Fix:
[✅] Semantic analyzer not reporting an undefined symbol error when calling a non-existing function.
[] Fix the instruction order after parameter parsing

# Features to implement:
[✅] Allocate registers from x0-x7 only for function parameters.
[] Constant Folding Optimization Pass
[✅] Semantic Analysis of function declaration and return statement
[] calculate space for register spilling
[✅] Loop statements
[] Support `import` statements
[] Introduce `mut` keyword

# Improvements to make:
[✅] Remove double insertion of function parameters in local params list and arguments list
[] Remove every IR type expect IRInstr (IMPORTANT)