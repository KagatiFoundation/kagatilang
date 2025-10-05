# NOTE: Following Example is Deprecated

# How to use

## Latest achivement
My compiler can compile and run the following code correctly:
```
import "std/io";

record IOObj {
    file_name: string;
    abs_path: string;
}

def do_sth(io: IOObj) -> IOObj {
    return io;
}

def main() -> integer {
    let io = IOObj {
        file_name = "hello.txt\n",
        abs_path = "/usr/bin\n"
    };
    let file = do_sth(io);
    print(file.file_name);
}
```

Generated assembly(Aarch64) for the code above:
```asm
.text
.extern _print

.extern _eprint

.extern _print_int

.global _do_sth
_do_sth:
	SUB SP, SP, #32
	STR x0, [SP]
	LDR x8, [x0, #40]
	STR x8, [SP, 0x18]
	LDR x8, [SP]
	MOV x0, x8
	ADD SP, SP, #32
	RET

.global _main
_main:
	SUB SP, SP, #112
	STP x29, x30, [SP, #96]
	ADD x29, SP, #96
   MOV x0, 0xc
   MOV x1, 0x1
   BL _object_new
   CBNZ x0, 1f
   BL _obj_alloc_fail
1:
	STR x0, [x29]
	LDR x8, [x29]
	LDR x9, [x8, #40]
	ADRP x8, .L.str.0@PAGE
	ADD x8, x8, .L.str.0@PAGEOFF
	MOV x0, x9
	MOV x1, x8
	MOV x2, 0xc
	BL _k_memcpy
	MOV x0, 0xb
   MOV x1, 0x1
   BL _object_new
   CBNZ x0, 1f
   BL _obj_alloc_fail
1:
	STR x0, [x29, -0x8]
	LDR x8, [x29, -0x8]
	LDR x9, [x8, #40]
	ADRP x8, .L.str.1@PAGE
	ADD x8, x8, .L.str.1@PAGEOFF
	MOV x0, x9
	MOV x1, x8
	MOV x2, 0xb
	BL _k_memcpy
	MOV x0, 0x10
   MOV x1, 0x2
   BL _object_new
   CBNZ x0, 1f
   BL _obj_alloc_fail
1:
	STR x0, [x29, -0x10]
	LDR x8, [x0, #40]
	LDR x9, [x29]
	STR x9, [x8, #0]
	LDR x9, [x29, -0x8]
	STR x9, [x8, #8]
	LDR x8, [x29, -0x10]
	STR x8, [x29, -0x18]
	LDR x9, [x8, #40]
	STR x9, [x29, -0x20]
	LDR x8, [x29, -0x18]
	MOV x0, x8
	BL _do_sth
	MOV x8, x0
	STR x8, [x29, -0x28]
	LDR x9, [x8, #40]
	STR x9, [x29, -0x30]
	LDR x8, [x29, -0x30]
	LDR x9, [x8, #0]
	MOV x0, x9
	BL _print
	LDP x29, x30, [SP, #96]
	ADD SP, SP, #112
   RET

.section __TEXT,__cstring
	.L.str.0:
	.asciz "hello.txt\n"
.section __TEXT,__cstring
	.L.str.1:
	.asciz "/usr/bin\n"
.section __DATA,__const
.align 3
.L__const.4.io:
	.xword .L.str.0
	.xword .L.str.1
```

Output:
```
hello.txt
```

NOTE:
1) `std/io` is inside the `Lib` folder.
2) `_object_new`, `_k_memcpy`, and other similar functions are garbage collection functions. However, the compiler is only able to incorporate 'allocation' functions. 'Free' or 'deallocation' functions are under progress. 