# Program Structure

A Beans program is made up of a series of procedure, type, and variable declarations.  Currently global variables do not work, and all type declarations except for type bindings are broken.

A procedure is declared with the "proc" keyword, followed by a name and a list of arguments is parenthesis.  Finally there is an arrow, the return type and a list of statements, terminated by the "end" keyword.

## Function Example:

```c
proc myFun(arg : bool) -> void
    // Statements go here
end
```

The return type can also be inferred as void if it is omitted.

A statement can be one of many forms, so instead of naming them all, I will just give examples.

## Statement Examples

Statements can be terminated with a semicolon, or with a newline which can only be placed at the end of statements.  If a newline is placed in the middle of an expression or other grammatic structure, the parser will signal an error.

Declaration:
```c
myVar : s32 
```


This declares a signed, 32 bit integer variable called myVar.

Declaration and initialization:
```c
myVar : s32 = 4
```

This declares a signed, 32 bit integer variable called myVar, initialzed to 3.

Type inferred declaration
```c
myVar := otherVar
```

This declares a variable myVar and initizalizes it with an other variable.  The type given is the type of otherVar.

(NOTE: The type of a variable cannot be inferred if the expression given is only integer literals.  This ensure that programmers consider what integer size they want)

These three declarations all declare immutable variables.  If you wish to have a variable be mutable, prefix any of them with the ``mut``keyword.

```c
myVar = 15
```

This assigns the value 15 to a variable myVar.

```c
return 3
```

This returns the value 3 from the function.

## Builtin Types

There are 8, 16, 32, and 64 bit integer types of both the signed and unsigned variety. These types are called s8, s16, s32, s64, u8, u16, u32, and u64.
There is also a builtin type ``bool`` which only holds the value ``true`` and ``false``.  There is also a builtin type ``char`` which can hold any unicode type.
Finally, the type ``void`` can be used in function return types to signal no return value.

## Expressions 

Integer literals:

```c
3
```

Character literals:

```c
'A'
```
```c
'\n'
```

Binary operators:

```c
3 + 4
```

```c
3 == 4 and 4 != 2
```

Record literal:

```c
TypeName { .field1 = value1, .field2 = value2 }
```

Function calls:

```c
functionName(var1, 3, 4 + 3)
```

## If Statements

```c
if x == 3 do
    doThing()
else
    doOtherThing()
end
```

## While Statements

```
while x == 3 do
    doThing()
end
```

## References

Beans allows non-nullable pointers known as "references".  You can take a reference to a variable by putting ``&`` or ``&mut`` before the variables name.  You may only take a mutable reference to a variable if the variable itself is mutable.

Examples:
``
myVar := 'a'

constPtr := &myVar

mutPtr := &mut myVar
``

You can fetch the value that is referenced with both a mutable and immutable variable, using ``@``.  You can assign to a mutable reference by prefixing the assignment with ``@``.

Examples:

``
myVar := 'a'

myPtr := &mut myVar

putchar(@myPtr)

@myPtr = 'b'
``

## External Declarations

Beans is fully C ABI compatible, and it is already able to link with libc.
To let the Beans compiler know of external symbols, you can use an external declaration.


```c
extern putchar (s32) -> s32
```

This declares a external function ``putchar`` which takes an s32 as a parameter and returns an s32.
