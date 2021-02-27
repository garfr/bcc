# BCC Todo List

BCC will develop through incremental expansion, where each new feature will add a single but complete feature.
The feature should have no glaring limitations within its scope, and should compile examples correctly.

## Todo

* Arithmetic operations
* Make hashtbl typesafe
* Unbreak error messages 
* Functions
* User defined types (this might be a bit weird and will probably require a design doc)

## In Progress 

* Cleanup and commenting, reduction of complexity
* Type inference

## Done

* Basic structure of the compiler
* Variable declarations and simple integer types
* Single value assignment from both variables and integer literals

## Source File Status

| Filename  | Status                                               |
|-----------|------------------------------------------------------|
| error.c   | Decent comments, code needs improvement              |
| utils.c   | Decent comments, well organized                      |
| lexer.c   | Well organized, comments where needed                |
| parser.c  | Extremely messy, no comments, bad internal structure |
| tac.c     | Extremely simple, no comments                        |
| codegen.c | Worst file in the project                            |

