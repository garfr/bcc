# BCC Todo List

BCC will develop through incremental expansion, where each new feature will add a single but complete feature.
The feature should have no glaring limitations within its scope, and should compile examples correctly.

## Todo

* Make hashtbl typesafe
* Unbreak error messages 
* User defined types (this might be a bit weird and will probably require a design doc)

## In Progress 

* Functions

## Done

* Basic structure of the compiler
* Variable declarations and simple integer types
* Single value assignment from both variables and integer literals
* Type inference
* Math
* Mutability control

## Source File Status

| Filename  | Status                                                      |
|-----------|-------------------------------------------------------------|
| error.c   | Decent comments, code needs improvement                     |
| utils.c   | Decent comments, well organized                             |
| lexer.c   | Well organized, comments where needed                       |
| parser.c  | Some comments, difficult to read                            |
| tac.c     | Very basic, with minimal comments (very few are needed)     |
| codegen.c | Besides being completely broken, reasonably well organized! |

