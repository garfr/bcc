//===-------------- pp.h - Pretty printing -----------------===//
//
// Part of BCC, which is MIT licensed
// See https//opensource.org/licenses/MIT
//
//===----------------------------- About ---------------------------------===//
//
// Contains functions for pretty printing the public types, decluttering a lot
// of source files.
//
//===---------------------------------------------------------------------===//

#include <lexer.h>
#include <parser.h>
#include <stdio.h>
#include <stdlib.h>
#include <tac.h>

void printToken(Token tok) {
    switch (tok.type) {
        case TOK_LET:
            printf("TOK_LET");
            break;
        case TOK_BOOL:
            printf("TOK_BOOL");
            break;
        case TOK_RETURN:
            printf("TOK_RETURN");
            break;
        case TOK_PROC:
            printf("TOK_PROC");
            break;
        case TOK_FALSE:
            printf("TOK_FALSE");
            break;
        case TOK_TRUE:
            printf("TOK_TRUE");
            break;
        case TOK_TYPE:
            printf("TOK_TYPE");
            break;
        case TOK_END:
            printf("TOK_END");
            break;
        case TOK_MUT:
            printf("TOK_MUT");
            break;
        case TOK_VOID:
            printf("TOK_VOID");
            break;
        case TOK_RECORD:
            printf("TOK_RECORD");
            break;
        case TOK_SYM:
            printf("TOK_SYM: '%.*s'", (int)tok.sym.len, tok.sym.text);
            break;
        case TOK_INT:
            printf("TOK_INT: '%.*s'", (int)tok.intnum.len, tok.intnum.text);
            break;
        case TOK_COLON:
            printf("TOK_COLON");
            break;
        case TOK_COLONEQUAL:
            printf("TOK_COLONEQUAL");
            break;
        case TOK_SEMICOLON:
            printf("TOK_SEMICOLON");
            break;
        case TOK_PERIOD:
            printf("TOK_PERIOD");
            break;
        case TOK_DOUBLECOLON:
            printf("TOK_DOUBLECOLON");
            break;
        case TOK_LPAREN:
            printf("TOK_LPAREN");
            break;
        case TOK_RPAREN:
            printf("TOK_RPAREN");
            break;
        case TOK_LBRACKET:
            printf("TOK_LBRACKET");
            break;
        case TOK_RBRACKET:
            printf("TOK_RBRACKET");
            break;
        case TOK_COMMA:
            printf("TOK_COMMA");
            break;
        case TOK_ARROW:
            printf("TOK_ARROW");
            break;
        case TOK_PLUS:
            printf("TOK_PLUS");
            break;
        case TOK_MINUS:
            printf("TOK_MINUS");
            break;
        case TOK_STAR:
            printf("TOK_STAR");
            break;
        case TOK_SLASH:
            printf("TOK_SLASH");
            break;
        case TOK_EQUAL:
            printf("TOK_EQUAL");
            break;
        case TOK_NEWLINE:
            printf("TOK_NEWLINE");
            break;
        case TOK_EOF:
            printf("TOK_EOF");
            break;
    }
    printf(" %zd-%zd\n", tok.start, tok.end);
}

void printType(Type *type) {
    switch (type->type) {
        case TYP_SINT:
            printf("'s%ld'", type->intsize * 8);
            break;
        case TYP_UINT:
            printf("'u%ld'", type->intsize * 8);
            break;
        case TYP_VOID:
            printf("void");
            break;
        case TYP_INTLIT:
            printf("TYP_INTLIT");
            break;
        case TYP_BOOL:
            printf("bool");
            break;
        case TYP_BINDING:
            printf("%.*s", (int)type->typeEntry->id.len,
                   type->typeEntry->id.text);
            break;
        case TYP_FUN: {
            printf("(");
            size_t i;
            for (i = 0; i < type->fun.args->numItems - 1; i++) {
                printType(*((Type **)indexVector(type->fun.args, i)));
                printf(", ");
            }
            printType(*((Type **)indexVector(type->fun.args, i)));
            printf(") -> ");
            printType(type->fun.retType);
            break;
        }

        case TYP_RECORD:
            printf("record");
            break;
    }
}

void printBinopOp(int op) {
    switch (op) {
        case BINOP_ADD:
            printf("+");
            break;
        case BINOP_SUB:
            printf("-");
            break;
        case BINOP_MULT:
            printf("*");
            break;
        case BINOP_DIV:
            printf("/");
            break;
    }
}

void printExpr(Expr *exp) {
    switch (exp->type) {
        case EXP_INT:
            printf("EXP_INT: '%.*s' : ", (int)exp->intlit.len,
                   exp->intlit.text);
            break;
        case EXP_VAR:
            printf("EXP_VAR: '%.*s'", (int)exp->var->id.len, exp->var->id.text);
            break;
        case EXP_BOOL:
            printf("EXP_BOOL: %s", exp->boolean ? "true" : "false");
            break;
        case EXP_BINOP:
            printf("EXP_BINOP: (");
            printType(exp->typeExpr);
            printf(") (");
            printExpr(exp->binop.exp1);
            printf(") ");
            printBinopOp(exp->binop.op);
            printf(" (");
            printExpr(exp->binop.exp2);
            printf(")");
            break;
        case EXP_FUNCALL: {
            printf("EXP_FUNCALL: %.*s(", (int)exp->funcall.name->id.len,
                   exp->funcall.name->id.text);
            for (size_t i = 0; i < exp->funcall.arguments->numItems; i++) {
                printExpr(*((Expr **)indexVector(exp->funcall.arguments, i)));
            }
            printf(")");
            break;
        }
        case EXP_RECORDLIT: {
            printf("EXP_RECORDLIT: %.*s literal", (int)exp->reclit.type->id.len,
                   exp->reclit.type->id.text);
            break;
        }
    }

    printf(" %zd-%zd", exp->start, exp->end);
}

void printStmt(Stmt *stmt) {
    switch (stmt->type) {
        case STMT_DEC:
            printf("STMT_DEC: '%.*s' : ", (int)stmt->dec.var->id.len,
                   stmt->dec.var->id.text);
            printType(((TypedEntry *)stmt->dec.var->data)->type);
            printf("'");
            break;
        case STMT_RETURN:
            printf("STMT_RETURN ");
            if (stmt->returnExp != NULL) {
                printf(": ");
                printExpr(stmt->returnExp);
            }
            break;
        case STMT_EXPR:
            printf("STMT_EXPR: ");
            printExpr(stmt->singleExpr);
            break;
        case STMT_DEC_ASSIGN:
            printf(
                "STMT_DEC_ASSIGN: '%.*s' : ", (int)stmt->dec_assign.var->id.len,
                stmt->dec_assign.var->id.text);
            printType(stmt->dec_assign.type);
            printf(" = (");
            printExpr(stmt->dec_assign.value);
            printf(")");
            break;
        case STMT_ASSIGN:
            printf("STMT_ASSIGN: '%.*s' = (", (int)stmt->assign.var->id.len,
                   stmt->assign.var->id.text);
            printExpr(stmt->assign.value);
            printf(")");
            break;
    }

    printf(" %zd-%zd", stmt->start, stmt->end);
}

void printParams(Vector *params) {
    size_t i;
    if (params->numItems == 0) {
        return;
    }
    for (i = 0; i < params->numItems - 1; i++) {
        Param param = *((Param *)indexVector(params, i));
        printf("%.*s : ", (int)param.var->id.len, param.var->id.text);
        printType(param.type);
        printf(", ");
    }
    Param param = *((Param *)indexVector(params, i));
    printf("%.*s : ", (int)param.var->id.len, param.var->id.text);
    printType(param.type);
}

void printToplevel(Toplevel top) {
    switch (top.type) {
        case TOP_VAR:
            printf("Internal compiler error: Not global variables yet.\n");
            exit(1);
        case TOP_PROC:
            printf("%.*s (", (int)top.fn->name.len, top.fn->name.text);
            printParams(top.fn->params);
            printf(") -> ");
            printType(top.fn->retType);
            printf("\n");
            for (size_t i = 0; i < top.fn->stmts->numItems; i++) {
                printf("\t");
                printStmt(*((Stmt **)indexVector(top.fn->stmts, i)));
            }
    }
}
void printAST(AST *ast) {
    for (size_t i = 0; i < ast->decs->numItems; i++) {
        printToplevel(*((Toplevel *)indexVector(ast->decs, i)));
        printf("\n");
    }
}

void printAddr(TACAddr addr) {
    switch (addr.type) {
        case ADDR_VAR:
            printf("'%.*s'", (int)addr.var->id.len, addr.var->id.text);
            break;
        case ADDR_INTLIT:
            printf("%.*s", (int)addr.intlit.len, addr.intlit.text);
            break;
        case ADDR_BOOL:
            printf(addr.boolean ? "true" : "false");
            break;
        case ADDR_TEMP:
            printf("*t%zd", addr.temp.num);
            break;
        case ADDR_TAG:
            printf("@%.*s", (int)addr.tag->id.len, addr.tag->id.text);
            break;
        case ADDR_EMPTY:
            break;
    }
}

void printOp(TACOp op) {
    switch (op) {
        case OP_COPY:
            printf("copy");
            break;
        case OP_ADD:
            printf("add");
            break;
        case OP_SUB:
            printf("sub");
            break;
        case OP_MUL:
            printf("mul");
            break;
        case OP_DIV:
            printf("div");
            break;
        case OP_ADDPARAM:
            printf("addparam");
            break;
        case OP_GETPARAM:
            printf("getparam");
            break;
        case OP_CALL:
            printf("call");
            break;
        case OP_RETURN:
            printf("return");
            break;
    }
}

void printInst(TACInst *inst) {
    switch (inst->type) {
        case INST_TAG:
            printf("\n%.*s: ", (int)inst->sym.len, inst->sym.text);
            break;
        case INST_OP:
            printOp(inst->op.op);
            printf("(");
            if (inst->op.args[0].type != ADDR_EMPTY) {
                printAddr(inst->op.args[0]);
                printf(", ");
            }
            if (inst->op.args[1].type != ADDR_EMPTY) {
                printAddr(inst->op.args[1]);
                printf(", ");
            }
            if (inst->op.args[2].type != ADDR_EMPTY) {
                printAddr(inst->op.args[2]);
            }
            printf(")");
    }
}

void printTAC(TAC *tac) {
    for (size_t i = 0; i < tac->codes->numItems; i++) {
        printInst(*((TACInst **)indexVector(tac->codes, i)));
        printf("\n");
    }
}
