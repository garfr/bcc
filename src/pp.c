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
#include <tac.h>

void printToken(Token tok) {
    switch (tok.type) {
        case TOK_LET:
            printf("TOK_LET");
            break;
        case TOK_PROC:
            printf("TOK_PROC");
            break;
        case TOK_MUT:
            printf("TOK_MUT");
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
        case TOK_SEMICOLON:
            printf("TOK_SEMICOLON");
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
        case TYP_INTLIT:
            printf("TYP_INTLIT");
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
        case EXP_BINOP:
            printf("EXPR_BINOP: (");
            printType(exp->typeExpr);
            printf(") (");
            printExpr(exp->binop.exp1);
            printf(") ");
            printBinopOp(exp->binop.op);
            printf(" (");
            printExpr(exp->binop.exp2);
            printf(")");
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

void printAST(AST *ast) {
    for (size_t i = 0; i < ast->stmts->numItems; i++) {
        printStmt(*((Stmt **)indexVector(ast->stmts, i)));
        printf("\n");
    }
}

void printAddr(TACAddr addr) {
    switch (addr.type) {
        case ADDR_VAR:
            printf("'%.*s'", (int)addr.var->id.len, addr.var->id.text);
            break;
        case ADDR_INTLIT:
            printf("'%.*s'", (int)addr.intlit.len, addr.intlit.text);
            break;
        case ADDR_TEMP:
            printf("(temp: %zd)", addr.temp.num);
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
    }
}

void printInst(TACInst *inst) {
    printOp(inst->op);
    printf(": [");
    printAddr(inst->args[0]);
    printf(", ");
    printAddr(inst->args[1]);
    printf(", ");
    printAddr(inst->args[2]);
    printf("]");
}

void printTAC(TAC *tac) {
    for (size_t i = 0; i < tac->codes->numItems; i++) {
        printInst(*((TACInst **)indexVector(tac->codes, i)));
        printf("\n");
    }
}
