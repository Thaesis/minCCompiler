%{
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<../src/tree.h>
#include<../src/strtab.h>

#define DEBUG

#ifndef DEBUG
    #define DEBUG_PRINT(msg)
#else
    #define DEBUG_PRINT(msg) printf("%s", msg)
#endif

extern int yylineno;
/* nodeTypes refer to different types of internal and external nodes that can be part of the abstract syntax tree.*/
enum nodeTypes {PROGRAM, DECLLIST, DECL, ARRAYDECL, VARDECL,
                FUNCTYPENAME, FUNCDECL, FORMALDECLLIST, FORMALDECL,
                FUNCBODY, LOCALDECLLIST, STMTLIST, STMT, COMPSTMT, ASSIGNSTMT, 
                CONDSTMT, ELSESTMT, LOOPSTMT, RETURNSTMT, VAR, EXPR,
                RELOP, ADDEXPR, ADDOP, TERM, MULOP, FACTOR,
                FUNCCALL, ARGLIST, IDENTIFIER, INTEGER, CHARACTER,
                TYPESPEC};

enum opType {ADD, SUB, MUL, DIV, LT, LTE, EQ, GTE, GT, NEQ};

/* NOTE: mC has two kinds of scopes for variables : local and global. Variables declared outside any
function are considered globals, whereas variables (and parameters) declared inside a function foo are local to foo. You should update the scope variable whenever you are inside a production that matches function definition (funDecl production). The rationale is that you are entering that function, so all variables, arrays, and other functions should be within this scope. You should pass this variable whenever you are calling the ST_insert or ST_lookup functions. This variable should be updated to scope = "" to indicate global scope whenever funDecl finishes. Treat these hints as helpful directions only. You may implement all of the functions as you like and not adhere to my instructions. As long as the directory structure is correct and the file names are correct, we are okay with it. */
char* scope = "";

%}

/* the union describes the fields available in the yylval variable */
%union
{
    int value;
    struct TreeNode* node;
    char charval;
    char* strval;
}

/*Add token declarations below. The type <value> indicates that the associated token will be of a value type such as integer, float etc., and <strval> indicates that the associated token will be of string type.*/
%token <strval>     ID
%token <strval>     KWD_INT
%token <value>      INTCONST
%token <strval>     KWD_CHAR 
%token <charval>    CHARCONST
%token <strval>     KWD_VOID
%token <strval>     KWD_IF
%token <strval>     KWD_WHILE
%token <strval>     KWD_RETURN
%token <strval>     LPAREN RPAREN
%token <charval>    LCRLY_BRKT RCRLY_BRKT
%token <strval>     KWD_ELSE
%token <strval>     LSQ_BRKT RSQ_BRKT
%token <strval>     COMMA
%token <value>      OPER_ASGN
%left <value>       OPER_ADD
%left <value>       OPER_SUB 
%left <value>       OPER_MUL
%left <value>       OPER_DIV
%nonassoc <value>   OPER_LT OPER_LTE OPER_GT OPER_GTE OPER_EQ OPER_NEQ
%token <strval>     SEMICLN

/* TODO: Declate non-terminal symbols as of type node. Provided below is one example. node is defined as 'struct treenode *node' in the above union data structure. This declaration indicates to parser that these non-terminal variables will be implemented using a 'treenode *' type data structure. Hence, the circles you draw when drawing a parse tree, the following lines are telling yacc that these will eventually become circles in an AST. This is one of the connections between the AST you draw by hand and how yacc implements code to concretize that. We provide with two examples: program and declList from the grammar. Make sure to add the rest.  */

%type <node> program declList decl varDecl typeSpecifier funcTypeName funcDecl formalDeclList formalDecl funcBody localDeclList stmtList stmt stmtCompnd assignStmt condStmt elseStmt loopStmt retStmt var expr relop addExpr addOp term mulop factor funcCallExpr argList

%start program

%%
/* TODO: Your grammar and semantic actions go here. We provide with two example productions and their associated code for adding non-terminals to the AST.*/

program         : declList
                 {
                    Tree* progNode = makeTree(PROGRAM);
                    addChild(progNode, $1);
                    ast = progNode;
                 }
                ;

declList        : decl
                 {
                    Tree* declListNode = makeTree(DECLLIST);
                    addChild(declListNode, $1);
                    $$ = declListNode;
                 }
                | declList decl
                 {
                    Tree* declListNode = makeTree(DECLLIST);
                    addChild(declListNode, $1);
                    addChild(declListNode, $2);
                    $$ = declListNode;
                 }
                ;

decl            : varDecl
                {
                    Tree* declNode = makeTree(DECL);
                    addChild(declNode, $1);
                    $$ = declNode;
                }
                | funcDecl
                {
                    Tree* declNode = makeTree(DECL);
                    addChild(declNode, $1);
                    $$ = declNode;
                }
                ;

varDecl         : typeSpecifier ID LSQ_BRKT INTCONST RSQ_BRKT SEMICLN
                {
                    Tree* varDeclNode = makeTree(ARRAYDECL);
                    addChild(varDeclNode, $1);
                    int index = ST_insert($2, scope, $1->data, ARRAY);
                    addChild(varDeclNode, makeTreeWithVal(IDENTIFIER, index));
                    addChild(varDeclNode, makeTreeWithVal(INTEGER, (int)$4));
                    $$ = varDeclNode;
                }
                | typeSpecifier ID SEMICLN
                {
                    Tree* varDeclNode = makeTree(VARDECL);
                    addChild(varDeclNode, $1);
                    int index = ST_insert($2, scope, $1->data, SCALAR);
                    addChild(varDeclNode, makeTreeWithVal(IDENTIFIER, index));
                    $$ = varDeclNode;
                }
                ;

typeSpecifier   : KWD_INT
                {   
                    $$ = makeTreeWithVal(TYPESPEC, INT);
                }
                | KWD_CHAR
                {
                    $$ = makeTreeWithVal(TYPESPEC, CHAR);
                }
                | KWD_VOID
                {  
                    $$ = makeTreeWithVal(TYPESPEC, VOID);
                }
                ;

funcDecl        : funcTypeName LPAREN formalDeclList RPAREN funcBody
                {
                   Tree* funcDeclNode = makeTree(FUNCDECL);
                   addChild(funcDeclNode, $1);
                   addChild(funcDeclNode, $3);
                   addChild(funcDeclNode, $5);
                   $$ = funcDeclNode;
                }
                | funcTypeName LPAREN RPAREN funcBody
                {
                    Tree* funcDeclNode = makeTree(FUNCDECL);
                    addChild(funcDeclNode, $1);
                    addChild(funcDeclNode, $4);
                    $$ = funcDeclNode;
                }
                ;

funcTypeName    : typeSpecifier ID
                {
                    Tree* funcTypeNameNode = makeTree(FUNCTYPENAME);
                    addChild(funcTypeNameNode, $1);
                    int index = ST_insert($2, scope, $1->data, FUNCTION);
                    addChild(funcTypeNameNode, makeTreeWithVal(IDENTIFIER, index));
                    $$ = funcTypeNameNode;
                }
                ;     

formalDeclList  : formalDecl 
                {
                    Tree* formalDeclListNode = makeTree(FORMALDECLLIST);
                    addChild(formalDeclListNode, $1);
                    $$ = formalDeclListNode;
                }
                | formalDecl COMMA formalDeclList
                {
                    Tree* formalDeclListNode = makeTree(FORMALDECLLIST);
                    addChild(formalDeclListNode, $1);
                    addChild(formalDeclListNode, $3);
                    $$ = formalDeclListNode;
                }
                ;

formalDecl      : typeSpecifier ID
                {   
                    Tree* formalDeclNode = makeTree(FORMALDECL);
                    addChild(formalDeclNode, $1);
                    int index = ST_insert($2, scope, $1->data, SCALAR);
                    Tree* idNode = makeTreeWithVal(IDENTIFIER, index);
                    addChild(formalDeclNode, idNode);
                    $$ = formalDeclNode;
                }
                | typeSpecifier ID LSQ_BRKT RSQ_BRKT
                {
                    Tree* formalDeclNode = makeTree(FORMALDECL);
                    addChild(formalDeclNode, $1);
                    int index = ST_insert($2, scope, $1->data, SCALAR);
                    addChild(formalDeclNode, makeTreeWithVal(IDENTIFIER, index));
                    $$ = formalDeclNode;
                }
                ;

funcBody        : LCRLY_BRKT localDeclList stmtList RCRLY_BRKT
                {   
                    Tree* funcBodyNode = makeTree(FUNCBODY);

                    if($2 != NULL) {
                        addChild(funcBodyNode, $2);
                    }

                    if($3 != NULL) {
                    addChild(funcBodyNode, $3);
                    }

                    $$ = funcBodyNode;
                }
                ;

localDeclList   :
                {
                    $$ = NULL;
                }
                | varDecl localDeclList
                {
                    Tree* localDeclListNode = makeTree(LOCALDECLLIST);
                    addChild(localDeclListNode, $1);

                    if($2 != NULL) {
                    addChild(localDeclListNode, $2);
                    }
                    
                    $$ = localDeclListNode;
                }
                ;

stmtList        :
                {
                    $$ = NULL;
                }
                | stmt stmtList
                {
                    Tree* stmtListNode = makeTree(STMTLIST);
                    addChild(stmtListNode, $1);

                    if($2 != NULL) {
                    addChild(stmtListNode, $2);
                    }

                    $$ = stmtListNode;
                }
                ;

stmt            : stmtCompnd
                {
                    Tree* stmtNode = makeTree(STMT);
                    addChild(stmtNode, $1);
                    $$ = stmtNode;
                }
                | assignStmt
                {
                    Tree* stmtNode = makeTree(STMT);
                    addChild(stmtNode, $1);
                    $$ = stmtNode;
                }
                | condStmt
                {
                    Tree* stmtNode = makeTree(STMT);
                    addChild(stmtNode, $1);
                    $$ = stmtNode;
                }
                | loopStmt
                {
                    Tree* stmtNode = makeTree(STMT);
                    addChild(stmtNode, $1);
                    $$ = stmtNode;;
                }
                | retStmt
                {
                    Tree* stmtNode = makeTree(STMT);
                    addChild(stmtNode, $1);
                    $$ = stmtNode;
                }
                ;

stmtCompnd      : LCRLY_BRKT stmtList RCRLY_BRKT
                {
                    Tree* compoundStmtNode = makeTree(COMPSTMT);
                    addChild(compoundStmtNode, $2);
                    $$ = compoundStmtNode;
                }
                ;

assignStmt      : var OPER_ASGN expr SEMICLN
                {
                    Tree* assignStmtNode = makeTree(ASSIGNSTMT);
                    addChild(assignStmtNode, $1);
                    addChild(assignStmtNode, $3);
                    $$ = assignStmtNode;
                }
                | expr SEMICLN
                {
                    Tree* assignStmtNode = makeTree(ASSIGNSTMT);
                    addChild(assignStmtNode, $1);
                    $$ = assignStmtNode;
                }
                ;

condStmt        : KWD_IF LPAREN expr RPAREN stmt elseStmt
                {
                    Tree* condStmtNode = makeTree(CONDSTMT);
                    addChild(condStmtNode, $3);
                    addChild(condStmtNode, $5);

                    if($6 != NULL) {
                        addChild(condStmtNode, $6);
                    }
                    
                    $$ = condStmtNode;
                }
                ;

elseStmt        :
                {
                    $$ = NULL;
                }
                | KWD_ELSE stmt
                {
                    Tree* elseStmtNode = makeTree(ELSESTMT);
                    addChild(elseStmtNode, $2);
                    $$ = elseStmtNode;
                }
                ;

loopStmt        : KWD_WHILE LPAREN expr RPAREN stmt
                {
                    Tree* whileStmtNode = makeTree(LOOPSTMT);
                    addChild(whileStmtNode, $3);
                    addChild(whileStmtNode, $5);
                    $$ = whileStmtNode;
                }
                ;
                
retStmt         : KWD_RETURN SEMICLN
                {
                    Tree* retStmt = makeTree(RETURNSTMT);
                    $$ = retStmt;   
                }
                | KWD_RETURN expr SEMICLN
                {
                    Tree* retStmt = makeTree(RETURNSTMT);
                    addChild(retStmt, $2);
                    $$ = retStmt;
                }
                ;

var             : ID
                {   
                    Tree* varNode = makeTree(VAR);

                    if(ST_lookup($1, scope) == -1)
                    {
                        yyerror("var: undeclared variable.");
                        $$ = varNode;
                    }
                    else
                    {
                        int index = ST_lookup($1, scope);
                        addChild(varNode, makeTreeWithVal(IDENTIFIER, index));
                        $$ = varNode;
                    }
                }
                | ID LSQ_BRKT addExpr RSQ_BRKT
                {
                    Tree* varNode = makeTree(VAR);

                    if(ST_lookup($1, scope) == -1)
                    {
                        yyerror("var: undeclared variable.");
                        $$ = varNode;
                    }
                    else
                    {
                        int index = ST_lookup($1, scope);
                        addChild(varNode, makeTreeWithVal(IDENTIFIER, index));
                        addChild(varNode, $3);
                        $$ = varNode;
                    }
                }
                ;

expr           : addExpr
                {
                    Tree* exprNode = makeTree(EXPR);
                    addChild(exprNode, $1);
                    $$ = exprNode;
                }
                | expr relop addExpr
                {
                    Tree* exprNode = makeTree(EXPR);
                    addChild(exprNode, $1);
                    addChild(exprNode, $2);
                    addChild(exprNode, $3);
                    $$ = exprNode;
                }
                ;

relop           : OPER_LTE
                {
                    $$ = makeTreeWithVal(RELOP, LTE);
                }
                | OPER_LT
                {
                    $$ = makeTreeWithVal(RELOP, LT);
                }
                | OPER_GT
                {
                    $$ = makeTreeWithVal(RELOP, GT);
                }
                | OPER_GTE
                {
                    $$ = makeTreeWithVal(RELOP, GTE);
                }
                | OPER_EQ
                {
                    $$ = makeTreeWithVal(RELOP, EQ);
                }
                | OPER_NEQ
                {
                    $$ = makeTreeWithVal(RELOP, NEQ);
                }
                ;

addExpr         : term
                {
                    Tree* addExprNode = makeTree(ADDEXPR);
                    addChild(addExprNode, $1);
                    $$ = addExprNode;
                }
                | addExpr addOp term
                {
                    Tree* addExprNode = makeTree(ADDEXPR);
                    addChild(addExprNode, $1);
                    addChild(addExprNode, $2);
                    addChild(addExprNode, $3);
                    $$ = addExprNode;
                }
                ;

addOp           : OPER_ADD
                {
                    $$ = makeTreeWithVal(ADDOP, ADD);
                }
                | OPER_SUB
                {
                    $$ = makeTreeWithVal(ADDOP, SUB);
                }
                ;

term            : factor
                {
                    Tree* termNode = makeTree(TERM);
                    addChild(termNode, $1);
                    $$ = termNode;
                }
                | term mulop factor
                {
                    Tree* termNode = makeTree(TERM);
                    addChild(termNode, $1);
                    addChild(termNode, $2);
                    addChild(termNode, $3);
                    $$ = termNode;
                }
                ;

mulop           : OPER_MUL
                {
                    $$ = makeTreeWithVal(MULOP, MUL);
                }
                | OPER_DIV
                {
                    $$ = makeTreeWithVal(MULOP, DIV);
                }
                ;

factor          : LPAREN expr RPAREN
                {
                    Tree* factorNode = makeTree(FACTOR);
                    addChild(factorNode, $2);
                    $$ = factorNode;
                }
                | var
                {
                    Tree* factorNode = makeTree(FACTOR);
                    addChild(factorNode, $1);
                    $$ = factorNode;
                }
                | funcCallExpr
                {
                    Tree* factorNode = makeTree(FACTOR);
                    addChild(factorNode, $1);
                    $$ = factorNode;
                }
                | INTCONST
                {   
                    Tree* factorNode = makeTree(FACTOR);
                    addChild(factorNode, makeTreeWithVal(INTEGER, (int)$1));
                    $$ = factorNode;
                }
                | CHARCONST
                {
                    Tree* factorNode = makeTree(FACTOR);
                    addChild(factorNode, makeTreeWithVal(CHARACTER, (char)$1));
                    $$ = factorNode;
                }
                ;

funcCallExpr    : ID LPAREN argList RPAREN
                {
                    Tree* funcCallExpr = makeTree(FUNCCALL);
                    
                    if(ST_lookup($1, scope) == -1) 
                    {
                        yyerror("funcCall: undefined function.");
                    }
                    else
                    {
                        int index = ST_lookup($1, scope);
                        addChild(funcCallExpr, makeTreeWithVal(IDENTIFIER, index));
                        addChild(funcCallExpr, $3);
                        $$ = funcCallExpr;
                    }
                }
                | ID LPAREN RPAREN
                {
                    if(ST_lookup($1, scope) == -1)
                    {
                        yyerror("funcCall: undefined function.");
                    }
                    else
                    {
                        int index = ST_lookup($1, scope);
                        $$ = makeTreeWithVal(FUNCTION, index);
                    }
                }
                ;

argList         : expr
                {
                    Tree* argListNode = makeTree(ARGLIST);
                    addChild(argListNode, $1);
                    $$ = argListNode;
                }
                | argList COMMA expr
                {
                    Tree* argListNode = makeTree(ARGLIST);
                    addChild(argListNode, $1);
                    addChild(argListNode, $3);
                    $$ = argListNode;
                }
                ;

%%

int yywarning(char* msg){
    printf("warning: line %d: %s\n", yylineno, msg);
    return 0;
}

int yyerror(char* msg){
    printf("Error {line %d: %s}\n", yylineno, msg);
    return 0;
}

void updateScope(const char* newScope) {

    if(scope != NULL) {
        free(scope);
    }

    scope = malloc(strlen(newScope) + 1);

    if(scope != NULL) {
        strcpy(scope, newScope);
    }

}