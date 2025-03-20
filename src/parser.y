%{
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<../src/tree.h>
#include<../src/strtab.h>

extern int yylineno;
extern TableNode* currentScope;
extern TableNode* globalScope;

/* nodeTypes refer to different types of internal and external nodes that can be part of the abstract syntax tree.*/
enum nodeTypes {PROGRAM, DECLLIST, DECL, ARRAYDECL, VARDECL,
                FUNCTYPENAME, FUNCDECL, FORMALDECLLIST, FORMALDECL,
                FUNCBODY, LOCALDECLLIST, STMTLIST, STMT, COMPSTMT, ASSIGNSTMT, 
                CONDSTMT, ELSESTMT, LOOPSTMT, RETURNSTMT, VAR, EXPR,
                RELOP, ADDEXPR, ADDOP, TERM, MULOP, FACTOR,
                FUNCCALL, ARGLIST, IDENTIFIER, INTEGER, CHARACTER,
                TYPESPEC};

enum opType {ADD, SUB, MUL, DIV, LT, LTE, EQ, GTE, GT, NEQ};

/*Buffer for snprintf*/
char buffer[256];

/* NOTE: mC has two kinds of scopes for variables : local and global. Variables declared outside any
function are considered globals, whereas variables (and parameters) declared inside a function foo are local to foo. You should update the currentScope variable whenever you are inside a production that matches function definition (funDecl production). The rationale is that you are entering that function, so all variables, arrays, and other functions should be within this currentScope. You should pass this variable whenever you are calling the ST_insert or ST_lookup functions. This variable should be updated to currentScope = "" to indicate global currentScope whenever funDecl finishes. Treat these hints as helpful directions only. You may implement all of the functions as you like and not adhere to my instructions. As long as the directory structure is correct and the file names are correct, we are okay with it. */
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
%token <value>      ERROR
%token <value>      ILLEGAL_TOK

/* TODO: Declate non-terminal symbols as of type node. Provided below is one example. node is defined as 'struct treenode *node' in the above union data structure. This declaration indicates to parser that these non-terminal variables will be implemented using a 'treenode *' type data structure. Hence, the circles you draw when drawing a parse tree, the following lines are telling yacc that these will eventually become circles in an AST. This is one of the connections between the AST you draw by hand and how yacc implements code to concretize that. We provide with two examples: program and declList from the grammar. Make sure to add the rest.  */

%type <node> program declList decl varDecl typeSpecifier funcTypeName funcDecl formalDeclList formalDecl funcBody localDeclList stmtList stmt stmtCompnd assignStmt condStmt elseStmt loopStmt retStmt var expr relop addExpr addOp term mulop factor funcCallExpr argList

%start program

%%
/* TODO: Your grammar and semantic actions go here. We provide with two example productions and their associated code for adding non-terminals to the AST.*/

program         : {
                    new_scope();
                  } 
                  declList
                 {
                    Tree* progNode = makeTree(PROGRAM);
                    addChild(progNode, $2);
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
                    int testIndex = ST_lookup($2);

                    if((int)$4 <= 0) {
                        snprintf(buffer, sizeof(buffer), "Var::%s[] | Cannot be initalized with size of <= 0.", $2);
                        yyerror(buffer);
                        clearWritingBuffer(buffer);
                    }

                    if(testIndex > 1 && testIndex < 1000){
                        snprintf(buffer, sizeof(buffer), "Var::%s[] | Has multiple definitions.", $2);
                        yyerror(buffer);
                        clearWritingBuffer(buffer);
                    }

                    Tree* varDeclNode = makeTree(ARRAYDECL);
                    addChild(varDeclNode, $1);
                    int index = ST_insert($2, $1->val, ARRAY, getScopeIndex(), $4, 0);
                    addChild(varDeclNode, makeTreeWithVal(IDENTIFIER, index));
                    addChild(varDeclNode, makeTreeWithVal(INTEGER, (int)$4));
                    $$ = varDeclNode;
                }
                | typeSpecifier ID SEMICLN
                {   
                    int testIndex = ST_lookup($2);

                    if(testIndex > 1 && testIndex < 1000){
                        snprintf(buffer, sizeof(buffer), "Var::%s | Has multiple definitions.", $2);
                        yyerror(buffer);
                        clearWritingBuffer(buffer);
                    }

                    Tree* varDeclNode = makeTree(VARDECL);
                    addChild(varDeclNode, $1);
                    int index = ST_insert($2, $1->val, SCALAR, getScopeIndex(), 1, 0);
                    addChild(varDeclNode, makeTreeWithVal(IDENTIFIER, index));
                    $$ = varDeclNode;
                }
                ;

typeSpecifier   : KWD_INT
                {   
                    $$ = makeTreeWithVal(TYPESPEC, 0);
                }
                | KWD_CHAR
                {
                    $$ = makeTreeWithVal(TYPESPEC, 1);
                }
                | KWD_VOID
                {  
                    $$ = makeTreeWithVal(TYPESPEC, 2);
                }
                ;

funcDecl        : funcTypeName LPAREN formalDeclList RPAREN
                {   
                    int index = getChild($1, 1)->val;
                    connect_params(index);
                } 
                 funcBody
                {
                   Tree* funcDeclNode = makeTree(FUNCDECL);
                   addChild(funcDeclNode, $1);
                   addChild(funcDeclNode, $3);
                   addChild(funcDeclNode, $6);
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

                    int testIndex = ST_lookup($2);

                    if(testIndex > 1){
                        snprintf(buffer, sizeof(buffer), "Function::%s | Has multiple definitions.", $2);
                        yyerror(buffer);
                        clearWritingBuffer(buffer);
                    }
                    
                    int index = ST_insert($2, $1->val, FUNCTION, getScopeIndex(), 0, 0);

                    addChild(funcTypeNameNode, makeTreeWithVal(IDENTIFIER, index));
                    new_scope();
                    currentScope->parent->strTable[index]->scope = getScopeIndex();
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

formalDecl      : typeSpecifier ID LSQ_BRKT RSQ_BRKT
                {
                    Tree* formalDeclNode = makeTree(FORMALDECL);
                    addChild(formalDeclNode, $1);

                    int index = ST_insert($2, $1->val, ARRAY, getScopeIndex(), 1, 0);

                    add_param($1->val, ARRAY, $2);

                    addChild(formalDeclNode, makeTreeWithVal(IDENTIFIER, index));
                    $$ = formalDeclNode;
                }
                | typeSpecifier ID
                {   
                    Tree* formalDeclNode = makeTree(FORMALDECL);
                    addChild(formalDeclNode, $1);
                    int index = ST_insert($2, $1->val, SCALAR, getScopeIndex(), 1, 0);
                    add_param($1->val, SCALAR, $2);
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

                    up_scope();
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
                    int index = getChild($1, 0)->val;
                    int varType = $1->type;
                    int exprType = $3->type;

                    if(varType != exprType) {
                        snprintf(buffer, sizeof(buffer), "Var::%s | Assignment type mismatch.", $2);
                        yyerror(buffer);
                        clearWritingBuffer(buffer);
                    }

                    Tree* assignStmtNode = makeTreeWithTypeVal(ASSIGNSTMT, currentScope->strTable[index]->data_type, currentScope->strTable[index]->value);
                    addChild(assignStmtNode, $1);
                    addChild(assignStmtNode, $3);
                    $$ = assignStmtNode;
                }
                | expr SEMICLN
                {
                    Tree* assignStmtNode = makeTreeWithTypeVal(ASSIGNSTMT, $1->type, $1->val);
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

var             : ID LSQ_BRKT addExpr RSQ_BRKT
                {
                    
                    int index = ST_lookup($1);
                    
                    TableNode* validScope = currentScope;

                    if(index > 1000) {
                        index -= 1000;
                        validScope = currentScope->parent;
                    }

                    if(index < 0)
                    {   
                        snprintf(buffer, sizeof(buffer), "Var::%s[] | Undeclared in currentScope.", $1);
                        yyerror(buffer);
                        clearWritingBuffer(buffer);

                        Tree* varNode = makeTree(VAR);
                        $$ = varNode;
                    } 
                    else
                    {
                        
                    Tree* varNode = makeTreeWithTypeVal(VAR, validScope->strTable[index]->data_type, $3->val);
                    addChild(varNode, makeTreeWithVal(IDENTIFIER, index));

                    if(validScope->strTable[index]->symbol_type != ARRAY) {
                        snprintf(buffer, sizeof(buffer), "Var::%s[] | Array reference to non-array object.", $1);
                        yyerror(buffer);
                        clearWritingBuffer(buffer);
                        goto exit;//Early exit
                    } 

                    if($3->type != INT_TYPE) {
                        snprintf(buffer, sizeof(buffer), "Var::%s[] | Attempt to index array with non-integer type.", $1);
                        yyerror(buffer);
                        clearWritingBuffer(buffer);
                        goto exit;//Early exit
                    }

                    if($3->val > validScope->strTable[index]->size) {
                        snprintf(buffer, sizeof(buffer), "Var::%s[] | Attempt to index array with out-of-bounds integer literal.", $1);
                        yyerror(buffer);
                        clearWritingBuffer(buffer);
                        goto exit; //Early exit
                    }

                    exit:
                    addChild(varNode, $3);
                    $$ = varNode;
                    }
                }
                | ID
                {   
                    int index = ST_lookup($1);
                    TableNode* validScope = currentScope;

                    if(index > 1000) {
                        index -= 1000;
                        validScope = currentScope->parent;
                    }

                    if(index < 0)
                    {   
                        snprintf(buffer, sizeof(buffer), "Var::%s | Undeclared in currentScope.", $1);
                        yyerror(buffer);
                        clearWritingBuffer(buffer);

                        Tree* varNode = makeTree(VAR);
                        $$ = varNode;
                    }
                    else 
                    {
                        Tree* varNode = makeTreeWithTypeVal(VAR, validScope->strTable[index]->data_type, validScope->strTable[index]->value);
                        addChild(varNode, makeTreeWithVal(IDENTIFIER, index));
                        $$ = varNode;
                    }
                }
                ;

expr           : addExpr
                {   
                    Tree* exprNode = makeTreeWithTypeVal(EXPR, $1->type, $1->val);
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
                    Tree* addExprNode = makeTreeWithTypeVal(ADDEXPR, $1->type, $1->val);
                    addChild(addExprNode, $1);
                    $$ = addExprNode;
                }
                | addExpr addOp term
                {   
                    if($1->type != $3->type) {
                        snprintf(buffer, sizeof(buffer), "Expression::%s | Arithmetic type mismatch.", $1);
                        yyerror(buffer);
                        clearWritingBuffer(buffer);
                    }

                    int val = 0;
                    if($2->val == ADD) {
                        val = $1->val + $3->val;
                    } else {
                        val = $1->val - $3->val;
                    }

                    Tree* addExprNode = makeTreeWithTypeVal(ADDEXPR, $1->type, val);
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
                    Tree* termNode = makeTreeWithTypeVal(TERM, $1->type, $1->val);
                    addChild(termNode, $1);
                    $$ = termNode;
                }
                | term mulop factor
                {   
                    if($1->type != $3->type) {
                        snprintf(buffer, sizeof(buffer), "Expression::%s | Arithmetic type mismatch.", $1);
                        yyerror(buffer);
                        clearWritingBuffer(buffer);
                    }

                    int newVal = 0;
                    if($2->val == MUL) {
                        newVal = $1->val * $3->val; 
                    } else {

                        if($3->val == 0) {
                            fprintf(stderr, "Error: Division by Zero");
                            exit(EXIT_FAILURE);
                        }

                        newVal = $1->val / $3->val;
                    }

                    Tree* termNode = makeTreeWithTypeVal(TERM, $1->type, newVal);
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
                    Tree* factorNode = makeTreeWithTypeVal(FACTOR, $2->type, $2->val);
                    addChild(factorNode, $2);
                    $$ = factorNode;
                }
                | var
                {   
                    if(getChild($1, 0) != NULL) {
                        int index = getChild($1, 0)->val;

                        if(currentScope->strTable[index] == NULL) {
                            Tree* factorNode = makeTreeWithTypeVal(FACTOR, $1->type, currentScope->parent->strTable[index]->value);
                            addChild(factorNode, $1);
                            $$ = factorNode;
                        }
                        else {
                            Tree* factorNode = makeTreeWithTypeVal(FACTOR, $1->type, currentScope->strTable[index]->value);
                            addChild(factorNode, $1);
                            $$ = factorNode;
                        }

                    }
                    else 
                    {
                        Tree* factorNode = makeTreeWithTypeVal(FACTOR, 2, 0);
                        addChild(factorNode, $1);
                        $$ = factorNode;
                    }
                }
                | funcCallExpr
                {
                    Tree* factorNode = makeTreeWithType(FACTOR, $1->type);
                    addChild(factorNode, $1);
                    $$ = factorNode;
                }
                | INTCONST
                {   
                    Tree* factorNode = makeTreeWithTypeVal(FACTOR, 0, (int)$1);
                    addChild(factorNode, makeTreeWithVal(INTEGER, (int)$1));
                    $$ = factorNode;
                }
                | CHARCONST
                {
                    Tree* factorNode = makeTreeWithType(FACTOR, 1);
                    addChild(factorNode, makeTreeWithVal(CHARACTER, (char)$1));
                    $$ = factorNode;
                }
                ;

funcCallExpr    : ID LPAREN argList RPAREN
                {   
                   // Bypass for specific calls of Output() required by assign4
                   if(strncmp($1, "output", sizeof($1) == 0)) {
                        Tree* funcCallExpr = makeTreeWithType(FUNCCALL, INT_TYPE);
                        addChild(funcCallExpr, makeTreeWithVal(IDENTIFIER, 9999)); // 9999 indicates a call out output as it is outside feasible bounds of the hash table.

                        // Function only expected to handle integers. No need to check arguments
                        goto exit1;
                    }
                   
                   
                    // Falling If statements so code will continue to run on error.
                    int index = ST_lookup($1);
                    TableNode* validScope = currentScope;

                    

                    if(index > 1000) {
                        index -= 1000;
                        validScope = currentScope->parent;
                    }

                    if(index < 0) 
                    {   
                        snprintf(buffer, sizeof(buffer), "Function::%s | Undeclared in currentScope.", $1);
                        yyerror(buffer);
                        clearWritingBuffer(buffer);
                    }

                    Tree* funcCallExpr = makeTreeWithType(FUNCCALL, validScope->strTable[index]->data_type);
                    addChild(funcCallExpr, makeTreeWithVal(IDENTIFIER, index));

                    // Check for valid number of arguments.
                    int numArgs = countChildren($3, EXPR);
                        
                    if(numArgs < validScope->strTable[index]->size) {
                        snprintf(buffer, sizeof(buffer), "Function Call::%s | Too few arguments.", $1);
                        yyerror(buffer);
                        clearWritingBuffer(buffer);
                        goto exit1; // Exit if numArgs incorrect.
                    } 
                        
                    if(numArgs > validScope->strTable[index]->size) {
                        snprintf(buffer, sizeof(buffer), "Function Call::%s | Arguments exceed matching function definition.", $1);
                        yyerror(buffer);
                        clearWritingBuffer(buffer);
                        goto exit1; // Exit if numArgs incorrect.
                    } 

                    int* paramTypes = getParamTypes(currentScope->parent->strTable[index]->params, currentScope->parent->strTable[index]->size);
                    int* argTypes = getArgTypes($3, EXPR, numArgs);

                    int valid = 1;
                    for(int i = 0; i < numArgs; i++) {
                        
                        if(paramTypes[i] != argTypes[i]){
                            valid = 0;
                            break;
                        }

                    }

                    free(paramTypes);
                    free(argTypes);

                    if(!valid) {
                        snprintf(buffer, sizeof(buffer), "Function Call::%s | Argument Type Mismatch.", $1);
                        yyerror(buffer);
                        clearWritingBuffer(buffer);
                    }

                    exit1:
                    addChild(funcCallExpr, $3);
                    $$ = funcCallExpr;
                }
                
                | ID LPAREN RPAREN
                {   
                    int index = ST_lookup($1);
                    TableNode* validScope = currentScope;

                    if(index > 1000) {
                        index -= 1000;
                        validScope = currentScope->parent;
                    }

                    if(index < 0)
                    {   
                        snprintf(buffer, sizeof(buffer), "Function::%s | Undeclared in scope.", $1);
                        yyerror(buffer);
                        clearWritingBuffer(buffer);

                        Tree* funcCallExpr = makeTree(FUNCCALL);
                        $$ = funcCallExpr;
                    } else {
                        Tree* funcCallExpr = makeTreeWithType(FUNCCALL, validScope->strTable[index]->data_type);
                        addChild(funcCallExpr, makeTreeWithVal(IDENTIFIER, index));
                        $$ = funcCallExpr;
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
    printf("Warning {ln %d: %s}\n", yylineno, msg);
    return 0;
}

int yyerror(char* msg){
    printf("Error {ln %d: %s}\n", yylineno, msg);
    exit(EXIT_FAILURE); // Remove for runtime compilation of parser including errors.
    return 0;
}

void clearWritingBuffer(char* buffer) {
    memset(buffer, '\0', sizeof(buffer));
}