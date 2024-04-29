parser grammar SYsUParser;

options {
  tokenVocab=SYsULexer;
}

//==============================================================================
// 表达式
//==============================================================================

//初始表达式：标识符|常量
//如 name 3
primaryExpression
    :   Identifier
    |   Constant
    |   LeftParen expression RightParen 
    ;
//后缀表达式：初始表达式
postfixExpression
    :   primaryExpression  
    |   postfixExpression LeftBracket expression RightBracket
    |   postfixExpression LeftParen expression RightParen
    |   postfixExpression LeftParen RightParen
    ;








//一元表达式：（后缀表达式|一元运算符 一元表达式）
unaryExpression
    :
    (postfixExpression
    |   unaryOperator unaryExpression
    )
    ;
//一元运算符：加|减
unaryOperator
    :   Plus | Minus | Exclaim
    ;
//乘法表达式：一元表达式（（乘|除|模）一元表达式）*
multiplicativeExpression
    :   unaryExpression ((Star|Slash|Percent) unaryExpression)*
    ;
//加法表达式：乘法表达式（（加|减）乘法表达式）*
additiveExpression
    :   multiplicativeExpression ((Plus|Minus) multiplicativeExpression)*
    ;

relationalExpression
    :   additiveExpression
    |   relationalExpression (Greater|Greaterequal|Less|Lessequal) additiveExpression
    ;

// judgeop
//     :   Greater|Greaterequal|Less|Lessequal
//     ;

equalityExpression
    :   relationalExpression
    |   equalityExpression (Equalequal|Exclaimequal) relationalExpression
    ;

andExpression
    :   equalityExpression
    |   andExpression Ampamp equalityExpression
    ;

orExpression
    :   andExpression
    |   orExpression Pipepipe andExpression
    ;



//赋值表达式：加法表达式|一元表达式=赋值表达式
assignmentExpression
    :   //additiveExpression
        orExpression
    |   unaryExpression Equal assignmentExpression
    ;


    
//表达式：赋值表达式（，赋值表达式）*
expression
    :   assignmentExpression (Comma assignmentExpression)*
    ;

  //============================================================================
  // 声明
  //============================================================================

//声明：类型说明符部分如int 初始化声明符列表（可选） ；
declaration
    :   declarationSpecifiers initDeclaratorList? Semi
    ;
//声明说明符s：声明说明符+   比如说const int
declarationSpecifiers
    :   declarationSpecifier+
    ;
//声明说明符：类型说明符
declarationSpecifier
    :   typeSpecifier
    |   qualSpecifier
    ;
//初始化声明符列表：初始化声明符（，初始化声明符）* a=1,b=2
initDeclaratorList
    :   initDeclarator (Comma initDeclarator)*
    ;
//初始化声明符：声明器（=初始化器）   a=1
initDeclarator
    :   declarator (Equal initializer)?
    ;

qualSpecifier
    :   Const
    ;


//类型说明符：int
typeSpecifier
    :   Int
    |   Void
    ;

//声明器：直接声明符
declarator
    :   directDeclarator
    ;
//直接声明符：声明符|直接声明符【赋值表达式（可选）】 a   a[1]
directDeclarator
    :   Identifier
    |   directDeclarator LeftBracket assignmentExpression? RightBracket
    |   directDeclarator LeftParen parameterList? RightParen
    ;

parameterList
  :     parameterDeclaration(Comma parameterDeclaration)*
  ;

parameterDeclaration
    :   declarationSpecifiers declarator
    ;

//标识符列表：标识符（，标识符）* a,b,c
identifierList
    :   Identifier (Comma Identifier)*
    ;
//初始化器：赋值表达式|{初始化列表（可选），（可选）} {0}
initializer
    :   assignmentExpression
    |   LeftBrace initializerList? Comma? RightBrace
    ;
//初始化列表：初始化器（，初始化器）* {0，1，2}
initializerList
    // :   designation? initializer (Comma designation? initializer)*
    :   initializer (Comma initializer)*
    ;


  //============================================================================
  // 语句
  //============================================================================

//语句：复合语句|表达式语句|跳转语句
statement
    :   compoundStatement
    |   expressionStatement
    |   selectStatement
    |   iterationStatement
    |   jumpStatement
    ;



//复合语句：{块项目列表（可选）}
compoundStatement
    :   LeftBrace blockItemList? RightBrace
    ;
//块项目列表：块项目+
blockItemList
    :   blockItem+
    ;
//块项目：语句|声明
blockItem
    :   statement
    |   declaration
    ;
//表达式语句：表达式（可选）；
expressionStatement
    :   expression? Semi
    ;


//选择语句

selectStatement
    :   ifStatement
    |   elseStatement
    ;


ifStatement
    :   If LeftParen expression RightParen statement
    ;

elseStatement
    :   ifStatement Else statement
    ;


 
//循环语句
iterationStatement
    :   While LeftParen expression RightParen statement
    ;


//跳转语句：return 表达式（可选）；
jumpStatement
    :   ((Return expression?)|
        Break|
        Continue)
    Semi
    ;
//编译单元：翻译单元（可选） EOF
compilationUnit
    :   translationUnit? EOF
    ;
//翻译单元：外部声明+
translationUnit
    :   externalDeclaration+
    ;
//外部声明：函数定义|声明
externalDeclaration
    :   functionDefinition
    |   declaration
    ;
//函数定义：声明说明符s 直接声明符 （） 复合语句
functionDefinition
    : //declarationSpecifiers directDeclarator LeftParen RightParen compoundStatement
    declarationSpecifiers directDeclarator LeftParen parameterList? RightParen compoundStatement
    ;

