lexer grammar SYsU_lang;

Int : 'int';
Return : 'return';
Const : 'const';
If : 'if';
Else : 'else';
While : 'while';
Void : 'void';
Break : 'break';
Continue : 'continue';
Greater : '>';
Percent : '%';
Ampamp : '&&';
Exclaim : '!';
Lessequal : '<=';
Greaterequal : '>=';
Exclaimequal : '!=';

LeftParen : '(';
RightParen : ')';
LeftBracket : '[';
RightBracket : ']';
LeftBrace : '{';
RightBrace : '}';

Plus : '+';
Minus : '-';
Star : '*';
Semi : ';';
Comma : ',';
Slash : '/';
Equal : '=';
Less : '<';
Pipepipe : '||';
Equalequal : '==';

Identifier
    :   IdentifierNondigit
        (   IdentifierNondigit
        |   Digit
        )*
    ;

fragment
IdentifierNondigit
    :   Nondigit
    ;

fragment
Nondigit
    :   [a-zA-Z_]
    ;

fragment
Digit
    :   [0-9]
    ;

Constant
    :   IntegerConstant
    ;

fragment
IntegerConstant
    :   DecimalConstant
    |   OctalConstant
    |   Sixteen
    ;

fragment
DecimalConstant
    :   NonzeroDigit Digit*
    ;

fragment
OctalConstant
    :   '0' OctalDigit*
    ;

fragment
Sixteen
    :   '0x' [0-9a-fA-F]*
    ;


fragment
NonzeroDigit
    :   [1-9]
    ;

fragment
OctalDigit
    :   [0-7]
    ;


// 预处理信息处理，可以从预处理信息中获得文件名以及行号
// 预处理信息前面的数组即行号
// 关键是将skip改成hidden channel


LineAfterPreprocessing
    :   '#' Whitespace* ~[\r\n]*
        -> channel(HIDDEN)
    ;

Whitespace
    :   [ \t]+
        -> channel(HIDDEN)
    ;

// 换行符号，可以利用这个信息来更新行号
Newline
    :   (   '\r' '\n'?
        |   '\n'
        )
        -> channel(HIDDEN)
    ;

