program ::= 'using' SIGMA '@' MONAD '{' expression '}'

expression ::= 'do' ID '=' expression ';' expression
    | 'if' val 'then' expression 'else' expression
    | 'handle' expression 'with' handler
    | 'return' val
    | op_call
    | primitive
    | application
    
val ::= NAT | BOOL | lambda| ID | rec_fun | '(' val ')'

lambda::= LAMBDA ID ':' type '.' expression

rec_fun ::= 'rec' ID ':' type '.' LAMBDA ID '.' expression
  
application ::= val val
    
op_call ::= OPR '(' (val (',' val)*)? ')' 

primitive ::= val BIN_OP val | UN_OP val

handler ::= '{' (clause ',')* ID '->' expression '}'

clause ::= OPR '(' (ID (',' ID)*)? ')' '->' ('c' | 's') expression

type ::= atom_type ('->' effect type)?

atom_type ::= '(' type ')' | 'Nat' | 'Bool'

effect ::= '{' (OPR (',' OPR)*)? '}'

SIGMA  : 'Nondeterminism' | 'Exceptions'
MONAD  : 'List'| 'Exceptions'
BIN_OP : '+' | '-' | '&&' | '||' ;
UN_OP  :  'succ' | 'pred' | 'iszero'
OPR    :  'choose' | 'raise' '<' ID '>';
LAMBDA : 'lambda' | 'λ' ;
BOOL   : 'true' | 'false' ;
NAT    : [0-9]+ ;
ID     : [a-zA-Z_] [a-zA-Z0-9_']*  ;

WS : [ \t\r\n]+ -> skip ;
COMMENT : '//' ~[\r\n]* -> skip ;
BLOCK_COMMENT : '/*' .*? '*/' -> skip ;
