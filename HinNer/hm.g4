// Gramàtica per expressions senzilles
grammar hm;
root : expr  EOF          // l'etiqueta ja és root
     ;

expr : LPAR expr RPAR expr?                  # par
     | '\\' var '->' apl                     # abst
     | num '::' tipus                        # numTipus
     | '(' OPS ')' '::' tipus                # operTipus
     | apl                                   # apli
     ;

apl  : oper apl                              # app
     | num                                   # numero
     | var                                   # variable
     |                                       # empty
     ;

var : VAR ;
num : NUM ;

oper : '(' OPS ')' apl ;
OPS: MUL|DIV|ADD|SUB;

tipus: TIPUS                                 # tip
     | TIPUS '->' tipus                      # tips
     ;


LPAR      : '(' ;
RPAR      : ')' ;
ADD       : '+' ;
SUB       : '-' ;
MUL       : '*' ;
DIV       : '/' ;
TIPUS     : [A-Z] ;


NUM : [0-9]+ ;
VAR: [a-zA-Z]+ [1-9]*;
WS  : [ \t\n\r]+ -> skip ;
