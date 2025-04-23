%token NUMBER
%token SIN COS TAN SQRT LOG

%left '+' '-'
%left '*' '/' '%'
%right UMINUS

%%

stmt    : expr
        ;

expr    : expr '+' term
        | expr '-' term
        | term
        ;

term    : term '*' factor
        | term '/' factor
        | term '%' factor
        | factor
        ;

factor  : '-' factor %prec UMINUS
        | funct '(' expr ')'
        | '(' expr ')'
        | NUMBER
        ;

funct   : SIN
        | COS
        | TAN
        | SQRT
        | LOG
        ;

%%
