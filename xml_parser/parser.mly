%token <string> STRING
%token <string> OPENING_TAG
%token <string> CLOSING_TAG
%token EOF
%start <unit> prog
%%

prog:
  | OPENING_TAG  { () }
  | CLOSING_TAG  { () }
  | STRING       { () }
  | EOF          { () } ;