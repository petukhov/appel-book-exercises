%token <string> STRING
%token <string> OPENING_TAG
%token <string> CLOSING_TAG
%token EOF
%start <Ast.xml> prog
%%

prog:
  | v = value { v }
  | EOF       { Ast.Tag ("EOF", []) } ;

value:
  | o_tag = OPENING_TAG; res = listing; c_tag=CLOSING_TAG; {
      if o_tag = c_tag 
      then Ast.Tag (o_tag, res) 
      else failwith "Mismatched tags"
    }
  | s = STRING                            { Ast.String s }

listing:
  | v = value; l = listing                { v :: l }
  | v = value                             { [v] }
  |                                       { [] }