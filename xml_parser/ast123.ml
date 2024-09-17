type 'a xml_ast = String of string | Tag of string * 'a list

let abc = Tag ("tag1", [ String "a"; Tag ("tag2", [ String "a"; String "b"; String "c" ]); String "c" ])
