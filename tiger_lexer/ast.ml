type xml =
  | Tag of string * xml list
  | String of string 
  | EMPTY 

let example = Tag ("tag1", [ String "a"; Tag ("tag2", [ String "a"; String "b"; String "c" ]); String "c" ])
