  let line = ref 1;;
  let file = if(Array.length(Sys.argv) > 1) then (ref Sys.argv.(1)) else (ref "<stdin>");;
