module String = struct
	 include String
	 
	 let starts_with (sub:string) (st:string) : bool =
		let sub_ln : int = String.length sub in
		if sub_ln > String.length st then false
		else String.sub st 0 sub_ln = sub
	 ;;

	 let repetitions (num_times:int) (st:string) : string = 
		let rec repetitions (num_times:int) (acc:string) : string = 
		  if num_times = 0 then acc
		  else repetitions (num_times-1) (st^acc)
		in repetitions num_times ""
	 ;;	
end
