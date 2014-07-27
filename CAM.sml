val cam = ["dupl", "fst", "swap", "snd", "plus", "stop" ];
val cam2 = ["load", "()", "quote", "1", "cons", "quote", "4", "cons", "cur", "8", "app", "stop", "dupl", "fst", "swap", "snd", "fst",  "plus", "return"];

datatype 'a lst = empty | elem of 'a * 'a lst;
datatype dty = I of int | R of real | B of bool | P of dty * dty | A of int | Null;
datatype instruction = qt of dty | ld of dty | cn | cr of dty | ap | st | dp | fs | sw | sn | pl | rt;

val x = 3;
val y = 4;

val STACK = ref (elem (A 0, empty));
val MEMORY = ref (elem (I x, elem (I y, empty)));
val SR = ref 0;
val MR = ref 1;

(*Helper functions*)
fun strip (elem x) = x;
fun this (x) = strip(!x);
fun next (x) = #2 (this(x));
fun toInt (I x) = x;
fun toReal (R x) = x;
fun toBool (B x) = x;
fun toPair (P x) = x;
fun toAddress (A x) = x;
fun nth (x,0) = hd(x)
	| nth (x,y) = nth(tl(x),y-1);

(* Prints out stack or memory *)
fun printList (empty) = print("(end)")
	| printList(x) = (  
	
		( fn (y) => case (y) of
			(I i) => print("( " ^ Int.toString(i) ^ " ")
			| (R r) => print("( " ^ Real.toString(r) ^ " ")
			| (B b) => print("( " ^ Bool.toString(b) ^ " ")
			| (A a) => print("( #" ^ Int.toString(a) ^ " ")
			| (_) => print("( unknown ")
		)(#1 (strip(x)));

		printList(#2 (strip(x)));
		print(") ") 
	);

(*Pops off the stack*)
fun pop () =
	if (!SR <> ~1) then 
		let 
			val y=this(STACK);
		in
			SR := !SR-1;
			STACK := #2 y;
	    		#1 y
	  	end
	else Null;

(*Takes in a string and returns true or false if its a boolean
or the nummerical value if it is an int or real *)
fun parse (x) = (
let
	val n = explode x;
	fun isDigit (d) = #"0" <= d andalso d <= #"9";
	fun numlist (z):int list = ( 
		if (z = []) then
			[]
		else if ( isDigit(hd(z)) ) then
	  		( numlist(tl(z)) )@[(ord(hd(z)) - 48)]
		else if ( hd(z) = #"." ) then
			( numlist(tl(z)) )@[10]
		else if ( hd(z) = #"-" ) then
			( numlist(tl(z)) )@[~1]
		else
			[]);

	val l = numlist(n);

	fun getDec(x) = ( case hd(x) of
		(10) => [] 
		| (_) => hd(x)::getDec(tl(x)));

	fun getLeadZ(x) = ( case hd(x) of
		(0) => 1 + getLeadZ(tl(x))
		| (_) => 0 );

	fun power(x,1) = x
		| power(x,0) = 1
		| power (x,y) = x*power(x,y-1);

	fun condence ([],y) = (0,0)
		| condence (x,y) =
		( case hd(x) of
			10 => ( 0, #1 (condence(tl(x), 0)))
			| ~1 => ( ~1 * (#1 (condence(tl(x),y))), (#2 (condence(tl(x),y))))
			| _ => ( hd(x) * power(10,y) + (#1 (condence(tl(x),y+1))), (#2 (condence(tl(x),y+1))))
		);

	fun intORreal (x,0) = (I x)
		| intORreal(x,y) = (R (Real.fromInt(y) + 
			(Real.fromInt(x)/Real.fromInt(power(10,getLeadZ(rev(getDec(l))) + size(Int.toString(x)))))));

	fun out() = ( 
		case (x) of 
			("true") => (B true)
			| ("false") => (B false)
			| ("()") => Null
			| (_) => intORreal(condence(l,0))

		); 
in
	out()
end
);

(*From top of stack or memory*)
fun getAddress (x,0) = (#1 (strip(x))) 
	| getAddress(x,~1) = Null
	| getAddress(x,y) = getAddress(#2 (strip(x)), y-1);

(*Pushes value into next free space*)
fun memPush (x) = (
	MR := !MR+1;
	MEMORY := elem( x, !MEMORY)
	);

(********************************************************************)
(*Duplicates element on the stack*)
fun  dupl () = ( STACK := elem(#1 (this(STACK)), !STACK), SR:= !SR+1 );

(*Puts a variable on the stack*)
fun quote (x) = (
	SR := !SR+1;
	STACK := elem( x, !STACK)
	);

fun load (x) = ( case !STACK of
	empty => ( STACK := elem(x, empty) )
	| _ => ( STACK := elem( x, #2 (this(STACK))))
	);

(*Gets the first object pointed to by the address on the stack*)
fun fst () = (fn x =>  case x of
	A x => quote( getAddress( !MEMORY, !MR - toAddress( pop() ) - 1 ) )
	| _ => ()
	)(#1 (this(STACK)));

(*Gets thw secnod thig pointed to by the object on the stack*)
fun snd () = (fn x =>  case x of
	A x => quote( getAddress( !MEMORY, !MR - toAddress( pop() ) - 2 ) ) 
	| _ => ()
	)(#1 (this(STACK)));

(*Swaps values on the stack*)
fun swap () = (
	let
	  val x = pop();
	  val y = pop();
	in
	  quote(x);
	  quote(y)
	end );

fun plus() = ( fn (x,y) => 
	case (x,y) of
	  (I x, I y) => quote( I (x + y) )
	| (R x, R y) => quote( R (x + y) )
	| (_,_) => ( quote(y); quote(x) )
	)(pop(),pop());
 
fun app(x) = (
	let
		val y = pop()
	in
		quote(x);
		swap();
		y
	end );

(*Pops top twp values on the stack, then puts them into memory
Puts their address into the stack*)
fun cons() = ( memPush(pop()); memPush(pop()); quote(A (!MR-2)) ); 

fun return() = (
	let
		val x = pop();
		val (I y) = pop();
	in
		quote(x);
		y
	end 	
	);

fun cur(x) = quote(x);

val stop = ();

(*Builds a list of instructions to execute from a list of strings
that was read in from the input file *)
fun setInstructions [] = []
	| setInstructions(x) =( case hd(x) of
 		"dupl" => dp::setInstructions(tl(x))
  		| "fst" => fs::setInstructions(tl(x))
		| "swap" => sw::setInstructions(tl(x))
  		| "snd" => sn::setInstructions(tl(x))
  		| "plus" => pl::setInstructions(tl(x))
  		| "quote" => (qt (parse(hd(tl(x)))))::setInstructions(tl(tl(x)))
  		| "stop" => st::setInstructions(tl(x))
		| "cur" => (cr (parse(hd(tl(x)))))::setInstructions(tl(tl(x)))
		| "return" => rt::setInstructions(tl(x))
		| "app" => ap::setInstructions(tl(x))
		| "cons" => cn::setInstructions(tl(x)) 
		| "load" => (ld (parse(hd(tl(x)))))::setInstructions(tl(tl(x)))
	);
	

fun run (IS, CR) = case (nth(IS,CR)) of
	dp => ( print("S: "); printList(!STACK); print("\t\t\t\tM: "); printList(!MEMORY);  print(" -- dupl\n"); dupl(); run(IS,CR+1) )
	| fs => ( print("S: "); printList(!STACK); print("\t\t\t\tM: "); printList(!MEMORY); print(" -- fst\n"); fst(); run(IS, CR+1) )
	| sw => ( print("S: "); printList(!STACK); print("\t\t\t\tM: "); printList(!MEMORY); print(" -- swap\n"); swap(); run(IS, CR+1) )
	| sn => ( print("S: "); printList(!STACK); print("\t\t\t\tM: "); printList(!MEMORY); print(" -- snd\n"); snd(); run(IS, CR+1) )
	| pl => ( print("S: "); printList(!STACK); print("\t\t\t\tM: "); printList(!MEMORY); print(" -- plus\n"); plus(); run(IS, CR+1) )
	| (qt x) => ( print("S: "); printList(!STACK); print("\t\t\t\tM: "); printList(!MEMORY); print(" -- quote\n"); quote(x); run(IS, CR+1) ) 
	| (ld x) => ( print("S: "); printList(!STACK); print("\t\t\t\tM: "); printList(!MEMORY); print(" -- load\n"); load(x); run(IS, CR+1) )
	| ap => ( print("S: "); printList(!STACK); print("\t\t\t\tM: "); printList(!MEMORY); print(" -- app\n"); run(IS, toInt(app(I (CR+1)))) )
	| cn => ( print("S: "); printList(!STACK); print("\t\t\t\tM: "); printList(!MEMORY); print(" -- cons\n"); cons(); run(IS, CR+1) )
	| rt => ( print("S: "); printList(!STACK); print("\t\t\t\tM: "); printList(!MEMORY); print(" -- return\n"); run(IS, return()) )
	| (cr x) => ( print("S: "); printList(!STACK); print("\t\t\t\tM: "); printList(!MEMORY); print(" -- cur\n"); cur(x); run(IS, CR+1) )	
	| st => ( print("S: "); printList(!STACK); print("\t\t\t\tM: "); printList(!MEMORY); print(" -- stop\n") ); 

setInstructions cam;
run(it,0);
val i = setInstructions cam2;
STACK := empty;
MEMORY := empty;
SR := 0;
MR := 0;
printList(!STACK);
printList(!MEMORY);
(*printList(!STACK);*)

