(* Implimentation of a Catigorical Abstract Machine (CAM) in ML
   by Brian Vermillion
   07/28/2014
*)


(*Reads in text from file called code.txt**********************************)
print("Input file is code.txt\n");
val inStream = TextIO.openIn("code.txt");
fun inText(x) = (
	let 
		val y = TextIO.input1(inStream)
		fun convert(SOME z) = z	
	in
		if (y <> NONE) then
			convert(y)::inText(x)
			
		else []
	end );

fun toString(x) = ( case hd(x) of
	#" " => "" 
	| #"\n" => ""
	| _ => Char.toString(hd(x))^toString(tl(x)) );

fun nthtail(x,0) = tl(x)
	| nthtail(x,y) = nthtail(tl(x),y-1);

fun code [] = []
	| code(x) = toString(x)::code(nthtail(x,String.size(toString(x)))); 

val cam = code(inText(inStream)); 
(**************************************************************************)

(*Defines data types*)
datatype 'a lst = empty | elem of 'a * 'a lst;
datatype dty = I of int | R of real | B of bool | A of int | Null;
datatype 'a nest = data of dty | doubleData of dty * dty |  nst of 'a list;
datatype instruction = qt of instruction nest | ld of instruction nest | cn | cr of instruction nest | ap | st | dp | fs | sw | sn | pl | rt | ml | br of instruction nest |  unknown;
(*******************)

(*************Initalizes stack and Memory**************)
val STACK = ref (elem (Null, empty));
val MEMORY = ref (elem (Null, empty));
val SR = ref 0;
val MR = ref 0;
(******************************************************)

(*Helper functions*)
fun strip (elem x) = x;
fun this (x) = strip(!x);
fun next (x) = #2 (this(x));
fun toInt (I x) = x;
fun toReal (R x) = x;
fun toBool (B x) = x;
fun toAddress (A x) = x;
fun clearStack() = ( STACK := empty; SR := 0 );
fun clearMem() = ( MEMORY := empty; MR := 0 );
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
			| Null => print("( null ")
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

(*Reads the top of stack*)
fun read () =
	if (!SR <> ~1) then 
	    	(#1 (this(STACK)))
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

	fun condence ([],y) = (0,-1)
		| condence (x,y) =
		( case hd(x) of
			10 => ( 0, #1 (condence(tl(x), 0)))
			| ~1 => ( ~1 * (#1 (condence(tl(x),y))), (#2 (condence(tl(x),y))))
			| _ => ( hd(x) * power(10,y) + (#1 (condence(tl(x),y+1))), (#2 (condence(tl(x),y+1))))
		);

	fun intORreal (0,-1) = Null
		| intORreal(x,0) = (I x)
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

(*Multiply top two values on stack*)
fun mult() = ( fn (x,y) => 
	case (x,y) of
	  (I x, I y) => quote( I (x * y) )
	| (R x, R y) => quote( R (x * y) )
	| (_,_) => ( quote(y); quote(x) )
	)(pop(),pop());

(*Add top two values on stack*)
fun plus() = ( fn (x,y) => 
	case (x,y) of
	  (I x, I y) => quote( I (x + y) )
	| (R x, R y) => quote( R (x + y) )
	| (_,_) => ( quote(y); quote(x) )
	)(pop(),pop());

(*Jumps to a instruction and puts the return address on the stack*)
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

(* Branch to different addresses if the value on the stack is a boolean *)
fun branch(x,y) = ( case (read()) of
	(B b) => ( pop(); if b then quote(x) else quote(y) )
	| _ => () );

		
val stop = ();

fun buildNest(x) = case(

(*Builds a list of instructions to execute from a list of strings
that was read in from the input file *)
fun setInstructions ([],y)  = []
	| setInstructions(x,y) =( case hd(x) of
 		"dupl" => dp::setInstructions(tl(x),y)
  		| "fst" => fs::setInstructions(tl(x),y)
		| "swap" => sw::setInstructions(tl(x),y)
  		| "snd" => sn::setInstructions(tl(x),y)
  		| "plus" => pl::setInstructions(tl(x),y)
  		| "quote" => (qt (parse(hd(tl(x)))))::setInstructions(tl(tl(x)),y)
  		| "stop" => st::setInstructions(tl(x),y)
		| "cur" => (cr (parse(hd(tl(x)))))::setInstructions(tl(tl(x)),y)
		| "return" => if y then rt::[] else rt::setInstructions(tl(x),y)
		| "app" => ap::setInstructions(tl(x),y)
		| "cons" => cn::setInstructions(tl(x),y) 
		| "load" => (ld (parse(hd(tl(x)))))::setInstructions(tl(tl(x)),y)
		| "mult" => ml::setInstructions(tl(x),y)
		| "branch" => (br (parse(hd(tl(x))),parse(hd(tl(tl(x))))))::setInstructions(tl(tl(tl(x))),y)
		| _ => ( print("Unknown Instruction\n"); unknown::setInstructions(tl(x),y) )
	);
	

fun run (IS, CR) = case (nth(IS,CR)) of
	dp => ( print("S: "); printList(!STACK); print("\nM: "); printList(!MEMORY);  print(" -- dupl\n\n"); dupl(); run(IS,CR+1) )
	| fs => ( print("S: "); printList(!STACK); print("\nM: "); printList(!MEMORY); print(" -- fst\n\n"); fst(); run(IS, CR+1) )
	| sw => ( print("S: "); printList(!STACK); print("\nM: "); printList(!MEMORY); print(" -- swap\n\n"); swap(); run(IS, CR+1) )
	| sn => ( print("S: "); printList(!STACK); print("\nM: "); printList(!MEMORY); print(" -- snd\n\n"); snd(); run(IS, CR+1) )
	| pl => ( print("S: "); printList(!STACK); print("\nM: "); printList(!MEMORY); print(" -- plus\n\n"); plus(); run(IS, CR+1) )
	| (qt x) => ( print("S: "); printList(!STACK); print("\nM: "); printList(!MEMORY); print(" -- quote\n\n"); quote(x); run(IS, CR+1) ) 
	| (ld x) => ( print("S: "); printList(!STACK); print("\nM: "); printList(!MEMORY); print(" -- load\n\n"); load(x); run(IS, CR+1) )
	| ap => ( print("S: "); printList(!STACK); print("\nM: "); printList(!MEMORY); print(" -- app\n\n"); run(IS, toInt(app(I (CR+1)))) )
	| cn => ( print("S: "); printList(!STACK); print("\nM: "); printList(!MEMORY); print(" -- cons\n\n"); cons(); run(IS, CR+1) )
	| rt => ( print("S: "); printList(!STACK); print("\nM: "); printList(!MEMORY); print(" -- return\n\n"); run(IS, return()) )
	| (cr x) => ( print("S: "); printList(!STACK); print("\nM: "); printList(!MEMORY); print(" -- cur\n\n"); cur(x); run(IS, CR+1) )	
	| ml => ( print("S: "); printList(!STACK); print("\nM: "); printList(!MEMORY); print(" -- mult\n\n"); mult(); run(IS, CR+1) )
	| br(x,y) => ( print("S: "); printList(!STACK); print("\nM: "); printList(!MEMORY); print(" -- branch\n\n"); branch(x,y); run(IS, CR+1 ) )
	| st => ( print("S: "); printList(!STACK); print("\nM: "); printList(!MEMORY); print(" -- stop\n\n") ); 

STACK := empty;
MEMORY := empty;

fun execute() = ( run(setInstructions(cam, false),0); read() ); 
fun printStack() = printList(!STACK);
fun printMemory() = printList(!MEMORY);

print("********************************************\nRun command execute to automaticaly run the \nCAM program loaded from the file.\nexecute() returns the top value on the stack\n*********************************************\nUseful commands:\n-printStack()\n-printMemory()\n-clearStack()\n-clearMemory()\n");



