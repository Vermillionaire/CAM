val cam = ["dupl", "fst", "swap", "snd", "plus", "stop" ];
val cam2 = ["quote", "4", "swap", "cons", "cur", "6", "app", "stop", "dupl", "fst", "swap", "snd", "add", "return"];

datatype 'a lst = empty | elem of 'a * 'a lst;
datatype dty = I of int | R of real | B of bool | P of dty * dty | A of int | Null;

val x = 3;
val y = 4;

val STACK = ref (elem (A 0, empty));
val MEMORY = ref (elem (I x, elem (I y, empty)));
val SR = ref 0;
val MR = ref 1;

fun strip (elem x) = x;
fun this (x) = strip(!x);
fun next (x) = #2 (this(x));
fun toInt (I x) = x;
fun toReal (R x) = x;
fun toBool (B x) = x;
fun toPair (P x) = x;
fun toAddress (A x) = x;
fun toType (x) = ( case x of
	  int => (I x)
	| real => (R x)
	| bool => (B x) );

(* Prints out stack or memory *)
fun printList (x) =( ( fn (y) => 
	case (y) of
		(I i) => print("( " ^ Int.toString(i) ^ " ")
		| (R r) => print("( " ^ Real.toString(r) ^ " ")
		| (B b) => print("( " ^ Bool.toString(b) ^ " ")
		| (A a) => print("( #" ^ Int.toString(a) ^ " ")
		| (_) => print("( unknown ")
	)(#1 x);

	( fn (z) =>
	case z of
	  empty => print("(end")
	 |_ => printList(strip(z))
	)(#2 x); 	
	print(") ") 
	);

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

(*From top of stack or memory*)
(*Follows addresses*)
fun getAddress (x,0) = 
	(fn (x) =>
	  case x of 
	    (A x) => getAddress(!MEMORY, !MR-x)
	    | _ => x 
	)( #1 (strip(x)) )
	| getAddress(x,~1) = Null
	| getAddress(x,y) = getAddress(#2 (strip(x)), y-1);

fun  dupl () = ( STACK := elem(#1 (this(STACK)), !STACK), SR:= !SR+1 );
fun quote (x) = (
	SR := !SR+1;
	STACK := elem( x, !STACK)
	);


fun fst () = (fn x =>  case x of
	A x => quote( getAddress( !MEMORY, !MR - toAddress( pop() ) ) )
	| _ => ()
	)(#1 (this(STACK)));

fun snd () = (fn x =>  case x of
	A x => quote( getAddress( !MEMORY, !MR - toAddress( pop() ) - 1 ) ) 
	| _ => ()
	)(#1 (this(STACK)));

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
 
val stop = ();


fun run (CR) = case (hd CR) of
  "dupl" => ( printList(this(STACK));  print(" -- dupl\n"); dupl(); run(tl CR) )
  | "fst" => ( printList(this(STACK)); print(" -- fst\n"); fst(); run(tl CR))
  | "swap" => ( printList(this(STACK)); print(" -- swap\n"); swap(); run(tl CR))
  | "snd" => ( printList(this(STACK)); print(" -- snd\n"); snd(); run(tl CR))
  | "plus" => ( printList(this(STACK)); print(" -- plus\n"); plus(); run(tl CR))
  | "quote"=> ( printList(this(STACK)); print(" -- quote\n"); quote( 
  | "stop" => ( printList(this(STACK)); print(" -- stop\n") ); 

(*
STACK;
dupl ();
STACK;
#2 (this(STACK));
dupl;
STACK;
#2 (this(STACK));
#2 (strip(next(STACK)));
quote(I 5);
STACK;
#2 (this(STACK));
quote(B true);
STACK;
#2 (this(STACK));
*)
