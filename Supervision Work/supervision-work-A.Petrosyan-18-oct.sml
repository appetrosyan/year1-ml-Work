(* Supervision Work For A. Petrosyan, for the Sunday 18-th October Deadline

*)

(*
--------------------------- Question 1 -------------------------------	
Read Chapter 1 "Standard ML", Chapter 2 "Names, Functions and Types", 	
Chapter 3 pages 69–80 "Lists" from ML for the Working Programmer.		
Make brief notes on what you read.						
----------------------------------------------------------------------	
										
	--------------------- Chapter 1 ------------------------		
  MetaLanguage - Funtional Language, designed to replace mathematicians. 	
	* Has Basic Function implementation capability.				
	* No true variables, only naming, not assignment			
	* Should work out of the box, but exception handling added just in case	
	* Can do very little. No harm to hardware.				
										
  And now the serious part:							
	* Operaions on Lists - List, ListPair: e.g. useful utilities accesed through 	
	  List.functionName(args)							
	* Integers belong to structure Int. There are FixedInts and InfInts. 		
	  The Former has length limitation, and the other doesn't (is expensive)
	* Most Real Operations are in Real, but some like Sqrt, sin, cos belong to Math.
	* Mutable Arrays are provided (hooray!!!) in Array, 
 	* Have Date, Time, Timer, TextIO, String and Char, just in case

*)
(*
	----------------------- Chapter 2 ------------------------	

	-------- Overview
  ML is intepreted, can have source files. Strict Semicolon Policy.
	Standard Declaration is 
	val <identifier> = <expr>;
  Things get named, not assigned. Changing values outside Let -- in -- end 
  don't happen.
 	
  it - is the last value displayed.
	
  Ml allows functions, declared as follows:
	fun <identifier> < <argument> | ( <argument list> )> = <expr>;
  NB: for single argument parentheses are optional


	-------- Identifiers and vars
  Identifiers begin with a letter, exclude "andlaso, in, as, let,... etc."
  can permit a pile of symbols as a name, for example $$$ or :-> but not =>, -> and :>
  Symbolics and alphabetics are interchangeable

  Arithmetic is handled well, negative sign is tilde: "~"

  Ints not implicitly converted: use Real.FromInt(). 
	<reals>::= <Numbers>.<Numbers> | <Numbers>E<Numbers> | <Numbers>E~<Numbers>
  NB: can't divide Integers, but can get quotient via div: a div b <=> a/b for ints
  
  Allows Standard escape Characters like \n,\t,\\,\" etc.
	

	-------- Conditions
  Conditionals: must have complete structure like:
	if <condition> then <expr> else <expr> 
  Never Omit Else. 

  Logic operators have a kink (what doesn't?). Always use andalso <=> && and orelse <=>||

	-------- Structures
  Supports tuples as oredered piles of values. A record identifies components by name 
  not by position, so access is like to a class in OOP. Syntax for tuple functions is:
  	fun <id> (<arg1>,<arg2>,...,<argn>) : <arg1 type> * ...*<argn type>=<expr>
  Can decalre as own type: 
	type vec = real*real 
  tuples can be stacked and nested;
  
  Records substitute structs and classes:
	val <id> = { <id1> = <expr>, <id2> = <expr>, ... };
  Three dots intepreted almost like a human would. declaration of some not all fields
  is possible and permissible.
  Access to Labels via:
  	#<id1> <id> <=> <id>.<id1>
  access to the n-th label via #n <id>
  	

	--------- infixes 
  Ml allows defining own infixes (that's why it allowed symbolic names). 
  First Decalre 
	infix <id of infix>;
  Then define
	fun ( <arg1> <id of infix> <arg2> ) = <expr>


	--------- Execution and Evaluation
  Ml Has Strict Evaluation. This makes recursion much easier, but causes problems with   #
  infinitely looping recursions. 

  Variables local to functions can be declared inside local--in--end and let--in--end.
	let val <id1> = <expr1> and <id2> = <expr2> ... in <expr> end;

  No iteration only recursion, but recursion is simplified, does only jumps so works
  better than in C++.
*)

(*
	--------------------- Chapter 3 ------------------------

  At long last.

  Lists is essentailly anything of the type 
	[<variable>,<variable>,<variable>,...,]
  Can permit tuples and Records to be on the list. 

  Exmpty list has special name: 
	nil=[]
  
  to add elements to a list use "::" also called cons.
	[a,b,c] <=> a::b::c::nil
  Lists can be acted upon: Example:
	fun prod [] = 1 | prod (n::ns) = n* (prod ns);
  The pattern like n::ns breaks the list to the form <Element>::<list>
*)











(*
--------------------------- Questions 2,3,4 --------------------------	
Complete exercises 1.1 to 1.6 from the notes. 				
----------------------------------------------------------------------	
*)

(*         --------------------- Exercise 1.1 ------------------------
  One Solution to the 2000 problem would be to halve the century. This would give you extra
  digits to store the information, but it will buy you at most 50 years, e.g. until 2049 
  when the problem should recur. Additionally because the other two digits are limited to 
  not only integetrs but one of them to 0 or 1, the other to [0,3], lots and lots of 
  possible meaningful combinations are unused. A slightly better soltuion would be to 
  encode the cycle (e.g. whcich century we are in, in the most restricted digit, e.g. 
 	Month = a%2 concat b. 
  But to avoid the main mistake it would be better to of course just count dates, and 
  compute the standard representation each time it's needed. 
*)

(*         --------------------- Exercise 1.2 ------------------------
*)
fun compyear (x,y) = if x=y then 0 else (* Trivial case*)
if (x<49 andalso y<49) orelse (x>49 andalso y>49) then 
		if x>y then 1 	(* Same Interval the later year has greater value*)
		else ~1
	else 
		if x<49 then 1 	(* x is year 2000 ish*)
		else ~1			(* Y is year 2000 ish*)
;
compyear (3,65); (*1*)
compyear (65,65);(*0*)
compyear (50,49);(*~1*)
fun subtractyear (x,y) =
 if x-y>49 orelse (x<49 andalso x-y>0) (*trivial Case, no overflow, y>0*)
		then x-y 
	else if x<y andalso x>49 then ~1 
	else if x-y+100>49 andalso x-y+100<100 
		then x-y+100 else ~1;
subtractyear (3,2); 
subtractyear (3,5); (*should output 1998*)
subtractyear (3,65); (* outputs error, e.g. -1)*)
subtractyear (65,20); (*same as above*)
subtractyear (65,15);
subtractyear (65,14);
subtractyear (32,55);


(*         --------------------- Exercise 1.3 ------------------------

  A competent programmer would know that a conditional expression already yields the 
  output. In the first case directly, and in the other - its negation.
*)

(*         --------------------- Exercise 1.4 ------------------------

  power, "needs" a type constraint, because none of its conditional bracnhes contain 
  an expression of type real for the compiler to determine its type automatically. 
  npower has an inconspicuous 2.0
*)

(*         --------------------- Exercise 1.5 ------------------------
*)

fun flopsum (x:real,n,total:real) = 
if n=0 then total 
else flopsum (x,n-1,total+x);

flopsum (0.1,1000000,0.0);
it-100000.0;
flopsum (0.3,500000,30.0);

(*         --------------------- Exercise 1.6 ------------------------
*)

fun goldratio (n:int) = if n=0 then 0.5+0.5*Math.sqrt 5.0 
else 1.0/(goldratio(n-1)-1.0);

goldratio(1);
goldratio(2);
goldratio(3);
goldratio(5);
goldratio(20);
goldratio(30);
goldratio(50)-goldratio(0);

(*         --------------------- Exercise 2.1 ------------------------
*)

fun itpow(x:real,res:real,n:int) = if n=1 then res else 
if n mod 2 = 0 then itpow(x,n div 2,res*res)
else itpow(x,n div 2, x*res*res);

itpow(3.0,3.0,2); (* since variables are immutable, can't initialise res:=x)*)
itpow(2.0,2.0,20);
itpow(4.0,4.0,8);
itpow(7.0,7.0,3);
itpow(2.0,2.0,10);

(*         --------------------- Exercise 2.3 ------------------------
  f(n)<= c |a_1 g_1(n) + a_2 g_2(n)+...+a_k g_k(n)|

  a_i forall i are constants => there exisits a_0, such that |a_0|>|a_i| forall i
  
  f(n)<= c_0 |b_1 g_1(n) + b_2 g_2(n)+...+b_k g_k(n)| where 
  	b_i = a_i/a_0 and c_0=c/a_0 
	since |b_i|<1 then #
  f(n)<= c_0 |b_1 g_1(n) + b_2 g_2(n)+...+b_k g_k(n)< c_0|g_1(n)+g_2(n)+...+g_k(n)|
	which is by deifinition of O notation the fact that f(n)=O(g_1,...g_k(n));
*)

(*         --------------------- Exercise 2.4 ------------------------
  Evaluation of each recurrence halves the number n. Thus until n reaches 1 it takes  
  at most Log_2 (n)+1 (truncated to the integer part) steps. each step consists of 1 
  addition, 1 multiplication (reserved fro until the last evaluation of T(1) and an 
  evaluation of the recurrence. The exact time (and memory) taken is 

  addition*(Log_2 (n)+1)+multiplication*(Log_2 (n)+1) + Base case
  Or if we do simple algebra:
  (Log_2 (n)+1)*(Addition+multiplication)+Base Case. 

  This by definition of the O notation is: 

  computation time/ memory usage = O(Log_2 (n)+1)=O(ln(n)) since Log_2(n)=ln n/ln 2 and
  1 is a constant.
*)

(*         --------------------- Exercise 3.1 ------------------------
*)

fun nsum [] = 0 | nsum (x::xs) = x + nsum (xs);	


	(* does (Length of list)*additions + base case. So O(n)
	Memory and runtime performance of the same order of magnitude*)

nsum [1,2,3,4,5,6,7,8,9]; (*should equal 50*)
nsum [1,3,5,7,9];
nsum [1,2,1,2,1,2];


fun nitsum(sum,[]) 	= sum | nitsum(sum, x::xs) = nitsum (sum+x,xs);
	(* Does (Length of List)*additions + Base case. So O(n) Runtime performance
	In terms of memory this is O(1), because the expression always evaluates to an 
	expression with one addition*)
nitsum(0,[1,2,3,4,5,6,7,8,9]);

(*         --------------------- Exercise 3.2 ------------------------
*)

fun getlast [x]= x | getlast (x::xs) = getlast (xs);
getlast [1,2,3,4,5,6,7,8];

(* Takes the first element each time, iterates trhough the list. SInce each evaluation 
doesn't pile up in Ram we have O(1) Memory Usage (one expression at all times) and 
O(n) computation time since the amount of steps taken is equal to the amount of 
evaluations done equal to the amount of elements in list.

Minor difference: the x was taken in square brackets, I took in round parentheses, 
*)

(*         --------------------- Exercise 3.2 ------------------------
*)

fun evenel[] = [] | evenel (x1::x2::xs) = x2::evenel xs | evenel (x::xs)=evenel xs;
evenel [1,2,3,4,5,6,7,8,9,0,1,3,2,3,1];
evenel [1,1,3,1,3,1,3,1,3,1,1,1];

(* works *)

(*
--------------------------- Question 5 -------------------------------	
http://www.cl.cam.ac.uk/teaching/exams/pastpapers/y1995p1q3.pdf		
----------------------------------------------------------------------
*)
infix $%;

fun  (x $% []) = false | (x $% y::ys) = x=y orelse x $% ys;
1 $% [~1,0,2,3,4,5,6];

fun intersect ([],[]) = [] 
| intersect (xs,[]) = xs 
| intersect([],ys) = [] 
| intersect (x::xs,ys) = 
if x $% ys 
then x::intersect(xs,ys)
 else intersect (xs,ys);

intersect ([3,1,2,4],[1,2,3,4,5,6,7,8,9,10]);
fun subtract([],[]) = [] 
| subtract(xs,[]) = [] 
| subtract([],ys) = [] 
| subtract(x::xs,ys) = 
if not(x $% ys) 
then x::subtract(xs,ys) 
else subtract(xs,ys);


subtract ([1,2,3,4,5],[3,2,1,4]);
subtract ([1,2,3],[4,5,6]);
subtract (["Anne","Mary","Rosemary"],["Mary"]);


fun union ([],[]) = []
| union (xs , []) = xs
| union ([], ys) = ys
| union (x::xs, ys) = 
if not(x $% ys) then 
x::union (xs,ys) else 
x::union (xs, subtract (ys, intersect (x::xs, ys)));




union ([1,2,3,4,5,6,7,87,~3,12],[7,6,5,4,3,2,1,0,~1,15,64,77,11]);
union (["Anne", "Mary","Rosie"],["John","Mary","Jacob","QBert"];
 
  


