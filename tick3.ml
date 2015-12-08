datatype 'a tree  = Lf
		  | Br of 'a * 'a tree * 'a tree;
infix tcons;
fun v tcons Lf = Br (v, Lf, Lf)
      | v tcons (Br (w, t1, t2)) = Br (v, w tcons t2, t1);
(* It acts like sort of a cons for trees, e.g. adds an element to the root*)


fun arrayoflist [] = Lf
  | arrayoflist (x::xs) = x tcons arrayoflist xs;

fun count Lf = 0
  | count (Br(v,t1,t2)) = 1+ count t1 +count t2;
  
exception NotFound
  
fun sub (Lf, _) =  raise NotFound
      | sub (Br(v,t1,t2), k) = 
	if k=1 then v
	else if k mod 2 =0
	  then sub (t1, k div 2)
	  else sub (t2, k div 2);
(* Inefficient
fun listhelp (t:'a tree,k:int) = if (k>count t) then [] else [sub (t,k)]@listhelp (t,k+1);
fun listofarray ar = listhelp (ar,1);
*)

fun interleave ([], ys) 	= ys
  | interleave (x::xs, ys) 	= x::interleave(ys,xs);
  
fun listofarray Br(v,t1,t2) 	= v::interleave((listofarray t1),(listofarray t2));
fun getel [x] = x;

fun subsofevensHelp (t,k) = 
if (k>count t) 
then [] 
else  
    if sub (t,k) mod 2 =0 
    then k::subsofevensHelp (t,k+1) 
    else subsofevensHelp(t,k+1);
    
fun getSubsOfEvens t = 
  subsofevensHelp(t,1);
  