(* Supervision Work For A. Petrosyan, for the Tuesday 3-rd November 

*)

datatype 'a tree  = Lf
		  | Br of 'a * 'a tree * 'a tree;
		  
(* Required By all exercises*)


(* -------------------------- Question 1 ----------------------------------- *)
(*
  Brief Notes 
  Will send Via separate email.  
*)

(* -------------------------- Questions 2-5 -------------------------------- *)

(* ----------- Exercise 7.1 -----------*)
		  
fun update (Lf, b:string, y) = Br ((b,y), Lf, Lf) 
  | update (Br((a,x),t1,t2),b,y) = 
    if b<a 
    then Br( (a,x), update (t1,b,y),t2)
    else 
    if a<b
    then Br((a,x),t1,update(t2,b,y))
    else Br((a,y),t1,t2);
    
		 

      
(*
	    Alice,6
	Tobias, 2	Gerald, 8
	    Lucy, 9
      
	  Gerald, 8
    Alice, 6	Lucy, 9
  Tobias, 2
      
  The Two trees differ in that they are constructed in different order
  In reality they correspond to different graphs, such that there is no link
  between Tobias and Lucy, while it was supposed to be there 
      
*)
(* ----------- Exercise 7.2 -----------*)

exception Collision

(* Already Present  at the given index*)


fun insert (Lf, b:string, y) = Br ((b,y), Lf, Lf) 
  | insert (Br((a,x),t1,t2),b,y) = 
    if b<a 
    then Br( (a,x), insert (t1,b,y),t2)
    else 
    if a<b
    then Br((a,x),t1,insert(t2,b,y))
    else   (* a = b*)
    if raise Collision;
    
(* ----------- Exercise 7.3 -----------*)
(*
  The issue is that to eliminate the exception one must either:
  
  a) We Could lookup the value first and then if the result is negative, then 
  update the tree. This is computationally difficult
  b) Return some tree or none. This is difficult in terms of defining some and 
  None for trees. 
  
*)

(* ----------- Exercise 7.4 -----------*)
(* 
  1) Take the given value
  2) Copy the tree up to the branch where it's located.
  3) If the Given pair of value Key are what we need to remove check
  4.1) if we have a leaf, then terminate
  4.2) if we have a subtree with only one nonleaf child then bring the 
  child up
  4.3.1) If the element we're about to delete has two children:
  Find the Smallest Element in the subtree to the right and cons
  it into the tree
  4.3.2) Delete the smallest element of the right subtree
  
  
  This approach will copy over one element and then do the same process
  of deletion over a different value. So this is in the Best case just a 
  Search (O(n)) in the worst case it should be double that plus the cost 
  of searching fro the smallest element to the right. Which isn't that bad. 
  
*)
(* ----------- Exercise 7.5 -----------*)

fun getel [x] = x;
fun getkey (v,k) = k;
fun getval (v,k) = v;

exception OutOfBounds

fun findmin Lf = raise OutOfBounds
  | findmin (Br((v,k),Lf, _)) = (v,k)
  | findmin (Br((v,k),t1,_)) = if k < getkey findmin t1
  then (v,k) 
  else findmin t1;

fun delete (Lf, _, _ ) 	    		= Lf
  | delete (Br((v0,i0), Lf,Lf)), v,k) 	= 
    if v0=v andalso k=i0
    then Lf 
    else Br((v0,i0), Lf,Lf)
  | delete (Br((v0,i0), t1, Lf),v,k) 	= 
    if v0=v andalso k=i0 
    then t1
    else delete(t1, v,k)
  | delete (Br((v1,i1),t1,t2),v,k) 	= 
    if v1 = v andalso k=i1 
    then 
      let val (vmin,kmin) = findmin t1
      in
      delete (Br((vmin,kmin),t1,t2),vmin, kmin)
      end 
    else 
      Br(Br((v1,i1),delete( Br((v2,k2),v,k)),delete(Br((v3,k3),t31,t32),v,k);
      
      
(* ----------- Exercise 7.6 -----------*)
(*

  Every function: Preorder, InOrder and PostOrder has a worst-case scenario, 
  
  For the Preorder and Inorder that is when the first subtree contains all the
  branches, because append is linear in the length of the first argument. In 
  the worst case, assuming that appending an empty list is also computationally
  expensive, we need to evaluate Preorder and Inorder on n subtrees. Each 
  requires appending, which takes O(n-1) time. 
    
  In that case the time complexity  is just the sum of Linear terms, from one
  to the size of the tree (or a number porportional to it
   e.g. 
   
   O(n^2) = n/k*(O(n-1) + O(n-2) + O(n-3) ... O(1) ) 
   
   Which is an Arithmetic progression
   
  The same is true of PostOrder. Its worst case scenarios are
    1) All elements in left subtree,
    2) All elements in Right subtree
    
  Because of appendig two lists in the first argument which takes 
  O(length of Right list)  +  O( Length of Left List) 
  
  and considering that 
    Length of Right List + Length of Left List is just the size of 
    the tree, we can infer that it's always computationally O(n^2)
    (for a blanaced tree that is just n/2*n/2 = n^2/4)
    
*)

(* ----------- Exercise 7.7 -----------*)

(* Let B(n), M(n) and E(n) be the time complexities of Preord, inord and 
and postord.

  preord:
  
  Base Case: 		B(1) = 1
  
  Recureence Relation:  traversing a tree of size n requires one consing and
  traversing trees of sizes m and k. Because the subtrees can't have more 
  elements than the initial tree, we can say that m+k+1 = n, e.g.
  m+k = n-1
  
  To bribng this to a single variable recurrence relation, let's consider that
  we're looking for an upper bound. The worst case scenatio for tree traversal 
  ia when the tree is unbalanced, e.g. when m=1 for every subsequent recursive 
  call.
  
  Thus 
  
	B(n) = 1 + 1 + B(n-1)    		(Recurrence Relation) 
	
  
  Table of Values of B(n) 
  
  n	B(n) 	2*n - 1
  1	1	1
  2	3	3
  3	5	5
  4	7	7
  5	9	9
  6	11	11
  7	13	13
  8	15	15
  9	17	17
  10	19	19
  11	21	21
  12	23	23
  13	25	25
  14	27	27
  15	29	29
  16	31	31
  17	33	33
  18	35	35
  19	37	37
  20	39	39
  21	41	41
  22	43	43
  23	45	45
  24	47	47
  25	49	49
  26	51	51
  27	53	53
  28	55	55
  29	57	57
  30	59	59
  -
  
  Let F_b = { B(n) = 2*n-1 } 
  
  Prove: 		F_b(n). 
  
  Proof by Mathematical induction
  
  Proof: 
  
  Base Case: 		B(1) = B(1) 
  
  Inductive Step: Assume F_b(k) is true. i.e. B(k) = 2k - 1 ----------------(1)
  
  Prove F_b(k+1) is true. 
  
   B(k+1) = 2+B(k) 			By Recureence Relation
	  = 2 + 2*k-1			By (1) 
	  = 2*(K+1) -1 			By Հայաստանի Հանրապետության Միջնակարրգ
					կրթության Ծրագիր
  Which is of the form of F_b(k+1). Hence Proof is complete.
  
  Thus The Time complexity of Preord is O(n) 
  []
 
  
  indord: 
  
  Base Case: M(0) = 1 
  
  Recurrence Relation: One call of inord consists of consing the value
  To a tail recursive call of inord over the right tree. THis is 
  Computationally the same as doing a preord and only after evalating 
  Every call adding 1. In other words the computational complexity of 
  Inord and preord are the same. Thus the argumebnt given for preord is 
  valid also for inord, and yeilds the same result
  
  e.g. Inord is also O(n)
  
  []
  
  Postord:
  
  Base case: E(0) = 1
  
  Recurrence Relation: Following a similar argument, postord is different
  from inord by just the order of evaluation and construction. If we were 
  Adding the values or performing some other operation that takes two 
  operands and returns one, this would be more efficient space-wise, but time-wise
  This is exactly tyhe same as Inord and Preord so:
  
  Time complexity of Postord is O(n) 
  
  []
*)
(* ----------- Exercise 7.8 -----------*)

(* Necessary Helper Functions*)
infix tcons;
fun v tcons Lf = Br (v, Lf, Lf)
      | v tcons (Br (w, t1, t2)) = Br (v, w tcons t2, t1);
(* quadratic Solution
exception NotFound
    
fun sub (Lf, _) =  raise NotFound
      | sub (Br(v,t1,t2), k) = 
	if k=1 then v
	else if k mod 2 =0
	  then sub (t1, k div 2)
	  else sub (t2, k div 2);
	  
fun listtoarray [] = Lf
  | listtoarray (x::xs) = x tcons listtoarray xs;	  

fun arrtolist (t:'a tree,k:int) = if (k>count t) 
  then [] 
  else [sub (t,k)] @ arrtolist(t,k+1);
*)

(* To turn a list to an Array, need to input arrtolist(arr,1);*)

(*
fun tail [] = []
  | tail (x::xs) = xs;
  
fun delfirst t = 
  | listtoarray ( tail ( arrtolist (t,1) ) );
*)

(* In other words turn the array into a list, thwor away the tail and 
construct an array out of the given list. 

Should be a better way.... 

This consists of calling the arrtolist Which is quadratic (considering 
it has to traverse the treee linearly fro every value. 
*)

(* ----------- Exercise 8.1 -----------*)

fun sw f x y = f y x;

(* This is a curried function that takes another function - f as its argument
and applies f(x)  to y, in such a way that it's the same as first applying it 
to y and then to x

In Layman's terms this swaps arguments supplied to arguments. If for example 
was the function that Knighted Prof. Paulson, it would actually have made someone
Named Knight a Prof. Paulson.
*)

(* ----------- Exercise 8.2 -----------*)

fun lexorder (x1, y1) (x,y) =
  x1 < x orelse ((x1 = x) andalso y1<y);
  
(* Using this function and supplying it to the more general insertion sort 
Algorithm provided in the lecture notes, is going to allow to sort a list 
of pairs. If these pairs were for exmaple representing the values of 
two-character stribngs, then the result would be Dictionary ordered list
of two-character strings. 
*)

(* ----------- Exercise 8.3 -----------*)

fun map2 f [] 		= []
  | map2 f ([]::xss) 	= map2 f xss
  | map2 f ((x::xs)::xss)	=    ((f x) :: hd(map2 f ([xs]))) :: map2 f xss; (*(f x)::(map2 f (xs::xss)) ;*)
  
  
(* Sample INputs*)
+
map2 (fn x=>x+1) [[1,2,3],[1,2,3]];


(* ----------- Exercise 8.4 -----------*)

datatype 'a option	= NONE | SOME of 'a;

fun optmap _ NONE 	= NONE		(* Nothing to map to*)
  | optmap f SOME (x) 	= SOME (f x);	(* Return Something*)
  

(* ----------- Exercise 9.1 -----------*)
(* Required for section on Sequences*)

datatype 'a seq = Null
		| Cons of 'a * (unit -> 'a seq);
		
fun head (Cons(x,_))	= x;
fun tail (Cons(_,xf))	= xf();
fun interleave (Null, ys) 		= ys
  | interleave (Cons(x,xs), ys) 	= Cons(x,fn () => interleave(ys,xs()));
	
fun seqmap f Null 	 = Null
  | seqmap f (Cons (x,xs)) = Cons (f x,(fn () => seqmap f (xs()) )); 
  
(* ----------- Exercise 9.2 -----------*)
(* It can be theoretically generalised to accept a pontentially infinite 
sequence of sequences and return a sequence. Unfrotunately even if we 
interleave the two sequences we'll get only a sequence of the first values
of each sequence, .e.g. the heads. 

This is perfectly acceptable, since you can't expect to fully output one of 
the lazy lists, much less output that in full. However unfortunately if we 
try to concatenate the sequences of all mulitples of all primes we won't 
receive the Integers, but rather a sequence of all primes.

*)

(* ----------- Exercise 9.3 -----------*)

fun change (till, 0)		= Cons([], (fn () => Null))(* Empty Sequqnece*)
  | change (Null, amt)		= Null			   (* Null*)
  | change (Cons(c,till),amt) 	=
    if amt<c then change (till(),amt)
    else 
      let fun allc Null	 		= Null
	    | allc Cons(cs,css) 	= Cons((c::cs), allc (css()))
      in 
	append(allc (change (c::till), amt-c),change (till,amt) )
      end;
      
      
     
datatype 'a ltree 	= lLf
			| lBr of 'a * (unit -> 'a ltree) * (unit -> 'a ltree);
			
fun ltreetoseq lLf 	= Null
  | ltreetoseq (lBr(vl,lt1f, lt2f)) =
  Cons( vl, fn () => (interleave (ltreetoseq( lt1f() ), ltreetoseq( lt2f() )) ) );
  
  
(*		Tripos Question 1	*)




datatype 'a fan = Wave of 'a *('a fan list)

(* -------------Helper Functions*)
fun revapp ([],ys) = ys
  | revapp (x::xs, ys) = revapp (xs,x::ys);
  
fun map f [] 		= []
  | map f (x::xs) 	= (f x)::(map f xs);
  
fun evaland [] 		= true
  | evaland (b::bs) 	= b andalso evaland bs;

(* ------------------Flip itself*)
fun flip (Wave(lbl, wvs)) = Wave (lbl, revapp( map flip wvs)) 

(*----------------- Paint itself*)

fun paint g (Wave (lbl, wvs)) = 
    Wave ( g lbl, map g wvs);
    
    
    
(*-------------------- Same_Shape*)

exception NonEqualLists

fun same_shape (Wave (_,[]),Wave (_, [])) 	= 
  true

  | same_shape (Wave (_,_),Wave (_, [])) 	=
  false

  | same_shape (Wave (_,[]),Wave (_, _)) 	=
  false
  
  | same_shape (Wave (_, w1::ws1), (_, w2::ws2))=
  let 
    fun zip ([],[])       = []
    | zip ([],_) 	= raise NonEqualLists
    | zip (_, []) 	= raise NonEqualLists
    | zip (x::xs,y::ys) = 
    (x,y)::zip (xs,ys)
  in
    same_shape (w1,w2) andalso evaland(map same_shape  zip (ws1,ws2))
  end;

  
  
(* --------------------------Types of Flip*)
(*
flip 		'a fan -> 'a fan
paint 		'a fan -> 'a fan
same_shape 	'a fan -> bool
*) 

fun foldr f ([], e) 	= e
  | foldr f (x::xs, e)  = f(x, foldr f (xs, e));
  
fun paper (Wave (x,fs), q) = foldr paper (fs, q+1);

(* 
   Foldr applies a unction to all of its subordinates making nested tuples
   paper applied to a fan, will generate nested tuples that have the same 
   structure as the given tree itself. 
 *)
  
