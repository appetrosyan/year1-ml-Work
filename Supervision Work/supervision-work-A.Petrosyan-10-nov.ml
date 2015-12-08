(* Supervision Work For A. Petrosyan, for the Tuesday 3-rd November 

*)

datatype 'a tree  = Lf
		  | Br of 'a * 'a tree * 'a tree;
		  
(* Required By all exercises*)


(* ----------- Exercise 10.1 -----------*)

(* 

The Drawbacks of Tree-Based Functional Queues are: 

1) Complexity: simple algorithms that can be implemented on lists (or in this 
case a pair of lists becomes less obvious, so that only Simon can do both 
without sufficient dip in productivity.

2) Adding an item into the queue, requires a computationally more ex[pensive 
algorithm. In a pair of lists we need only cons one element at the head. 
With trees, you need to first lookup thee place where the new element  
needs to be added (e.g,. traverse the tree to the respected leaf and replace
with a branch. 

3) A binary search tree occupies much more space. A list allocates space to 
an element and a reference to the next element. A branch contains the key and 
two pointers to its subtreees, so if the value is just another reference it takes 
1.5 times more space. This gets even worse considering that every update operation
xcopies the whole tree (Plus minus one branch).

Advantages:

1) Better worst-case scenaio performance. The efficiency of the Double -list 
queu larely depends on the input. If it's arranged in such a way so that norm
performs the most expensive operation every time (And for that you just need to 
pop/push/pop alternaterly when the queue is sufficiently full), the Binary Tree's 
efficiency  (e.g. all opertaions are O(log n) ) Will become more evident.

2) More predictable. Although The tree based approach will get you slower perfomance 
In general, you know how many operations it will take to push/pop etc. and you know 
that the delay doesn't vary much on the input (e.g. how many pushes and Pops need 
be done , and in what sequnce). 

*)

(* ----------- Exercise 10.2 -----------*)
(* 
This approach intorduces a huge inefficiency, because you don't know at what level 
will you find the answer. To be able to traverse a tree with a breadth first 
algorithm You'd need to enqueue all of its elements of the same level.
Since trees can be very large and the answer might be at one of the top levels,
you'll just be wasting space on the off-chance that you MIGHT 
need it to store the whole queue. 

Linked lists in truth occupy larger space than arrays, but because they only 
occupy it if and when they need it, linked lists are a better option.

*)

(* ----------- Exercise 10.3 -----------*)

(* Definition of a Queue and operations on it*)

datatype 'a queue = Q of 'a list * 'a list;
fun norm (Q([],tls)) 	= Q(rev tls, [])
  | norm q		= q;
  
fun qnull (Q([],[])) 	= true | qnull _ = false;

fun enq (Q (hds, tls),x)= norm (Q(hds, x::tls));
fun deq (Q(x::hds, tls))= norm (Q(hds, tls));



(*Beadth - First Tree traversal*)

fun breadth q = 
  if qnull q then []
  else 
    let 
      fun traverse (Lf, qu) 		= breadth (deq qu)
	| traverse (Br(v,t,u), qu) 	= v::breadth(enq(enq(deq qu, t), u))
    in 
      traverse (qhd q, q)
    end;
    
(* ----------- Exercise 10.4 -----------*)
    
 
(* Iterative deeepening Doesn't give you any benefits if the branching factor
is near one, e.g. the tree is unbalanced. 

In that case Breadth-first search also doesn't make any sense, because the breadth
is also approximately 1. 

Depth - First search, however does effectively the same as breadth first on an
unbalanced tree, but does so more efficiently and it certainly deosn't do
any extra work as does iterative deepening, and doesn't occupy more space.

*)

(* ----------- Exercise 10.5 -----------*)

(* It's a "functional array", where the label is the same as the index. 
The term functional array is inappropriate, because the term array in all 
languages means just a block of adjacent memory cells, which is hardware 
specific. 

Here the term 

Binary Allocation Tree would be more appropriate, because it

a) reflects what it does: it allocates data to a binary tree, 
allowing for simple lookup etc. 
b) reflects most of its properties. This is a binary tree, albeit
it has little to do with search, other than the ordering of the 
items pushed in. 

*)



(* ----------- Exercise 11.1 -----------*)


(* 
From here on out, assume that all lists fed into poly are sparse 
representation. e.g. they are decreasing, contain only one element once and are
in decreasing order.
*)

(*Important!!! fed lists need to be sets*)
fun member (a,[]) = false
  | member (a, (x::xs)) = 
  a=x orelse member (a,xs);
  
(*A set is a subset if all of its members are members of the superset*)  
  
fun subset ([],_, true) = true		(* We're done, after checking*)
  | subset (_,_,false)  = false		(* one element is not shared*)
  | subset ((x::xs), ys,b) =		
    subset (xs, ys, b andalso member (x,ys));

(*OBviously input should be of the form (xs,ys,true)*)
    
(* A set intersection comprises of all the elements of the first set that 
belong to the other*)

fun intersect ([],_) = []
(*| intersect (_,[]) = []*)  (*Redundant*)
  | intersect ((x::xs), ys) = 
  if member (x,ys) 
	then x::intersect(xs,ys)
	else intersection(xs,ys);

(* A set Union is the Set that contains all the elements of the first and 
all the elements of the second, the trick here is not to intorduce duplicates*)

fun union ([], ys) = ys
 | union (xs, []) = xs
 | union ((x::xs), ys) =
 if member (x, ys) then 
 union (xs, ys)
 else union (xs, x::ys);
 
(* We Accumulate unique elements into the second list, throw away duplicates
When acting on sparse representation it would be reuqired also to sort the end 
result. 

However in that case we need to know what an order is and specifically, we'll 
need to assume that the list has the form of tuples of int*real type. 
*)
(* ----------- Exercise 11.2 -----------*)

(*  Polysum

adds the two polynomials explicitly checking that the end result is 
going to be in decreasing order. when there are tow identical elements a new 
single element is constructed (with a new Floating Point coefficient.). 
Additionally, the sum of the two sparses may not be sparse, if the resultant 
Float is zero, but that is explicitly a separate case, where no element is
consed into the final result.

PolyProd

Termproduct, a subordinate procedure presrves the sparse representation. 
There is no possibility that a product of two nonzero numbers be zero, the 
product of integers is greater than both numbers multiplied, so the result is
decreasing. Although it is possible to create duplicates the fact that the 
multiplied numbers are in decreasing order means its impossible to obtain the 
same degree member in more than one termprod call. 

polysum preserves sparse representation, we've just poved that the polynomials
it will be fed will be sparse, => The result will also be sparse.

*)

(* ----------- Exercise 11.3 -----------*)

(* Polysum takes two polynomials iterates over the longest and then copies over
the smaller one. Which means that the recurrence relation ah the form 

	T(n,m) = 1+ T(n-1,m) + m
	
Let's consider this as a univariate function of just n, 
e.g. introduce  
	t_m (n) Such that 
	
	T(n,m) = 1 + t_m(n) + m = 1 + n + m

and let's prove that it is linear with respect to n. For this let us consider 
the action of polysum separate from the end copying over of the second polynomial.
Let the Runtime be
	t_m(n) = 1+ t_m(n-1) 
	
It is obvious that t(n) has the closed form  t(n) = n. 
	
	T(n,m) = 1 + t_m(n) + m = 1 + n + m  	By Definition of t(n)

Which has the form 

	O(n+m) 
	
[]
*)
(* ----------- Exercise 12.1 -----------*)


(* An int ref list is a list of references to ints, e.g. this is a list that 
can have any individual value of it changed (or Mutated) but cannot change 
itself. E.g. you can't rearrange a list of refs, in terms you can't add an 
element or throw an element away. You can individually swap the values of the
refs to rearrange the values pointed to, but the references will still point
to the same memory cells. 
*) 

(* For example*)
val a = [ref 1, ref 2, ref 6, ref 4];

(* A ref int list is a reference to a mutable list. You can't directly change 
the values of each element, but by assigning the list a different value get a
some similar results. Also, the list is mutable itself, which means that you 
can add elements, throw elements away. 

*)

(* ----------- Exercise 12.2 -----------*)


(* The same thing as in lecture 2, takes linear time, and unit space*)
fun npower (e,n) = 
	let val ee = ref e
	    val xx = ref n
	in 
		while !xx<>0
			do (ee:= !ee*e; xx:= !xx-1);
		!ee;
	end;
(* The Recursive npow is much more efficient*)


(* ----------- Exercise 12.3 -----------*)

(* While (c_1, B) do C_2 will evaluate or execute c_1, then check B, if it's 
true it will proceed to c_2 and then return and do this all over again, until
B is false, when it will terminate. 

*)

(* ----------- Exercise 12.4 -----------*)

(* This function takes two references as arguments and swaps them by values. 
It's polymorphic, in terms it's type is 'a ref * 'a ref -> unit*)

fun swap (xr,yr) = 
  let zr = ref !xr
  in 
    xr:= !yr;
    yr:= !zr;
    ()
  end;
  
(* In terms of procedural programming, this is a "Void" function*)

(* ----------- Tripos Question 1 -----------*)

(* A binary search tree represents a dictionary, where traversal along the 
tree is lookup using the binary search algorithm. Provided keys can be ordered
this lookup takes logarithmic time. 

The Update function needs to find the appropriate place for a key-value pair. 
That is basically lookup with creating a new binary tree, with just the path 
in it. SO this basically means that the time it takes is O(log n) because the 
most computationally expensive are the lookup function and the copying of the 
path to the updated tree's current branch (the one we updated). 
*)

datatype 'a mtree = lf
		| = br of 'a * mtree ref * mtree reference
		
(* THis datatype allows to create binary search trees withou8t copying over 
exisiting branch nodes, but rather by updating the subtrees. 

Lookup on the other hand works exactly the same as if it were completely 
functional. 

*)
exception notfound

fun mlookup (br((a,x), t1r, t2r), wh) = 
	if wh < a
		then mlookup (!t1r, wh)
	else wh >a 
		then mlookup (!t2r, wh)
	else x
  | mlookup (lf,_) = raise notfound;
  
fun mupdate (lf, b,y) = br((b,y), ref lf, ref lf)
  | mupdate (br((a,x),t1,t2), b,y) =
	if 	b<a 
		then t1:= mupdate (!t1, b,y) 
	else if b>a
		then t2:= mupdate (!t2, b,y)
	else 
		br((a,y), t1,t2);
		

(* ----------- Tripos Question 1 -----------*)

(* ML allows having mutable data via references, or links to cells in memory

THe contents of memory can be updated
	xr := New_Value_of_Xr_expression;
	
The values can then be accessed, and put into expressions:
	!xr; while !xr>0 do C1, C2, 
	
THere are also arrays which can be made using the Array.array function, also
using tabulate, (n,f) so that f(i) equals Arrays.sub(A,i).

THere's also the While loop, which allows to implement truly iterative 
features (inherently non-recursive). 

The typical syntax is 

while (condition:boolean) 
do 
	(command1; command2; ... comand n);
	
*)

datatype ’a meal = Snack of ’a 
		 | Lunch of ’a meal * ’a meal 
		 | Feast of ’a meal * ’a meal * ’a meal;

		(*Given Code*)
fun snacker m = 
let val l = ref [] 
fun munch (Snack x) 		 = (l := x :: !l) 
  | munch (Lunch (m1,m2))	 = (munch m1; munch m2) 
  | munch (Feast (m1,m2,m3))	 = (munch m1; munch m2; munch m3) 
in munch m; 
!l end;

		(* Replacement*)
fun eat (Snack x) 		= [x]
  | eat (Lunch (m1,m2))		= (eat m1)@(eat m2)
  | eat (Feast (m1,m2,m3))	= (eat m1)@(eat m2)@(eat m3);

(* The two functions create a list of snack nodes, which are present in every 
meal. The above function doe s that by directly consing values, the asked for
function does so by recursion and appending the contents of every meal. 

*)

fun gluttony (Snack x) m2 	= m2
  | gluttony (Lunch(s1, s2)) m2 = (Lunch(gluttony s1 m2, gluttony s2 m2))
  | gluttony (Feast(f1,f2,f3)) 	= (Feast(gluttony f1 m2, gluttony f2 m2, 
			gluttony f3 m3));
			
fun glut k m1 m2 = 
 let val km = ref k 
     fun glu (Snack x) m2 = 
	if !km=1 then m2 
	else km:= !km-1; Snack x
       | glu (Lunch(s1,s2)) m2 = Lunch(glu s1 m2, glu s2 m2)
       | glu (Feast(s1,s2,s3)) m2 = Feast(glu s1 m2, glu s2 m2, glu s3 m2)
 in 
    glu m1 m2
 end;
 
(* Using reference type to create a variable external to a helper function
and allow it to decrease the counter each time it encounters a snack. 

When the counter hits one, the first snack to come up (if it does would be 
replaced, no exception if there are fewer than k snacks. THis will create 
an almost perfect copy of m1 (if m1 was a reference we could do that directly)
with the k-th snack node replaced by m2.

*)



(*



                              ,  ,
                         _  _ \`.)\  _  _
                        __)\)`-)   )'|//(  ___
                      _.--'     ___  `' <,',-\
                     /,-._  ,\|'   `-._   `<_
                       /_( (  _       `-.  <_`
                      (.-_\/ _-.  )\,'   \ `.`_
                     ,| /O|:|O\ \/  \;  . \ \<
                   .';|/__/;\__\ |   \.'   \(\`
                  / ; /  .'      |    \     ; \-.
                 / ; /__;__     /      ; ,--.`.`.\
                /.' /" ;   "-. |       |'      ; `.
               /;   !  ;      `|       |        `. `.
               |    \ O; O     |       ;          ;. `.
               \    ,\  ;     /\      /            ;   \
                \ ,'  `._;_.-'  \   .'             :    \
                 '          /    ),'                ;    \
                            |    /                   ;    \
                            >-. /                    ;     |
                           /   |                     ;     |
                       __,-|   |      .-             ;     |
                    ,-' /`. .  \    _;_              ;     |-.
                   ( /`-.      ,"""' `.`-.           ;     /. \
                    `| .'`-.,-/        \            ;'    /   |
                     |    .'`|                      ;         |
                      \      ;                     ;'        /
                       \     `               .--.;'        .'
                        `._.-`-.    .       / (_)\     /.-'
                            hjw `--'-.      | | \| _.-'
                                      `--....\ \--'
                                     ,==._,==.`.`.
                             (\ |\_  \`=. `, /__) )
                             _\`'  `~-`=._.=7.__.'
                            /_.~')    _.'/`;='.`=.
                                /,'),'  /   `     )
                               '  (/    `._.='`=='
                               
                               
		Goodbye Prof. Paulson. 
		
Taken from http://www.heartnsoul.com/ascii_art/disney.txt, because I'm clearly not good enough. 
*)
