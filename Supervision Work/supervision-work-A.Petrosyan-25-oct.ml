(* Supervision Work For A. Petrosyan, for the Sunday 18-th October Deadline

*)

(* -------------------------- Question 1 ----------------------------------- *)
(*
  Brief Notes 
  Will send Via separate email.  
*)

(* -------------------------- Questions 2-5 -------------------------------- *)

(* ----------- Exercise 4.2 -----------*)
fun signsplit ([],ys,zs) 	= (ys,zs) 
  | signsplit (x::xs,ys,zs) 	= 
  if x<0 
    then signsplit(xs,x::ys,zs)
    else signsplit(xs,ys,x::zs);
      
(* ---------Example Inputs ------------*)
    
signsplit([~1,3,4,~2,5,0,0,~6],[],[]);
signsplit([~231,32,2324,~3*(~2),~5,~0,0,~6],[],[~4]);
        
    
    
(* ----------- Exercise 4.3 -----------*)
(*
fun zip (x::xs,y::ys)	= (x,y)::zip(xs,ys) 
  | zip ([],[])		= [];
  
  Is inefficient, the pattern matching is non-exhaustive, it assumes that the 
  user is in the right mind and won't try to "zip" two lists of different 
  lengths.  As opposed to the zip in the Lecture notes, this one will raise
  an exception if that scenario occurs, instead of just returning an empty 
  list. 
*)




(* ----------- Exercise 4.6 -----------*)
(* We are given f = fn: 'a*'b -> 'b*'a and
		g = fn: 'a-> 'a list
   Knowing nothing else we can predict what the input and output should be if
   we evaluate f(1,true)  and g (0).
   
   Simply speaking we know that the result of f is a tuple, the second component
   has the same type as the first argument, thus it is going to something of 
   of the type (bool, int) e.g. for example (true, 1).
   
   The same logic aplies to g. The answer might have the form of a list of 
   integers. like [] or [1,2,3,4,5]. 
*)

(* ----------- Exercise 5.2 -----------*)
fun getmin [x] 		= x
  | getmin (x::xs) 	= 
      if x > getmin (xs) 
	then getmin (xs) 
	else 	x;
      
  (*complains of non-exahustive pattern-matching, unfortunately stating a 
  a pattern for an empty list will do away with polymorphysm
  *)
  
  (* Sample inputs*)
  getmin [3,1,3,2,4,5,1,2,3,0];  
  getmin [~2,3,~275,3,4,2,3,1];

fun selSort []	 	= []
  | selSort (x::xs) 	= getmin (x::xs) :: selSort xs;
  
  (* Sample Inputs *)
  selSort [ ~1,2,~1,3,2,1,4,5,6,12,3];
  selSort [1,2,3,4,5];
  

(* ----------- Exercise 5.1 -----------*)
(*
  Let selSort be the Funtion. Each recursive call of the function consists of
  (a) calling getmin on the rest of the list and calling itself on a list of 
  length n-1, if the initial length of the list was n.
  
  *Lemma 1	: prove that time complexity of getmin is O(n).
  
  *Proof	: Let t(n) be the time it takes to find the minimum element 
  of a list of length n. For the trivial case of a single element list it takes 
      t(1) = 1 				
      t(n) = 1 + t(n-1). 		By Def. of getmin (see ex. 5.2) 
      
  Let's tabulate the result 
    n  t(n) 
    1 = 1
    2 = 2
    3 = 3  
    4 = 4
    5 = 5
    6 = 6
    7 = 7
    8 = 8
    9 = 9
  Assume closed form is t(n) = n-1;
  
  1) t(1) 	= 1-1 = 0 
  2) Let F(i) == {t(i) = i }  
     Assume F(k-1) is true.
  3) Prove F(k).
     t(k) = 1 + t(k-1)		  By recursive relation 
	  = 1 + k-1 	          By Inductive assumption
	  = k
      Which is of form F(K) 
  *Lemma 2	: prove that time complexity of selSort is O(n^2).
  
  *Proof	: Let T(n) be the Time complexity of selection sort. 
  By definition of selSort, T(n) = t(n) + T(n-1) 
				 = n + T(n-1) By Lemma 1. 
  and in the triial case T(1) = 1. 
  
  Let's tabulate known values
  n T(n) 
  1 = 1
  2 = 3
  3 = 6
  4 = 10 
  5 = 15
  
  Assume Closed form is n(n+1)/2 
  
  1) t(1) 	= 1*2/2 = 1
     t(2)	= 2*3/2 = 3
  2) Let F(k) 	== {T(k) = k(k+1)/2}
     Assume F(k) true.
  3) Prove F(k+1).
     T(k+1) = k+1 + T(k) 	By Recursive Relation (see ex. 5.2) 
	    = k+1 + k(k+1)/2
	    = 1/2 ( k(k+1) +2(k+1) ) By K. F. Gauss
	    = (k+1)(k+2)/2 		
    Which is of the Form of F(k+1).
    
  Hence SelSort is of O(n^2) complexity. 

(* ----------- Exercise 5.4 -----------*)

fun issorted [] 	  = true
  | issorted [x] 	  = true
  | issorted (x1::x2::xs) = x1 <= x2 andalso issorted (x2::xs);

(* Sample Inputs*)
  issorted [1,2,3,4,5,6,7];
  issorted [1,2,3,4,5,7,6];
  
fun bsort [] 		= []
  | bsort [x] 		= [x]
  | bsort (x1::x2::xs)  = 
  if issorted (x1::x2::xs) 
	then x1::x2::xs
  else if x1>x2   
	then bsort (x2::x1::xs)
	else bsort ( x1:: bsort (x2::xs));
  
(* Sample Inputs*)
  bsort [1,2,3,4,5,6,7,91,1,2,3,123,2,24,5,5,456,4,5];
  bsort [2,2,2,1,23,,12,3,23,23212,32,32323];
  
(* ----------- Exercise 5.3 -----------*)

(* The Time complexity of Bubble sort heavily Depends on input. 
Its best case input is an already sorted list. IN that case it takes as much 
time as it takes to iterate through all the elements and check if the list is
sorted. This is done in O(n), by the issorted function. 

  * Lemma 3: issorted checks with O(n) time complexity.
  
  * Proof  : Let t'(n) be the time complexity of issorted check. Its recursive
  definition gives us t(n) = 1 + t(n) and t(1) = 1, which is the same as in 
  Lemma 1. There we have proved that a function with this time complexity has 
  the closed form of t(n) = n, (except t(0) = 1 as well). 
  
However, Bubble sort has a distinct disadvantage if the list is sorted in the 
opposite direction. e.g. descending instead of ascending. In that case, we know

  (a) Each evaluation consists of swapping the first values. This will take 
  the first time n, then n-1 and so on, until the list is sorted. 
  
  (b) After the k-th swap the list of n-k elements is checked against being 
  sorted. In our case this intorduces O(n-k) time wasted on checks. e.g. 
  
  In the Long run adding those two factors together we get
  
  bsort = (\sum {from k=1 to k=n } 1 + (n-k))*n
  e.g. it does n passes (even in the worst case scenario, we know that the 
  biggest element will be at the end of the list, repeating twice - two etc)
  each pass consists of checking an n-k element list for being sorted and 
  swapping first two elements
  
  This gives us roughly O(n^2). 
*)

(* ----------- Exercise 6.1 -----------*)
datatype weekday  = Monday
		  | Tuesday
		  | Wednesday
		  | Thursday
		  | Friday
		  | Saturday
		  | Sunday
;
  
  
(* This datatype is essentially redundant. The main use of enumerators is 
  apt when a simple numeric indexation is impossible without loss of 
  information For example: when dealing with a changing number of colours 
  (bordeaux and red aren't the same colours). Here, the neumber of weekdays
  doesn't change the order opearation can be defined without the need for 
  a specific weekday.
*)

(* ----------- Exercise 6.2 -----------*)

datatype 'a tree  = lf
		  | br of 'a * 'a tree * 'a tree;
		  
fun sumLabels lf = 0
  | sumLabels (br(value, subtree1, subtree2)) = 
  value + sumLabels subtree1 + sumLabels subtree2;

  sumLabels (br(1, br(2, br(4,lf,lf) , br (5,lf,lf)),br(3,lf,lf)));
  
(* ----------- Exercise 6.3 -----------*)

(* The Given Function creates a binary tree out of all numbers until n+1. 
The Numbers are arranged in the following way

1			    1
2			 2   	3
3		    4	  5 	6     7 
4		8 9   10   11  12 13 14 15
etc. 
In other words the n-th level branch contains all numbers from 2^(n-1) to
2^n - 1. In the above example the 4-th row has all the successive numbers
between 2^3 and 2^4-1

(* ----------- Exercise 6.4 -----------*)
datatype expr	= Number of real
		| Variable of string
		| negation of expr
		| multiplication of expr*expr
		| adiition of expr*expr
		
(* ----------- Exercise 6.4 -----------*)

exception var_occurred
fun eval (variable (value))		= raise var_occurred
  | eval (Number (value))		= value
  | eval (multiplication (expr1, expr2)	= (eval expr1) * (eval expr2)
  | eval (addition (expr1, expr2) 	= (eval expr1) + (eval expr2) 
  | eval (negation (expr) 		= (~1.0)*(eval expr1);
  
(* -------------------------- Question 5 ----------------------------------- *)
(*
O(1) 
    eval in the above example if given simple input. 
    gettail, e.g. return the first element of a list.
O(n)
    counting the number of elements in a list. Summing all the elements of a 
    list
O(n Log n) 
    Mergesort. LEt's assume that mergesort is fed lists of length 2^n (for all
    other input we can add a few zero eklements at the beginnging. 
    
    Each recursivecall calls two instances of mergesort  and checks every 
    sublist for being sorted (or single element) 
    
    Thus we have 2 recursons to n/2 and one linear check every time
    e.g.
      T(n) = T(n/2) +n
    
    In the trivial case we have T(1) = 1
    
    n 		T(n) 		n log_2 +n
    1		1		1
    2		4		4
    4		12		12
    8		32		32
    16		80		80
    32		192		192
    64		448		448
    128		1024		1024
    256		2304		2304
    512		5120		5120
    1024	11264		11264
    2048	24576		24576
    4096	53248		53248
    8192	114688		114688
    16384	245760		245760
    32768	524288		524288
    65536	1114112		1114112
    131072	2359296		2359296
    262144	4980736		4980736
    524288	10485760	10485760
    1048576	22020096	22020096
    2097152	46137344	46137344
    4194304	96468992	96468992
    8388608	201326592	201326592
    16777216	419430400	419430400
    33554432	872415232	872415232
    
    
    Assume closed form is n*log_2 n +n
    
    1) T(1) 	= 1*0+1
    Let F(j) 	= {T(j) = j*log_2 j +j}
    2) Assume that j<k => F(j) is true
    3) Prove F(k+1)
      T(k+1) 	= 2*T((k+1)/2) + k+1 	By Recursive Relation
		= 2*(k+1)/2 *log ((K=1)/2) + 2*(k+1)/2 +k+1
		= (k+1) (log(K+1) - 1) + (k+1) +k+1
		= (k+1) (log(k+1) + (k+1) 
    Which is of the Form of F(k)
    
O(n^2) 
    We have proved that selsort is a O(n^2)  algorithm in Exercise 5.1
O(2^n) 
    
    A function to generate all subsets of a given has to generate 2^n lists, 
    which essentially means that the time and space complexities of that 
    algorithm are 2^n.
    
*)

(*    Some ASCII Art to rest your eyes. 
      This really is harder than it looks
              ___.-~"~-._   __....__
            .'    `    \ ~"~        ``-.
           /` _      )  `\              `\
          /`  a)    /     |               `\
         :`        /      |                 \
    <`-._|`  .-.  (      /   .            `;\\
     `-. `--'_.'-.;\___/'   .      .       | \\
  _     /:--`     |        /     /        .'  \\
 ("\   /`/        |       '     '         /    :`;
 `\'\_/`/         .\     /`~`=-.:        /     ``
   `._.'          /`\    |      `\      /(
                 /  /\   |        `Y   /  \
           jgs  J  /  Y  |         |  /`\  \
               /  |   |  |         |  |  |  |
              "---"  /___|        /___|  /__|
                     '"""         '"""  '"""

*)

(* -------------------------- Question 6 ----------------------------------- *)
 
  
fun concat (x,[]) 	= x 
  | concat ([],x) 	= x 
  | concat (x::xs, y) 	= x::concat (xs,y);
fun rotator ([], n:int,l:int)	=[[]]
  | rotator (x::xs, n, l) = 
  if n<l 
    then concat([x::xs],rotator(concat(xs,[x]),n+1,l))
    else [];

(* This algorithm generates Cyclic permutations by appending to 
an empty list (in the base case) a list of the same length with the 
first element at the end and the previous one at the beginning. 

BEcause there is no possibility in ML to revers pointers and mutate
data this very space and time inefficient (it takes time to copy over 
a list which it essentially does every single time. 

Now concerning time complexity:
There need to be done (n-1) permutations each forcibly copying over 
a list of length n-1. Last this whole list of n lists needs to be 
concatenated together. 

Thus the computational complexity can be given as
T(n) = n + (n-1)*(n-1).
     = n^2 - n +1 
     e.g.
T(n) = O(n^2).


(* -------------------------- Question 7 ----------------------------------- *)

(* Just to remember (refresh) quick sort*)
(*
fun qsort [] 			= []
  | qsort [x] 			= [x]
  | qsort (a::bs) 		= 
  local fun partition (l,r,[])	= 
	      (qsort l) @ (a::qsort r) 
	  partition (l,r,x::xs) = 
	      if x<= a then partition (x::l,r,xs)
	      else 	    partition (l,x::r,xs)
  in partition ([],[],bs) 
  end;
*)
fun select ([],i)	 	= []
  | select ((x::xs),i) 	= 
  if i=0 
    then getmin(xs)
    else  select (xs,i-1);

  
  
(* Helper for least*)
fun getlen [] = 0
  | getlen x::xs = 1+ getlen (xs);
  
(* Modification of Quicksort. *)
fun least ([], _)  			= []
					(*Trivial Case*)
  | least ((a::bs),n)		= 
  let fun partition (l,r,[]):int list=	(*When Done Paritioning check		*)
	    if (getlen l) <n		(*If we have less than n, l has		*)
					(*some of the final output. The pivot is*)
	      then			(*Also one of the n least. 		*)
					(* Take left, find least in right.	*)
		  l @ (a::least (r, n-(getlen l+1)) )
	 
	    else if (getlen l) =n	(*We're in luck, left part is the anwser*)
	   
	      then l
	      else 			(*Right has no elements of interest	*)
		least (l,n)		(*Toss right and find the least in left.*)
		
	   | partition (l,r,(x::xs)) =  (*Paritioning itself*)
	   
	      if x<= a then partition (x::l,r,xs) (*Smaller element goes left*) 
	      else 	    partition (l,x::r,xs) (*Larger element goes right*)
	      
  in partition ([],[],bs) 
  end;

(*sample inputs*)
least ([3,2,1,0,3,2,1,0],3);
least ([1,2,3,4,5,5,6,7],3);			  (*Worst Case*)
least([7,6,5,4,3,2,1],4);			  (*Worst Case*)
least([77,77,77,33,22,44,11,22,11,77,88,66,55],6);

  

  
  

