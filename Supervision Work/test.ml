(* Supervision Work For A. Petrosyan, for the Sunday 18-th October Deadline

*)

(* -------------------------- Question 1 ----------------------------------- *)
(*
  Brief Notes 
  
*)

(* -------------------------- Questions 2-5 -------------------------------- *)

(* ----------- Exercise 4.2 -----------*)
fun signSplitter ([], [] , []) 		= 
    | signSplitter ([],positives:int list, negatives:int list) = 
	  (postitives, negatives)
    | signSplitter (x::xs, positives, negatives) =
    if x<0 
    then signSplitter (xs, positives, x::negatives) 
    else signSplitter (xs, x::positives, negatives);
    
(* ---------Example Inputs ------------*)
    
signSplitter([~1,3,4,~2,5,0,0,~6],[],[]);
signSplitter([~231,32,2324,~3*(~2),~5,~0,0,~6],[],[~4]);
    
    
    
    
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
  if x > getmin (xs) then getmin (xs) 
  else 	x;

fun selSort []	 	= []
  | selSort (xs) 	= getmin xs :: selSort xs;
  

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
  *Lemma 1	: prove that time complexity of selSort is O(n^2).
  
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
fun bsort [] 		= []
  | bsort [x]		= [x]
  | bsort (x1::x2::xs)  = 
  if x1 < x2 	     then bsort (x1::bsort(x2::xs))
  else 			  bsort (x2::x1::xs);


  
  
    