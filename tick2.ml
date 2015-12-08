fun last [x] = x 
| last (x::xs) = last (xs);
fun butlast [x] = []
| butlast (x::xs) = x::butlast(xs);
fun nth ([],_) = []
| nth (x::xs,0) = [x]
| nth (x::xs, n:int) = nth (xs,n-1);