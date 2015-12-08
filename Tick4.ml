datatype 'a stream = Cons of 'a * (unit -> 'a stream);
fun from k = Cons(k, fn()=> from(k+1));

fun nfold f n x 	= if n=0 then x 
else f( nfold f (n-1) x);

fun sum n m  	= nfold (fn x => x+1) n m;
fun product n m	= nfold (fn x => x+m) (n-1) m;
fun power m n	= nfold (fn x => x*m) (n-1) m;

fun nth ((Cons(curval, rest)), ind) 	= 
  if ind = 1 then curval
  else nth (rest(), ind-1);
  
fun sqrcomp k	= Cons ( k*k, fn () => sqrcomp(k+1) );
  
val squares 	= sqrcomp 1;

nth (squares, 49);

fun map2 f (Cons (x,xs)) (Cons(y,ys)) = 
 Cons( f x y, ( fn () => (map2 f (xs()) (ys())) ) );