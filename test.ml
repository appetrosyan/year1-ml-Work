datatype 'a stream = Cons of 'a * (unit -> 'a stream);

fun from k = Cons(k, fn()=> from(k+1));

fun nfold f n x 	= if n=0 then x 
else f( nfold f (n-1) x);

fun ** a b = a*b;

fun nth ((Cons(curval, rest)), ind) 	= 
  if ind <= 1 then curval
  else nth (rest(), ind-1);
  
fun map2 f (Cons (x,xs)) (Cons(y,ys)) = 
 Cons( f x y, ( fn () => (map2 f (xs()) (ys())) ) );
 
fun map f x (Cons(y,xs)) = Cons (f x y, fn () => map f (x) (xs()));
 
fun compare (a,b) = if (a>b) then GREATER 
else if (a=b) then EQUAL
else LESS;

fun merge ((Cons(x,xs)),(Cons(y,ys))) = 
 case compare (x,y) of 
	GREATER => Cons(y, fn () => merge (Cons(x,xs),ys()))
 |	EQUAL	=> Cons(x, fn () => merge (xs(),ys()))
 |	LESS	=> Cons(x, fn () => merge (xs(),Cons(y,ys)));
	
fun pows23f() =   Cons(1, fn()=>  
	merge(
	map ** 2 (pows23f())
	,
	map ** 3 (pows23f())
	)
);
	

fun pows235f() = Cons (1, fn() =>
	merge(
	map ** 2 (pows235f()),
	merge (
	map ** 3 (pows235f()),
	map ** 5 (pows235f())
	)
	)
);

val pows23 = pows23f();
val pows235 = pows235f();