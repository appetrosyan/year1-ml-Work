fun exph (x:real, z:real, n:int) :real = if n=0 then z else exph(x,1.0+x*z/(Real.fromInt n),n-1); (* Initial Value z=1.0*) 
fun eapprox (n) = exph (1.0,1.0,n-1);
fun exp (x:real, n:int) = exph(x,1.0,n-1);
fun gcd (a:int, b:int) = 
if a=b then a
else if a>b then gcd (b,a)
else if a=0 then b
else if (a mod 2 = 0 andalso b mod 2 = 0) then 2*gcd (a div 2, b div 2)
else if (a mod 2 = b mod 2) then gcd (b div 2 - a div 2, a)
else if a mod 2 = 1 then gcd (a, b div 2)
else gcd (a div 2, b); (*if b mod 2 = 1*)