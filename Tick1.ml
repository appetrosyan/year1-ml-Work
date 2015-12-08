    fun evalquad (a:real,b:real,c:real,x:real) = a*x*x+b*x+c;
    fun facr (n) = if n=0 then 1 else if n>0 then n * facr(n-1) else n*facr(n+1);
    fun factorial (n,total) = if n=0 then total else if n>0 then factorial (n-1, n * total) else factorial (n+1, n * total);
    fun faci(n) = factorial (n,1);
    fun sumt (n):real = if n=1 then 1.0 else 1.0 + 0.5*sumt(n-1);