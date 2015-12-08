datatype 'a tree  = Lf
		  | Br of 'a * 'a tree * 'a tree;
infix tcons;
fun v tcons Lf = Br (v, Lf, Lf)
      | v tcons (Br (w, t1, t2)) = Br (v, w tcons t2, t1);
(* It acts like sort of a cons for trees, e.g. adds an element to the root*)



fun insert (a:string, Lf) = Br(a, Lf,Lf)
  | insert (a, Br(v, t1,t2)) = 
	case (String.compare (a,v)) of
		GREATER	=> Br(v, t1,insert (a,t2))
	|	EQUAL	=> Br(v, t1,t2)
	|	LESS 	=> Br(v,insert (a,t1), t2);
	
fun member (a:string, Lf) = false
  | member (a, Br(v:string,t1,t2)) = 
	case (String.compare (a,v)) of
		GREATER => member (a,t2)
	|	EQUAL	=> true
	|	LESS	=> member (a,t1);
	
	
	
fun union (Lf, Lf) 		 = Lf
 |  union (Lf, (Br(v,l2,r2)))	 = Br(v,l2,r2)
 |  union ((Br(v,l1,r1)),Lf) 	 = Br(v,l1,r1)
 |  union ((Br(v1,l1,r1)), t2)	 = 
	insert (v1,union (l1,union(r1,t2)));
	
fun inter (Lf, _) = Lf
 |  inter (_, Lf) = Lf
 |  inter ((Br(v,l1,r1)),t2) = 
	if 	member (v,t2)  
	then insert (v, union (inter (l1,t2),inter (r1,t2)))
	else union (inter (l1,t2),inter (r1,t2));
	
	
fun remove (a:string, Lf) = Lf
 |  remove (a:string, (Br(v,l,r)))=
	case (String.compare(a,v)) of
		GREATER => remove (a,r)
	|	LESS	=> remove (a,l)
	|	EQUAL	=> union (l,r);