open List;;
type exp =
	Const of int
   | Abs of exp
   | Ident of string
   | Plus of exp*exp
   | Minus of exp*exp
   | Times of exp*exp
   | Div of exp*exp
   | Mod of exp*exp
   | Expo of exp*exp
   | Equal of exp*exp
   | Gt of exp*exp
   | Lt of exp*exp
   | Ge of exp*exp
   | Le of exp*exp
   | T | F
   | Not of exp
   | And of exp*exp
   | Or of exp*exp
   | Implies of exp*exp
   | Tuple of exp list
   | Proj of exp*exp;;

type answer = Const' of int  | T' | F' | Tuple' of answer list;;

let rec exponential (x,n) y = match n with
	0 -> y
   | _ -> if(n>0) then exponential (x,n-1) (y*x) else 0;;

let rho s = match s with
	"iden1" -> Const'(7)
	| "iden2" -> Const'(19);;

let rec eval e = match e with
	Const n -> Const' n
   | Abs n -> let foo a = match eval a with
  				Const'(b) -> b in
  			 let x = foo n in
  			if x>0 then Const'(x) else Const'(-x)
   | Ident x -> rho x
   | Plus(e1,e2) -> let pl a b = match (eval a, eval b) with
  					(Const'(n1),Const'(n2)) -> Const'(n1+n2) in
  				   pl e1 e2
   | Minus(e1,e2) -> let mi a b = match (eval a, eval b) with
  					(Const'(n1),Const'(n2)) -> Const'(n1-n2) in
  				   mi e1 e2
   | Times(e1,e2) -> let ti a b = match (eval a, eval b) with
  					(Const'(n1),Const'(n2)) -> Const'(n1*n2) in
  				   ti e1 e2
   | Div(e1,e2) -> let di a b = match (eval a, eval b) with
  					(Const'(n1),Const'(n2)) -> Const'(n1/n2) in
  				   di e1 e2
   | Mod(e1,e2) -> let mo a b = match (eval a, eval b) with
  					(Const'(n1),Const'(n2)) -> Const'(n1 mod n2) in
  				   mo e1 e2
   | Expo(e1,e2) -> let foo a b = match (eval a,eval b) with
  					(Const'(n1),Const'(n2)) -> (n1,n2) in 
  					Const'(exponential (foo e1 e2) 1)
   | Equal(e1,e2) -> if (eval e1)=(eval e2) then T' else F'
   | Gt(e1,e2) -> let foo a b = match (eval a,eval b) with
  					(Const'(n1),Const'(n2)) -> (n1,n2) in 
  					let (x,y) = foo e1 e2 in
  					if x>y then T' else F'
   | Lt(e1,e2) -> let foo a b = match (eval a,eval b) with
  					(Const'(n1),Const'(n2)) -> (n1,n2) in 
  				 let (x,y) = foo e1 e2 in
  					if x<y then T' else F'
   | Ge(e1,e2) -> let foo a b = match (eval a,eval b) with
  					(Const'(n1),Const'(n2)) -> (n1,n2) in 
  				 let (x,y) = foo e1 e2 in
  					if x>=y then T' else F'
   | Le(e1,e2) -> let foo a b = match (eval a,eval b) with
  					(Const'(n1),Const'(n2)) -> (n1,n2) in 
  				 let (x,y) = foo e1 e2 in
  					if x<=y then T' else F'
   | T -> T'
   | F -> F'
   | Not(e) -> if (eval e)=T' then F' else T'
   | And(e1,e2) -> if (eval e1)=T' && (eval e2)=T' then T' else F'
   | Or(e1,e2) -> if (eval e1)=T'  || (eval e2)=T' then T' else F'
   | Implies(e1,e2) -> if (eval e1)=T' && (eval e2)=F' then F' else T'
   | Tuple(l) -> let rec evalOnTuple l = match l with
   						[] -> []
   					| x::xs -> [eval x]@(evalOnTuple xs) in
   				 Tuple'(evalOnTuple l)
   | Proj(i, tuple) -> let rec projection l i = match i with
					   		0 -> hd l
					   	| _ -> projection (tl l) (i-1) and
					   extr tuple = match tuple with
					 	Tuple(l) -> l and
					   constextr i = match eval i with
						Const' i' -> i' in
					 eval (projection (extr tuple) (constextr i));;


type opcode = CONST of int
			| ABS
			| PLUS
			| MINUS
			| TIMES
			| DIV
			| MOD
			| EXPO
			| EQUAL
			| GREATER
			| LESSER
			| GREATEROREQUAL
			| LESSEROREQUAL
			| NOT
			| AND
			| OR
			| IMPLIES
			| TRUE
			| FALSE
			| LOOKUP of string
			| TUPLE of int
			| PROJ;;

let rec compile e = match e with
	Const n -> [CONST(n)]
 | Abs n -> (compile n)@[ABS]
 | Plus(e1,e2) -> (compile e1)@(compile e2)@[PLUS]
 | Minus(e1,e2) -> (compile e1)@(compile e2)@[MINUS]
 | Times(e1,e2) -> (compile e1)@(compile e2)@[TIMES]
 | Div(e1,e2) -> (compile e1)@(compile e2)@[DIV]
 | Mod(e1,e2) -> (compile e1)@(compile e2)@[MOD]
 | Expo(e1,e2) -> (compile e1)@(compile e2)@[EXPO]
 | Equal(e1,e2) -> (compile e1)@(compile e2)@[EQUAL]
 | Gt(e1,e2) -> (compile e1)@(compile e2)@[GREATER]
 | Lt(e1,e2) -> (compile e1)@(compile e2)@[LESSER]	
 | Ge(e1,e2) -> (compile e1)@(compile e2)@[GREATEROREQUAL]
 | Le(e1,e2) -> (compile e1)@(compile e2)@[LESSEROREQUAL]
 | Not e1 -> (compile e1)@[NOT]
 | And(e1,e2) -> (compile e1)@(compile e2)@[AND]
 | Or(e1,e2) -> (compile e1)@(compile e2)@[OR]
 | Implies(e1,e2) -> (compile e1)@(compile e2)@[IMPLIES]
 | T -> [TRUE]
 | F -> [FALSE]
 | Ident x -> [LOOKUP(x)]
 | Tuple(l) -> let rec compileOnTuple l = match l with
 					[] -> []
 				| x::xs -> (compile x)@(compileOnTuple xs) in
 				(compileOnTuple l)@[TUPLE(length l)]
 | Proj(i,tuple) -> (compile tuple)@compile(i)@[PROJ];;

let table = Hashtbl.create 123456;;
Hashtbl.add table "x" (Const'(7));;
Hashtbl.add table "y" (Const'(19));;

let extract n = match n with
	Const' a -> a;;

let rec execute (s,t,c) = match (s,t,c) with
   (s, t, []) -> hd s
 | (s, t, CONST(n)::c') -> execute ((Const'(n))::s, t, c')
 | (s, t, LOOKUP(x)::c') -> execute ((Hashtbl.find t x)::s, t, c')
 | (n::s, t, ABS::c') -> if (extract n)>0 then execute (n::s, t, c') else execute (Const'(-(extract n))::s, t, c')
 | (n2::n1::s', t, PLUS::c') -> execute ((Const'((extract n1)+(extract n2)))::s', t, c')
 | (n2::n1::s', t, MINUS::c') -> execute ((Const'((extract n1)-(extract n2)))::s', t, c')
 | (n2::n1::s', t, TIMES::c') -> execute ((Const'((extract n1)*(extract n2)))::s', t, c')
 | (n2::n1::s', t, DIV::c') -> execute ((Const'((extract n1)/(extract n2)))::s', t, c')
 | (n2::n1::s', t, MOD::c') -> execute ((Const'((extract n1) mod (extract n2)))::s', t, c')
 | (n2::n1::s', t, EXPO::c') -> execute ((Const'(exponential((extract n1),(extract n2)) 1))::s', t, c')
 | (n2::n1::s', t, EQUAL::c') -> if (extract n1)=(extract n2) then execute (T'::s', t, c') else execute (F'::s', t, c')
 | (n2::n1::s', t, GREATER::c') -> if (extract n1)>(extract n2) then execute (T'::s', t, c') else execute (F'::s', t, c')
 | (n2::n1::s', t, LESSER::c') -> if (extract n1)<(extract n2) then execute (T'::s', t, c') else execute (F'::s', t, c')
 | (n2::n1::s', t, GREATEROREQUAL::c') -> if (extract n1)>=(extract n2) then execute (T'::s', t, c') else execute (F'::s', t, c')
 | (n2::n1::s', t, LESSEROREQUAL::c') -> if (extract n1)<=(extract n2) then execute (T'::s', t, c') else execute (F'::s', t, c')
 | (n::s', t, NOT::c') -> if n=F' then execute (T'::s', t, c') else execute (F'::s', t, c')
 | (n2::n1::s', t, AND::c') -> if n1=T' && n2=T' then execute (T'::s', t, c') else execute (F'::s', t, c')
 | (n2::n1::s', t, OR::c') -> if n1=T' || n2=T' then execute (T'::s', t, c') else execute (F'::s', t, c')
 | (n2::n1::s', t, IMPLIES::c') -> if n1=T' && n2=F' then execute (F'::s', t, c') else execute (T'::s', t, c')
 | (s, t, TRUE::c') -> execute(T'::s, t, c')
 | (s, t, FALSE::c') -> execute(F'::s, t, c')
 | (s, t, TUPLE(n)::c') -> let rec extr s (tuplist,s') n = match n with
 							0 -> (tuplist,s')
						  | _ -> extr (tl s) (([hd s]@tuplist),(tl s)) (n-1) in
 							let (tuplist, s') = extr s ([],s) n in
 							execute([Tuple'(tuplist)]@s' , t, c')
 | (n::(Tuple'(lis))::s, t, PROJ::c') -> let rec foo l i = match i with
					   		0 -> hd l
					   	  | _ -> foo (tl l) (i-1) and
					   		foo2 n = match n with
					   		Const' i -> i in
					   		execute((foo lis (foo2 n))::s, t, c');;
