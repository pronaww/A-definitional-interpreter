(* Note:
 * Please make suitable changes to the shared test cases so that
 * the constructors match your signature definition.
 *)

(*--==Compile & Execute==--*)
let table = Hashtbl.create 123456;;
Hashtbl.add table "iden1" (Const'(7));;
Hashtbl.add table "iden2" (Const'(19));;


let a = Plus(Const(1),Const(2));;
let compileA = compile a;;
let executeA = execute([], table, compileA);;
let b = Times(Const(6),Const(6));;
let compileB = compile b;;
let executeB = execute([], table, compileB);;
let c = Expo(Const(2),Const(4));;
let compileC = compile c;;
let executeC = execute([], table, compileC);;
let d = Div(Const(6),Const(3));;
let compileD = compile d;;
let executeD = execute([], table, compileD);;

let e = Ident("iden1");;
let compileE = compile e;;
let executeE = execute([], table, compileE);;
let f = Ident("iden2");;
let compileF = compile f;;
let executeF = execute([], table, compileF);;


let g = Abs(Const(-1));;
let compileG = compile g;;
let executeG = execute([], table, compileG);;
let h = Proj(Const(2), Tuple([Const(12);Const(121);Const(33)]));;
let compileH = compile h;;
let executeH = execute([], table, compileH);;

let i = Minus(Proj(Const(2),Tuple[Const(2);Const(5);Const(8)]),Const(1));;
let compileI = compile i;;
let executeI = execute([], table, compileI);;
let j = Mod(Proj(Const(2), Tuple[Const(2);Const(5);Const(8)]),Const(2));;
let compileJ = compile j;;
let executeJ = execute([], table, compileJ);;

let k = Or(
	Equal(Const(5),Const(5)),
	And(Equal(Minus(Const(2),Const(1)),Const(1)),
		Mod(Proj(Const(2), Tuple[Const(2);Const(5);Const(8)]),Const(2))
	)
);;
let compileK = compile k;;
let executeK = execute([], table, compileK);;

let l = And(T, F);;
let compileL = compile l;;
let executeL = execute([], table, compileL);;
let l = Implies(Not(Implies(Or(T, F), And(T, F))),Implies(And(T, F), Or(T, F)));;
let compileL = compile l;;
let executeL = execute([], table, compileL);;

let m = Ge(Const(4),Const(2));;
let compileM = compile m;;
let executeM = execute([], table, compileM);;
let n = Le(Const(4),Const(2));;
let compileN = compile n;;
let executeN = execute([], table, compileN);;
(* let o = Ifthenelse(Gtr(Const(4),Const(2)),Plus(Const(1),Const(3)),Minus(Const(1),Const(3))); *)

(* Lambda is a lambda function of type exp*exp and LetinEnd is a ternary operator of type exp*exp*exp *)
(* let p = Apply(Lambda(Ident("x"),LetinEnd(Para[Assgn(Ident("a"),Const(2))],Plus(Ident("a"),Ident("x")))),Const(2)) *)
