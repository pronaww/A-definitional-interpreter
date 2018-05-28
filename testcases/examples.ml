let e0 = Const(56);;
let e0_compile = compile e0;;
eval e0;;
execute ([], table, e0_compile);;

let e1 = Abs(Const(-56));;
let e1_compile = compile e1;;
eval e1;;
execute ([], table, e1_compile);;

let e2 = Ident("x");;
let e2_compile = compile e2;;
eval e2;;
execute ([], table, e2_compile);;

let e3 = Plus(Const(5),Const(6));;
let e3_compile = compile e3;;
eval e3;;
execute ([], table, e3_compile);;

let e4 = Minus(Const(5),Const(6));;
let e4_compile = compile e4;;
eval e4;;
execute ([], table, e4_compile);;

let e5 = Times(Const(5),Const(6));;
let e5_compile = compile e5;;
eval e5;;
execute ([], table, e5_compile);;

let e6 = Div(Const(121),Const(11));;
let e6_compile = compile e6;;
eval e6;;
execute ([], table, e6_compile);;

let e7 = Mod(Const(19),Const(6));;
let e7_compile = compile e7;;
eval e7;;
execute ([], table, e7_compile);;

let e8 = Expo(Const(2),Const(10));;
let e8_compile = compile e8;;
eval e8;;
execute ([], table, e8_compile);;

let e9 = Equal(Const(5),Const(5));;
let e9_compile = compile e9;;
eval e9;;
execute ([], table, e9_compile);;

let e10 = Gt(Const(7),Const(18));;
let e10_compile = compile e10;;
eval e10;;
execute ([], table, e10_compile);;

let e11 = Lt(Const(5),Const(6));;
let e11_compile = compile e11;;
eval e11;;
execute ([], table, e11_compile);;

let e12 = Ge(Const(6),Const(11));;
let e12_compile = compile e12;;
eval e12;;
execute ([], table, e12_compile);;

let e13 = Le(Const(5),Const(6));;
let e13_compile = compile e13;;
eval e13;;
execute ([], table, e13_compile);;

let e14 = Not(e13);;
let e14_compile = compile e14;;
eval e14;;
execute ([], table, e14_compile);;

let e15 = And(e11,e13);;
let e15_compile = compile e15;;
eval e15;;
execute ([], table, e15_compile);;

let e16 = Or(e11,e13);;
let e16_compile = compile e16;;
eval e16;;
execute ([], table, e16_compile);;

let e17 = Implies(e11,e13);;
let e17_compile = compile e17;;
eval e17;;
execute ([], table, e17_compile);;

let e18 = Tuple([e1;e2;e3;e4;e5;e6]);;
let e18_compile = compile e18;;
eval e18;;
execute ([], table, e18_compile);;

let e19 = Proj(Const(3),e18);;
let e19_compile = compile e19;;
eval e19;;
execute ([], table, e19_compile);;




let exp0 = Abs(Times(Const(5),Const(-6)));;
let exp0_compile = compile exp0;;
eval exp0;;
execute ([], table, exp0_compile);;

let exp1 = Mod(Div(Const(45),Const(9)),Const(2));;
let exp1_compile = compile exp1;;
eval exp1;;
execute ([], table, exp1_compile);;

let exp2 = Expo(Minus(Const(10),Ident("x")),Const(4));;
let exp2_compile = compile exp2;;
eval exp2;;
execute ([], table, exp2_compile);;

let exp3 = Equal(exp1,exp2);;
let exp3_compile = compile exp3;;
eval exp3;;
execute ([], table, exp3_compile);;

let exp4 = Gt(exp0,exp1);;
let exp4_compile = compile exp4;;
eval exp4;;
execute ([], table, exp4_compile);;

let exp5 = Lt(exp1,exp2);;
let exp5_compile = compile exp5;;
eval exp5;;
execute ([], table, exp5_compile);;

let exp6 = Le(exp2,exp1);;
let exp6_compile = compile exp6;;
eval exp6;;
execute ([], table, exp6_compile);;

let exp7 = And(exp5,exp6);;
let exp7_compile = compile exp7;;
eval exp7;;
execute ([], table, exp7_compile);;

let exp8 = Or(exp4,exp7);;
let exp8_compile = compile exp8;;
eval exp8;;
execute ([], table, exp8_compile);;

let exp9 = Implies(exp4,exp8);;
let exp9_compile = compile exp9;;
eval exp9;;
execute ([], table, exp9_compile);;

let exp10 = Tuple([exp3;exp4;exp5;exp6;exp7;exp8]);;
let exp10_compile = compile exp10;;
eval exp10;;
execute ([], table, exp10_compile);;

let exp11 = Proj(Const(3), exp10);;
let exp11_compile = compile exp11;;
eval exp11;;
execute ([], table, exp11_compile);;

let exp12 = Tuple([Const(0);exp0;exp1;exp2]);;
let exp12_compile = compile exp12;;
eval exp12;;
execute ([], table, exp12_compile);;

let exp13 = Tuple([exp2;exp1;Plus(Const(3),Const(99));exp0])
let exp13_compile = compile exp13;;
eval exp13;;
execute ([], table, exp13_compile);;

let exp14 = Proj(exp1,exp12);;
let exp14_compile = compile exp14;;
eval exp14;;
execute ([], table, exp14_compile);;

let exp15 = Proj(Const(3),exp13);;
let exp15_compile = compile exp15;;
eval exp15;;
execute ([], table, exp15_compile);;

let exp16 = Equal(exp14, exp15);;
let exp16_compile = compile exp16;;
eval exp16;;
execute ([], table, exp16_compile);;
