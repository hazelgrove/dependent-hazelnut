// open Terms;
// let string_of_name = (x: name) =>
//   switch (x) {
//   | Hole => "?"
//   | Text(x) => x
//   };
// // let empty_exp: exp = Hole;
// let rec string_of_typ = (t: typ) =>
//   switch (t) {
//   | Hole => "?"
//   | Base(t) => t
//   | Arrow(t1, t2) =>
//     "(" ++ string_of_typ(t1) ++ "⇒" ++ string_of_typ(t2) ++ ")"
//   };
// let rec string_of_exp = (e: exp) =>
//   switch (e) {
//   | Hole => "?"
//   | Var(x) => x
//   | Fun(x, t, e) =>
//     string_of_name(x)
//     ++ ":"
//     ++ string_of_typ(t)
//     ++ " → "
//     ++ string_of_exp(e)
//   | Ap(e1, e2) =>
//     "(" ++ string_of_exp(e1) ++ ")(" ++ string_of_exp(e2) ++ ")"
//   | Let(x, t, e1, e2) =>
//     "let "
//     ++ string_of_name(x)
//     ++ ":"
//     ++ string_of_typ(t)
//     ++ " = "
//     ++ string_of_exp(e1)
//     ++ " in "
//     ++ string_of_exp(e2)
//   };
// let string_of_zname = (z: zname) =>
//   switch (z) {
//   | Cursor(x) => string_of_name(x)
//   };
// let rec string_of_ztyp = (t: ztyp) =>
//   switch (t) {
//   | Cursor(t) => "{" ++ string_of_typ(t) ++ "}"
//   | LArrow(z, t) =>
//     "(" ++ string_of_ztyp(z) ++ "⇒" ++ string_of_typ(t) ++ ")"
//   | RArrow(t, z) =>
//     "(" ++ string_of_typ(t) ++ "⇒" ++ string_of_ztyp(z) ++ ")"
//   };
// let rec string_of_zexp = (e: zexp) =>
//   switch (e) {
//   | Cursor(e) => "{" ++ string_of_exp(e) ++ "}"
//   | XFun(z, t, e) =>
//     string_of_zname(z)
//     ++ ":"
//     ++ string_of_typ(t)
//     ++ " → "
//     ++ string_of_exp(e)
//   | TFun(x, z, e) =>
//     string_of_name(x)
//     ++ ":"
//     ++ string_of_ztyp(z)
//     ++ " → "
//     ++ string_of_exp(e)
//   | EFun(x, t, z) =>
//     string_of_name(x)
//     ++ ":"
//     ++ string_of_typ(t)
//     ++ " → "
//     ++ string_of_zexp(z)
//   | LAp(z, e) => "(" ++ string_of_zexp(z) ++ ")(" ++ string_of_exp(e) ++ ")"
//   | RAp(e, z) => "(" ++ string_of_exp(e) ++ ")(" ++ string_of_zexp(z) ++ ")"
//   | XLet(x, t, e1, e2) =>
//     "let "
//     ++ string_of_zname(x)
//     ++ ":"
//     ++ string_of_typ(t)
//     ++ " = "
//     ++ string_of_exp(e1)
//     ++ " in "
//     ++ string_of_exp(e2)
//   | TLet(x, t, e1, e2) =>
//     "let "
//     ++ string_of_name(x)
//     ++ ":"
//     ++ string_of_ztyp(t)
//     ++ " = "
//     ++ string_of_exp(e1)
//     ++ " in "
//     ++ string_of_exp(e2)
//   | E1Let(x, t, e1, e2) =>
//     "let "
//     ++ string_of_name(x)
//     ++ ":"
//     ++ string_of_typ(t)
//     ++ " = "
//     ++ string_of_zexp(e1)
//     ++ " in "
//     ++ string_of_exp(e2)
//   | E2Let(x, t, e1, e2) =>
//     "let "
//     ++ string_of_name(x)
//     ++ ":"
//     ++ string_of_typ(t)
//     ++ " = "
//     ++ string_of_exp(e1)
//     ++ " in "
//     ++ string_of_zexp(e2)
//   };
