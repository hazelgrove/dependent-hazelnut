open Terms;

let simply_typed_library =
  E1Let(
    Text("thm1"),
    Arrow(
      Hole,
      Arrow(Hole, Base("a"), Arrow(Hole, Base("b"), Base("c"))),
      Arrow(Hole, Base("b"), Arrow(Hole, Base("a"), Base("c"))),
    ),
    Cursor(Hole),
    Let(
      Text("thm2"),
      Arrow(
        Hole,
        Arrow(Hole, Base("a"), Arrow(Hole, Base("b"), Base("c"))),
        Arrow(
          Hole,
          Arrow(Hole, Base("a"), Base("b")),
          Arrow(Hole, Base("a"), Base("c")),
        ),
      ),
      Hole,
      Let(
        Text("thm3"),
        Arrow(
          Hole,
          Arrow(Hole, Arrow(Hole, Base("a"), Base("a")), Base("b")),
          Base("b"),
        ),
        Hole,
        Hole,
      ),
    ),
  );

let library =
  E1Let(
    Text("exists"),
    Arrow(
      Text("A"),
      Typ,
      Arrow(Text("P"), Arrow(Text("x"), Var("A"), Typ), Typ),
    ),
    Cursor(Hole),
    Let(
      Text("exists-con"),
      Arrow(
        Text("A"),
        Typ,
        Arrow(
          Text("P"),
          Arrow(Text("x"), Var("A"), Typ),
          Arrow(
            Text("a"),
            Var("A"),
            Arrow(
              Text("p"),
              Ap(Var("P"), Var("a")),
              Ap(Ap(Var("exists"), Var("A")), Var("P")),
            ),
          ),
        ),
      ),
      Hole,
      Let(
        Text("exists-rec"),
        Arrow(
          Text("A"),
          Typ,
          Arrow(
            Text("P"),
            Arrow(Text("x"), Var("A"), Typ),
            Arrow(
              Text("M"),
              Typ,
              Arrow(
                Text("go"),
                Arrow(
                  Text("a"),
                  Var("A"),
                  Arrow(Text("p"), Ap(Var("P"), Var("a")), Var("M")),
                ),
                Arrow(
                  Hole,
                  Ap(Ap(Var("exists"), Var("A")), Var("P")),
                  Var("M"),
                ),
              ),
            ),
          ),
        ),
        Hole,
        Let(
          Text("thm"),
          Arrow(
            Text("A"),
            Typ,
            Arrow(
              Text("P"),
              Arrow(
                Text("x"),
                Var("A"),
                Arrow(Text("y"), Var("A"), Typ),
              ),
              Arrow(
                Hole,
                Ap(
                  Ap(Var("exists"), Var("A")),
                  Fun(
                    Text("x"),
                    Var("A"),
                    Arrow(
                      Text("y"),
                      Var("A"),
                      Ap(Ap(Var("P"), Var("x")), Var("y")),
                    ),
                  ),
                ),
                Arrow(
                  Text("y"),
                  Var("A"),
                  Ap(
                    Ap(Var("exists"), Var("A")),
                    Fun(
                      Text("x"),
                      Var("A"),
                      Ap(Ap(Var("P"), Var("x")), Var("y")),
                    ),
                  ),
                ),
              ),
            ),
          ),
          Hole,
          Hole,
        ),
      ),
    ),
  );
