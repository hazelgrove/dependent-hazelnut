open Terms;

let exists_library =
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
          Fun(
            Text("B"),
            Typ,
            Fun(
              Text("P"),
              Arrow(
                Text("x"),
                Var("B"),
                Arrow(Text("y"), Var("B"), Typ),
              ),
              Fun(
                Text("f1"),
                Ap(
                  Ap(Var("exists"), Var("B")),
                  Fun(
                    Text("x"),
                    Var("B"),
                    Arrow(
                      Text("y"),
                      Var("B"),
                      Ap(Ap(Var("P"), Var("x")), Var("y")),
                    ),
                  ),
                ),
                Fun(
                  Text("y"),
                  Var("B"),
                  Let(
                    Text("M"),
                    Typ,
                    Ap(
                      Ap(Var("exists"), Var("B")),
                      Fun(
                        Text("x"),
                        Var("B"),
                        Ap(Ap(Var("P"), Var("x")), Var("y")),
                      ),
                    ),
                    Let(
                      Text("Px"),
                      Arrow(Text("x"), Var("B"), Typ),
                      Fun(
                        Text("x"),
                        Var("B"),
                        Arrow(
                          Text("y"),
                          Var("B"),
                          Ap(Ap(Var("P"), Var("x")), Var("y")),
                        ),
                      ),
                      Let(
                        Text("go"),
                        Arrow(
                          Text("a"),
                          Var("B"),
                          Arrow(
                            Text("p"),
                            Ap(Var("Px"), Var("a")),
                            Var("M"),
                          ),
                        ),
                        Fun(
                          Text("a"),
                          Var("B"),
                          Fun(
                            Text("p"),
                            Ap(Var("Px"), Var("a")),
                            Ap(
                              Ap(
                                Ap(
                                  Ap(Var("exists-con"), Var("B")),
                                  Fun(
                                    Text("x"),
                                    Var("B"),
                                    Ap(Ap(Var("P"), Var("x")), Var("y")),
                                  ),
                                ),
                                Var("a"),
                              ),
                              Ap(Var("p"), Var("y")),
                            ),
                          ),
                        ),
                        Ap(
                          Ap(
                            Ap(
                              Ap(
                                Ap(Var("exists-rec"), Var("B")),
                                Var("Px"),
                              ),
                              Var("M"),
                            ),
                            Var("go"),
                          ),
                          Var("f1"),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ),
          Hole,
        ),
      ),
    ),
  );

let bugs_library =
  E1Let(
    Text("goal-out-of-scope-(at a)"),
    Hole,
    E2Let(
      Text("goal"),
      Hole,
      Arrow(Text("y"), Hole, Var("y")),
      E1Let(
        Text("attempt"),
        Var("goal"),
        EFun(Hole, Hole, Cursor(Var("a"))),
        Hole,
      ),
    ),
    Let(
      Text("visual-capture"),
      Hole,
      Fun(
        Text("x"),
        Hole,
        Let(
          Text("t"),
          Hole,
          Var("x"),
          Let(
            Text("x"),
            Hole,
            Hole,
            Let(Text("s"), Var("t"), Hole, Var("s")),
          ),
        ),
      ),
      Hole,
    ),
  );

let eq_library =
  E1Let(
    Text("eq"),
    Arrow(
      Text("A"),
      Typ,
      Arrow(Text("a"), Var("A"), Arrow(Text("b"), Var("A"), Typ)),
    ),
    Cursor(Hole),
    Let(
      Text("refl"),
      Arrow(
        Text("A"),
        Typ,
        Arrow(
          Text("a"),
          Var("A"),
          Ap(Ap(Ap(Var("eq"), Var("A")), Var("a")), Var("a")),
        ),
      ),
      Hole,
      Let(
        Text("J"),
        Arrow(
          Text("A"),
          Typ,
          Arrow(
            Text("P"),
            Arrow(
              Text("a"),
              Var("A"),
              Arrow(
                Text("b"),
                Var("A"),
                Arrow(
                  Text("e"),
                  Ap(Ap(Ap(Var("eq"), Var("A")), Var("a")), Var("b")),
                  Typ,
                ),
              ),
            ),
            Arrow(
              Text("p"),
              Arrow(
                Text("a"),
                Var("A"),
                Ap(
                  Ap(Ap(Var("P"), Var("a")), Var("a")),
                  Ap(Ap(Var("refl"), Var("A")), Var("a")),
                ),
              ),
              Arrow(
                Text("a"),
                Var("A"),
                Arrow(
                  Text("b"),
                  Var("A"),
                  Arrow(
                    Text("e"),
                    Ap(Ap(Ap(Var("eq"), Var("A")), Var("a")), Var("b")),
                    Ap(Ap(Ap(Var("P"), Var("a")), Var("b")), Var("e")),
                  ),
                ),
              ),
            ),
          ),
        ),
        Hole,
        Let(
          Text("J-eq"),
          Arrow(
            Text("A"),
            Typ,
            Arrow(
              Text("P"),
              Arrow(
                Text("a"),
                Var("A"),
                Arrow(
                  Text("b"),
                  Var("A"),
                  Arrow(
                    Text("e"),
                    Ap(Ap(Ap(Var("eq"), Var("A")), Var("a")), Var("b")),
                    Typ,
                  ),
                ),
              ),
              Arrow(
                Text("p"),
                Arrow(
                  Text("a"),
                  Var("A"),
                  Ap(
                    Ap(Ap(Var("P"), Var("a")), Var("a")),
                    Ap(Ap(Var("refl"), Var("A")), Var("a")),
                  ),
                ),
                Arrow(
                  Text("a"),
                  Var("A"),
                  Ap(
                    Ap(
                      Ap(
                        Var("eq"),
                        Ap(
                          Ap(Ap(Var("P"), Var("a")), Var("a")),
                          Ap(Ap(Var("refl"), Var("A")), Var("a")),
                        ),
                      ),
                      Ap(
                        Ap(
                          Ap(
                            Ap(
                              Ap(Ap(Var("J"), Var("A")), Var("P")),
                              Var("p"),
                            ),
                            Var("a"),
                          ),
                          Var("a"),
                        ),
                        Ap(Ap(Var("refl"), Var("A")), Var("a")),
                      ),
                    ),
                    Ap(Var("p"), Var("a")),
                  ),
                ),
              ),
            ),
          ),
          Hole,
          Let(
            Text("subst"),
            Arrow(
              Text("A"),
              Typ,
              Arrow(
                Text("a"),
                Var("A"),
                Arrow(
                  Text("b"),
                  Var("A"),
                  Arrow(
                    Text("P"),
                    Arrow(Hole, Var("A"), Typ),
                    Arrow(
                      Hole,
                      Ap(
                        Ap(Ap(Var("eq"), Var("A")), Var("a")),
                        Var("b"),
                      ),
                      Arrow(
                        Hole,
                        Ap(Var("P"), Var("a")),
                        Ap(Var("P"), Var("b")),
                      ),
                    ),
                  ),
                ),
              ),
            ),
            Fun(
              Text("A"),
              Typ,
              Fun(
                Text("a"),
                Var("A"),
                Fun(
                  Text("b"),
                  Var("A"),
                  Fun(
                    Text("P"),
                    Arrow(Hole, Var("A"), Typ),
                    Fun(
                      Text("e"),
                      Ap(
                        Ap(Ap(Var("eq"), Var("A")), Var("a")),
                        Var("b"),
                      ),
                      Ap(
                        Ap(
                          Ap(
                            Ap(
                              Ap(
                                Ap(Var("J"), Var("A")),
                                Fun(
                                  Text("x"),
                                  Var("A"),
                                  Fun(
                                    Text("y"),
                                    Var("A"),
                                    Fun(
                                      Hole,
                                      Ap(
                                        Ap(
                                          Ap(Var("eq"), Var("A")),
                                          Var("x"),
                                        ),
                                        Var("y"),
                                      ),
                                      Arrow(
                                        Hole,
                                        Ap(Var("P"), Var("x")),
                                        Ap(Var("P"), Var("y")),
                                      ),
                                    ),
                                  ),
                                ),
                              ),
                              Fun(
                                Text("a"),
                                Var("A"),
                                Fun(
                                  Text("p"),
                                  Ap(Var("P"), Var("a")),
                                  Var("p"),
                                ),
                              ),
                            ),
                            Var("a"),
                          ),
                          Var("b"),
                        ),
                        Var("e"),
                      ),
                    ),
                  ),
                ),
              ),
            ),
            Let(
              Text("subst-eq"),
              Arrow(
                Text("A"),
                Typ,
                Arrow(
                  Text("a"),
                  Var("A"),
                  Arrow(
                    Text("P"),
                    Arrow(Hole, Var("A"), Typ),
                    Arrow(
                      Text("p"),
                      Ap(Var("P"), Var("a")),
                      Ap(
                        Ap(
                          Ap(Var("eq"), Ap(Var("P"), Var("a"))),
                          Ap(
                            Ap(
                              Ap(
                                Ap(
                                  Ap(
                                    Ap(Var("subst"), Var("A")),
                                    Var("a"),
                                  ),
                                  Var("a"),
                                ),
                                Var("P"),
                              ),
                              Ap(Ap(Var("refl"), Var("A")), Var("a")),
                            ),
                            Var("p"),
                          ),
                        ),
                        Var("p"),
                      ),
                    ),
                  ),
                ),
              ),
              Fun(
                Text("A"),
                Typ,
                Fun(
                  Text("a"),
                  Var("A"),
                  Fun(
                    Text("P"),
                    Arrow(Hole, Var("A"), Typ),
                    Fun(Text("p"), Ap(Var("P"), Var("a")), Var("todo")),
                  ),
                ),
              ),
              Hole,
            ),
          ),
        ),
      ),
    ),
  );

let library = eq_library /*            */;
//   Let(
//     Text("nat"),
//     Typ,
//     Hole,
//     Let(
//       Text("Z"),
//       Var("nat"),
//       Hole,
//       Let(
//         Text("S"),
//         Arrow(Hole, Var("nat"), Var("nat")),
//         Hole,
//         Let(
//           Text("nat-rec"),
//           Arrow(
//             Text("M"),
//             Typ,
//             Arrow(
//               Text("pZ"),
//               Var("M"),
//               Arrow(
//                 Text("pS"),
//                 Arrow(Hole, Var("M"), Var("M")),
//                 Arrow(Hole, Var("nat"), Var("M")),
//               ),
//             ),
//           ),
//           Hole,
//           Let(
//             Text("plus"),
//             Arrow(
//               Hole,
//               Var("nat"),
//               Arrow(Hole, Var("nat"), Var("nat")),
//             ),
//             Fun(
//               Text("n"),
//               Var("nat"),
//               Fun(
//                 Text("m"),
//                 Var("nat"),
//                 Ap(
//                   Ap(
//                     Ap(Ap(Var("nat-rec"), Var("nat")), Var("m")),
//                     Fun(
//                       Text("sum"),
//                       Var("nat"),
//                       Ap(Var("S"), Var("sum")),
//                     ),
//                   ),
//                   Var("n"),
//                 ),
//               ),
//             ),
