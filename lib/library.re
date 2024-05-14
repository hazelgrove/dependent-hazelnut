open Terms;

// let simply_typed_library =
//   E1Let(
//     Text("thm1"),
//     Arrow(
//       Hole,
//       Arrow(Hole, Base("a"), Arrow(Hole, Base("b"), Base("c"))),
//       Arrow(Hole, Base("b"), Arrow(Hole, Base("a"), Base("c"))),
//     ),
//     Cursor(Hole),
//     Let(
//       Text("thm2"),
//       Arrow(
//         Hole,
//         Arrow(Hole, Base("a"), Arrow(Hole, Base("b"), Base("c"))),
//         Arrow(
//           Hole,
//           Arrow(Hole, Base("a"), Base("b")),
//           Arrow(Hole, Base("a"), Base("c")),
//         ),
//       ),
//       Hole,
//       Let(
//         Text("thm3"),
//         Arrow(
//           Hole,
//           Arrow(Hole, Arrow(Hole, Base("a"), Base("a")), Base("b")),
//           Base("b"),
//         ),
//         Hole,
//         Hole,
//       ),
//     ),
//   );

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
          Fun(
            Text("A"),
            Typ,
            Fun(
              Text("P"),
              Arrow(
                Text("x"),
                Var("A"),
                Arrow(Text("y"), Var("A"), Typ),
              ),
              Fun(
                Text("f1"),
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
                Fun(
                  Text("y"),
                  Var("A"),
                  Let(
                    Text("Px"),
                    Arrow(Text("x"), Var("A"), Typ),
                    Fun(
                      Text("x"),
                      Var("A"),
                      Arrow(
                        Text("y"),
                        Var("A"),
                        Ap(Ap(Var("P"), Var("x")), Var("y")),
                      ),
                    ),
                    Let(
                      Text("go"),
                      Arrow(
                        Text("a"),
                        Var("A"),
                        Arrow(
                          Text("p"),
                          Arrow(
                            Text("y"),
                            Var("A"),
                            Ap(Ap(Var("P"), Var("a")), Var("y")),
                          ),
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
                      Fun(
                        Text("a"),
                        Var("A"),
                        Fun(
                          Text("p"),
                          Arrow(
                            Text("y"),
                            Var("A"),
                            Ap(Ap(Var("P"), Var("a")), Var("y")),
                          ),
                          Ap(
                            Ap(
                              Ap(
                                Ap(Var("exists-con"), Var("A")),
                                Fun(
                                  Text("x"),
                                  Var("A"),
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
                              Ap(Var("exists-rec"), Var("A")),
                              Var("Px"),
                            ),
                            Ap(
                              Ap(Var("exists"), Var("A")),
                              Fun(
                                Text("x"),
                                Var("A"),
                                Ap(Ap(Var("P"), Var("x")), Var("y")),
                              ),
                            ),
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
          Let(
            Text("nat"),
            Typ,
            Hole,
            Let(
              Text("Z"),
              Var("nat"),
              Hole,
              Let(
                Text("S"),
                Arrow(Hole, Var("nat"), Var("nat")),
                Hole,
                Let(
                  Text("nat-rec"),
                  Arrow(
                    Text("M"),
                    Typ,
                    Arrow(
                      Text("pZ"),
                      Var("M"),
                      Arrow(
                        Text("pS"),
                        Arrow(Hole, Var("M"), Var("M")),
                        Arrow(Hole, Var("nat"), Var("M")),
                      ),
                    ),
                  ),
                  Hole,
                  Let(
                    Text("plus"),
                    Arrow(
                      Hole,
                      Var("nat"),
                      Arrow(Hole, Var("nat"), Var("nat")),
                    ),
                    Fun(
                      Text("n"),
                      Var("nat"),
                      Fun(
                        Text("m"),
                        Var("nat"),
                        Ap(
                          Ap(
                            Ap(Ap(Var("nat-rec"), Var("nat")), Var("m")),
                            Fun(
                              Text("sum"),
                              Var("nat"),
                              Ap(Var("S"), Var("sum")),
                            ),
                          ),
                          Var("n"),
                        ),
                      ),
                    ),
                    Let(
                      Text("eq"),
                      Arrow(
                        Text("A"),
                        Typ,
                        Arrow(
                          Text("a"),
                          Var("A"),
                          Arrow(Text("b"), Var("A"), Typ),
                        ),
                      ),
                      Hole,
                      Let(
                        Text("refl"),
                        Arrow(
                          Text("A"),
                          Typ,
                          Arrow(
                            Text("a"),
                            Var("A"),
                            Ap(
                              Ap(Ap(Var("eq"), Var("A")), Var("a")),
                              Var("a"),
                            ),
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
                                    Ap(
                                      Ap(
                                        Ap(Var("eq"), Var("A")),
                                        Var("a"),
                                      ),
                                      Var("b"),
                                    ),
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
                                    Ap(
                                      Ap(Var("refl"), Var("A")),
                                      Var("a"),
                                    ),
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
                                      Ap(
                                        Ap(
                                          Ap(Var("eq"), Var("A")),
                                          Var("a"),
                                        ),
                                        Var("b"),
                                      ),
                                      Ap(
                                        Ap(
                                          Ap(Var("P"), Var("a")),
                                          Var("b"),
                                        ),
                                        Var("e"),
                                      ),
                                    ),
                                  ),
                                ),
                              ),
                            ),
                          ),
                          Hole,
                          Let(
                            Text("J-eq"),
                            Hole,
                            Fun(
                              Text("A"),
                              Typ,
                              Fun(
                                Text("P"),
                                Arrow(
                                  Text("a"),
                                  Var("A"),
                                  Arrow(
                                    Text("b"),
                                    Var("A"),
                                    Arrow(
                                      Text("e"),
                                      Ap(
                                        Ap(
                                          Ap(Var("eq"), Var("A")),
                                          Var("a"),
                                        ),
                                        Var("b"),
                                      ),
                                      Typ,
                                    ),
                                  ),
                                ),
                                Fun(
                                  Text("p"),
                                  Arrow(
                                    Text("a"),
                                    Var("A"),
                                    Ap(
                                      Ap(
                                        Ap(Var("P"), Var("a")),
                                        Var("a"),
                                      ),
                                      Ap(
                                        Ap(Var("refl"), Var("A")),
                                        Var("a"),
                                      ),
                                    ),
                                  ),
                                  Fun(
                                    Text("a"),
                                    Var("A"),
                                    Ap(
                                      Ap(
                                        Ap(
                                          Var("eq"),
                                          Ap(
                                            Ap(
                                              Ap(Var("P"), Var("a")),
                                              Var("a"),
                                            ),
                                            Ap(
                                              Ap(Var("refl"), Var("A")),
                                              Var("a"),
                                            ),
                                          ),
                                        ),
                                        Ap(
                                          Ap(
                                            Ap(
                                              Ap(
                                                Ap(
                                                  Ap(Var("J"), Var("A")),
                                                  Var("P"),
                                                ),
                                                Var("p"),
                                              ),
                                              Var("a"),
                                            ),
                                            Var("a"),
                                          ),
                                          Ap(
                                            Ap(Var("refl"), Var("A")),
                                            Var("a"),
                                          ),
                                        ),
                                      ),
                                      Ap(Var("p"), Var("a")),
                                    ),
                                  ),
                                ),
                              ),
                            ),
                            Hole,
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
    ),
  );

// let thing =
//   Let(
//     Text("exists"),
//     Arrow(
//       Text("A"),
//       Typ,
//       Arrow(Text("P"), Arrow(Text("x"), Var("A"), Typ), Typ),
//     ),
//     Hole,
//     Let(
//       Text("exists-con"),
//       Arrow(
//         Text("A"),
//         Typ,
//         Arrow(
//           Text("P"),
//           Arrow(Text("x"), Var("A"), Typ),
//           Arrow(
//             Text("a"),
//             Var("A"),
//             Arrow(
//               Text("p"),
//               Ap(Var("P"), Var("a")),
//               Ap(Ap(Var("exists"), Var("A")), Var("P")),
//             ),
//           ),
//         ),
//       ),
//       Hole,
//       Let(
//         Text("exists-rec"),
//         Arrow(
//           Text("A"),
//           Typ,
//           Arrow(
//             Text("P"),
//             Arrow(Text("x"), Var("A"), Typ),
//             Arrow(
//               Text("M"),
//               Typ,
//               Arrow(
//                 Text("go"),
//                 Arrow(
//                   Text("a"),
//                   Var("A"),
//                   Arrow(Text("p"), Ap(Var("P"), Var("a")), Var("M")),
//                 ),
//                 Arrow(
//                   Hole,
//                   Ap(Ap(Var("exists"), Var("A")), Var("P")),
//                   Var("M"),
//                 ),
//               ),
//             ),
//           ),
//         ),
//         Hole,
//         ,
//       ),
//     ),
//   );
