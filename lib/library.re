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
            // Let(
            //   Text("subst-eq"),
            //   Arrow(
            //     Text("A"),
            //     Typ,
            //     Arrow(
            //       Text("a"),
            //       Var("A"),
            //       Arrow(
            //         Text("P"),
            //         Arrow(Hole, Var("A"), Typ),
            //         Arrow(
            //           Text("p"),
            //           Ap(Var("P"), Var("a")),
            //           Ap(
            //             Ap(
            //               Ap(Var("eq"), Ap(Var("P"), Var("a"))),
            //               Ap(
            //                 Ap(
            //                   Ap(
            //                     Ap(
            //                       Ap(
            //                         Ap(Var("subst"), Var("A")),
            //                         Var("a"),
            //                       ),
            //                       Var("a"),
            //                     ),
            //                     Var("P"),
            //                   ),
            //                   Ap(Ap(Var("refl"), Var("A")), Var("a")),
            //                 ),
            //                 Var("p"),
            //               ),
            //             ),
            //             Var("p"),
            //           ),
            //         ),
            //       ),
            //     ),
            //   ),
            //   Fun(
            //     Text("A"),
            //     Typ,
            //     Fun(
            //       Text("a"),
            //       Var("A"),
            //       Fun(
            //         Text("P"),
            //         Arrow(Hole, Var("A"), Typ),
            //         Fun(
            //           Text("p"),
            //           Ap(Var("P"), Var("a")),
            //           Let(
            //             Text("eq1"),
            //             Ap(
            //               Ap(
            //                 Ap(Var("eq"), Hole),
            //                 Ap(
            //                   Ap(
            //                     Ap(
            //                       Ap(
            //                         Ap(
            //                           Ap(Var("J"), Var("A")),
            //                           Fun(
            //                             Text("x"),
            //                             Var("A"),
            //                             Fun(
            //                               Text("y"),
            //                               Var("A"),
            //                               Fun(
            //                                 Hole,
            //                                 Ap(
            //                                   Ap(
            //                                     Ap(Var("eq"), Var("A")),
            //                                     Var("x"),
            //                                   ),
            //                                   Var("y"),
            //                                 ),
            //                                 Arrow(
            //                                   Hole,
            //                                   Ap(Var("P"), Var("x")),
            //                                   Ap(Var("P"), Var("y")),
            //                                 ),
            //                               ),
            //                             ),
            //                           ),
            //                         ),
            //                         Fun(
            //                           Text("a"),
            //                           Var("A"),
            //                           Fun(
            //                             Text("p"),
            //                             Ap(Var("P"), Var("a")),
            //                             Var("p"),
            //                           ),
            //                         ),
            //                       ),
            //                       Var("a"),
            //                     ),
            //                     Var("a"),
            //                   ),
            //                   Ap(Ap(Var("refl"), Var("A")), Var("a")),
            //                 ),
            //               ),
            //               Fun(
            //                 Text("z"),
            //                 Ap(Var("P"), Var("a")),
            //                 Var("z"),
            //               ),
            //             ),
            //             Ap(
            //               Ap(
            //                 Ap(
            //                   Ap(Var("J-eq"), Var("A")),
            //                   Fun(
            //                     Text("x"),
            //                     Var("A"),
            //                     Fun(
            //                       Text("y"),
            //                       Var("A"),
            //                       Fun(
            //                         Hole,
            //                         Ap(
            //                           Ap(
            //                             Ap(Var("eq"), Var("A")),
            //                             Var("x"),
            //                           ),
            //                           Var("y"),
            //                         ),
            //                         Arrow(
            //                           Hole,
            //                           Ap(Var("P"), Var("x")),
            //                           Ap(Var("P"), Var("y")),
            //                         ),
            //                       ),
            //                     ),
            //                   ),
            //                 ),
            //                 Fun(
            //                   Text("a"),
            //                   Var("A"),
            //                   Fun(
            //                     Text("p"),
            //                     Ap(Var("P"), Var("a")),
            //                     Var("p"),
            //                   ),
            //                 ),
            //               ),
            //               Var("a"),
            //             ),
            //             Let(
            //               Text("go"),
            //               Hole,
            //               Hole,
            //               Ap(
            //                 Ap(
            //                   Ap(
            //                     Ap(
            //                       Ap(
            //                         Ap(
            //                           Var("subst"),
            //                           Arrow(
            //                             Hole,
            //                             Ap(Var("P"), Var("a")),
            //                             Ap(Var("P"), Var("a")),
            //                           ),
            //                         ),
            //                         Ap(
            //                           Ap(
            //                             Ap(
            //                               Ap(
            //                                 Ap(
            //                                   Ap(Var("J"), Var("A")),
            //                                   Fun(
            //                                     Text("x"),
            //                                     Var("A"),
            //                                     Fun(
            //                                       Text("y"),
            //                                       Var("A"),
            //                                       Fun(
            //                                         Hole,
            //                                         Ap(
            //                                           Ap(
            //                                             Ap(
            //                                               Var("eq"),
            //                                               Var("A"),
            //                                             ),
            //                                             Var("x"),
            //                                           ),
            //                                           Var("y"),
            //                                         ),
            //                                         Arrow(
            //                                           Hole,
            //                                           Ap(
            //                                             Var("P"),
            //                                             Var("x"),
            //                                           ),
            //                                           Ap(
            //                                             Var("P"),
            //                                             Var("y"),
            //                                           ),
            //                                         ),
            //                                       ),
            //                                     ),
            //                                   ),
            //                                 ),
            //                                 Fun(
            //                                   Text("a"),
            //                                   Var("A"),
            //                                   Fun(
            //                                     Text("p"),
            //                                     Ap(Var("P"), Var("a")),
            //                                     Var("p"),
            //                                   ),
            //                                 ),
            //                               ),
            //                               Var("a"),
            //                             ),
            //                             Var("a"),
            //                           ),
            //                           Ap(
            //                             Ap(Var("refl"), Var("A")),
            //                             Var("a"),
            //                           ),
            //                         ),
            //                       ),
            //                       Fun(
            //                         Text("x"),
            //                         Ap(Var("P"), Var("a")),
            //                         Var("x"),
            //                       ),
            //                     ),
            //                     Hole,
            //                   ),
            //                   Var("eq1"),
            //                 ),
            //                 Hole,
            //               ),
            //             ),
            //           ),
            //         ),
            //       ),
            //     ),
            //   ),
            Let(
              Text("sym"),
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
                      Hole,
                      Ap(
                        Ap(Ap(Var("eq"), Var("A")), Var("a")),
                        Var("b"),
                      ),
                      Ap(
                        Ap(Ap(Var("eq"), Var("A")), Var("b")),
                        Var("a"),
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
                      Text("f1"),
                      Ap(
                        Ap(Ap(Var("eq"), Var("A")), Var("a")),
                        Var("b"),
                      ),
                      Ap(
                        Ap(
                          Ap(
                            Ap(
                              Ap(Ap(Var("subst"), Var("A")), Var("a")),
                              Var("b"),
                            ),
                            Fun(
                              Text("x"),
                              Var("A"),
                              Ap(
                                Ap(Ap(Var("eq"), Var("A")), Var("x")),
                                Var("a"),
                              ),
                            ),
                          ),
                          Var("f1"),
                        ),
                        Ap(Ap(Var("refl"), Var("A")), Var("a")),
                      ),
                    ),
                  ),
                ),
              ),
              Let(
                Text("trans"),
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
                        Text("c"),
                        Var("A"),
                        Arrow(
                          Hole,
                          Ap(
                            Ap(Ap(Var("eq"), Var("A")), Var("a")),
                            Var("b"),
                          ),
                          Arrow(
                            Hole,
                            Ap(
                              Ap(Ap(Var("eq"), Var("A")), Var("b")),
                              Var("c"),
                            ),
                            Ap(
                              Ap(Ap(Var("eq"), Var("A")), Var("a")),
                              Var("c"),
                            ),
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
                        Text("c"),
                        Var("A"),
                        Fun(
                          Text("f1"),
                          Ap(
                            Ap(Ap(Var("eq"), Var("A")), Var("a")),
                            Var("b"),
                          ),
                          Fun(
                            Text("f2"),
                            Ap(
                              Ap(Ap(Var("eq"), Var("A")), Var("b")),
                              Var("c"),
                            ),
                            Ap(
                              Ap(
                                Ap(
                                  Ap(
                                    Ap(
                                      Ap(Var("subst"), Var("A")),
                                      Var("b"),
                                    ),
                                    Var("c"),
                                  ),
                                  Fun(
                                    Text("x"),
                                    Var("A"),
                                    Ap(
                                      Ap(
                                        Ap(Var("eq"), Var("A")),
                                        Var("a"),
                                      ),
                                      Var("x"),
                                    ),
                                  ),
                                ),
                                Var("f2"),
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
                  Text("cong"),
                  Arrow(
                    Text("A"),
                    Typ,
                    Arrow(
                      Text("B"),
                      Typ,
                      Arrow(
                        Text("a"),
                        Var("A"),
                        Arrow(
                          Text("b"),
                          Var("A"),
                          Arrow(
                            Text("f"),
                            Arrow(Hole, Var("A"), Var("B")),
                            Arrow(
                              Hole,
                              Ap(
                                Ap(Ap(Var("eq"), Var("A")), Var("a")),
                                Var("b"),
                              ),
                              Ap(
                                Ap(
                                  Ap(Var("eq"), Var("B")),
                                  Ap(Var("f"), Var("a")),
                                ),
                                Ap(Var("f"), Var("b")),
                              ),
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
                      Text("B"),
                      Typ,
                      Fun(
                        Text("a"),
                        Var("A"),
                        Fun(
                          Text("b"),
                          Var("A"),
                          Fun(
                            Text("f"),
                            Arrow(Hole, Var("A"), Var("B")),
                            Fun(
                              Text("f1"),
                              Ap(
                                Ap(Ap(Var("eq"), Var("A")), Var("a")),
                                Var("b"),
                              ),
                              Ap(
                                Ap(
                                  Ap(
                                    Ap(
                                      Ap(
                                        Ap(Var("subst"), Var("A")),
                                        Var("a"),
                                      ),
                                      Var("b"),
                                    ),
                                    Fun(
                                      Text("x"),
                                      Var("A"),
                                      Ap(
                                        Ap(
                                          Ap(Var("eq"), Var("B")),
                                          Ap(Var("f"), Var("a")),
                                        ),
                                        Ap(Var("f"), Var("x")),
                                      ),
                                    ),
                                  ),
                                  Var("f1"),
                                ),
                                Ap(
                                  Ap(Var("refl"), Var("B")),
                                  Ap(Var("f"), Var("a")),
                                ),
                              ),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                  Let(
                    Text("eq-step"),
                    Arrow(
                      Text("A"),
                      Typ,
                      Arrow(
                        Text("B"),
                        Typ,
                        Arrow(
                          Text("a"),
                          Var("B"),
                          Arrow(
                            Text("b"),
                            Var("B"),
                            Arrow(
                              Text("f"),
                              Arrow(Hole, Var("B"), Var("A")),
                              Arrow(
                                Text("c"),
                                Var("A"),
                                Arrow(
                                  Hole,
                                  Ap(
                                    Ap(Ap(Var("eq"), Var("B")), Var("a")),
                                    Var("b"),
                                  ),
                                  Arrow(
                                    Hole,
                                    Ap(
                                      Ap(
                                        Ap(Var("eq"), Var("A")),
                                        Ap(Var("f"), Var("b")),
                                      ),
                                      Var("c"),
                                    ),
                                    Ap(
                                      Ap(
                                        Ap(Var("eq"), Var("A")),
                                        Ap(Var("f"), Var("a")),
                                      ),
                                      Var("c"),
                                    ),
                                  ),
                                ),
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
                        Text("B"),
                        Typ,
                        Fun(
                          Text("a"),
                          Var("B"),
                          Fun(
                            Text("b"),
                            Var("B"),
                            Fun(
                              Text("f"),
                              Arrow(Hole, Var("B"), Var("A")),
                              Fun(
                                Text("c"),
                                Var("A"),
                                Fun(
                                  Text("f1"),
                                  Ap(
                                    Ap(Ap(Var("eq"), Var("B")), Var("a")),
                                    Var("b"),
                                  ),
                                  Fun(
                                    Text("f2"),
                                    Ap(
                                      Ap(
                                        Ap(Var("eq"), Var("A")),
                                        Ap(Var("f"), Var("b")),
                                      ),
                                      Var("c"),
                                    ),
                                    Ap(
                                      Ap(
                                        Ap(
                                          Ap(
                                            Ap(
                                              Ap(Var("trans"), Var("A")),
                                              Ap(Var("f"), Var("a")),
                                            ),
                                            Ap(Var("f"), Var("b")),
                                          ),
                                          Var("c"),
                                        ),
                                        Ap(
                                          Ap(
                                            Ap(
                                              Ap(
                                                Ap(
                                                  Ap(Var("cong"), Var("B")),
                                                  Var("A"),
                                                ),
                                                Var("a"),
                                              ),
                                              Var("b"),
                                            ),
                                            Var("f"),
                                          ),
                                          Var("f1"),
                                        ),
                                      ),
                                      Var("f2"),
                                    ),
                                  ),
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
                            Text("nat-ind"),
                            Arrow(
                              Text("P"),
                              Arrow(Hole, Var("nat"), Typ),
                              Arrow(
                                Hole,
                                Ap(Var("P"), Var("Z")),
                                Arrow(
                                  Hole,
                                  Arrow(
                                    Text("x"),
                                    Var("nat"),
                                    Arrow(
                                      Hole,
                                      Ap(Var("P"), Var("x")),
                                      Ap(
                                        Var("P"),
                                        Ap(Var("S"), Var("x")),
                                      ),
                                    ),
                                  ),
                                  Arrow(
                                    Text("n"),
                                    Var("nat"),
                                    Ap(Var("P"), Var("n")),
                                  ),
                                ),
                              ),
                            ),
                            Hole,
                            Let(
                              Text("nat-ind-eq-Z"),
                              Arrow(
                                Text("P"),
                                Arrow(Hole, Var("nat"), Typ),
                                Arrow(
                                  Text("pZ"),
                                  Ap(Var("P"), Var("Z")),
                                  Arrow(
                                    Text("pS"),
                                    Arrow(
                                      Text("x"),
                                      Var("nat"),
                                      Arrow(
                                        Hole,
                                        Ap(Var("P"), Var("x")),
                                        Ap(
                                          Var("P"),
                                          Ap(Var("S"), Var("x")),
                                        ),
                                      ),
                                    ),
                                    Ap(
                                      Ap(
                                        Ap(
                                          Var("eq"),
                                          Ap(Var("P"), Var("Z")),
                                        ),
                                        Ap(
                                          Ap(
                                            Ap(
                                              Ap(Var("nat-ind"), Var("P")),
                                              Var("pZ"),
                                            ),
                                            Var("pS"),
                                          ),
                                          Var("Z"),
                                        ),
                                      ),
                                      Var("pZ"),
                                    ),
                                  ),
                                ),
                              ),
                              Hole,
                              Let(
                                Text("nat-ind-eq-S"),
                                Arrow(
                                  Text("P"),
                                  Arrow(Hole, Var("nat"), Typ),
                                  Arrow(
                                    Text("pZ"),
                                    Ap(Var("P"), Var("Z")),
                                    Arrow(
                                      Text("pS"),
                                      Arrow(
                                        Text("x"),
                                        Var("nat"),
                                        Arrow(
                                          Hole,
                                          Ap(Var("P"), Var("x")),
                                          Ap(
                                            Var("P"),
                                            Ap(Var("S"), Var("x")),
                                          ),
                                        ),
                                      ),
                                      Arrow(
                                        Text("x"),
                                        Var("nat"),
                                        Ap(
                                          Ap(
                                            Ap(
                                              Var("eq"),
                                              Ap(
                                                Var("P"),
                                                Ap(Var("S"), Var("x")),
                                              ),
                                            ),
                                            Ap(
                                              Ap(
                                                Ap(
                                                  Ap(
                                                    Var("nat-ind"),
                                                    Var("P"),
                                                  ),
                                                  Var("pZ"),
                                                ),
                                                Var("pS"),
                                              ),
                                              Ap(Var("S"), Var("x")),
                                            ),
                                          ),
                                          Ap(
                                            Ap(Var("pS"), Var("x")),
                                            Ap(
                                              Ap(
                                                Ap(
                                                  Ap(
                                                    Var("nat-ind"),
                                                    Var("P"),
                                                  ),
                                                  Var("pZ"),
                                                ),
                                                Var("pS"),
                                              ),
                                              Var("x"),
                                            ),
                                          ),
                                        ),
                                      ),
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
                                          Ap(
                                            Ap(
                                              Var("nat-ind"),
                                              Fun(Hole, Hole, Var("nat")),
                                            ),
                                            Var("m"),
                                          ),
                                          Fun(Hole, Hole, Var("S")),
                                        ),
                                        Var("n"),
                                      ),
                                    ),
                                  ),
                                  Let(
                                    Text("plus-Z"),
                                    Arrow(
                                      Text("n"),
                                      Var("nat"),
                                      Ap(
                                        Ap(
                                          Ap(Var("eq"), Var("nat")),
                                          Ap(
                                            Ap(Var("plus"), Var("n")),
                                            Var("Z"),
                                          ),
                                        ),
                                        Var("n"),
                                      ),
                                    ),
                                    Ap(
                                      Ap(
                                        Ap(
                                          Var("nat-ind"),
                                          Fun(
                                            Text("n"),
                                            Var("nat"),
                                            Ap(
                                              Ap(
                                                Ap(Var("eq"), Var("nat")),
                                                Ap(
                                                  Ap(Var("plus"), Var("n")),
                                                  Var("Z"),
                                                ),
                                              ),
                                              Var("n"),
                                            ),
                                          ),
                                        ),
                                        Ap(
                                          Ap(
                                            Ap(
                                              Var("nat-ind-eq-Z"),
                                              Fun(Hole, Hole, Var("nat")),
                                            ),
                                            Var("Z"),
                                          ),
                                          Fun(Hole, Hole, Var("S")),
                                        ),
                                      ),
                                      Fun(
                                        Text("x"),
                                        Var("nat"),
                                        Fun(
                                          Text("f1"),
                                          Ap(
                                            Fun(
                                              Text("n"),
                                              Var("nat"),
                                              Ap(
                                                Ap(
                                                  Ap(Var("eq"), Var("nat")),
                                                  Ap(
                                                    Ap(
                                                      Var("plus"),
                                                      Var("n"),
                                                    ),
                                                    Var("Z"),
                                                  ),
                                                ),
                                                Var("n"),
                                              ),
                                            ),
                                            Var("x"),
                                          ),
                                          Ap(
                                            Ap(
                                              Ap(
                                                Ap(
                                                  Ap(
                                                    Ap(
                                                      Ap(
                                                        Ap(
                                                          Var("eq-step"),
                                                          Var("nat"),
                                                        ),
                                                        Var("nat"),
                                                      ),
                                                      Ap(
                                                        Ap(
                                                          Var("plus"),
                                                          Ap(
                                                            Var("S"),
                                                            Var("x"),
                                                          ),
                                                        ),
                                                        Var("Z"),
                                                      ),
                                                    ),
                                                    Ap(
                                                      Var("S"),
                                                      Ap(
                                                        Ap(
                                                          Var("plus"),
                                                          Var("x"),
                                                        ),
                                                        Var("Z"),
                                                      ),
                                                    ),
                                                  ),
                                                  Fun(
                                                    Text("x"),
                                                    Var("nat"),
                                                    Var("x"),
                                                  ),
                                                ),
                                                Ap(Var("S"), Var("x")),
                                              ),
                                              Ap(
                                                Ap(
                                                  Ap(
                                                    Ap(
                                                      Var("nat-ind-eq-S"),
                                                      Fun(
                                                        Hole,
                                                        Hole,
                                                        Var("nat"),
                                                      ),
                                                    ),
                                                    Var("Z"),
                                                  ),
                                                  Fun(Hole, Hole, Var("S")),
                                                ),
                                                Var("x"),
                                              ),
                                            ),
                                            Ap(
                                              Ap(
                                                Ap(
                                                  Ap(
                                                    Ap(
                                                      Ap(
                                                        Ap(
                                                          Ap(
                                                            Var("eq-step"),
                                                            Var("nat"),
                                                          ),
                                                          Var("nat"),
                                                        ),
                                                        Ap(
                                                          Ap(
                                                            Var("plus"),
                                                            Var("x"),
                                                          ),
                                                          Var("Z"),
                                                        ),
                                                      ),
                                                      Var("x"),
                                                    ),
                                                    Fun(
                                                      Text("x"),
                                                      Var("nat"),
                                                      Ap(
                                                        Var("S"),
                                                        Var("x"),
                                                      ),
                                                    ),
                                                  ),
                                                  Ap(Var("S"), Var("x")),
                                                ),
                                                Var("f1"),
                                              ),
                                              Ap(
                                                Ap(Var("refl"), Var("nat")),
                                                Ap(Var("S"), Var("x")),
                                              ),
                                            ),
                                          ),
                                        ),
                                      ),
                                    ),
                                    Let(
                                      Text("int"),
                                      Typ,
                                      Hole,
                                      Let(
                                        Text("dif"),
                                        Arrow(
                                          Hole,
                                          Var("nat"),
                                          Arrow(
                                            Hole,
                                            Var("nat"),
                                            Var("int"),
                                          ),
                                        ),
                                        Hole,
                                        Let(
                                          Text("int-equiv"),
                                          Arrow(
                                            Hole,
                                            Var("nat"),
                                            Arrow(
                                              Hole,
                                              Var("nat"),
                                              Arrow(
                                                Hole,
                                                Var("nat"),
                                                Arrow(Hole, Var("nat"), Typ),
                                              ),
                                            ),
                                          ),
                                          Fun(
                                            Text("a"),
                                            Var("nat"),
                                            Fun(
                                              Text("b"),
                                              Var("nat"),
                                              Fun(
                                                Text("c"),
                                                Var("nat"),
                                                Fun(
                                                  Text("d"),
                                                  Var("nat"),
                                                  Ap(
                                                    Ap(
                                                      Ap(
                                                        Var("eq"),
                                                        Var("nat"),
                                                      ),
                                                      Ap(
                                                        Ap(
                                                          Var("plus"),
                                                          Var("a"),
                                                        ),
                                                        Var("d"),
                                                      ),
                                                    ),
                                                    Ap(
                                                      Ap(
                                                        Var("plus"),
                                                        Var("c"),
                                                      ),
                                                      Var("b"),
                                                    ),
                                                  ),
                                                ),
                                              ),
                                            ),
                                          ),
                                          Let(
                                            Text("int-ind"),
                                            Arrow(
                                              Text("P"),
                                              Arrow(Hole, Var("int"), Typ),
                                              Arrow(
                                                Text("go"),
                                                Arrow(
                                                  Text("a"),
                                                  Var("nat"),
                                                  Arrow(
                                                    Text("b"),
                                                    Var("nat"),
                                                    Ap(
                                                      Var("P"),
                                                      Ap(
                                                        Ap(
                                                          Var("dif"),
                                                          Var("a"),
                                                        ),
                                                        Var("b"),
                                                      ),
                                                    ),
                                                  ),
                                                ),
                                                Arrow(
                                                  Text("a"),
                                                  Var("int"),
                                                  Ap(Var("P"), Var("a")),
                                                ),
                                              ),
                                            ),
                                            Hole,
                                            Let(
                                              Text("int-lift"),
                                              Arrow(
                                                Text("P"),
                                                Typ,
                                                Arrow(
                                                  Text("go"),
                                                  Arrow(
                                                    Hole,
                                                    Var("nat"),
                                                    Arrow(
                                                      Hole,
                                                      Var("nat"),
                                                      Var("P"),
                                                    ),
                                                  ),
                                                  Arrow(
                                                    Hole,
                                                    Arrow(
                                                      Text("a"),
                                                      Var("nat"),
                                                      Arrow(
                                                        Text("b"),
                                                        Var("nat"),
                                                        Arrow(
                                                          Text("c"),
                                                          Var("nat"),
                                                          Arrow(
                                                            Text("d"),
                                                            Var("nat"),
                                                            Arrow(
                                                              Hole,
                                                              Ap(
                                                                Ap(
                                                                  Ap(
                                                                    Ap(
                                                                    Var(
                                                                    "int-equiv",
                                                                    ),
                                                                    Var("a"),
                                                                    ),
                                                                    Var("b"),
                                                                  ),
                                                                  Var("c"),
                                                                ),
                                                                Var("d"),
                                                              ),
                                                              Ap(
                                                                Ap(
                                                                  Ap(
                                                                    Var("eq"),
                                                                    Var("P"),
                                                                  ),
                                                                  Ap(
                                                                    Ap(
                                                                    Var("go"),
                                                                    Var("a"),
                                                                    ),
                                                                    Var("b"),
                                                                  ),
                                                                ),
                                                                Ap(
                                                                  Ap(
                                                                    Var("go"),
                                                                    Var("c"),
                                                                  ),
                                                                  Var("d"),
                                                                ),
                                                              ),
                                                            ),
                                                          ),
                                                        ),
                                                      ),
                                                    ),
                                                    Arrow(
                                                      Hole,
                                                      Var("int"),
                                                      Var("P"),
                                                    ),
                                                  ),
                                                ),
                                              ),
                                              Hole,
                                              Let(
                                                Text("int-lift-eq"),
                                                Arrow(
                                                  Text("P"),
                                                  Typ,
                                                  Arrow(
                                                    Text("go"),
                                                    Arrow(
                                                      Hole,
                                                      Var("nat"),
                                                      Arrow(
                                                        Hole,
                                                        Var("nat"),
                                                        Var("P"),
                                                      ),
                                                    ),
                                                    Arrow(
                                                      Text("h"),
                                                      Arrow(
                                                        Text("a"),
                                                        Var("nat"),
                                                        Arrow(
                                                          Text("b"),
                                                          Var("nat"),
                                                          Arrow(
                                                            Text("c"),
                                                            Var("nat"),
                                                            Arrow(
                                                              Text("d"),
                                                              Var("nat"),
                                                              Arrow(
                                                                Hole,
                                                                Ap(
                                                                  Ap(
                                                                    Ap(
                                                                    Ap(
                                                                    Var(
                                                                    "int-equiv",
                                                                    ),
                                                                    Var("a"),
                                                                    ),
                                                                    Var("b"),
                                                                    ),
                                                                    Var("c"),
                                                                  ),
                                                                  Var("d"),
                                                                ),
                                                                Ap(
                                                                  Ap(
                                                                    Ap(
                                                                    Var("eq"),
                                                                    Var("P"),
                                                                    ),
                                                                    Ap(
                                                                    Ap(
                                                                    Var("go"),
                                                                    Var("a"),
                                                                    ),
                                                                    Var("b"),
                                                                    ),
                                                                  ),
                                                                  Ap(
                                                                    Ap(
                                                                    Var("go"),
                                                                    Var("c"),
                                                                    ),
                                                                    Var("d"),
                                                                  ),
                                                                ),
                                                              ),
                                                            ),
                                                          ),
                                                        ),
                                                      ),
                                                      Arrow(
                                                        Text("a"),
                                                        Var("nat"),
                                                        Arrow(
                                                          Text("b"),
                                                          Var("nat"),
                                                          Ap(
                                                            Ap(
                                                              Ap(
                                                                Var("eq"),
                                                                Var("P"),
                                                              ),
                                                              Ap(
                                                                Ap(
                                                                  Ap(
                                                                    Ap(
                                                                    Var(
                                                                    "int-lift",
                                                                    ),
                                                                    Var("P"),
                                                                    ),
                                                                    Var("go"),
                                                                  ),
                                                                  Var("h"),
                                                                ),
                                                                Ap(
                                                                  Ap(
                                                                    Var(
                                                                    "dif",
                                                                    ),
                                                                    Var("a"),
                                                                  ),
                                                                  Var("b"),
                                                                ),
                                                              ),
                                                            ),
                                                            Ap(
                                                              Ap(
                                                                Var("go"),
                                                                Var("a"),
                                                              ),
                                                              Var("b"),
                                                            ),
                                                          ),
                                                        ),
                                                      ),
                                                    ),
                                                  ),
                                                ),
                                                Hole,
                                                Let(
                                                  Text("int-dif-eq"),
                                                  Arrow(
                                                    Text("a"),
                                                    Var("nat"),
                                                    Arrow(
                                                      Text("b"),
                                                      Var("nat"),
                                                      Arrow(
                                                        Text("c"),
                                                        Var("nat"),
                                                        Arrow(
                                                          Text("d"),
                                                          Var("nat"),
                                                          Arrow(
                                                            Hole,
                                                            Ap(
                                                              Ap(
                                                                Ap(
                                                                  Ap(
                                                                    Var(
                                                                    "int-equiv",
                                                                    ),
                                                                    Var("a"),
                                                                  ),
                                                                  Var("b"),
                                                                ),
                                                                Var("c"),
                                                              ),
                                                              Var("d"),
                                                            ),
                                                            Ap(
                                                              Ap(
                                                                Ap(
                                                                  Var("eq"),
                                                                  Var("int"),
                                                                ),
                                                                Ap(
                                                                  Ap(
                                                                    Var(
                                                                    "dif",
                                                                    ),
                                                                    Var("a"),
                                                                  ),
                                                                  Var("b"),
                                                                ),
                                                              ),
                                                              Ap(
                                                                Ap(
                                                                  Var("dif"),
                                                                  Var("c"),
                                                                ),
                                                                Var("d"),
                                                              ),
                                                            ),
                                                          ),
                                                        ),
                                                      ),
                                                    ),
                                                  ),
                                                  Hole,
                                                  Let(
                                                    Text("int-plus"),
                                                    Arrow(
                                                      Hole,
                                                      Var("int"),
                                                      Arrow(
                                                        Hole,
                                                        Var("int"),
                                                        Var("int"),
                                                      ),
                                                    ),
                                                    Fun(
                                                      Text("z1"),
                                                      Var("int"),
                                                      Fun(
                                                        Text("z2"),
                                                        Var("int"),
                                                        Let(
                                                          Text("go"),
                                                          Arrow(
                                                            Hole,
                                                            Var("nat"),
                                                            Arrow(
                                                              Hole,
                                                              Var("nat"),
                                                              Arrow(
                                                                Hole,
                                                                Var("int"),
                                                                Var("int"),
                                                              ),
                                                            ),
                                                          ),
                                                          Fun(
                                                            Text("a"),
                                                            Var("nat"),
                                                            Fun(
                                                              Text("b"),
                                                              Var("nat"),
                                                              Let(
                                                                Text("go"),
                                                                Arrow(
                                                                  Hole,
                                                                  Var("nat"),
                                                                  Arrow(
                                                                    Hole,
                                                                    Var(
                                                                    "nat",
                                                                    ),
                                                                    Var(
                                                                    "int",
                                                                    ),
                                                                  ),
                                                                ),
                                                                Fun(
                                                                  Text("c"),
                                                                  Var("nat"),
                                                                  Fun(
                                                                    Text("d"),
                                                                    Var(
                                                                    "nat",
                                                                    ),
                                                                    Ap(
                                                                    Ap(
                                                                    Var(
                                                                    "dif",
                                                                    ),
                                                                    Ap(
                                                                    Ap(
                                                                    Var(
                                                                    "plus",
                                                                    ),
                                                                    Var("a"),
                                                                    ),
                                                                    Var("c"),
                                                                    ),
                                                                    ),
                                                                    Ap(
                                                                    Ap(
                                                                    Var(
                                                                    "plus",
                                                                    ),
                                                                    Var("b"),
                                                                    ),
                                                                    Var("d"),
                                                                    ),
                                                                    ),
                                                                  ),
                                                                ),
                                                                Let(
                                                                  Text("pf"),
                                                                  Arrow(
                                                                    Text(
                                                                    "c1",
                                                                    ),
                                                                    Var(
                                                                    "nat",
                                                                    ),
                                                                    Arrow(
                                                                    Text(
                                                                    "d1",
                                                                    ),
                                                                    Var(
                                                                    "nat",
                                                                    ),
                                                                    Arrow(
                                                                    Text(
                                                                    "c2",
                                                                    ),
                                                                    Var(
                                                                    "nat",
                                                                    ),
                                                                    Arrow(
                                                                    Text(
                                                                    "d2",
                                                                    ),
                                                                    Var(
                                                                    "nat",
                                                                    ),
                                                                    Arrow(
                                                                    Hole,
                                                                    Ap(
                                                                    Ap(
                                                                    Ap(
                                                                    Ap(
                                                                    Var(
                                                                    "int-equiv",
                                                                    ),
                                                                    Var("c1"),
                                                                    ),
                                                                    Var("d1"),
                                                                    ),
                                                                    Var("c2"),
                                                                    ),
                                                                    Var("d2"),
                                                                    ),
                                                                    Ap(
                                                                    Ap(
                                                                    Ap(
                                                                    Var("eq"),
                                                                    Var(
                                                                    "int",
                                                                    ),
                                                                    ),
                                                                    Ap(
                                                                    Ap(
                                                                    Var("go"),
                                                                    Var("c1"),
                                                                    ),
                                                                    Var("d1"),
                                                                    ),
                                                                    ),
                                                                    Ap(
                                                                    Ap(
                                                                    Var("go"),
                                                                    Var("c2"),
                                                                    ),
                                                                    Var("d2"),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                  ),
                                                                  Fun(
                                                                    Text(
                                                                    "c1",
                                                                    ),
                                                                    Var(
                                                                    "nat",
                                                                    ),
                                                                    Fun(
                                                                    Text(
                                                                    "d1",
                                                                    ),
                                                                    Var(
                                                                    "nat",
                                                                    ),
                                                                    Fun(
                                                                    Text(
                                                                    "c2",
                                                                    ),
                                                                    Var(
                                                                    "nat",
                                                                    ),
                                                                    Fun(
                                                                    Text(
                                                                    "d2",
                                                                    ),
                                                                    Var(
                                                                    "nat",
                                                                    ),
                                                                    Fun(
                                                                    Text(
                                                                    "f1",
                                                                    ),
                                                                    Ap(
                                                                    Ap(
                                                                    Ap(
                                                                    Ap(
                                                                    Var(
                                                                    "int-equiv",
                                                                    ),
                                                                    Var("c1"),
                                                                    ),
                                                                    Var("d1"),
                                                                    ),
                                                                    Var("c2"),
                                                                    ),
                                                                    Var("d2"),
                                                                    ),
                                                                    Let(
                                                                    Text(
                                                                    "arith",
                                                                    ),
                                                                    Ap(
                                                                    Ap(
                                                                    Ap(
                                                                    Var("eq"),
                                                                    Var(
                                                                    "nat",
                                                                    ),
                                                                    ),
                                                                    Ap(
                                                                    Ap(
                                                                    Var(
                                                                    "plus",
                                                                    ),
                                                                    Ap(
                                                                    Ap(
                                                                    Var(
                                                                    "plus",
                                                                    ),
                                                                    Var("a"),
                                                                    ),
                                                                    Var("c1"),
                                                                    ),
                                                                    ),
                                                                    Ap(
                                                                    Ap(
                                                                    Var(
                                                                    "plus",
                                                                    ),
                                                                    Var("b"),
                                                                    ),
                                                                    Var("d2"),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    Ap(
                                                                    Ap(
                                                                    Var(
                                                                    "plus",
                                                                    ),
                                                                    Ap(
                                                                    Ap(
                                                                    Var(
                                                                    "plus",
                                                                    ),
                                                                    Var("a"),
                                                                    ),
                                                                    Var("c2"),
                                                                    ),
                                                                    ),
                                                                    Ap(
                                                                    Ap(
                                                                    Var(
                                                                    "plus",
                                                                    ),
                                                                    Var("b"),
                                                                    ),
                                                                    Var("d1"),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    Hole,
                                                                    Ap(
                                                                    Ap(
                                                                    Ap(
                                                                    Ap(
                                                                    Ap(
                                                                    Var(
                                                                    "int-dif-eq",
                                                                    ),
                                                                    Ap(
                                                                    Ap(
                                                                    Var(
                                                                    "plus",
                                                                    ),
                                                                    Var("a"),
                                                                    ),
                                                                    Var("c1"),
                                                                    ),
                                                                    ),
                                                                    Ap(
                                                                    Ap(
                                                                    Var(
                                                                    "plus",
                                                                    ),
                                                                    Var("b"),
                                                                    ),
                                                                    Var("d1"),
                                                                    ),
                                                                    ),
                                                                    Ap(
                                                                    Ap(
                                                                    Var(
                                                                    "plus",
                                                                    ),
                                                                    Var("a"),
                                                                    ),
                                                                    Var("c2"),
                                                                    ),
                                                                    ),
                                                                    Ap(
                                                                    Ap(
                                                                    Var(
                                                                    "plus",
                                                                    ),
                                                                    Var("b"),
                                                                    ),
                                                                    Var("d2"),
                                                                    ),
                                                                    ),
                                                                    Var(
                                                                    "arith",
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                  ),
                                                                  Ap(
                                                                    Ap(
                                                                    Ap(
                                                                    Var(
                                                                    "int-lift",
                                                                    ),
                                                                    Var(
                                                                    "int",
                                                                    ),
                                                                    ),
                                                                    Var("go"),
                                                                    ),
                                                                    Var("pf"),
                                                                  ),
                                                                ),
                                                              ),
                                                            ),
                                                          ),
                                                          Let(
                                                            Text("pf"),
                                                            Arrow(
                                                              Text("a1"),
                                                              Var("nat"),
                                                              Arrow(
                                                                Text("b1"),
                                                                Var("nat"),
                                                                Arrow(
                                                                  Text("a2"),
                                                                  Var("nat"),
                                                                  Arrow(
                                                                    Text(
                                                                    "b2",
                                                                    ),
                                                                    Var(
                                                                    "nat",
                                                                    ),
                                                                    Arrow(
                                                                    Hole,
                                                                    Ap(
                                                                    Ap(
                                                                    Ap(
                                                                    Ap(
                                                                    Var(
                                                                    "int-equiv",
                                                                    ),
                                                                    Var("a1"),
                                                                    ),
                                                                    Var("b1"),
                                                                    ),
                                                                    Var("a2"),
                                                                    ),
                                                                    Var("b2"),
                                                                    ),
                                                                    Ap(
                                                                    Ap(
                                                                    Ap(
                                                                    Var("eq"),
                                                                    Arrow(
                                                                    Hole,
                                                                    Var(
                                                                    "int",
                                                                    ),
                                                                    Var(
                                                                    "int",
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    Ap(
                                                                    Ap(
                                                                    Var("go"),
                                                                    Hole,
                                                                    ),
                                                                    Hole,
                                                                    ),
                                                                    ),
                                                                    Hole,
                                                                    ),
                                                                    ),
                                                                  ),
                                                                ),
                                                              ),
                                                            ),
                                                            Hole,
                                                            Ap(
                                                              Ap(
                                                                Ap(
                                                                  Ap(
                                                                    Ap(
                                                                    Var(
                                                                    "int-lift",
                                                                    ),
                                                                    Arrow(
                                                                    Hole,
                                                                    Var(
                                                                    "int",
                                                                    ),
                                                                    Var(
                                                                    "int",
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    Var("go"),
                                                                  ),
                                                                  Var("pf"),
                                                                ),
                                                                Var("z1"),
                                                              ),
                                                              Var("z2"),
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
    // ),
  );

let library = eq_library /*            */;
let context = [];
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
