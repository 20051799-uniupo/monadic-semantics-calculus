@ -0,0 +1,250 @@
---Example 18---
- predfun succ 0:
ExpConf (Do "p" (IsZero (NumVal (Succ Zero))) (If (IdentifierVal "p") (Magic (Raise "PredZero") []) (Pred (NumVal (Succ Zero))))) =>* Right (Ok (NumVal Zero))
ExpConf (Do "p" (IsZero (NumVal (Succ Zero))) (If (IdentifierVal "p") (Magic (Raise "PredZero") []) (Pred (NumVal (Succ Zero))))) ->
Right (ExpConf (Do "p" (Ret (BoolVal False)) (If (IdentifierVal "p") (Magic (Raise "PredZero") []) (Pred (NumVal (Succ Zero)))))) =>
Right (ExpConf (If (BoolVal False) (Magic (Raise "PredZero") []) (Pred (NumVal (Succ Zero))))) =>
Right (ExpConf (Pred (NumVal (Succ Zero)))) =>
Right (ExpConf (Ret (NumVal Zero))) =>
Right (ResConf (Ok (NumVal Zero)))
- predfun 0:
ExpConf (Do "p" (IsZero (NumVal Zero)) (If (IdentifierVal "p") (Magic (Raise "PredZero") []) (Pred (NumVal Zero)))) =>* Left "PredZero"
ExpConf (Do "p" (IsZero (NumVal (Succ Zero))) (If (IdentifierVal "p") (Magic (Raise "PredZero") []) (Pred (NumVal (Succ Zero))))) ->
Right (ExpConf (Do "p" (Ret (BoolVal True)) (If (IdentifierVal "p") (Magic (Raise "PredZero") []) (Pred (NumVal Zero))))) =>
Right (ExpConf (If (BoolVal True) (Magic (Raise "PredZero") []) (Pred (NumVal Zero)))) =>
Right (ExpConf (Magic (Raise "PredZero") [])) =>
Left "PredZero"
---Example 19---
- e:
ExpConf (Do "y" (Magic Choose []) (If (IdentifierVal "y") (Ret (NumVal Zero)) (Ret (NumVal (Succ Zero))))) =>* [Ok (NumVal Zero),Ok (NumVal (Succ Zero))]
ExpConf (Do "y" (Magic Choose []) (If (IdentifierVal "y") (Ret (NumVal Zero)) (Ret (NumVal (Succ Zero))))) ->
[ExpConf (Do "y" (Ret (BoolVal True)) (If (IdentifierVal "y") (Ret (NumVal Zero)) (Ret (NumVal (Succ Zero))))), 
 ExpConf (Do "y" (Ret (BoolVal False)) (If (IdentifierVal "y") (Ret (NumVal Zero)) (Ret (NumVal (Succ Zero)))))] =>
[ExpConf (If (BoolVal True) (Ret (NumVal Zero)) (Ret (NumVal (Succ Zero)))),
 ExpConf (If (BoolVal False) (Ret (NumVal Zero)) (Ret (NumVal (Succ Zero))))] =>
[ExpConf (Ret (NumVal Zero)),ExpConf (Ret (NumVal (Succ Zero)))] =>
[ResConf (Ok (NumVal Zero)),ResConf (Ok (NumVal (Succ Zero)))]
- chfun 0:
ExpConf (App 
          (RecLamVal "f" "x" 
            (Do "y" (Magic Choose []) 
              (If (IdentifierVal "y") 
                (Ret (IdentifierVal "x"))
                (Do "s" (Suc (IdentifierVal "x")) 
                  (App (IdentifierVal "f") (IdentifierVal "s"))
                )
              )
            )
          )
          (NumVal Zero)
        ) 
->
[ExpConf (Do "y" (Magic Choose []) 
           (If (IdentifierVal "y") 
             (Ret (NumVal Zero))
             (Do "s" (Suc (NumVal Zero))
               (App 
                 (RecLamVal "f" "x" 
                   (Do "y" (Magic Choose [])
                     (If (IdentifierVal "y") 
                       (Ret (IdentifierVal "x"))
                       (Do "s" (Suc (IdentifierVal "x")) 
                         (App (IdentifierVal "f") (IdentifierVal "s"))
                       )
                      )
                    )
                  )
                  (IdentifierVal "s")
                )
              )
           )
          )  
] =>
[ExpConf (Do "y" (Ret (BoolVal True)) (If (IdentifierVal "y") (Ret (NumVal Zero)) (Do "s" (Suc (NumVal Zero)) 
            (App (RecLamVal "f" "x" (Do "y" (Magic Choose []) (If (IdentifierVal "y") (Ret (IdentifierVal "x")) (Do "s" (Suc (IdentifierVal "x")) 
                   (App (IdentifierVal "f") (IdentifierVal "s")))))) (IdentifierVal "s"))))),
 ExpConf (Do "y" (Ret (BoolVal False)) (If (IdentifierVal "y") (Ret (NumVal Zero)) (Do "s" (Suc (NumVal Zero)) 
            (App (RecLamVal "f" "x" (Do "y" (Magic Choose []) (If (IdentifierVal "y") (Ret (IdentifierVal "x")) (Do "s" (Suc (IdentifierVal "x")) 
                   (App (IdentifierVal "f") (IdentifierVal "s")))))) (IdentifierVal "s")))))
] =>
[ExpConf (If (BoolVal True) (Ret (NumVal Zero)) (Do "s" (Suc (NumVal Zero)) 
            (App (RecLamVal "f" "x" (Do "y" (Magic Choose []) (If (IdentifierVal "y") (Ret (IdentifierVal "x")) (Do "s" (Suc (IdentifierVal "x")) 
                 (App (IdentifierVal "f") (IdentifierVal "s")))))) (IdentifierVal "s")))),
 ExpConf (If (BoolVal False) (Ret (NumVal Zero)) (Do "s" (Suc (NumVal Zero)) 
            (App (RecLamVal "f" "x" (Do "y" (Magic Choose []) (If (IdentifierVal "y") (Ret (IdentifierVal "x")) (Do "s" (Suc (IdentifierVal "x")) 
                 (App (IdentifierVal "f") (IdentifierVal "s")))))) (IdentifierVal "s"))))
] =>
[ExpConf (Ret (NumVal Zero)),
 ExpConf (Do "s" (Suc (NumVal Zero)) 
            (App (RecLamVal "f" "x" (Do "y" (Magic Choose []) (If (IdentifierVal "y") (Ret (IdentifierVal "x")) (Do "s" (Suc (IdentifierVal "x")) 
               (App (IdentifierVal "f") (IdentifierVal "s")))))) (IdentifierVal "s")))
] =>
[ResConf (Ok (NumVal Zero)),
 ExpConf (Do "s" (Ret (NumVal (Succ Zero))) 
            (App (RecLamVal "f" "x" (Do "y" (Magic Choose []) (If (IdentifierVal "y") (Ret (IdentifierVal "x")) (Do "s" (Suc (IdentifierVal "x")) 
               (App (IdentifierVal "f") (IdentifierVal "s")))))) (IdentifierVal "s")))
] =>
[ResConf (Ok (NumVal Zero)),
 ExpConf (App (RecLamVal "f" "x" (Do "y" (Magic Choose []) (If (IdentifierVal "y") (Ret (IdentifierVal "x")) 
                 (Do "s" (Suc (IdentifierVal "x")) (App (IdentifierVal "f") (IdentifierVal "s")))))
              ) (NumVal (Succ Zero))
          )
] =>
[ResConf (Ok (NumVal Zero)), 
 ExpConf (Do "y" (Magic Choose []) 
           (If (IdentifierVal "y") (Ret (NumVal (Succ Zero))) 
             (Do "s" (Suc (NumVal (Succ Zero))) 
                (App (RecLamVal "f" "x" (Do "y" (Magic Choose []) 
                       (If (IdentifierVal "y") (Ret (IdentifierVal "x")) (Do "s" (Suc (IdentifierVal "x")) (App (IdentifierVal "f") (IdentifierVal "s")))))
                      ) (IdentifierVal "s")
                )
             )
           )
          )
] =>
[ResConf (Ok (NumVal Zero)),
 ExpConf (Do "y" (Ret (BoolVal True)) 
           (If (IdentifierVal "y") (Ret (NumVal (Succ Zero))) 
            (Do "s" (Suc (NumVal (Succ Zero))) 
                (App (RecLamVal "f" "x" (Do "y" (Magic Choose []) (If (IdentifierVal "y") (Ret (IdentifierVal "x")) 
                                                                      (Do "s" (Suc (IdentifierVal "x")) (App (IdentifierVal "f") (IdentifierVal "s")))
                                                                   )
                                        )
                     ) 
                     (IdentifierVal "s")
                )
            )
           )
         ),
 ExpConf (Do "y" (Ret (BoolVal False)) 
          (If (IdentifierVal "y") (Ret (NumVal (Succ Zero))) 
            (Do "s" (Suc (NumVal (Succ Zero))) 
              (App (RecLamVal "f" "x" (Do "y" (Magic Choose []) (If (IdentifierVal "y") (Ret (IdentifierVal "x")) 
                                                                     (Do "s" (Suc (IdentifierVal "x")) (App (IdentifierVal "f") (IdentifierVal "s"))
                                                                     )
                                                                )
                                       )
                    ) 
                    (IdentifierVal "s")
              )
            )
           )
        )
] =>
[ResConf (Ok (NumVal Zero)),
 ExpConf (If (BoolVal True) (Ret (NumVal (Succ Zero))) 
             (Do "s" (Suc (NumVal (Succ Zero))) 
                (App (RecLamVal "f" "x" (Do "y" (Magic Choose []) 
                                            (If (IdentifierVal "y") (Ret (IdentifierVal "x")) 
                                              (Do "s" (Suc (IdentifierVal "x")) (App (IdentifierVal "f") (IdentifierVal "s"))
                                              )
                                            )
                                        )
                    ) (IdentifierVal "s")))
        ),
 ExpConf (If (BoolVal False) (Ret (NumVal (Succ Zero))) 
             (Do "s" (Suc (NumVal (Succ Zero))) 
                 (App (RecLamVal "f" "x" (Do "y" (Magic Choose []) 
                                            (If (IdentifierVal "y") (Ret (IdentifierVal "x")) 
                                              (Do "s" (Suc (IdentifierVal "x")) (App (IdentifierVal "f") (IdentifierVal "s"))
                                              )
                                            ))) 
                      (IdentifierVal "s"))))
] =>
[ResConf (Ok (NumVal Zero)),
 ExpConf (Ret (NumVal (Succ Zero))),
 ExpConf (Do "s" (Suc (NumVal (Succ Zero))) 
            (App (RecLamVal "f" "x" (Do "y" (Magic Choose []) 
                                        (If (IdentifierVal "y") (Ret (IdentifierVal "x")) 
                                            (Do "s" (Suc (IdentifierVal "x")) (App (IdentifierVal "f") (IdentifierVal "s")))
                                        )
                                    )
                 ) (IdentifierVal "s")))
] =>
[ResConf (Ok (NumVal Zero)),
 ResConf (Ok (NumVal (Succ Zero))),
 ExpConf (Do "s" (Ret (NumVal (Succ (Succ Zero)))) 
            (App (RecLamVal "f" "x" (Do "y" (Magic Choose []) 
                                        (If (IdentifierVal "y") (Ret (IdentifierVal "x")) 
                                          (Do "s" (Suc (IdentifierVal "x")) (App (IdentifierVal "f") (IdentifierVal "s")))
                                        ))) 
                 (IdentifierVal "s")))
 ] =>
[ResConf (Ok (NumVal Zero)),
 ResConf (Ok (NumVal (Succ Zero))),
 ExpConf (App (RecLamVal "f" "x" (Do "y" (Magic Choose []) 
                                    (If (IdentifierVal "y") (Ret (IdentifierVal "x")) 
                                       (Do "s" (Suc (IdentifierVal "x")) (App (IdentifierVal "f") (IdentifierVal "s")))
                                    )
                                  )
               ) 
               (NumVal (Succ (Succ Zero)))
         )
 ] =>
 [ResConf (Ok (NumVal Zero)),
  ResConf (Ok (NumVal (Succ Zero))),
  ExpConf (Do "y" (Magic Choose []) 
           (If (IdentifierVal "y") (Ret (NumVal (Succ (Succ Zero)))) 
              (Do "s" (Suc (NumVal (Succ (Succ Zero)))) 
                  (App (RecLamVal "f" "x" (Do "y" (Magic Choose []) 
                                            (If (IdentifierVal "y") (Ret (IdentifierVal "x")) 
                                              (Do "s" (Suc (IdentifierVal "x")) (App (IdentifierVal "f") (IdentifierVal "s")))
                                            )
                                           )) 
                        (IdentifierVal "s")))))
] =>
[ResConf (Ok (NumVal Zero)),
 ResConf (Ok (NumVal (Succ Zero))),
 ExpConf (Do "y" (Ret (BoolVal True)) (If (IdentifierVal "y") (Ret (NumVal (Succ (Succ Zero)))) 
                                        (Do "s" (Suc (NumVal (Succ (Succ Zero)))) 
                                          (App (RecLamVal "f" "x" (Do "y" (Magic Choose []) (If (IdentifierVal "y") (Ret (IdentifierVal "x")) 
                                                                                              (Do "s" (Suc (IdentifierVal "x")) 
                                                                                                (App (IdentifierVal "f") (IdentifierVal "s"))
                                                                                              )
                                                                                            )
                                                                   )) 
                                                (IdentifierVal "s")
                                          )
                                        )
                                     )
         ),
 ExpConf (Do "y" (Ret (BoolVal False)) (If (IdentifierVal "y") (Ret (NumVal (Succ (Succ Zero)))) 
                                         (Do "s" (Suc (NumVal (Succ (Succ Zero)))) 
                                           (App (RecLamVal "f" "x" (Do "y" (Magic Choose []) (If (IdentifierVal "y") (Ret (IdentifierVal "x")) 
                                                                                               (Do "s" (Suc (IdentifierVal "x")) 
                                                                                                 (App (IdentifierVal "f") (IdentifierVal "s"))
                                                                                                )
                                                                                              )
                                                                    )
                                                ) 
                                                (IdentifierVal "s")
                                            )
                                         )
                                        )
         )
] =>
[ResConf (Ok (NumVal Zero)),
 ResConf (Ok (NumVal (Succ Zero))),
 ExpConf (If (BoolVal True) (Ret (NumVal (Succ (Succ Zero)))) 
          (Do "s" (Suc (NumVal (Succ (Succ Zero)))) 
             (App (RecLamVal "f" "x" (Do "y" (Magic Choose []) (If (IdentifierVal "y") (Ret (IdentifierVal "x")) 
                                                                 (Do "s" (Suc (IdentifierVal "x")) 
                                                                   (App (IdentifierVal "f") (IdentifierVal "s"))
                                                                  )
                                                                ))) 
                   (IdentifierVal "s")))),
ExpConf (If (BoolVal False) (Ret (NumVal (Succ (Succ Zero)))) 
          (Do "s" (Suc (NumVal (Succ (Succ Zero)))) 
             (App (RecLamVal "f" "x" (Do "y" (Magic Choose []) (If (IdentifierVal "y") (Ret (IdentifierVal "x")) 
                                                                 (Do "s" (Suc (IdentifierVal "x")) 
                                                                    (App (IdentifierVal "f") (IdentifierVal "s"))
                                                                  )))) 
                   (IdentifierVal "s"))))
] =>
[ResConf (Ok (NumVal Zero)),
 ResConf (Ok (NumVal (Succ Zero))),
 ExpConf (Ret (NumVal (Succ (Succ Zero)))),
 ExpConf (Do "s" (Suc (NumVal (Succ (Succ Zero)))) 
          (App (RecLamVal "f" "x" (Do "y" (Magic Choose []) 
                                    (If (IdentifierVal "y") (Ret (IdentifierVal "x")) 
                                       (Do "s" (Suc (IdentifierVal "x")) (App (IdentifierVal "f") (IdentifierVal "s"))
                                       )
                                    )
                                   )) 
                (IdentifierVal "s")))
] =>
[ResConf (Ok (NumVal Zero)),
 ResConf (Ok (NumVal (Succ Zero))),
 ResConf (Ok (NumVal (Succ (Succ Zero)))),
 ExpConf (Do "s" (Ret (NumVal (Succ (Succ (Succ Zero))))) 
           (App (RecLamVal "f" "x" (Do "y" (Magic Choose []) 
                                     (If (IdentifierVal "y") (Ret (IdentifierVal "x")) 
                                       (Do "s" (Suc (IdentifierVal "x")) (App (IdentifierVal "f") (IdentifierVal "s")))
                                      )))
                (IdentifierVal "s")
          ))
]