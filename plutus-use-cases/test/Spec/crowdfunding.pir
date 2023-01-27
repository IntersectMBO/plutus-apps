(program
  (let
    (nonrec)
    (datatypebind
      (datatype
        (tyvardecl CampaignAction (type))

        CampaignAction_match
        (vardecl Collect CampaignAction) (vardecl Refund CampaignAction)
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Bool (type))

        Bool_match
        (vardecl True Bool) (vardecl False Bool)
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Extended (fun (type) (type)))
        (tyvardecl a (type))
        Extended_match
        (vardecl Finite (fun a [ Extended a ]))
        (vardecl NegInf [ Extended a ])
        (vardecl PosInf [ Extended a ])
      )
    )
    (datatypebind
      (datatype
        (tyvardecl LowerBound (fun (type) (type)))
        (tyvardecl a (type))
        LowerBound_match
        (vardecl LowerBound (fun [ Extended a ] (fun Bool [ LowerBound a ])))
      )
    )
    (datatypebind
      (datatype
        (tyvardecl UpperBound (fun (type) (type)))
        (tyvardecl a (type))
        UpperBound_match
        (vardecl UpperBound (fun [ Extended a ] (fun Bool [ UpperBound a ])))
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Interval (fun (type) (type)))
        (tyvardecl a (type))
        Interval_match
        (vardecl
          Interval (fun [ LowerBound a ] (fun [ UpperBound a ] [ Interval a ]))
        )
      )
    )
    (typebind (tyvardecl ScriptPurpose (type)) (all a (type) (fun a a)))
    (typebind (tyvardecl DCert (type)) (all a (type) (fun a a)))
    (typebind (tyvardecl TxInInfo (type)) (all a (type) (fun a a)))
    (typebind (tyvardecl TxOut (type)) (all a (type) (fun a a)))
    (typebind (tyvardecl StakingCredential (type)) (all a (type) (fun a a)))
    (typebind
      (tyvardecl Tuple2 (fun (type) (fun (type) (type))))
      (lam a (type) (lam a (type) (all a (type) (fun a a))))
    )
    (let
      (rec)
      (datatypebind
        (datatype
          (tyvardecl List (fun (type) (type)))
          (tyvardecl a (type))
          Nil_match
          (vardecl Nil [ List a ])
          (vardecl Cons (fun a (fun [ List a ] [ List a ])))
        )
      )
      (let
        (nonrec)
        (datatypebind
          (datatype
            (tyvardecl TxInfo (type))

            TxInfo_match
            (vardecl
              TxInfo
              (fun
                [ List TxInInfo ]
                (fun
                  [ List TxInInfo ]
                  (fun
                    [ List TxOut ]
                    (fun
                      [
                        [
                          (lam
                            k (type) (lam v (type) [ List [ [ Tuple2 k ] v ] ])
                          )
                          (con bytestring)
                        ]
                        [
                          [
                            (lam
                              k
                              (type)
                              (lam v (type) [ List [ [ Tuple2 k ] v ] ])
                            )
                            (con bytestring)
                          ]
                          (con integer)
                        ]
                      ]
                      (fun
                        [
                          [
                            (lam
                              k
                              (type)
                              (lam v (type) [ List [ [ Tuple2 k ] v ] ])
                            )
                            (con bytestring)
                          ]
                          [
                            [
                              (lam
                                k
                                (type)
                                (lam v (type) [ List [ [ Tuple2 k ] v ] ])
                              )
                              (con bytestring)
                            ]
                            (con integer)
                          ]
                        ]
                        (fun
                          [ List DCert ]
                          (fun
                            [
                              [
                                (lam
                                  k
                                  (type)
                                  (lam v (type) [ List [ [ Tuple2 k ] v ] ])
                                )
                                StakingCredential
                              ]
                              (con integer)
                            ]
                            (fun
                              [ Interval (con integer) ]
                              (fun
                                [ List (con bytestring) ]
                                (fun
                                  [
                                    [
                                      (lam
                                        k
                                        (type)
                                        (lam
                                          v (type) [ List [ [ Tuple2 k ] v ] ]
                                        )
                                      )
                                      ScriptPurpose
                                    ]
                                    (con data)
                                  ]
                                  (fun
                                    [
                                      [
                                        (lam
                                          k
                                          (type)
                                          (lam
                                            v (type) [ List [ [ Tuple2 k ] v ] ]
                                          )
                                        )
                                        (con bytestring)
                                      ]
                                      (con data)
                                    ]
                                    (fun (con bytestring) TxInfo)
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
        (datatypebind
          (datatype
            (tyvardecl ScriptContext (type))

            ScriptContext_match
            (vardecl
              ScriptContext (fun TxInfo (fun ScriptPurpose ScriptContext))
            )
          )
        )
        (datatypebind
          (datatype
            (tyvardecl Maybe (fun (type) (type)))
            (tyvardecl a (type))
            Maybe_match
            (vardecl Just (fun a [ Maybe a ])) (vardecl Nothing [ Maybe a ])
          )
        )
        (datatypebind
          (datatype
            (tyvardecl ValidityInterval (fun (type) (type)))
            (tyvardecl a (type))
            ValidityInterval_match
            (vardecl
              ValidityInterval
              (fun [ Maybe a ] (fun [ Maybe a ] [ ValidityInterval a ]))
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            WValidityInterval
            (all
              a
              (type)
              (fun [ Maybe a ] (fun [ Maybe a ] [ ValidityInterval a ]))
            )
          )
          (abs
            a
            (type)
            (lam
              dt
              [ Maybe a ]
              (lam dt [ Maybe a ] [ [ { ValidityInterval a } dt ] dt ])
            )
          )
        )
        (datatypebind
          (datatype
            (tyvardecl Ordering (type))

            Ordering_match
            (vardecl EQ Ordering) (vardecl GT Ordering) (vardecl LT Ordering)
          )
        )
        (datatypebind
          (datatype
            (tyvardecl Ord (fun (type) (type)))
            (tyvardecl a (type))
            Ord_match
            (vardecl
              CConsOrd
              (fun
                [ (lam a (type) (fun a (fun a Bool))) a ]
                (fun
                  (fun a (fun a Ordering))
                  (fun
                    (fun a (fun a Bool))
                    (fun
                      (fun a (fun a Bool))
                      (fun
                        (fun a (fun a Bool))
                        (fun
                          (fun a (fun a Bool))
                          (fun
                            (fun a (fun a a)) (fun (fun a (fun a a)) [ Ord a ])
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
        (termbind
          (nonstrict)
          (vardecl fOrdPOSIXTime [ Ord (con integer) ])
          [
            [
              [
                [
                  [
                    [
                      [
                        [
                          { CConsOrd (con integer) }
                          (lam
                            x
                            (con integer)
                            (lam
                              y
                              (con integer)
                              [
                                [
                                  [
                                    { (builtin ifThenElse) Bool }
                                    [ [ (builtin equalsInteger) x ] y ]
                                  ]
                                  True
                                ]
                                False
                              ]
                            )
                          )
                        ]
                        (lam
                          x
                          (con integer)
                          (lam
                            y
                            (con integer)
                            {
                              [
                                [
                                  {
                                    [
                                      Bool_match
                                      [
                                        [
                                          [
                                            { (builtin ifThenElse) Bool }
                                            [ [ (builtin equalsInteger) x ] y ]
                                          ]
                                          True
                                        ]
                                        False
                                      ]
                                    ]
                                    (all dead (type) Ordering)
                                  }
                                  (abs dead (type) EQ)
                                ]
                                (abs
                                  dead
                                  (type)
                                  {
                                    [
                                      [
                                        {
                                          [
                                            Bool_match
                                            [
                                              [
                                                [
                                                  { (builtin ifThenElse) Bool }
                                                  [
                                                    [
                                                      (builtin
                                                        lessThanEqualsInteger
                                                      )
                                                      x
                                                    ]
                                                    y
                                                  ]
                                                ]
                                                True
                                              ]
                                              False
                                            ]
                                          ]
                                          (all dead (type) Ordering)
                                        }
                                        (abs dead (type) LT)
                                      ]
                                      (abs dead (type) GT)
                                    ]
                                    (all dead (type) dead)
                                  }
                                )
                              ]
                              (all dead (type) dead)
                            }
                          )
                        )
                      ]
                      (lam
                        x
                        (con integer)
                        (lam
                          y
                          (con integer)
                          [
                            [
                              [
                                { (builtin ifThenElse) Bool }
                                [ [ (builtin lessThanInteger) x ] y ]
                              ]
                              True
                            ]
                            False
                          ]
                        )
                      )
                    ]
                    (lam
                      x
                      (con integer)
                      (lam
                        y
                        (con integer)
                        [
                          [
                            [
                              { (builtin ifThenElse) Bool }
                              [ [ (builtin lessThanEqualsInteger) x ] y ]
                            ]
                            True
                          ]
                          False
                        ]
                      )
                    )
                  ]
                  (lam
                    x
                    (con integer)
                    (lam
                      y
                      (con integer)
                      [
                        [
                          [
                            { (builtin ifThenElse) Bool }
                            [ [ (builtin lessThanEqualsInteger) x ] y ]
                          ]
                          False
                        ]
                        True
                      ]
                    )
                  )
                ]
                (lam
                  x
                  (con integer)
                  (lam
                    y
                    (con integer)
                    [
                      [
                        [
                          { (builtin ifThenElse) Bool }
                          [ [ (builtin lessThanInteger) x ] y ]
                        ]
                        False
                      ]
                      True
                    ]
                  )
                )
              ]
              (lam
                x
                (con integer)
                (lam
                  y
                  (con integer)
                  {
                    [
                      [
                        {
                          [
                            Bool_match
                            [
                              [
                                [
                                  { (builtin ifThenElse) Bool }
                                  [ [ (builtin lessThanEqualsInteger) x ] y ]
                                ]
                                True
                              ]
                              False
                            ]
                          ]
                          (all dead (type) (con integer))
                        }
                        (abs dead (type) y)
                      ]
                      (abs dead (type) x)
                    ]
                    (all dead (type) dead)
                  }
                )
              )
            ]
            (lam
              x
              (con integer)
              (lam
                y
                (con integer)
                {
                  [
                    [
                      {
                        [
                          Bool_match
                          [
                            [
                              [
                                { (builtin ifThenElse) Bool }
                                [ [ (builtin lessThanEqualsInteger) x ] y ]
                              ]
                              True
                            ]
                            False
                          ]
                        ]
                        (all dead (type) (con integer))
                      }
                      (abs dead (type) x)
                    ]
                    (abs dead (type) y)
                  ]
                  (all dead (type) dead)
                }
              )
            )
          ]
        )
        (datatypebind
          (datatype
            (tyvardecl Campaign (type))

            Campaign_match
            (vardecl
              Campaign
              (fun
                (con integer)
                (fun (con integer) (fun (con bytestring) Campaign))
              )
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            compare (all a (type) (fun [ Ord a ] (fun a (fun a Ordering))))
          )
          (abs
            a
            (type)
            (lam
              v
              [ Ord a ]
              [
                { [ { Ord_match a } v ] (fun a (fun a Ordering)) }
                (lam
                  v
                  [ (lam a (type) (fun a (fun a Bool))) a ]
                  (lam
                    v
                    (fun a (fun a Ordering))
                    (lam
                      v
                      (fun a (fun a Bool))
                      (lam
                        v
                        (fun a (fun a Bool))
                        (lam
                          v
                          (fun a (fun a Bool))
                          (lam
                            v
                            (fun a (fun a Bool))
                            (lam
                              v (fun a (fun a a)) (lam v (fun a (fun a a)) v)
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ]
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            hull_ccompare
            (all
              a
              (type)
              (fun [ Ord a ] (fun [ Extended a ] (fun [ Extended a ] Ordering)))
            )
          )
          (abs
            a
            (type)
            (lam
              dOrd
              [ Ord a ]
              (lam
                ds
                [ Extended a ]
                (lam
                  ds
                  [ Extended a ]
                  (let
                    (nonrec)
                    (termbind
                      (strict)
                      (vardecl fail (fun (con unit) Ordering))
                      (lam
                        ds
                        (con unit)
                        {
                          [
                            [
                              [
                                {
                                  [ { Extended_match a } ds ]
                                  (all dead (type) Ordering)
                                }
                                (lam
                                  default_arg0
                                  a
                                  (abs
                                    dead
                                    (type)
                                    {
                                      [
                                        [
                                          [
                                            {
                                              [ { Extended_match a } ds ]
                                              (all dead (type) Ordering)
                                            }
                                            (lam
                                              l
                                              a
                                              (abs
                                                dead
                                                (type)
                                                {
                                                  [
                                                    [
                                                      [
                                                        {
                                                          [
                                                            { Extended_match a }
                                                            ds
                                                          ]
                                                          (all
                                                            dead (type) Ordering
                                                          )
                                                        }
                                                        (lam
                                                          r
                                                          a
                                                          (abs
                                                            dead
                                                            (type)
                                                            [
                                                              [
                                                                [
                                                                  { compare a }
                                                                  dOrd
                                                                ]
                                                                l
                                                              ]
                                                              r
                                                            ]
                                                          )
                                                        )
                                                      ]
                                                      (abs
                                                        dead
                                                        (type)
                                                        (error Ordering)
                                                      )
                                                    ]
                                                    (abs
                                                      dead
                                                      (type)
                                                      (error Ordering)
                                                    )
                                                  ]
                                                  (all dead (type) dead)
                                                }
                                              )
                                            )
                                          ]
                                          (abs dead (type) (error Ordering))
                                        ]
                                        (abs dead (type) GT)
                                      ]
                                      (all dead (type) dead)
                                    }
                                  )
                                )
                              ]
                              (abs
                                dead
                                (type)
                                {
                                  [
                                    [
                                      [
                                        {
                                          [ { Extended_match a } ds ]
                                          (all dead (type) Ordering)
                                        }
                                        (lam
                                          l
                                          a
                                          (abs
                                            dead
                                            (type)
                                            {
                                              [
                                                [
                                                  [
                                                    {
                                                      [
                                                        { Extended_match a } ds
                                                      ]
                                                      (all dead (type) Ordering)
                                                    }
                                                    (lam
                                                      r
                                                      a
                                                      (abs
                                                        dead
                                                        (type)
                                                        [
                                                          [
                                                            [
                                                              { compare a } dOrd
                                                            ]
                                                            l
                                                          ]
                                                          r
                                                        ]
                                                      )
                                                    )
                                                  ]
                                                  (abs
                                                    dead (type) (error Ordering)
                                                  )
                                                ]
                                                (abs
                                                  dead (type) (error Ordering)
                                                )
                                              ]
                                              (all dead (type) dead)
                                            }
                                          )
                                        )
                                      ]
                                      (abs dead (type) (error Ordering))
                                    ]
                                    (abs dead (type) GT)
                                  ]
                                  (all dead (type) dead)
                                }
                              )
                            ]
                            (abs dead (type) LT)
                          ]
                          (all dead (type) dead)
                        }
                      )
                    )
                    (termbind
                      (strict)
                      (vardecl fail (fun (con unit) Ordering))
                      (lam
                        ds
                        (con unit)
                        {
                          [
                            [
                              [
                                {
                                  [ { Extended_match a } ds ]
                                  (all dead (type) Ordering)
                                }
                                (lam
                                  default_arg0
                                  a
                                  (abs
                                    dead
                                    (type)
                                    {
                                      [
                                        [
                                          [
                                            {
                                              [ { Extended_match a } ds ]
                                              (all dead (type) Ordering)
                                            }
                                            (lam
                                              l
                                              a
                                              (abs
                                                dead
                                                (type)
                                                {
                                                  [
                                                    [
                                                      [
                                                        {
                                                          [
                                                            { Extended_match a }
                                                            ds
                                                          ]
                                                          (all
                                                            dead (type) Ordering
                                                          )
                                                        }
                                                        (lam
                                                          r
                                                          a
                                                          (abs
                                                            dead
                                                            (type)
                                                            [
                                                              [
                                                                [
                                                                  { compare a }
                                                                  dOrd
                                                                ]
                                                                l
                                                              ]
                                                              r
                                                            ]
                                                          )
                                                        )
                                                      ]
                                                      (abs
                                                        dead
                                                        (type)
                                                        (error Ordering)
                                                      )
                                                    ]
                                                    (abs
                                                      dead
                                                      (type)
                                                      (error Ordering)
                                                    )
                                                  ]
                                                  (all dead (type) dead)
                                                }
                                              )
                                            )
                                          ]
                                          (abs dead (type) (error Ordering))
                                        ]
                                        (abs dead (type) GT)
                                      ]
                                      (all dead (type) dead)
                                    }
                                  )
                                )
                              ]
                              (abs
                                dead
                                (type)
                                {
                                  [
                                    [
                                      [
                                        {
                                          [ { Extended_match a } ds ]
                                          (all dead (type) Ordering)
                                        }
                                        (lam
                                          l
                                          a
                                          (abs
                                            dead
                                            (type)
                                            {
                                              [
                                                [
                                                  [
                                                    {
                                                      [
                                                        { Extended_match a } ds
                                                      ]
                                                      (all dead (type) Ordering)
                                                    }
                                                    (lam
                                                      r
                                                      a
                                                      (abs
                                                        dead
                                                        (type)
                                                        [
                                                          [
                                                            [
                                                              { compare a } dOrd
                                                            ]
                                                            l
                                                          ]
                                                          r
                                                        ]
                                                      )
                                                    )
                                                  ]
                                                  (abs
                                                    dead (type) (error Ordering)
                                                  )
                                                ]
                                                (abs
                                                  dead (type) (error Ordering)
                                                )
                                              ]
                                              (all dead (type) dead)
                                            }
                                          )
                                        )
                                      ]
                                      (abs dead (type) (error Ordering))
                                    ]
                                    (abs dead (type) GT)
                                  ]
                                  (all dead (type) dead)
                                }
                              )
                            ]
                            (abs dead (type) LT)
                          ]
                          (all dead (type) dead)
                        }
                      )
                    )
                    (termbind
                      (strict)
                      (vardecl fail (fun (con unit) Ordering))
                      (lam
                        ds
                        (con unit)
                        {
                          [
                            [
                              [
                                {
                                  [ { Extended_match a } ds ]
                                  (all dead (type) Ordering)
                                }
                                (lam
                                  default_arg0
                                  a
                                  (abs
                                    dead
                                    (type)
                                    {
                                      [
                                        [
                                          [
                                            {
                                              [ { Extended_match a } ds ]
                                              (all dead (type) Ordering)
                                            }
                                            (lam
                                              l
                                              a
                                              (abs
                                                dead
                                                (type)
                                                {
                                                  [
                                                    [
                                                      [
                                                        {
                                                          [
                                                            { Extended_match a }
                                                            ds
                                                          ]
                                                          (all
                                                            dead (type) Ordering
                                                          )
                                                        }
                                                        (lam
                                                          r
                                                          a
                                                          (abs
                                                            dead
                                                            (type)
                                                            [
                                                              [
                                                                [
                                                                  { compare a }
                                                                  dOrd
                                                                ]
                                                                l
                                                              ]
                                                              r
                                                            ]
                                                          )
                                                        )
                                                      ]
                                                      (abs
                                                        dead
                                                        (type)
                                                        (error Ordering)
                                                      )
                                                    ]
                                                    (abs
                                                      dead
                                                      (type)
                                                      (error Ordering)
                                                    )
                                                  ]
                                                  (all dead (type) dead)
                                                }
                                              )
                                            )
                                          ]
                                          (abs dead (type) (error Ordering))
                                        ]
                                        (abs dead (type) GT)
                                      ]
                                      (all dead (type) dead)
                                    }
                                  )
                                )
                              ]
                              (abs
                                dead
                                (type)
                                {
                                  [
                                    [
                                      [
                                        {
                                          [ { Extended_match a } ds ]
                                          (all dead (type) Ordering)
                                        }
                                        (lam
                                          l
                                          a
                                          (abs
                                            dead
                                            (type)
                                            {
                                              [
                                                [
                                                  [
                                                    {
                                                      [
                                                        { Extended_match a } ds
                                                      ]
                                                      (all dead (type) Ordering)
                                                    }
                                                    (lam
                                                      r
                                                      a
                                                      (abs
                                                        dead
                                                        (type)
                                                        [
                                                          [
                                                            [
                                                              { compare a } dOrd
                                                            ]
                                                            l
                                                          ]
                                                          r
                                                        ]
                                                      )
                                                    )
                                                  ]
                                                  (abs
                                                    dead (type) (error Ordering)
                                                  )
                                                ]
                                                (abs
                                                  dead (type) (error Ordering)
                                                )
                                              ]
                                              (all dead (type) dead)
                                            }
                                          )
                                        )
                                      ]
                                      (abs dead (type) (error Ordering))
                                    ]
                                    (abs dead (type) GT)
                                  ]
                                  (all dead (type) dead)
                                }
                              )
                            ]
                            (abs dead (type) LT)
                          ]
                          (all dead (type) dead)
                        }
                      )
                    )
                    (termbind
                      (strict)
                      (vardecl fail (fun (con unit) Ordering))
                      (lam
                        ds
                        (con unit)
                        {
                          [
                            [
                              [
                                {
                                  [ { Extended_match a } ds ]
                                  (all dead (type) Ordering)
                                }
                                (lam
                                  default_arg0
                                  a
                                  (abs
                                    dead
                                    (type)
                                    {
                                      [
                                        [
                                          [
                                            {
                                              [ { Extended_match a } ds ]
                                              (all dead (type) Ordering)
                                            }
                                            (lam
                                              l
                                              a
                                              (abs
                                                dead
                                                (type)
                                                {
                                                  [
                                                    [
                                                      [
                                                        {
                                                          [
                                                            { Extended_match a }
                                                            ds
                                                          ]
                                                          (all
                                                            dead (type) Ordering
                                                          )
                                                        }
                                                        (lam
                                                          r
                                                          a
                                                          (abs
                                                            dead
                                                            (type)
                                                            [
                                                              [
                                                                [
                                                                  { compare a }
                                                                  dOrd
                                                                ]
                                                                l
                                                              ]
                                                              r
                                                            ]
                                                          )
                                                        )
                                                      ]
                                                      (abs
                                                        dead
                                                        (type)
                                                        (error Ordering)
                                                      )
                                                    ]
                                                    (abs
                                                      dead
                                                      (type)
                                                      (error Ordering)
                                                    )
                                                  ]
                                                  (all dead (type) dead)
                                                }
                                              )
                                            )
                                          ]
                                          (abs dead (type) (error Ordering))
                                        ]
                                        (abs dead (type) GT)
                                      ]
                                      (all dead (type) dead)
                                    }
                                  )
                                )
                              ]
                              (abs
                                dead
                                (type)
                                {
                                  [
                                    [
                                      [
                                        {
                                          [ { Extended_match a } ds ]
                                          (all dead (type) Ordering)
                                        }
                                        (lam
                                          l
                                          a
                                          (abs
                                            dead
                                            (type)
                                            {
                                              [
                                                [
                                                  [
                                                    {
                                                      [
                                                        { Extended_match a } ds
                                                      ]
                                                      (all dead (type) Ordering)
                                                    }
                                                    (lam
                                                      r
                                                      a
                                                      (abs
                                                        dead
                                                        (type)
                                                        [
                                                          [
                                                            [
                                                              { compare a } dOrd
                                                            ]
                                                            l
                                                          ]
                                                          r
                                                        ]
                                                      )
                                                    )
                                                  ]
                                                  (abs
                                                    dead (type) (error Ordering)
                                                  )
                                                ]
                                                (abs
                                                  dead (type) (error Ordering)
                                                )
                                              ]
                                              (all dead (type) dead)
                                            }
                                          )
                                        )
                                      ]
                                      (abs dead (type) (error Ordering))
                                    ]
                                    (abs dead (type) GT)
                                  ]
                                  (all dead (type) dead)
                                }
                              )
                            ]
                            (abs dead (type) LT)
                          ]
                          (all dead (type) dead)
                        }
                      )
                    )
                    {
                      [
                        [
                          [
                            {
                              [ { Extended_match a } ds ]
                              (all dead (type) Ordering)
                            }
                            (lam
                              default_arg0
                              a
                              (abs
                                dead
                                (type)
                                {
                                  [
                                    [
                                      [
                                        {
                                          [ { Extended_match a } ds ]
                                          (all dead (type) Ordering)
                                        }
                                        (lam
                                          default_arg0
                                          a
                                          (abs
                                            dead
                                            (type)
                                            {
                                              [
                                                [
                                                  [
                                                    {
                                                      [
                                                        { Extended_match a } ds
                                                      ]
                                                      (all dead (type) Ordering)
                                                    }
                                                    (lam
                                                      default_arg0
                                                      a
                                                      (abs
                                                        dead
                                                        (type)
                                                        [ fail (con unit ()) ]
                                                      )
                                                    )
                                                  ]
                                                  (abs
                                                    dead
                                                    (type)
                                                    [ fail (con unit ()) ]
                                                  )
                                                ]
                                                (abs
                                                  dead
                                                  (type)
                                                  {
                                                    [
                                                      [
                                                        [
                                                          {
                                                            [
                                                              {
                                                                Extended_match a
                                                              }
                                                              ds
                                                            ]
                                                            (all
                                                              dead
                                                              (type)
                                                              Ordering
                                                            )
                                                          }
                                                          (lam
                                                            default_arg0
                                                            a
                                                            (abs
                                                              dead
                                                              (type)
                                                              [
                                                                fail
                                                                (con unit ())
                                                              ]
                                                            )
                                                          )
                                                        ]
                                                        (abs
                                                          dead
                                                          (type)
                                                          [ fail (con unit ()) ]
                                                        )
                                                      ]
                                                      (abs dead (type) EQ)
                                                    ]
                                                    (all dead (type) dead)
                                                  }
                                                )
                                              ]
                                              (all dead (type) dead)
                                            }
                                          )
                                        )
                                      ]
                                      (abs dead (type) GT)
                                    ]
                                    (abs
                                      dead
                                      (type)
                                      {
                                        [
                                          [
                                            [
                                              {
                                                [ { Extended_match a } ds ]
                                                (all dead (type) Ordering)
                                              }
                                              (lam
                                                default_arg0
                                                a
                                                (abs
                                                  dead
                                                  (type)
                                                  [ fail (con unit ()) ]
                                                )
                                              )
                                            ]
                                            (abs
                                              dead (type) [ fail (con unit ()) ]
                                            )
                                          ]
                                          (abs
                                            dead
                                            (type)
                                            {
                                              [
                                                [
                                                  [
                                                    {
                                                      [
                                                        { Extended_match a } ds
                                                      ]
                                                      (all dead (type) Ordering)
                                                    }
                                                    (lam
                                                      default_arg0
                                                      a
                                                      (abs
                                                        dead
                                                        (type)
                                                        [ fail (con unit ()) ]
                                                      )
                                                    )
                                                  ]
                                                  (abs
                                                    dead
                                                    (type)
                                                    [ fail (con unit ()) ]
                                                  )
                                                ]
                                                (abs dead (type) EQ)
                                              ]
                                              (all dead (type) dead)
                                            }
                                          )
                                        ]
                                        (all dead (type) dead)
                                      }
                                    )
                                  ]
                                  (all dead (type) dead)
                                }
                              )
                            )
                          ]
                          (abs
                            dead
                            (type)
                            {
                              [
                                [
                                  [
                                    {
                                      [ { Extended_match a } ds ]
                                      (all dead (type) Ordering)
                                    }
                                    (lam default_arg0 a (abs dead (type) LT))
                                  ]
                                  (abs dead (type) EQ)
                                ]
                                (abs dead (type) LT)
                              ]
                              (all dead (type) dead)
                            }
                          )
                        ]
                        (abs
                          dead
                          (type)
                          {
                            [
                              [
                                [
                                  {
                                    [ { Extended_match a } ds ]
                                    (all dead (type) Ordering)
                                  }
                                  (lam
                                    default_arg0
                                    a
                                    (abs
                                      dead
                                      (type)
                                      {
                                        [
                                          [
                                            [
                                              {
                                                [ { Extended_match a } ds ]
                                                (all dead (type) Ordering)
                                              }
                                              (lam
                                                default_arg0
                                                a
                                                (abs
                                                  dead
                                                  (type)
                                                  [ fail (con unit ()) ]
                                                )
                                              )
                                            ]
                                            (abs
                                              dead (type) [ fail (con unit ()) ]
                                            )
                                          ]
                                          (abs
                                            dead
                                            (type)
                                            {
                                              [
                                                [
                                                  [
                                                    {
                                                      [
                                                        { Extended_match a } ds
                                                      ]
                                                      (all dead (type) Ordering)
                                                    }
                                                    (lam
                                                      default_arg0
                                                      a
                                                      (abs
                                                        dead
                                                        (type)
                                                        [ fail (con unit ()) ]
                                                      )
                                                    )
                                                  ]
                                                  (abs
                                                    dead
                                                    (type)
                                                    [ fail (con unit ()) ]
                                                  )
                                                ]
                                                (abs dead (type) EQ)
                                              ]
                                              (all dead (type) dead)
                                            }
                                          )
                                        ]
                                        (all dead (type) dead)
                                      }
                                    )
                                  )
                                ]
                                (abs dead (type) GT)
                              ]
                              (abs
                                dead
                                (type)
                                {
                                  [
                                    [
                                      [
                                        {
                                          [ { Extended_match a } ds ]
                                          (all dead (type) Ordering)
                                        }
                                        (lam
                                          default_arg0
                                          a
                                          (abs
                                            dead (type) [ fail (con unit ()) ]
                                          )
                                        )
                                      ]
                                      (abs dead (type) [ fail (con unit ()) ])
                                    ]
                                    (abs
                                      dead
                                      (type)
                                      {
                                        [
                                          [
                                            [
                                              {
                                                [ { Extended_match a } ds ]
                                                (all dead (type) Ordering)
                                              }
                                              (lam
                                                default_arg0
                                                a
                                                (abs
                                                  dead
                                                  (type)
                                                  [ fail (con unit ()) ]
                                                )
                                              )
                                            ]
                                            (abs
                                              dead (type) [ fail (con unit ()) ]
                                            )
                                          ]
                                          (abs dead (type) EQ)
                                        ]
                                        (all dead (type) dead)
                                      }
                                    )
                                  ]
                                  (all dead (type) dead)
                                }
                              )
                            ]
                            (all dead (type) dead)
                          }
                        )
                      ]
                      (all dead (type) dead)
                    }
                  )
                )
              )
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            fOrdUpperBound0_c
            (all
              a
              (type)
              (fun [ Ord a ] (fun [ UpperBound a ] (fun [ UpperBound a ] Bool)))
            )
          )
          (abs
            a
            (type)
            (lam
              w
              [ Ord a ]
              (lam
                w
                [ UpperBound a ]
                (lam
                  w
                  [ UpperBound a ]
                  [
                    { [ { UpperBound_match a } w ] Bool }
                    (lam
                      ww
                      [ Extended a ]
                      (lam
                        ww
                        Bool
                        [
                          { [ { UpperBound_match a } w ] Bool }
                          (lam
                            ww
                            [ Extended a ]
                            (lam
                              ww
                              Bool
                              {
                                [
                                  [
                                    [
                                      {
                                        [
                                          Ordering_match
                                          [
                                            [ [ { hull_ccompare a } w ] ww ] ww
                                          ]
                                        ]
                                        (all dead (type) Bool)
                                      }
                                      (abs
                                        dead
                                        (type)
                                        {
                                          [
                                            [
                                              {
                                                [ Bool_match ww ]
                                                (all dead (type) Bool)
                                              }
                                              (abs dead (type) ww)
                                            ]
                                            (abs dead (type) True)
                                          ]
                                          (all dead (type) dead)
                                        }
                                      )
                                    ]
                                    (abs dead (type) False)
                                  ]
                                  (abs dead (type) True)
                                ]
                                (all dead (type) dead)
                              }
                            )
                          )
                        ]
                      )
                    )
                  ]
                )
              )
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            contains
            (all
              a
              (type)
              (fun [ Ord a ] (fun [ Interval a ] (fun [ Interval a ] Bool)))
            )
          )
          (abs
            a
            (type)
            (lam
              w
              [ Ord a ]
              (lam
                w
                [ Interval a ]
                (lam
                  w
                  [ Interval a ]
                  [
                    { [ { Interval_match a } w ] Bool }
                    (lam
                      ww
                      [ LowerBound a ]
                      (lam
                        ww
                        [ UpperBound a ]
                        [
                          { [ { LowerBound_match a } ww ] Bool }
                          (lam
                            ww
                            [ Extended a ]
                            (lam
                              ww
                              Bool
                              [
                                { [ { Interval_match a } w ] Bool }
                                (lam
                                  ww
                                  [ LowerBound a ]
                                  (lam
                                    ww
                                    [ UpperBound a ]
                                    [
                                      { [ { LowerBound_match a } ww ] Bool }
                                      (lam
                                        ww
                                        [ Extended a ]
                                        (lam
                                          ww
                                          Bool
                                          {
                                            [
                                              [
                                                [
                                                  {
                                                    [
                                                      Ordering_match
                                                      [
                                                        [
                                                          [
                                                            { hull_ccompare a }
                                                            w
                                                          ]
                                                          ww
                                                        ]
                                                        ww
                                                      ]
                                                    ]
                                                    (all dead (type) Bool)
                                                  }
                                                  (abs
                                                    dead
                                                    (type)
                                                    {
                                                      [
                                                        [
                                                          {
                                                            [ Bool_match ww ]
                                                            (all
                                                              dead (type) Bool
                                                            )
                                                          }
                                                          (abs
                                                            dead
                                                            (type)
                                                            {
                                                              [
                                                                [
                                                                  {
                                                                    [
                                                                      Bool_match
                                                                      ww
                                                                    ]
                                                                    (all
                                                                      dead
                                                                      (type)
                                                                      Bool
                                                                    )
                                                                  }
                                                                  (abs
                                                                    dead
                                                                    (type)
                                                                    [
                                                                      [
                                                                        [
                                                                          {
                                                                            fOrdUpperBound0_c
                                                                            a
                                                                          }
                                                                          w
                                                                        ]
                                                                        ww
                                                                      ]
                                                                      ww
                                                                    ]
                                                                  )
                                                                ]
                                                                (abs
                                                                  dead
                                                                  (type)
                                                                  False
                                                                )
                                                              ]
                                                              (all
                                                                dead (type) dead
                                                              )
                                                            }
                                                          )
                                                        ]
                                                        (abs
                                                          dead
                                                          (type)
                                                          [
                                                            [
                                                              [
                                                                {
                                                                  fOrdUpperBound0_c
                                                                  a
                                                                }
                                                                w
                                                              ]
                                                              ww
                                                            ]
                                                            ww
                                                          ]
                                                        )
                                                      ]
                                                      (all dead (type) dead)
                                                    }
                                                  )
                                                ]
                                                (abs dead (type) False)
                                              ]
                                              (abs
                                                dead
                                                (type)
                                                [
                                                  [
                                                    [
                                                      { fOrdUpperBound0_c a } w
                                                    ]
                                                    ww
                                                  ]
                                                  ww
                                                ]
                                              )
                                            ]
                                            (all dead (type) dead)
                                          }
                                        )
                                      )
                                    ]
                                  )
                                )
                              ]
                            )
                          )
                        ]
                      )
                    )
                  ]
                )
              )
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            toPlutusInterval
            (all a (type) (fun [ ValidityInterval a ] [ Interval a ]))
          )
          (abs
            a
            (type)
            (lam
              w
              [ ValidityInterval a ]
              [
                { [ { ValidityInterval_match a } w ] [ Interval a ] }
                (lam
                  ww
                  [ Maybe a ]
                  (lam
                    ww
                    [ Maybe a ]
                    [
                      [
                        { Interval a }
                        {
                          [
                            [
                              {
                                [ { Maybe_match a } ww ]
                                (all dead (type) [ LowerBound a ])
                              }
                              (lam
                                v
                                a
                                (abs
                                  dead
                                  (type)
                                  [
                                    [ { LowerBound a } [ { Finite a } v ] ] True
                                  ]
                                )
                              )
                            ]
                            (abs
                              dead
                              (type)
                              [ [ { LowerBound a } { NegInf a } ] True ]
                            )
                          ]
                          (all dead (type) dead)
                        }
                      ]
                      {
                        [
                          [
                            {
                              [ { Maybe_match a } ww ]
                              (all dead (type) [ UpperBound a ])
                            }
                            (lam
                              v
                              a
                              (abs
                                dead
                                (type)
                                [
                                  [ { UpperBound a } [ { Finite a } v ] ] False
                                ]
                              )
                            )
                          ]
                          (abs
                            dead
                            (type)
                            [ [ { UpperBound a } { PosInf a } ] True ]
                          )
                        ]
                        (all dead (type) dead)
                      }
                    ]
                  )
                )
              ]
            )
          )
        )
        (datatypebind
          (datatype
            (tyvardecl Monoid (fun (type) (type)))
            (tyvardecl a (type))
            Monoid_match
            (vardecl
              CConsMonoid
              (fun [ (lam a (type) (fun a (fun a a))) a ] (fun a [ Monoid a ]))
            )
          )
        )
        (let
          (rec)
          (termbind
            (strict)
            (vardecl
              fFoldableNil_cfoldMap
              (all
                m
                (type)
                (all
                  a (type) (fun [ Monoid m ] (fun (fun a m) (fun [ List a ] m)))
                )
              )
            )
            (abs
              m
              (type)
              (abs
                a
                (type)
                (lam
                  dMonoid
                  [ Monoid m ]
                  (lam
                    ds
                    (fun a m)
                    (lam
                      ds
                      [ List a ]
                      {
                        [
                          [
                            { [ { Nil_match a } ds ] (all dead (type) m) }
                            (abs
                              dead
                              (type)
                              [
                                { [ { Monoid_match m } dMonoid ] m }
                                (lam
                                  v
                                  [ (lam a (type) (fun a (fun a a))) m ]
                                  (lam v m v)
                                )
                              ]
                            )
                          ]
                          (lam
                            x
                            a
                            (lam
                              xs
                              [ List a ]
                              (abs
                                dead
                                (type)
                                [
                                  [
                                    [
                                      {
                                        [ { Monoid_match m } dMonoid ]
                                        [ (lam a (type) (fun a (fun a a))) m ]
                                      }
                                      (lam
                                        v
                                        [ (lam a (type) (fun a (fun a a))) m ]
                                        (lam v m v)
                                      )
                                    ]
                                    [ ds x ]
                                  ]
                                  [
                                    [
                                      [
                                        { { fFoldableNil_cfoldMap m } a }
                                        dMonoid
                                      ]
                                      ds
                                    ]
                                    xs
                                  ]
                                ]
                              )
                            )
                          )
                        ]
                        (all dead (type) dead)
                      }
                    )
                  )
                )
              )
            )
          )
          (let
            (nonrec)
            (termbind
              (strict)
              (vardecl txSignedBy (fun TxInfo (fun (con bytestring) Bool)))
              (lam
                ds
                TxInfo
                (lam
                  k
                  (con bytestring)
                  [
                    { [ TxInfo_match ds ] Bool }
                    (lam
                      ds
                      [ List TxInInfo ]
                      (lam
                        ds
                        [ List TxInInfo ]
                        (lam
                          ds
                          [ List TxOut ]
                          (lam
                            ds
                            [
                              [
                                (lam
                                  k
                                  (type)
                                  (lam v (type) [ List [ [ Tuple2 k ] v ] ])
                                )
                                (con bytestring)
                              ]
                              [
                                [
                                  (lam
                                    k
                                    (type)
                                    (lam v (type) [ List [ [ Tuple2 k ] v ] ])
                                  )
                                  (con bytestring)
                                ]
                                (con integer)
                              ]
                            ]
                            (lam
                              ds
                              [
                                [
                                  (lam
                                    k
                                    (type)
                                    (lam v (type) [ List [ [ Tuple2 k ] v ] ])
                                  )
                                  (con bytestring)
                                ]
                                [
                                  [
                                    (lam
                                      k
                                      (type)
                                      (lam v (type) [ List [ [ Tuple2 k ] v ] ])
                                    )
                                    (con bytestring)
                                  ]
                                  (con integer)
                                ]
                              ]
                              (lam
                                ds
                                [ List DCert ]
                                (lam
                                  ds
                                  [
                                    [
                                      (lam
                                        k
                                        (type)
                                        (lam
                                          v (type) [ List [ [ Tuple2 k ] v ] ]
                                        )
                                      )
                                      StakingCredential
                                    ]
                                    (con integer)
                                  ]
                                  (lam
                                    ds
                                    [ Interval (con integer) ]
                                    (lam
                                      ds
                                      [ List (con bytestring) ]
                                      (lam
                                        ds
                                        [
                                          [
                                            (lam
                                              k
                                              (type)
                                              (lam
                                                v
                                                (type)
                                                [ List [ [ Tuple2 k ] v ] ]
                                              )
                                            )
                                            ScriptPurpose
                                          ]
                                          (con data)
                                        ]
                                        (lam
                                          ds
                                          [
                                            [
                                              (lam
                                                k
                                                (type)
                                                (lam
                                                  v
                                                  (type)
                                                  [ List [ [ Tuple2 k ] v ] ]
                                                )
                                              )
                                              (con bytestring)
                                            ]
                                            (con data)
                                          ]
                                          (lam
                                            ds
                                            (con bytestring)
                                            {
                                              [
                                                [
                                                  {
                                                    [
                                                      {
                                                        Maybe_match
                                                        (con bytestring)
                                                      }
                                                      [
                                                        [
                                                          [
                                                            {
                                                              {
                                                                fFoldableNil_cfoldMap
                                                                [
                                                                  (lam
                                                                    a
                                                                    (type)
                                                                    [ Maybe a ]
                                                                  )
                                                                  (con
                                                                    bytestring
                                                                  )
                                                                ]
                                                              }
                                                              (con bytestring)
                                                            }
                                                            [
                                                              [
                                                                {
                                                                  CConsMonoid
                                                                  [
                                                                    (lam
                                                                      a
                                                                      (type)
                                                                      [
                                                                        Maybe a
                                                                      ]
                                                                    )
                                                                    (con
                                                                      bytestring
                                                                    )
                                                                  ]
                                                                }
                                                                (lam
                                                                  ds
                                                                  [
                                                                    (lam
                                                                      a
                                                                      (type)
                                                                      [
                                                                        Maybe a
                                                                      ]
                                                                    )
                                                                    (con
                                                                      bytestring
                                                                    )
                                                                  ]
                                                                  (lam
                                                                    b
                                                                    [
                                                                      (lam
                                                                        a
                                                                        (type)
                                                                        [
                                                                          Maybe
                                                                          a
                                                                        ]
                                                                      )
                                                                      (con
                                                                        bytestring
                                                                      )
                                                                    ]
                                                                    {
                                                                      [
                                                                        [
                                                                          {
                                                                            [
                                                                              {
                                                                                Maybe_match
                                                                                (con
                                                                                  bytestring
                                                                                )
                                                                              }
                                                                              ds
                                                                            ]
                                                                            (all
                                                                              dead
                                                                              (type)
                                                                              [
                                                                                (lam
                                                                                  a
                                                                                  (type)
                                                                                  [
                                                                                    Maybe
                                                                                    a
                                                                                  ]
                                                                                )
                                                                                (con
                                                                                  bytestring
                                                                                )
                                                                              ]
                                                                            )
                                                                          }
                                                                          (lam
                                                                            ipv
                                                                            (con
                                                                              bytestring
                                                                            )
                                                                            (abs
                                                                              dead
                                                                              (type)
                                                                              ds
                                                                            )
                                                                          )
                                                                        ]
                                                                        (abs
                                                                          dead
                                                                          (type)
                                                                          b
                                                                        )
                                                                      ]
                                                                      (all
                                                                        dead
                                                                        (type)
                                                                        dead
                                                                      )
                                                                    }
                                                                  )
                                                                )
                                                              ]
                                                              {
                                                                Nothing
                                                                (con bytestring)
                                                              }
                                                            ]
                                                          ]
                                                          (lam
                                                            x
                                                            (con bytestring)
                                                            {
                                                              [
                                                                [
                                                                  {
                                                                    [
                                                                      Bool_match
                                                                      [
                                                                        [
                                                                          [
                                                                            {
                                                                              (builtin
                                                                                ifThenElse
                                                                              )
                                                                              Bool
                                                                            }
                                                                            [
                                                                              [
                                                                                (builtin
                                                                                  equalsByteString
                                                                                )
                                                                                k
                                                                              ]
                                                                              x
                                                                            ]
                                                                          ]
                                                                          True
                                                                        ]
                                                                        False
                                                                      ]
                                                                    ]
                                                                    (all
                                                                      dead
                                                                      (type)
                                                                      [
                                                                        Maybe
                                                                        (con
                                                                          bytestring
                                                                        )
                                                                      ]
                                                                    )
                                                                  }
                                                                  (abs
                                                                    dead
                                                                    (type)
                                                                    [
                                                                      {
                                                                        Just
                                                                        (con
                                                                          bytestring
                                                                        )
                                                                      }
                                                                      x
                                                                    ]
                                                                  )
                                                                ]
                                                                (abs
                                                                  dead
                                                                  (type)
                                                                  {
                                                                    Nothing
                                                                    (con
                                                                      bytestring
                                                                    )
                                                                  }
                                                                )
                                                              ]
                                                              (all
                                                                dead (type) dead
                                                              )
                                                            }
                                                          )
                                                        ]
                                                        ds
                                                      ]
                                                    ]
                                                    (all dead (type) Bool)
                                                  }
                                                  (lam
                                                    ds
                                                    (con bytestring)
                                                    (abs dead (type) True)
                                                  )
                                                ]
                                                (abs dead (type) False)
                                              ]
                                              (all dead (type) dead)
                                            }
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ]
                )
              )
            )
            (lam
              c
              Campaign
              (lam
                con
                (con bytestring)
                (lam
                  act
                  CampaignAction
                  (lam
                    ds
                    ScriptContext
                    [
                      { [ ScriptContext_match ds ] Bool }
                      (lam
                        ds
                        TxInfo
                        (lam
                          ds
                          ScriptPurpose
                          {
                            [
                              [
                                {
                                  [ CampaignAction_match act ]
                                  (all dead (type) Bool)
                                }
                                (abs
                                  dead
                                  (type)
                                  [
                                    { [ TxInfo_match ds ] Bool }
                                    (lam
                                      ds
                                      [ List TxInInfo ]
                                      (lam
                                        ds
                                        [ List TxInInfo ]
                                        (lam
                                          ds
                                          [ List TxOut ]
                                          (lam
                                            ds
                                            [
                                              [
                                                (lam
                                                  k
                                                  (type)
                                                  (lam
                                                    v
                                                    (type)
                                                    [ List [ [ Tuple2 k ] v ] ]
                                                  )
                                                )
                                                (con bytestring)
                                              ]
                                              [
                                                [
                                                  (lam
                                                    k
                                                    (type)
                                                    (lam
                                                      v
                                                      (type)
                                                      [
                                                        List [ [ Tuple2 k ] v ]
                                                      ]
                                                    )
                                                  )
                                                  (con bytestring)
                                                ]
                                                (con integer)
                                              ]
                                            ]
                                            (lam
                                              ds
                                              [
                                                [
                                                  (lam
                                                    k
                                                    (type)
                                                    (lam
                                                      v
                                                      (type)
                                                      [
                                                        List [ [ Tuple2 k ] v ]
                                                      ]
                                                    )
                                                  )
                                                  (con bytestring)
                                                ]
                                                [
                                                  [
                                                    (lam
                                                      k
                                                      (type)
                                                      (lam
                                                        v
                                                        (type)
                                                        [
                                                          List
                                                          [ [ Tuple2 k ] v ]
                                                        ]
                                                      )
                                                    )
                                                    (con bytestring)
                                                  ]
                                                  (con integer)
                                                ]
                                              ]
                                              (lam
                                                ds
                                                [ List DCert ]
                                                (lam
                                                  ds
                                                  [
                                                    [
                                                      (lam
                                                        k
                                                        (type)
                                                        (lam
                                                          v
                                                          (type)
                                                          [
                                                            List
                                                            [ [ Tuple2 k ] v ]
                                                          ]
                                                        )
                                                      )
                                                      StakingCredential
                                                    ]
                                                    (con integer)
                                                  ]
                                                  (lam
                                                    ds
                                                    [ Interval (con integer) ]
                                                    (lam
                                                      ds
                                                      [ List (con bytestring) ]
                                                      (lam
                                                        ds
                                                        [
                                                          [
                                                            (lam
                                                              k
                                                              (type)
                                                              (lam
                                                                v
                                                                (type)
                                                                [
                                                                  List
                                                                  [
                                                                    [ Tuple2 k ]
                                                                    v
                                                                  ]
                                                                ]
                                                              )
                                                            )
                                                            ScriptPurpose
                                                          ]
                                                          (con data)
                                                        ]
                                                        (lam
                                                          ds
                                                          [
                                                            [
                                                              (lam
                                                                k
                                                                (type)
                                                                (lam
                                                                  v
                                                                  (type)
                                                                  [
                                                                    List
                                                                    [
                                                                      [
                                                                        Tuple2 k
                                                                      ]
                                                                      v
                                                                    ]
                                                                  ]
                                                                )
                                                              )
                                                              (con bytestring)
                                                            ]
                                                            (con data)
                                                          ]
                                                          (lam
                                                            ds
                                                            (con bytestring)
                                                            {
                                                              [
                                                                [
                                                                  {
                                                                    [
                                                                      Bool_match
                                                                      [
                                                                        [
                                                                          [
                                                                            {
                                                                              contains
                                                                              (con
                                                                                integer
                                                                              )
                                                                            }
                                                                            fOrdPOSIXTime
                                                                          ]
                                                                          [
                                                                            {
                                                                              toPlutusInterval
                                                                              (con
                                                                                integer
                                                                              )
                                                                            }
                                                                            [
                                                                              [
                                                                                {
                                                                                  WValidityInterval
                                                                                  (con
                                                                                    integer
                                                                                  )
                                                                                }
                                                                                [
                                                                                  {
                                                                                    Just
                                                                                    (con
                                                                                      integer
                                                                                    )
                                                                                  }
                                                                                  [
                                                                                    {
                                                                                      [
                                                                                        Campaign_match
                                                                                        c
                                                                                      ]
                                                                                      (con
                                                                                        integer
                                                                                      )
                                                                                    }
                                                                                    (lam
                                                                                      ds
                                                                                      (con
                                                                                        integer
                                                                                      )
                                                                                      (lam
                                                                                        ds
                                                                                        (con
                                                                                          integer
                                                                                        )
                                                                                        (lam
                                                                                          ds
                                                                                          (con
                                                                                            bytestring
                                                                                          )
                                                                                          ds
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  ]
                                                                                ]
                                                                              ]
                                                                              [
                                                                                {
                                                                                  Just
                                                                                  (con
                                                                                    integer
                                                                                  )
                                                                                }
                                                                                [
                                                                                  {
                                                                                    [
                                                                                      Campaign_match
                                                                                      c
                                                                                    ]
                                                                                    (con
                                                                                      integer
                                                                                    )
                                                                                  }
                                                                                  (lam
                                                                                    ds
                                                                                    (con
                                                                                      integer
                                                                                    )
                                                                                    (lam
                                                                                      ds
                                                                                      (con
                                                                                        integer
                                                                                      )
                                                                                      (lam
                                                                                        ds
                                                                                        (con
                                                                                          bytestring
                                                                                        )
                                                                                        ds
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                ]
                                                                              ]
                                                                            ]
                                                                          ]
                                                                        ]
                                                                        ds
                                                                      ]
                                                                    ]
                                                                    (all
                                                                      dead
                                                                      (type)
                                                                      Bool
                                                                    )
                                                                  }
                                                                  (abs
                                                                    dead
                                                                    (type)
                                                                    [
                                                                      [
                                                                        txSignedBy
                                                                        ds
                                                                      ]
                                                                      [
                                                                        {
                                                                          [
                                                                            Campaign_match
                                                                            c
                                                                          ]
                                                                          (con
                                                                            bytestring
                                                                          )
                                                                        }
                                                                        (lam
                                                                          ds
                                                                          (con
                                                                            integer
                                                                          )
                                                                          (lam
                                                                            ds
                                                                            (con
                                                                              integer
                                                                            )
                                                                            (lam
                                                                              ds
                                                                              (con
                                                                                bytestring
                                                                              )
                                                                              ds
                                                                            )
                                                                          )
                                                                        )
                                                                      ]
                                                                    ]
                                                                  )
                                                                ]
                                                                (abs
                                                                  dead
                                                                  (type)
                                                                  False
                                                                )
                                                              ]
                                                              (all
                                                                dead (type) dead
                                                              )
                                                            }
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  ]
                                )
                              ]
                              (abs
                                dead
                                (type)
                                [
                                  { [ TxInfo_match ds ] Bool }
                                  (lam
                                    ds
                                    [ List TxInInfo ]
                                    (lam
                                      ds
                                      [ List TxInInfo ]
                                      (lam
                                        ds
                                        [ List TxOut ]
                                        (lam
                                          ds
                                          [
                                            [
                                              (lam
                                                k
                                                (type)
                                                (lam
                                                  v
                                                  (type)
                                                  [ List [ [ Tuple2 k ] v ] ]
                                                )
                                              )
                                              (con bytestring)
                                            ]
                                            [
                                              [
                                                (lam
                                                  k
                                                  (type)
                                                  (lam
                                                    v
                                                    (type)
                                                    [ List [ [ Tuple2 k ] v ] ]
                                                  )
                                                )
                                                (con bytestring)
                                              ]
                                              (con integer)
                                            ]
                                          ]
                                          (lam
                                            ds
                                            [
                                              [
                                                (lam
                                                  k
                                                  (type)
                                                  (lam
                                                    v
                                                    (type)
                                                    [ List [ [ Tuple2 k ] v ] ]
                                                  )
                                                )
                                                (con bytestring)
                                              ]
                                              [
                                                [
                                                  (lam
                                                    k
                                                    (type)
                                                    (lam
                                                      v
                                                      (type)
                                                      [
                                                        List [ [ Tuple2 k ] v ]
                                                      ]
                                                    )
                                                  )
                                                  (con bytestring)
                                                ]
                                                (con integer)
                                              ]
                                            ]
                                            (lam
                                              ds
                                              [ List DCert ]
                                              (lam
                                                ds
                                                [
                                                  [
                                                    (lam
                                                      k
                                                      (type)
                                                      (lam
                                                        v
                                                        (type)
                                                        [
                                                          List
                                                          [ [ Tuple2 k ] v ]
                                                        ]
                                                      )
                                                    )
                                                    StakingCredential
                                                  ]
                                                  (con integer)
                                                ]
                                                (lam
                                                  ds
                                                  [ Interval (con integer) ]
                                                  (lam
                                                    ds
                                                    [ List (con bytestring) ]
                                                    (lam
                                                      ds
                                                      [
                                                        [
                                                          (lam
                                                            k
                                                            (type)
                                                            (lam
                                                              v
                                                              (type)
                                                              [
                                                                List
                                                                [
                                                                  [ Tuple2 k ] v
                                                                ]
                                                              ]
                                                            )
                                                          )
                                                          ScriptPurpose
                                                        ]
                                                        (con data)
                                                      ]
                                                      (lam
                                                        ds
                                                        [
                                                          [
                                                            (lam
                                                              k
                                                              (type)
                                                              (lam
                                                                v
                                                                (type)
                                                                [
                                                                  List
                                                                  [
                                                                    [ Tuple2 k ]
                                                                    v
                                                                  ]
                                                                ]
                                                              )
                                                            )
                                                            (con bytestring)
                                                          ]
                                                          (con data)
                                                        ]
                                                        (lam
                                                          ds
                                                          (con bytestring)
                                                          {
                                                            [
                                                              [
                                                                {
                                                                  [
                                                                    Bool_match
                                                                    [
                                                                      [
                                                                        [
                                                                          {
                                                                            contains
                                                                            (con
                                                                              integer
                                                                            )
                                                                          }
                                                                          fOrdPOSIXTime
                                                                        ]
                                                                        [
                                                                          {
                                                                            toPlutusInterval
                                                                            (con
                                                                              integer
                                                                            )
                                                                          }
                                                                          [
                                                                            [
                                                                              {
                                                                                WValidityInterval
                                                                                (con
                                                                                  integer
                                                                                )
                                                                              }
                                                                              [
                                                                                {
                                                                                  Just
                                                                                  (con
                                                                                    integer
                                                                                  )
                                                                                }
                                                                                [
                                                                                  {
                                                                                    [
                                                                                      Campaign_match
                                                                                      c
                                                                                    ]
                                                                                    (con
                                                                                      integer
                                                                                    )
                                                                                  }
                                                                                  (lam
                                                                                    ds
                                                                                    (con
                                                                                      integer
                                                                                    )
                                                                                    (lam
                                                                                      ds
                                                                                      (con
                                                                                        integer
                                                                                      )
                                                                                      (lam
                                                                                        ds
                                                                                        (con
                                                                                          bytestring
                                                                                        )
                                                                                        ds
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                ]
                                                                              ]
                                                                            ]
                                                                            {
                                                                              Nothing
                                                                              (con
                                                                                integer
                                                                              )
                                                                            }
                                                                          ]
                                                                        ]
                                                                      ]
                                                                      ds
                                                                    ]
                                                                  ]
                                                                  (all
                                                                    dead
                                                                    (type)
                                                                    Bool
                                                                  )
                                                                }
                                                                (abs
                                                                  dead
                                                                  (type)
                                                                  [
                                                                    [
                                                                      txSignedBy
                                                                      ds
                                                                    ]
                                                                    con
                                                                  ]
                                                                )
                                                              ]
                                                              (abs
                                                                dead
                                                                (type)
                                                                False
                                                              )
                                                            ]
                                                            (all
                                                              dead (type) dead
                                                            )
                                                          }
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                ]
                              )
                            ]
                            (all dead (type) dead)
                          }
                        )
                      )
                    ]
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)