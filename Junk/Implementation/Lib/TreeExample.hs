module TreeExample where

import Tree

t0 = T "x" [T "x0" [],
            T "y"  [
                    T "y0" [],
                    T "y1" [],
                    T "z"  [
                        T "w" [
                            T "w0" [],
                            T "w1" [],
                            T "w2" []
                            ],
                        T "z1" [],
                        T "z2" []
                        ],
                     T "y3" []
                     ],
            T "x2" []
            ]
t1 =  T "u" [   T "u0"  [],
                T "v"   [   T "v0" [],
                            T "v1" [],
                            T "v2" []
                        ],
                T "u2" []
            ]



