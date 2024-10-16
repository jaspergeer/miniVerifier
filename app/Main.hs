module Main where

import Prelude hiding (False, True)

import MiniC
import SymEx

main :: IO ()
main = do
    let
        -- Selection Sort
        sort =
            prog
                ["len"] -- Formal parameters: we sort the heap from 0 to len - 1
                ["i", "j", "min", "minIndex", "tmp"] -- Variable Declarations
                [ "i" .:= lit 0
                , while
                    (var "i" .< qv "len")
                    -- Loop Invariant:
                    -- 1) i >= 0
                    -- 2) from index 0 to i - 1 is sorted
                    -- 3) every element of the unsorted portion is greater than or equal to every one in the sorted portion
                    ( forAll ["n", "m"] $ -- forall n, m ...
                    -- 1)
                        (var "i" .>= lit 0)
                            -- 2)
                            .&& ( ((qv "n" .< var "i") .&& (qv "n" .> lit 0))
                                    .=> (sel (qv "n") .>= sel (qv "n" .- lit 1)) -- is sorted
                                )
                            -- 3)
                            .&& ( ((qv "n" .< var "i") .&& (qv "n" .>= lit 0) .&& (qv "m" .>= var "i") .&& (qv "m" .< qv "len"))
                                    .=> (sel (qv "n") .<= sel (qv "m")) -- is larger
                                )
                    )
                    [ "j" .:= var "i"
                    , "minIndex" .:= var "i"
                    , "min" .:= sel (var "i")
                    , while
                        (var "j" .< qv "len")
                        -- Loop Invariant:
                        -- 1) min is the smallest of the region we have visited
                        -- 2) if i > 0 then min is greater than sel (i - 1)
                        -- 3) every element of the unsorted portion is greater than or equal to every one in the sorted portion
                        -- 4) the heap up to i is sorted
                        -- 5) j >= i
                        -- 6) sel minIndex == min
                        -- 7) minIndex >= i
                        ( forAll ["m", "n"] $ -- forall n ...
                        -- 1)
                            (((qv "n" .>= var "i") .&& (qv "n" .< var "j")) .=> (sel (qv "n") .>= var "min"))
                                -- 2)
                                .&& ((var "i" .> lit 0) .=> (var "min" .>= sel (var "i" .- lit 1)))
                                -- 3)
                                .&& ( ((qv "n" .< var "i") .&& (qv "n" .>= lit 0) .&& (qv "m" .>= var "i") .&& (qv "m" .< qv "len"))
                                        .=> (sel (qv "n") .<= sel (qv "m"))
                                    )
                                -- 4)
                                .&& ( ((qv "n" .< var "i") .&& (qv "n" .> lit 0))
                                        .=> (sel (qv "n") .>= sel (qv "n" .- lit 1)) -- is sorted
                                    )
                                -- 5)
                                .&& (var "j" .>= var "i")
                                -- 6)
                                .&& (sel (var "minIndex") .== var "min")
                                -- 7)
                                .&& (var "minIndex" .>= var "i")
                        )
                        [ if'
                            (sel (var "j") .<= var "min")
                            [ "min" .:= sel (var "j")
                            , "minIndex" .:= var "j"
                            ]
                            []
                        , "j" .:= (var "j" .+ lit 1)
                        ]
                    , -- Now swap the min element of the unsorted region with the element at i
                      "tmp" .:= sel (var "i")
                    , store (var "minIndex") (var "tmp")
                    , store (var "i") (var "min")
                    , "i" .:= (var "i" .+ lit 1)
                    ]
                , -- Assert that the list is sorted up to len
                  assert $ forAll ["n"] (((qv "n" .< qv "len") .&& (qv "n" .> lit 0)) .=> (sel (qv "n") .>= sel (qv "n" .- lit 1)))
                ]
        -- Program that sets heap locations 0...9 to 0...9 --- we can also verify this!
        countUp =
            prog
                ["x"]
                ["i", "len"]
                [ "i" .:= lit 0
                , "len" .:= lit 10
                , while
                    (var "i" .< var "len")
                    (forAll ["n"] $ ((qv "n" .< var "i") .&& (qv "n" .>= lit 0)) .=> (sel (qv "n") .== qv "n"))
                    [ store (var "i") (var "i")
                    , "i" .:= (var "i" .+ lit 1)
                    ]
                , assert $ forAll ["n"] $ ((qv "n" .< lit 10) .&& (qv "n" .>= lit 0)) .=> (sel (qv "n") .== qv "n")
                ]
    putStr $ show $ symbolicExecution sort -- Verify selection sort program
