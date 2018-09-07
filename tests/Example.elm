module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, intRange)
import Test exposing (..)


suite : Test
suite =
    describe "Generation module"
        [ describe "String.reverse" -- Nest as many descriptions as you like.
            [ test "has no effect on a palindrome" <|
                \_ ->
                    let
                        palindrome =
                            "hannah"
                    in
                        Expect.equal palindrome (String.reverse palindrome)

            -- fuzz runs the test 100 times with randomly-generated inputs!
            , fuzz2 (intRange 1 50) (intRange 1 50) "" <|
                \row col  ->
                    (row + col)
                        |> Expect.notEqual 42
            ]
        ]
