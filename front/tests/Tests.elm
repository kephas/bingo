module Tests exposing (..)

import Expect
import List.Extra exposing (setAt)
import Main exposing (..)
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


fakeBoardOneRowTicked =
    { title = "Don't care"
    , size = 3
    , cells =
        [ { ticked = False, text = "Don't care" }
        , { ticked = False, text = "Don't care" }
        , { ticked = False, text = "Don't care" }
        , { ticked = True, text = "Don't care" }
        , { ticked = True, text = "Don't care" }
        , { ticked = True, text = "Don't care" }
        , { ticked = False, text = "Don't care" }
        , { ticked = False, text = "Don't care" }
        , { ticked = False, text = "Don't care" }
        ]
    }


fakeBoardOneColTicked =
    { title = "Don't care"
    , size = 3
    , cells =
        [ { ticked = False, text = "0: 0,0" }
        , { ticked = True, text = "1: 0,1" }
        , { ticked = False, text = "2: 0,2" }
        , { ticked = False, text = "3: 1,0" }
        , { ticked = True, text = "4: 1,1" }
        , { ticked = False, text = "5, 1,2" }
        , { ticked = False, text = "6, 2,0" }
        , { ticked = True, text = "7: 2,1" }
        , { ticked = False, text = "8: 2,2" }
        ]
    }


fakeBoardOneDiagTicked =
    { title = "Don't care"
    , size = 3
    , cells =
        [ { ticked = False, text = "0: 0,0" }
        , { ticked = False, text = "1: 0,1" }
        , { ticked = True, text = "2: 0,2" }
        , { ticked = False, text = "3: 1,0" }
        , { ticked = True, text = "4: 1,1" }
        , { ticked = False, text = "5, 1,2" }
        , { ticked = True, text = "6, 2,0" }
        , { ticked = False, text = "7: 2,1" }
        , { ticked = False, text = "8: 2,2" }
        ]
    }


all : Test
all =
    describe "Coordinate manipulations"
        [ test "Same row" <|
            \_ ->
                Expect.equal (sameRow 5 { row = 4, col = 2 })
                    [ { row = 4, col = 0 }, { row = 4, col = 1 }, { row = 4, col = 2 }, { row = 4, col = 3 }, { row = 4, col = 4 } ]
        , test "Same col" <|
            \_ ->
                Expect.equal (sameCol 4 { row = 3, col = 1 })
                    [ { row = 0, col = 1 }, { row = 1, col = 1 }, { row = 2, col = 1 }, { row = 3, col = 1 } ]
        , describe "Coord to num" <|
            [ test "Corner to num" <|
                \_ ->
                    Expect.equal (coordToNum 5 { row = 4, col = 4 }) 24
            , test "Some other cell to num" <|
                \_ ->
                    Expect.equal (coordToNum 4 { row = 3, col = 2 }) 14
            ]
        , describe "Row ticked?" <|
            [ test "None ticked" <|
                \_ ->
                    Expect.equal (isRowTicked fakeBoard { row = 3, col = 2 }) (Just False)
            , test "Not the one ticked" <|
                \_ ->
                    Expect.equal (isRowTicked fakeBoardOneRowTicked { row = 0, col = 2 }) (Just False)
            , test "One ticked" <|
                \_ ->
                    Expect.equal (isRowTicked fakeBoardOneRowTicked { row = 1, col = 0 }) (Just True)
            ]
        , describe "Col ticked?" <|
            [ test "None ticked" <|
                \_ ->
                    Expect.equal (isColTicked fakeBoard { row = 3, col = 2 }) (Just False)
            , test "Not the one ticked" <|
                \_ ->
                    Expect.equal (isColTicked fakeBoardOneColTicked { row = 0, col = 2 }) (Just False)
            , test "One ticked" <|
                \_ ->
                    Expect.equal (isColTicked fakeBoardOneColTicked { row = 0, col = 1 }) (Just True)
            ]
        , describe "Bingo at some cell?" <|
            [ test "No bingo" <|
                \_ ->
                    Expect.equal (hasBingoAt 0 fakeBoard) False
            , test "Wrong cell" <|
                \_ ->
                    Expect.equal (hasBingoAt 0 fakeBoardOneColTicked) False
            , test "Bingo on column" <|
                \_ ->
                    Expect.equal (hasBingoAt 1 fakeBoardOneColTicked) True
            , test "Bingo on row" <|
                \_ ->
                    Expect.equal (hasBingoAt 4 fakeBoardOneRowTicked) True
            , test "Bingo on diag" <|
                \_ ->
                    Expect.equal (hasBingoAt 6 fakeBoardOneDiagTicked) True
            ]
        ]
