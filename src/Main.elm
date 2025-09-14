module Main exposing (..)

import Array exposing (Array)
import Browser
import Dict exposing (Dict, insert)
import Html exposing (Html, select)
import Html.Events exposing (onClick)
import List exposing (map)
import Random
import Random.Extra exposing (maybe)
import Random.List
import Random.Set exposing (notInSet)


type Card
    = BeFruitful
    | Multiply



-- | FillTheEarth
-- | Dominion
-- | Flourish
-- | GoldenHooves
-- | FallingRock
-- | Wolves
-- | Lightning
-- | Storm
-- | Meteor
-- | Shephion
-- | Plague
-- | Crwoding
-- | Slump
-- | SheepDog
-- | PlanningSheep
-- | AllPurposeSheep
-- | Inspiration


beFruitful : List Sheep -> Sheep -> List Sheep
beFruitful field target =
    List.append field [ target ]


multiply : List Sheep -> List Sheep
multiply field =
    List.append field [ Three ]


cardToString : Card -> String
cardToString card =
    case card of
        BeFruitful ->
            "産めよ"

        Multiply ->
            "増やせよ"



-- FillTheEarth ->
--     "地に満ちよ"
-- Dominion ->
--     "統率"
-- Flourish ->
--     "繁栄"
-- GoldenHooves ->
--     "黄金の蹄"
-- FallingRock ->
--     "落石"
-- Wolves ->
--     "狼"
-- Lightning ->
--     "落雷"
-- Storm ->
--     "嵐"
-- Meteor ->
--     "メテオ"
-- Shephion ->
--     "シェフィオン"
-- Plague ->
--     "疫病"
-- Crwoding ->
--     "過密"
-- Slump ->
--     "暴落"
-- SheepDog ->
--     "牧羊犬"
-- PlanningSheep ->
--     "対策ひつじ"
-- AllPurposeSheep ->
--     "万能ひつじ"
-- Inspiration ->
--     "霊感"


allCards : List Card
allCards =
    [ BeFruitful
    , BeFruitful
    , BeFruitful
    , Multiply

    -- , FillTheEarth
    -- , Dominion
    -- , Dominion
    -- , Flourish
    -- , GoldenHooves
    -- , FallingRock
    -- , Wolves
    -- , Lightning
    -- , Storm
    -- , Meteor
    -- , Shephion
    -- , Plague
    -- , Crwoding
    -- , Slump
    -- , SheepDog
    -- , PlanningSheep
    -- , AllPurposeSheep
    -- , Inspiration
    ]


initHands : Random.Generator (List Card)
initHands =
    Random.list 5 (Random.uniform BeFruitful allCards)


initDeck : Random.Generator (List Card)
initDeck =
    Random.List.shuffle allCards


showCard : Card -> Html Msg
showCard card =
    cardToButton card <| cardToString card


cardToButton : Card -> String -> Html Msg
cardToButton card text =
    if card == Multiply then
        Html.button
            [ onClick UseMultiply
            ]
            [ Html.text text
            ]

    else
        Html.text text


sheepToString : Sheep -> String
sheepToString sheep =
    case sheep of
        One ->
            "1"

        Three ->
            "3"

        Ten ->
            "10"

        Thirty ->
            "30"

        OneHundred ->
            "100"

        ThreeHundred ->
            "300"

        Thousand ->
            "1000"


showSheep : Maybe Sheep -> Bool -> FieldId -> Html Msg
showSheep maybeSheep selected index =
    case maybeSheep of
        Just sheep ->
            Html.div []
                [ if selected then
                    Html.button
                        [ onClick (ToggleSelectSheep index) ]
                        [ Html.text "◯" ]

                  else
                    Html.button
                        [ onClick (ToggleSelectSheep index) ]
                        [ Html.text "☓" ]
                , Html.text <| sheepToString sheep
                ]

        Nothing ->
            Html.div [] [ Html.text "-" ]


type Sheep
    = One
    | Three
    | Ten
    | Thirty
    | OneHundred
    | ThreeHundred
    | Thousand


type alias Field =
    Array (Maybe Sheep)


type alias FieldId =
    Int


type alias Model =
    { count : Int
    , field : Field
    , hands : List Card
    , deck : List Card
    , selectedSheep : Dict FieldId Bool
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { count = 0
      , field = Array.fromList [ Just One, Nothing, Nothing, Nothing, Nothing ]
      , hands = []
      , deck = []
      , selectedSheep =
            Dict.fromList
                [ ( 0, False )
                , ( 1, False )
                , ( 2, False )
                , ( 3, False )
                , ( 4, False )
                ]
      }
    , Cmd.batch [ Random.generate Init initDeck ]
    )


type Msg
    = Increment
    | Decrement
    | Init (List Card)
    | DrawFive
    | ToggleSelectSheep FieldId
    | UseMultiply


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )

        Init deck ->
            ( { model | deck = deck }
            , Cmd.none
            )

        DrawFive ->
            let
                hands =
                    List.take 5 model.deck

                rest =
                    List.drop 5 model.deck
            in
            ( { model | hands = List.concat [ model.hands, hands ], deck = rest }
            , Cmd.none
            )

        ToggleSelectSheep fieldId ->
            ( { model
                | selectedSheep =
                    Dict.get fieldId model.selectedSheep
                        |> (\x ->
                                case x of
                                    Just selected ->
                                        if selected then
                                            insert fieldId False model.selectedSheep

                                        else
                                            insert fieldId
                                                True
                                                model.selectedSheep

                                    Nothing ->
                                        model.selectedSheep
                           )
              }
            , Cmd.none
            )

        UseMultiply ->
            let
                updatedField =
                    findLastNothing model.field
                        |> Maybe.map (\i -> Array.set i (Just Three) model.field)
                        |> Maybe.withDefault model.field

                updatedHands =
                    removeFirst Multiply model.hands
            in
            ( { model | field = updatedField, hands = updatedHands }, Cmd.none )


findLastNothing : Array (Maybe a) -> Maybe Int
findLastNothing array =
    array
        |> Array.toIndexedList
        |> List.filter (\( _, v ) -> v == Nothing)
        -- |> List.reverse
        |> List.head
        |> Maybe.map Tuple.first


removeFirst : a -> List a -> List a
removeFirst item list =
    case list of
        [] ->
            []

        x :: xs ->
            if x == item then
                xs

            else
                x :: removeFirst item xs


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.button [ onClick DrawFive ] [ Html.text "draw" ]
        , Html.div [] (Array.toList (Array.indexedMap (\index sheep -> showSheep sheep (Dict.get index model.selectedSheep |> Maybe.withDefault False) index) model.field))
        , Html.div [] (map showCard model.hands)
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
