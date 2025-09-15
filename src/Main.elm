module Main exposing (..)

import Array exposing (Array)
import Browser
import Dict exposing (Dict, insert)
import Html exposing (Html, select)
import Html.Events exposing (onClick)
import List exposing (map)
import Random
import Random.Char exposing (dominoTile)
import Random.Extra exposing (maybe)
import Random.List
import Random.Set exposing (notInSet)


type Card
    = BeFruitful
    | Multiply
      -- | FillTheEarth
    | Dominion



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
        Dominion ->
            "統率"



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
    , Dominion

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
    case card of
        BeFruitful ->
            Html.button
                [ onClick UseBeFruitful
                ]
                [ Html.text text
                ]

        Multiply ->
            Html.button
                [ onClick UseMultiply
                ]
                [ Html.text text
                ]

        Dominion ->
            Html.button
                [ onClick UseDominion ]
                [ Html.text text ]


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


showSheep : Maybe Sheep -> ( FieldId, Bool ) -> Html Msg
showSheep maybeSheep ( index, selected ) =
    Html.div []
        [ Html.button
            [ onClick <| ToggleSelectSheep index
            ]
            [ Html.text <|
                if selected then
                    "◯"

                else
                    "☓"
            ]
        , maybeSheep
            |> Maybe.map sheepToString
            |> Maybe.withDefault "-"
            |> Html.text
        ]


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


type alias SelectedSheep =
    Dict FieldId Bool


type alias FieldId =
    Int


type alias Model =
    { count : Int
    , field : Field
    , hands : List Card
    , deck : List Card
    , selectedSheep : SelectedSheep
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
    | UseBeFruitful
    | UseMultiply
    | UseDominion


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

        UseBeFruitful ->
            let
                selectedSheepIndexes =
                    model.selectedSheep
                        |> Dict.filter (\_ value -> value)
                        |> Dict.keys

                targetSheep =
                    case selectedSheepIndexes of
                        [ index ] ->
                            Array.get index model.field
                                |> Maybe.withDefault Nothing

                        _ ->
                            Nothing

                updatedField =
                    targetSheep
                        |> Maybe.map
                            (\target ->
                                findLastNothing model.field
                                    |> Maybe.map (\i -> Array.set i (Just target) model.field)
                                    |> Maybe.withDefault model.field
                            )
                        |> Maybe.withDefault model.field

                updatedSelectedSheep =
                    if model.field /= updatedField then
                        resetSelectedSheep model.selectedSheep

                    else
                        model.selectedSheep

                updatedHands =
                    case targetSheep of
                        Just _ ->
                            removeFirst BeFruitful model.hands

                        Nothing ->
                            model.hands
            in
            ( { model | field = updatedField, hands = updatedHands, selectedSheep = updatedSelectedSheep }, Cmd.none )

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

        UseDominion ->
            let
                selectedSheepIndexes =
                    model.selectedSheep
                        |> Dict.filter (\_ selected -> selected)
                        |> Dict.keys

                selectedSheep =
                    selectedSheepIndexes
                        |> List.map (\i -> Array.get i model.field |> Maybe.withDefault Nothing)

                updatedField =
                    case selectedSheep of
                        [ Just sheep1, Just sheep2, Just sheep3 ] ->
                            if sheep1 == sheep2 && sheep2 == sheep3 then
                                dominion sheep1 model.field

                            else
                                model.field

                        _ ->
                            model.field

                updatedHands =
                    if model.field /= updatedField then
                        removeFirst Dominion model.hands

                    else
                        model.hands

                updateSelectedSheep =
                    if model.field /= updatedField then
                        resetSelectedSheep model.selectedSheep

                    else
                        model.selectedSheep
            in
            ( { model | hands = updatedHands, field = updatedField, selectedSheep = updateSelectedSheep }, Cmd.none )


dominion : Sheep -> Field -> Field
dominion target field =
    field
        |> Array.toList
        |> removeFirst (Just target)
        |> removeFirst (Just target)
        |> removeFirst (Just target)
        |> List.append [ Just (levelUp target), Nothing, Nothing ]
        |> Array.fromList


levelUp : Sheep -> Sheep
levelUp sheep =
    case sheep of
        One ->
            Three

        Three ->
            Ten

        Ten ->
            Thirty

        Thirty ->
            OneHundred

        OneHundred ->
            ThreeHundred

        ThreeHundred ->
            Thousand

        Thousand ->
            Thousand


resetSelectedSheep : Dict a Bool -> Dict a Bool
resetSelectedSheep dict =
    Dict.map (\_ _ -> False) dict


findFirstJust : Array (Maybe a) -> Maybe Int
findFirstJust array =
    array
        |> Array.toIndexedList
        |> List.filter (\( _, v ) -> v /= Nothing)
        |> List.head
        |> Maybe.map Tuple.first


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


showField : Field -> SelectedSheep -> Html Msg
showField field selectedSheep =
    let
        buttons =
            List.map2 (\sheep ( index, selected ) -> showSheep sheep ( index, selected ))
                (Array.toList field)
                (Dict.toList selectedSheep)
    in
    Html.div [] buttons


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.button [ onClick DrawFive ] [ Html.text "draw" ]
        , showField model.field model.selectedSheep
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
