module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Events exposing (onClick)
import List exposing (map)
import Random
import Random.List


type Card
    = BeFruitful
    | Multiply
    | FillTheEarth
    | Dominion
    | Flourish
    | GoldenHooves
    | FallingRock
    | Wolves
    | Lightning
    | Storm
    | Meteor
    | Shephion
    | Plague
    | Crwoding
    | Slump
    | SheepDog
    | PlanningSheep
    | AllPurposeSheep
    | Inspiration


cardToString : Card -> String
cardToString card =
    case card of
        BeFruitful ->
            "産めよ"

        Multiply ->
            "増やせよ"

        FillTheEarth ->
            "地に満ちよ"

        Dominion ->
            "統率"

        Flourish ->
            "繁栄"

        GoldenHooves ->
            "黄金の蹄"

        FallingRock ->
            "落石"

        Wolves ->
            "狼"

        Lightning ->
            "落雷"

        Storm ->
            "嵐"

        Meteor ->
            "メテオ"

        Shephion ->
            "シェフィオン"

        Plague ->
            "疫病"

        Crwoding ->
            "過密"

        Slump ->
            "暴落"

        SheepDog ->
            "牧羊犬"

        PlanningSheep ->
            "対策ひつじ"

        AllPurposeSheep ->
            "万能ひつじ"

        Inspiration ->
            "霊感"


allCards : List Card
allCards =
    [ BeFruitful
    , BeFruitful
    , BeFruitful
    , Multiply
    , FillTheEarth
    , Dominion
    , Dominion
    , Flourish
    , GoldenHooves
    , FallingRock
    , Wolves
    , Lightning
    , Storm
    , Meteor
    , Shephion
    , Plague
    , Crwoding
    , Slump
    , SheepDog
    , PlanningSheep
    , AllPurposeSheep
    , Inspiration
    ]


initHands : Random.Generator (List Card)
initHands =
    Random.list 5 (Random.uniform BeFruitful allCards)


initDeck : Random.Generator (List Card)
initDeck =
    Random.List.shuffle allCards


showCard : Card -> Html msg
showCard card =
    Html.div []
        [ Html.text <| cardToString card
        ]


type Sheep
    = One
    | Three
    | Ten
    | Thirty
    | OneHundred
    | ThreeHundred
    | Thousand


type alias Model =
    { count : Int
    , hands : List Card
    , deck : List Card
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { count = 0, hands = [], deck = [] }
    , Cmd.batch [ Random.generate Init initDeck ]
    )


type Msg
    = Increment
    | Decrement
    | Init (List Card)
    | DrawFive


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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.button [ onClick DrawFive ] [ Html.text "draw" ]
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
