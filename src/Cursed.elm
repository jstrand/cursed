module Cursed exposing
    ( Attributes
    , Color(..)
    , Position
    , Screen
    , border
    , character
    , defaultAttributes
    , empty
    , render
    , text
    )

{-| Text User Interface (TUI) module for creating simple character based screens.
-}

import Array exposing (Array)
import Html
import Html.Attributes


type Color
    = Black
    | White


type alias Attributes =
    { foreground : Color
    , background : Color
    }


{-| Some reasonable defaults to use for a start.
-}
defaultAttributes =
    { foreground = White
    , background = Black
    }


type alias Position =
    ( Int, Int )


type alias Content =
    { character : Char
    , attributes : Attributes
    }


type alias Screen =
    Array (Array Content)


{-| A default empty screen useful to start with and then adding content to.
-}
empty : Int -> Int -> Screen
empty width height =
    let
        defaultRow =
            Array.repeat width (Content ' ' defaultAttributes)
    in
    Array.repeat height defaultRow


{-| Render a screen to html.
-}
render : Screen -> Html.Html a
render screen =
    let
        renderCharacter char =
            Html.span
                (toHtmlAttributes char.attributes)
                [ Html.text (String.fromChar char.character)
                ]

        renderRow row =
            Array.map renderCharacter row |> Array.toList

        renderRows =
            Array.map renderRow screen
                |> Array.toList
                |> List.intersperse [ Html.br [] [] ]
                |> List.concat
    in
    Html.pre [] renderRows


{-| Output a single character to the screen.
Setting a character outside the screen has no effect.
-}
character : Attributes -> Position -> Char -> Screen -> Screen
character attributes ( x, y ) char screen =
    let
        content =
            { character = char, attributes = attributes }
    in
    case Array.get y screen of
        Just row ->
            Array.set y (Array.set x content row) screen

        Nothing ->
            screen


{-| Output a text at the given position. Line breaks should work as
expected allowing for paragraph output.
-}
text : Attributes -> Position -> String -> Screen -> Screen
text attributes position content =
    vertical (applyStringHorizontally attributes) position (String.lines content)


{-| Draw a border at the given position using box drawing characters.
-}
border : Attributes -> Int -> Int -> Position -> Screen -> Screen
border attributes width height ( x, y ) =
    horizontalLine attributes '╔' '═' '╗' ( x, y ) width
        >> verticalLine1 attributes '║' ( x, y + 1 ) height
        >> verticalLine1 attributes '║' ( x + width - 1, y + 1 ) height
        >> horizontalLine attributes '╚' '═' '╝' ( x, y + height - 1 ) width



-- Helper functions


toHtmlAttributes : Attributes -> List (Html.Attribute msg)
toHtmlAttributes attributes =
    [ Html.Attributes.style "background" (color attributes.background)
    , Html.Attributes.style "color" (color attributes.foreground)
    ]


color : Color -> String
color c =
    case c of
        Black ->
            "black"

        White ->
            "white"


line :
    (Position -> List Char -> Screen -> Screen)
    -> Char
    -> Char
    -> Char
    -> Position
    -> Int
    -> Screen
    -> Screen
line apply start middle end position length =
    let
        content =
            start
                :: List.repeat (length - 2) middle
                ++ [ end ]
    in
    apply position content


horizontalLine : Attributes -> Char -> Char -> Char -> Position -> Int -> Screen -> Screen
horizontalLine attributes =
    line (horizontal (applyChar attributes))


horizontalLine1 : Attributes -> Char -> Position -> Int -> Screen -> Screen
horizontalLine1 attributes char =
    horizontalLine attributes char char char


verticalLine : Attributes -> Char -> Char -> Char -> Position -> Int -> Screen -> Screen
verticalLine attributes =
    line (vertical (applyChar attributes))


verticalLine1 : Attributes -> Char -> Position -> Int -> Screen -> Screen
verticalLine1 attributes char =
    verticalLine attributes char char char


applyChar : Attributes -> ( Position, Char ) -> Screen -> Screen
applyChar attributes ( position, char ) =
    character attributes position char


applyStringHorizontally : Attributes -> ( Position, String ) -> Screen -> Screen
applyStringHorizontally attributes ( position, string ) screen =
    horizontal (applyChar attributes) position (String.toList string) screen


type alias Apply content =
    ( Position, content ) -> Screen -> Screen


applyToPositions : Apply content -> List Position -> List content -> Screen -> Screen
applyToPositions apply positions content screen =
    List.map2 Tuple.pair positions content
        |> List.foldl apply screen


applyList : Apply content -> (Int -> Position) -> Int -> List content -> Screen -> Screen
applyList apply makePosition start content =
    let
        positions =
            List.range start (List.length content + start)
                |> List.map makePosition
    in
    applyToPositions apply positions content


makePositionX x =
    Tuple.pair x


makePositionY y x =
    Tuple.pair x y


vertical : Apply content -> Position -> List content -> Screen -> Screen
vertical apply ( x, y ) =
    applyList apply (makePositionX x) y


horizontal : Apply content -> Position -> List content -> Screen -> Screen
horizontal apply ( x, y ) =
    applyList apply (makePositionY y) x
