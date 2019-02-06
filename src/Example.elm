module Example exposing (main)

import Cursed


attributes =
    Cursed.defaultAttributes


exampleText =
    """
# Cursed

Prototyping a text based UI library for the Web written in Elm.

It's a silly idea, but I like it.
"""


main =
    Cursed.empty 80 10
        |> Cursed.border attributes 80 10 ( 0, 0 )
        |> Cursed.text attributes ( 2, 1 ) exampleText
        |> Cursed.render
