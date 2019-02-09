module Index exposing (root, txtbox)

import Debug as D
import Html exposing (..)
import Html.Attributes exposing (class, href, src, id, autofocus, name, placeholder, style)
import Html.Events exposing (onInput)
import Html.Lazy exposing (lazy2)

import Image exposing (..)

txtbox msg =
  div
    [class "bloc-query"]
    [ div
        [ class "query-wrapper"]
        [ input
            [ onInput msg
            , id "query"
            , placeholder "filter: book, tan, lyah, hpffp, karen, etc."
            , name "query"
            , autofocus True
            ]
            []
        ]
    ]

root imgs = div
         [ class "root" ]
         [ imgRoot imgs ]

-- TODO put 3 cats in the model insted of hardcoding everything
imgRoot ls =
  let isCat cname x = Just cname == x.cat
      ts = List.filter (isCat "tan") ls
      bs = List.filter (isCat "book") ls
  in
    div
      [ class "bloc-img"]
      [ when (not <| List.isEmpty ts) <| lazy2 viewImages ("tan", "Mascot / tan") ts
      , when (not <| List.isEmpty bs) <| lazy2 viewImages ("book", "Anime girls holding haskell books") bs
      ]

viewImages (cname, lname) is =
    div
      [ class (cname++"-box bloc-img-cat"), id cname]
      [ h1 [] [text lname]
      , ul
          [ class "img-list" ]
          (List.map viewImage is)
      ]

viewImage image =
  let imgpath = "img/" ++ image.fn
      cssDisplay = if image.active then "initial" else "none"
  in
    li
      [ style "display" cssDisplay ]
      [ div
          [ class "img-item-box" ]
          [ aimg imgpath
          , viewInfo image
          ]
      ]

viewInfo image =
  let wppath  = "img/wp/" ++ image.fn
  in
    span [] <| List.concat
      [ case image.src of
             Just s -> [a [href s]
                          [text "source"]
                       , when image.wp (text " - ")
                       ]
             Nothing -> []
      , [ when image.wp (a [href wppath] [text "wp"]) ]
      ]

aimg : String -> Html msg
aimg link =
    a [ href link, class "img-item"]
        [ img [ src link ]
              []
        ]

when : Bool -> Html a -> Html a
when p m = if p then m else empty

empty : Html a
empty = text ""
