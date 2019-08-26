module Main exposing (..)

import Browser
import Debug
import Html exposing (Html, Attribute, text, div, input, a, img, ul, h1, li, span)
import Html.Attributes exposing (id, style, href, class, src, autofocus, name, placeholder)
import Html.Events exposing (onInput, onClick)
import Html.Lazy exposing (lazy2)
import Http

import Image exposing (Image, moeDecoder, foo, imgpath, wppath, thumbnail)
import Model exposing (..)
import Query exposing (query)
import Utils exposing (..)

main = Browser.element
       { init = init
       , update = update
       , view = view
       , subscriptions = always Sub.none
       }

init : () -> (Model, Cmd Msg)
init _ = (Start, getMoe)

-- model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    focus i m = { m | focus = Just i}
    unfocus m = { m | focus = Nothing }
  in
    case msg of
      GetMoe r ->
        case r of
          Ok is -> let imgs_ = List.map Query.expandTags is
                     in (mkImList imgs_, Cmd.none)
          Err e -> (Failure e, Cmd.none)

      Query s ->
        case model of
          ImList r -> (ImList <| { r | show = query s r.get }, Cmd.none)
          _ -> Debug.log "Query: patmatch error in update." (model, Cmd.none)

      Focus i ->
        case model of
          ImList r -> (ImList <| focus i r, Cmd.none)
          _ -> Debug.log "Focus: patmatch error in update." (model, Cmd.none)

      Unfocus ->
        case model of
          ImList r -> (ImList <| unfocus r, Cmd.none)
          _ -> Debug.log "Unfocus: patmatch error in update." (model, Cmd.none)


-- view
view : Model -> Html Msg
view model =
    case model of
        Failure e -> div [] [ text "something happened :^)"] |> Debug.log ("getMoe: " ++ Debug.toString e)
        Start -> text "..."
        ImList is -> case is.focus of
                         Nothing -> div [] (viewList is)
                         Just i -> div [] (viewList is ++ [viewFocus i])

viewList : {a | show : List Image} -> List (Html Msg)
viewList ils = [ txtbox Query, root ils.show ]

viewFocus : FocusModel -> Html Msg
viewFocus fm =
    div [ id "img-focus" , onClick Unfocus]
        [ focused fm
        , div [ class "if-wps" ]
              (sideList fm)
        ]


sideList fm =
  let one attr url = a ([class "if-wps-img"] ++ attr)
                       [ img [ src url ] [] ]
  in
    List.map (one []) fm.init
    ++ [ one [class "if-wps-head"] fm.head ]
    ++ List.map (one []) fm.tail

focused fm =
    div [ class "if-left" ]
        [ div [ class "if-image" ]
              [ a [ href fm.head ]
                  [ img [src fm.head ] [] ]
              ]
        ]

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

-- @TODO put 3 cats in the model insted of hardcoding everything
imgRoot : List Image -> Html Msg
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

viewImages : (String, String) -> List Image -> Html Msg
viewImages (cname, lname) is =
    div
      [ class (cname++"-box bloc-img-cat"), id cname]
      [ h1 [] [text lname]
      , ul
          [ class "img-list" ]
          (List.map viewImage is)
      ]

viewImage : Image -> Html Msg
viewImage image =
  let cssDisplay = if image.active then "initial" else "none"
  in
    li
      [ style "display" cssDisplay ]
      [ div
          [ class "img-item-box" ]
          [ aimg <| imgpath image
          , viewInfo image
          ]
      ]

viewInfo : Image -> Html Msg
viewInfo image =
  let haswp = List.isEmpty image.wp |> not
      fmod = case image.wp of
               [] -> { init= [], tail= [], head= imgpath image }
               x::xs -> { init= [imgpath image], head= wppath x, tail=xs}
  in
    span [] <| List.concat
      [ case image.src of
             Just s -> [ a [href s]
                           [text "source"]
                       , when haswp (text " - ")
                       ]
             Nothing -> []
      , [ when haswp <|
            a [ href "#", onClick <| Focus fmod  ]
              [ text "wp" ]
        ]
      ]

aimg : String -> Html msg
aimg link =
    a [ href link, class "img-item"]
      [ img [ src (thumbnail link) ]
            []
      ]

-- actions
getMoe = Http.get
  { url = "/moe"
  , expect = Http.expectJson GetMoe moeDecoder
  } -- expectJson : (Result Error a -> msg)
