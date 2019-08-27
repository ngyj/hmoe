module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events as E
import Debug
import Html exposing (Html, Attribute, text, div, input, a, img, ul, h1, li, span)
import Html.Attributes exposing (id, style, href, class, src, autofocus, name, placeholder)
import Html.Events exposing (onInput, onClick)
import Html.Lazy exposing (lazy2)
import Json.Decode as D
import Http

import Image exposing (Image, moeDecoder, foo, imgpath, wppath, thumbnail)
import Model exposing (..)
import Query exposing (query)
import Utils exposing (..)

main = Browser.element
       { init = init
       , update = update
       , view = view
       , subscriptions = subscriptions
       }

subscriptions : Model -> Sub Msg
subscriptions model = case model of
                        ImList _ -> E.onKeyDown keyHandler
                        _ -> Sub.none

keyHandler : D.Decoder Msg
keyHandler =
  let
    h k = case k of
           "Escape" -> Unfocus
           "ArrowRight" -> KeyInput Right
           "ArrowLeft" -> KeyInput Left
           _ -> Debug.log k Nop
    keyDecoder = D.field "key" D.string
  in
    D.map h keyDecoder

init : () -> (Model, Cmd Msg)
init _ = (Start, getMoe)

-- model
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    focus i m = { m | focus = Just i}
    unfocus m = { m | focus = Nothing }
    isFocus m = case m of
      ImList r -> isJust r.focus
      _ -> False
    overCurrentFocus m f = let g x = { x | current = modBy (Array.length x.data) <| f x.current}
                           in case m of
                                ImList r -> ImList { r | focus = Maybe.map g r.focus }
                                _ -> m
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
      Nop -> (model, Cmd.none)

      KeyInput Right -> (overCurrentFocus model <| (+) 1, Cmd.none)
      KeyInput Left -> (overCurrentFocus model <| (-) 1, Cmd.none)
      -- _ -> Debug.log "update: unhandled patmatch" (model, Cmd.none)


-- view
view : Model -> Html Msg
view model =
    case model of
        Failure e -> div [] [ text "something happened :^)"] |> Debug.log ("getMoe: " ++ Debug.toString e)
        Start -> text "..."
        ImList is -> case is.focus of
                       Nothing -> div [id "main"] (viewList is)
                       Just i -> div [id "main"] (viewList is ++ [viewFocus i])

viewList : {a | show : List Image} -> List (Html Msg)
viewList ils = [ txtbox Query, root ils.show ]

viewFocus : FocusModel -> Html Msg
viewFocus fm =
    div [ id "img-focus" ]
        [ focused fm
        , div [ class "if-wps" ]
              (sideList fm)
        ]


sideList fm =
  let focusOn i = Focus {fm | current = i }
      viewThumb i url = a [ (id << (++) "gi" << String.fromInt) i
                          , class "if-wps-img"
                          , if (i == fm.current) then (class "if-wps-head") else style "" ""
                          , onClick (focusOn i)]
                          [ img [ src url ] [] ]
  in
    Array.toList <| Array.indexedMap viewThumb fm.data

focused fm = let current = Maybe.withDefault "/moe/static/404.png" <| Array.get fm.current fm.data in
    div [ class "if-left" , onClick Unfocus ]
        [ div [ class "if-image" ]
              [ a [ href current ]
                  [ img [src current ] [] ]
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

-- @TODO put 3 cats in the model instead of hardcoding everything
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
      fmod = { data = Array.fromList (imgpath image :: List.map wppath image.wp)
             , current = if List.isEmpty image.wp then 0 else 1
             }
  in
    span [] <| List.concat
      [ case image.src of
             Just s -> [ a [href s]
                           [text "source"]
                       , when haswp (text " - ")
                       ]
             Nothing -> []
      , [ when haswp <|
            a [ onClick <| Focus fmod , class "link"]
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
