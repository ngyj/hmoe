module Main exposing (..)

import Browser
import Debug
import Html exposing (Html, Attribute, text, div, input)
import Html.Attributes exposing (id, style)
import Html.Events exposing (onInput)
import Http
import Index exposing (root)

import Image exposing (Image, moeDecoder, foo)
import Query exposing (query)
import Utils exposing (..)

main = Browser.element
       { init = init
       , update = update
       , view = view
       , subscriptions = always Sub.none
       }

-- model
type Model = Failure Http.Error
           | Start
           | ImList { get : List Image
                    , show : List Image
                    , modal : Maybe Image
                    }

init : () -> (Model, Cmd Msg)
init _ = (Start, getMoe)

mkImList : List Image -> Model
mkImList imgs = ImList {get=imgs, show=imgs, modal=Nothing}

-- update
type Msg = GetMoe (Result Http.Error (List Image))
         | Query String
         | ShowImage Image

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetMoe r ->
      case r of
        Ok imgs -> let imgs_ = List.map Query.expandTags imgs
                   in (mkImList imgs_, Cmd.none)
        Err e -> (Failure e, Cmd.none)

    Query "x" -> (model, Cmd.map (always <| ShowImage foo) Cmd.none)
    Query s ->
      case model of
        ImList r -> (ImList <| { r | show = query s r.get }, Cmd.none)
        _ -> Debug.log "Query: should not happen." (model, Cmd.none)

    ShowImage i ->
      case model of
        ImList r -> (ImList <| { r | modal = Just i}, Cmd.none)
        _ -> Debug.log "ShowImage: should not happen." (model, Cmd.none)

-- view
view : Model -> Html Msg
view model =
    case model of
        Failure e -> div [] [ text "something happened :^)"] |> Debug.log ("getMoe: " ++ Debug.toString e)
        Start -> text "..."
        ImList is -> div [] [ Index.txtbox Query
                            , Index.root is.show
                            ]

-- modal
viewModal (ImList is) =
    case is.modal of
      Nothing -> div [] [ Index.txtbox Query, Index.root is.show ]
      Just i -> div maskStyle
                    [ div modalStyle
                          [ text "foo" ]
                    ]
viewModal _ = div [] []


-- @TODO move this in a .css
maskStyle : List (Attribute msg)
maskStyle =
    List.map (uncurry style)
        [ ("background-color", "rgba(0,0,0,0.3)")
        , ("position", "fixed")
        , ("top", "0")
        , ("left", "0")
        , ("width", "100%")
        , ("height", "100%")
        ]
modalStyle : List (Attribute msg)
modalStyle =
  List.map (uncurry style)
    [ ("background-color", "rgba(255,255,255,1.0)")
    , ("position", "absolute")
    , ("top", "50%")
    , ("left", "50%")
    , ("height", "auto")
    , ("max-height", "80%")
    , ("width", "700px")
    , ("max-width", "95%")
    , ("padding", "10px")
    , ("border-radius", "3px")
    , ("box-shadow", "1px 1px 5px rgba(0,0,0,0.5)")
    , ("transform", "translate(-50%, -50%)")
    ]

-- api calls
getMoe = Http.get
  { url = "/moe"
  , expect = Http.expectJson GetMoe moeDecoder
  } -- expectJson : (Result Error a -> msg)
