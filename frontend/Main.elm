module Main exposing (..)

import Browser
import Debug
import Html exposing (Html, text, div, input)
import Html.Attributes exposing (id)
import Html.Events exposing (onInput)
import Http
import Index exposing (root)

import Image exposing (Image, moeDecoder)
import Query exposing (query)

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
                    , show : List Image}

init : () -> (Model, Cmd Msg)
init _ = (Start, getMoe)

-- update
type Msg = GetMoe (Result Http.Error (List Image))
         | Query String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetMoe r ->
        case r of
          Ok imgs -> let imgs_ = List.map Query.expandTags imgs
                     in (ImList {get=imgs_, show=imgs_}, Cmd.none)
          Err e -> (Failure e, Cmd.none)

    Query s ->
      case model of
        ImList r -> (ImList <| { r | show = query s r.get }, Cmd.none)
        _ -> Debug.log "Query: should not happen." (model, Cmd.none)

-- view
view : Model -> Html Msg
view model =
    case model of
        Failure e -> div [] [ text "something happened :^)"] |> Debug.log ("getMoe: " ++ Debug.toString e)
        Start -> text "..."
        ImList is -> div [] [ Index.txtbox Query -- TODO
                            , Index.root is.show
                            ]

-- api calls
getMoe = Http.get
  { url = "/moe"
  , expect = Http.expectJson GetMoe moeDecoder
  } -- expectJson : (Result Error a -> msg)
