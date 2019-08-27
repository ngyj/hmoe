module Model exposing (..)

import Array exposing (Array)
import Http

import Image exposing (..)

type alias FocusModel = { data : Array String
                        , current : Int
                        }

type Model = Failure Http.Error
           | Start
           | ImList { get : List Image
                    , show : List Image
                    , focus : Maybe FocusModel
                    }

mkImList : List Image -> Model
mkImList is = ImList {get=is, show=is, focus=Nothing}

type Key = Left | Right

type Msg = GetMoe (Result Http.Error (List Image)) -- ^ fetching image list from backend
         | Query String       -- ^ filter displayed images
         | Focus FocusModel   -- ^ gallery-style focus on an image and its wallpapers
         | Unfocus            -- ^ quit the Focus mode
         | Nop
         | KeyInput Key
