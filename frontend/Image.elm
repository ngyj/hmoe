module Image exposing (..)

import Json.Decode as D exposing (Decoder, field, list, string, bool, nullable)
import Json.Decode.Pipeline as DP exposing (required, hardcoded)

type alias Image =
  { fn  : String
  , cat : Maybe String
  , src : Maybe String
  , tags : List String
  , wp  : List String
  , active : Bool
  }

imgSample = [ { fn ="haskell01.png"
              , cat = Just "tan"
              , src = Just "https://www.pixiv.net/member_illust.php?mode=medium&illust_id=47784511"
              , tags = [ "chomado","original" ]
              , wp = [] }
            , { fn ="haskell03.png"
              , cat = Just "book"
              , src   = Nothing
              , tags = [ "karen","kiniro_mosaic","hpffp" ]
              , wp = ["foobar"] }
            ]

foo = { fn = "", cat = Nothing, src = Nothing, tags = [], wp = [] }

moeDecoder : Decoder (List Image)
moeDecoder = field "data" (list imgDecoder)

imgDecoder : Decoder Image
imgDecoder = D.succeed Image
           |> required  "imFn" string
           |> required  "imCat" (nullable string)
           |> required  "imSrc" (nullable string) -- fallback on image.google.com ?
           |> required  "imTag" (list string)
           |> required  "imWp"  (list string)
           |> hardcoded True
