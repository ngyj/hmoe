module Query exposing (query,expandTags)

import Image exposing (Image)
import String exposing (startsWith)


query : String -> List Image -> List Image
query s = List.filter (match s)
          -- TODO replace by active flag
          -- List.map (\i -> {i | active = match s i})


match : String -> Image -> Bool
match s img = List.any (startsWith s) img.tags
              || case img.cat of
                     Just c -> startsWith s c
                     Nothing -> False

-- FIXME this is shit
expandTags : Image -> Image
expandTags img = img.tags
               |> List.map (String.split "_")
               |> List.filter (\l -> List.length l > 1)
               |> List.concat
               |> \ts -> { img | tags = img.tags ++ ts }
