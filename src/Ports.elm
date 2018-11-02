port module Ports exposing (ScorePortData, fileContentRead, fileSelected)


type alias ScorePortData =
    { contents : String
    , filename : String
    }


port fileSelected : String -> Cmd msg


port fileContentRead : (ScorePortData -> msg) -> Sub msg
