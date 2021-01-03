port module OAuthPorts exposing (randomBytes, genRandomBytes)

import Msg exposing (Msg)

port randomBytes : (List Int -> msg) -> Sub msg
port genRandomBytes : Int -> Cmd msg

