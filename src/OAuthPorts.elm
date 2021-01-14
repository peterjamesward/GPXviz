port module OAuthPorts exposing (randomBytes, genRandomBytes)

import Msg exposing (Msg)

port randomBytesStrava : (List Int -> msg) -> Sub msg
port genRandomBytesStrava : Int -> Cmd msg

