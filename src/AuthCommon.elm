module AuthCommon exposing (..)


import Base64.Encode as Base64
import Bytes exposing (Bytes)
import Bytes.Encode as Bytes
import OAuth
import Url exposing (Protocol(..), Url)
toBytes : List Int -> Bytes
toBytes =
    List.map Bytes.unsignedInt8 >> Bytes.sequence >> Bytes.encode


base64 : Bytes -> String
base64 =
    Base64.bytes >> Base64.encode


convertBytes : List Int -> { state : String }
convertBytes =
    toBytes >> base64 >> (\state -> { state = state })


oauthErrorToString : { error : OAuth.ErrorCode, errorDescription : Maybe String } -> String
oauthErrorToString { error, errorDescription } =
    let
        desc =
            errorDescription |> Maybe.withDefault "" |> String.replace "+" " "
    in
    OAuth.errorCodeToString error ++ ": " ++ desc


defaultHttpsUrl : Url
defaultHttpsUrl =
    { protocol = Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }
