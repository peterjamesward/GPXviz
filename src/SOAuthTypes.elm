module SOAuthTypes exposing (..)

import Json.Decode as Json
import OAuth
import OAuth.Implicit as OAuth
import Url exposing (Url)


type alias Model =
    { redirectUri : Url
    , flow : Flow
    }


type Flow
    = Idle
    | Authorized OAuth.Token
    | Done UserInfo
    | Errored Error


type Error
    = ErrStateMismatch
    | ErrAuthorization OAuth.AuthorizationError
    | ErrHTTPGetUserInfo


type alias UserInfo =
    { firstname : String
    , lastname : String
    }


type alias Configuration =
    { authorizationEndpoint : Url
    , userInfoEndpoint : Url
    , userInfoDecoder : Json.Decoder UserInfo
    , clientId : String
    , scope : List String
    }


type Msg
    = NoOp
    | SignInRequested
    | GotRandomBytes (List Int)
    | GotAccessToken (Result Http.Error OAuth.AuthorizationSuccess)
    | UserInfoRequested
    | GotUserInfo (Result Http.Error UserInfo)
    | SignOutRequested
