module StravaAuth exposing (..)

import Base64.Encode as Base64
import Browser exposing (Document, application)
import Browser.Navigation as Navigation exposing (Key)
import Bytes exposing (Bytes)
import Bytes.Encode as Bytes
import Delay exposing (TimeUnit(..), after)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import OAuth
import OAuth.Implicit as OAuth
import OAuthPorts exposing (genRandomBytes)
import OAuthTypes exposing (..)
import Url exposing (Protocol(..), Url)



{-
   main : Program (Maybe (List Int)) Model OAuthMsg
   main =
       application
           { init =
               Maybe.map convertBytes >> init
           , update =
               update
           , subscriptions =
               always <| randomBytes GotRandomBytes
           , onUrlRequest =
               always NoOp
           , onUrlChange =
               always NoOp
           , view =
               view
                   { title = "Spotify - Flow: Implicit"
                   , btnClass = class "btn-spotify"
                   }
           }
-}


{-| OAuth configuration.

Note that this demo also fetches basic user information with the obtained access token,
hence the user info endpoint and JSON decoder

<http://www.strava.com/oauth/authorize?client_id=59195&response_type=code&redirect_uri=http://localhost/exchange_token&scope=read>

<https://www.strava.com/api/v3/athlete>

-}
configuration : Configuration
configuration =
    { authorizationEndpoint =
        { defaultHttpsUrl | host = "www.strava.com", path = "/authorize" }
    , userInfoEndpoint =
        { defaultHttpsUrl | host = "www.strava.com", path = "/api/v3/athlete" }
    , userInfoDecoder =
        Json.map2 UserInfo
            (Json.field "firstname" Json.string)
            (Json.field "lastname" Json.string)
    , clientId =
        "59195"
    , scope =
        []
    }


{-| During the authentication flow, we'll run twice into the `init` function:

  - The first time, for the application very first run. And we proceed with the `Idle` state,
    waiting for the user (a.k.a you) to request a sign in.

  - The second time, after a sign in has been requested, the user is redirected to the
    authorization server and redirects the user back to our application, with an access
    token and other fields as query parameters.

When query params are present (and valid), we consider the user `Authorized`.

-}
init : Maybe { state : String } -> Url -> Key -> (OAuthMsg -> msgType) -> ( Model, Cmd msgType )
init mflags origin navigationKey wrapperMsg =
    let
        redirectUri =
            { origin | query = Nothing, fragment = Nothing }

        clearUrl =
            Navigation.replaceUrl navigationKey (Url.toString redirectUri)
    in
    case OAuth.parseToken origin of
        OAuth.Empty ->
            ( { flow = Idle, redirectUri = redirectUri }
            , Cmd.none
            )

        -- It is important to set a `state` when making the authorization request
        -- and to verify it after the redirection. The state can be anything but its primary
        -- usage is to prevent cross-site request forgery; at minima, it should be a short,
        -- non-guessable string, generated on the fly.
        --
        -- We remember any previously generated state  state using the browser's local storage
        -- and give it back (if present) to the elm application upon start
        OAuth.Success { token, state } ->
            case mflags of
                Nothing ->
                    ( { flow = Errored ErrStateMismatch, redirectUri = redirectUri }
                    , clearUrl
                    )

                Just flags ->
                    if state /= Just flags.state then
                        ( { flow = Errored ErrStateMismatch, redirectUri = redirectUri }
                        , clearUrl
                        )

                    else
                        ( { flow = Authorized token, redirectUri = redirectUri }
                        , Cmd.batch
                            -- Artificial delay to make the live demo easier to follow.
                            -- In practice, the access token could be requested right here.
                            [ after 750 Millisecond (wrapperMsg UserInfoRequested)
                            , clearUrl
                            ]
                        )

        OAuth.Error error ->
            ( { flow = Errored <| ErrAuthorization error, redirectUri = redirectUri }
            , clearUrl
            )


getUserInfo : Configuration -> OAuth.Token -> Cmd OAuthMsg
getUserInfo { userInfoDecoder, userInfoEndpoint } token =
    Http.request
        { method = "GET"
        , body = Http.emptyBody
        , headers = OAuth.useToken token []
        , url = Url.toString userInfoEndpoint
        , expect = Http.expectJson GotUserInfo userInfoDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



{- On the JavaScript's side, we have:

   app.ports.genRandomBytes.subscribe(n => {
     const buffer = new Uint8Array(n);
     crypto.getRandomValues(buffer);
     const bytes = Array.from(buffer);
     localStorage.setItem("bytes", bytes);
     app.ports.randomBytes.send(bytes);
   });
-}


update : OAuthMsg -> Model -> ( Model, Cmd OAuthMsg )
update msg model  =
    let
        ( newModel, authMsg ) =
            case ( model.flow, msg ) of
                ( Idle, SignInRequested ) ->
                    signInRequested model

                ( Idle, GotRandomBytes bytes ) ->
                    gotRandomBytes model bytes

                ( Authorized token, UserInfoRequested ) ->
                    userInfoRequested model token

                ( Authorized _, GotUserInfo userInfoResponse ) ->
                    gotUserInfo model userInfoResponse

                ( Done _, SignOutRequested ) ->
                    signOutRequested model

                _ ->
                    noOp model

    in
    ( newModel, authMsg )


noOp : Model -> ( Model, Cmd OAuthMsg )
noOp model =
    ( model, Cmd.none )


signInRequested : Model -> ( Model, Cmd OAuthMsg )
signInRequested model =
    ( { model | flow = Idle }
    , genRandomBytes 16
    )


gotRandomBytes : Model -> List Int -> ( Model, Cmd OAuthMsg )
gotRandomBytes model bytes =
    let
        { state } =
            convertBytes bytes

        authorization =
            { clientId = configuration.clientId
            , redirectUri = model.redirectUri
            , scope = configuration.scope
            , state = Just state
            , url = configuration.authorizationEndpoint
            }
    in
    ( { model | flow = Idle }
    , authorization
        |> OAuth.makeAuthorizationUrl
        |> Url.toString
        |> Navigation.load
    )


userInfoRequested : Model -> OAuth.Token -> ( Model, Cmd OAuthMsg )
userInfoRequested model token  =
    ( { model | flow = Authorized token }
    , getUserInfo configuration token
    )



--gotUserInfo : Model -> Result Http.Error UserInfo -> ( Model, Cmd Msg )


gotUserInfo model userInfoResponse =
    case userInfoResponse of
        Err _ ->
            ( { model | flow = Errored ErrHTTPGetUserInfo }
            , Cmd.none
            )

        Ok userInfo ->
            ( { model | flow = Done userInfo }
            , Cmd.none
            )


signOutRequested : Model -> ( Model, Cmd OAuthMsg )
signOutRequested model =
    ( { model | flow = Idle }
    , Navigation.load (Url.toString model.redirectUri)
    )


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
