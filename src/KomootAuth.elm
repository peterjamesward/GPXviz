module KomootAuth exposing (..)

import AuthCommon exposing (convertBytes, defaultHttpsUrl)
import Base64.Encode as Base64
import Browser.Navigation as Navigation exposing (Key)
import Bytes exposing (Bytes)
import Bytes.Encode as Bytes
import Delay exposing (TimeUnit(..), after)
import Element exposing (..)
import Element.Input exposing (button)
import Http
import Json.Decode as Json
import KomootClientSecret
import OAuth
import OAuth.AuthorizationCode as OAuth
import OAuthPorts exposing (genRandomBytes)
import Url exposing (Protocol(..), Url)
import Url.Builder as Builder


type alias Model =
    { redirectUri : Url
    , flow : Flow
    }


dummyModel =
    Model defaultHttpsUrl Idle


type Flow
    = Idle
    | Authorized OAuth.AuthorizationCode
    | Authenticated OAuth.Token
    | Done UserInfo OAuth.Token
    | Errored Error


type Error
    = ErrStateMismatch
    | ErrAuthorization OAuth.AuthorizationError
    | ErrAuthentication OAuth.AuthenticationError
    | ErrHTTPGetAccessToken
    | ErrHTTPGetUserInfo


type alias UserInfo =
    { id : Int
    , firstname : String
    , lastname : String
    }


type alias Configuration =
    { authorizationEndpoint : Url
    , userInfoEndpoint : Url
    , tokenEndpoint : Url
    , userInfoDecoder : Json.Decoder UserInfo
    , clientId : String
    , scope : List String
    , clientSecret : String
    }


type OAuthMsg
    = NoOp
    | SignInRequested
    | GotRandomBytes (List Int)
    | AccessTokenRequested
    | GotAccessToken (Result Http.Error OAuth.AuthenticationSuccess)
    | UserInfoRequested
    | GotUserInfo (Result Http.Error UserInfo)
    | SignOutRequested


{-| OAuth configuration.

Note that this demo also fetches basic user information with the obtained access token,
hence the user info endpoint and JSON decoder

<http://www.strava.com/oauth/authorize?client_id=59195&response_type=code&redirect_uri=http://localhost/exchange_token&scope=read>

<https://www.strava.com/api/v3/athlete>

-}
host =
    "https://auth.komoot.de"


configuration : Configuration
configuration =
    { authorizationEndpoint =
        { defaultHttpsUrl | host = host, path = "/oauth/authorize" }
    , tokenEndpoint =
        { defaultHttpsUrl | host = host, path = "/oauth/token" }
    , userInfoEndpoint =
        { defaultHttpsUrl | host = host, path = "/api/v3/athlete" }
    , userInfoDecoder =
        Json.map3 UserInfo
            (Json.succeed 0)
            (Json.field "username" Json.string)
            (Json.field "username" Json.string)
    , clientId =
        "gpxmagic-64qn28"
    , clientSecret = KomootClientSecret.secret
    , scope =
        [ "profile" ]
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
    case OAuth.parseCode origin of
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
        OAuth.Success { code, state } ->
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
                        ( { flow = Authorized code, redirectUri = redirectUri }
                        , Cmd.batch
                            -- Artificial delay to make the live demo easier to follow.
                            -- In practice, the access token could be requested right here.
                            [ after 750 Millisecond (wrapperMsg AccessTokenRequested)
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


update : OAuthMsg -> Model -> ( Model, Cmd OAuthMsg )
update msg model =
    case ( model.flow, msg ) of
        ( Idle, SignInRequested ) ->
            signInRequested model

        ( Idle, GotRandomBytes bytes ) ->
            gotRandomBytes model bytes

        ( Authorized code, AccessTokenRequested ) ->
            accessTokenRequested model code

        ( Authorized _, GotAccessToken authenticationResponse ) ->
            gotAccessToken model authenticationResponse

        ( Authenticated token, UserInfoRequested ) ->
            userInfoRequested model token

        ( Authenticated _, GotUserInfo userInfoResponse ) ->
            gotUserInfo model userInfoResponse

        ( Done _ _, SignOutRequested ) ->
            signOutRequested model

        _ ->
            noOp model


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
userInfoRequested model token =
    ( { model | flow = Authenticated token }
    , getUserInfo configuration token
    )


getAccessToken : Configuration -> Url -> OAuth.AuthorizationCode -> Cmd OAuthMsg
getAccessToken { clientId, tokenEndpoint } redirectUri code =
    Http.request <|
        OAuth.makeTokenRequest GotAccessToken
            { credentials =
                { clientId = clientId
                , secret = Just KomootClientSecret.secret
                }
            , code = code
            , url = tokenEndpoint
            , redirectUri = redirectUri
            }


accessTokenRequested : Model -> OAuth.AuthorizationCode -> ( Model, Cmd OAuthMsg )
accessTokenRequested model code =
    ( { model | flow = Authorized code }
    , getAccessToken configuration model.redirectUri code
    )


gotAccessToken : Model -> Result Http.Error OAuth.AuthenticationSuccess -> ( Model, Cmd OAuthMsg )
gotAccessToken model authenticationResponse =
    case authenticationResponse of
        Err (Http.BadBody body) ->
            case Json.decodeString OAuth.defaultAuthenticationErrorDecoder body of
                Ok error ->
                    ( { model | flow = Errored <| ErrAuthentication error }
                    , Cmd.none
                    )

                _ ->
                    ( { model | flow = Errored ErrHTTPGetAccessToken }
                    , Cmd.none
                    )

        Err _ ->
            ( { model | flow = Errored ErrHTTPGetAccessToken }
            , Cmd.none
            )

        Ok { token } ->
            ( { model | flow = Authenticated token }
            , Cmd.none
            )


gotUserInfo : Model -> Result Http.Error UserInfo -> ( Model, Cmd OAuthMsg )
gotUserInfo model userInfoResponse =
    case ( model.flow, userInfoResponse ) of
        ( _, Err _ ) ->
            ( { model | flow = Errored ErrHTTPGetUserInfo }
            , Cmd.none
            )

        ( Authenticated token, Ok userInfo ) ->
            ( { model | flow = Done userInfo token }
            , Cmd.none
            )

        _ ->
            ( { model | flow = Errored ErrStateMismatch }
            , Cmd.none
            )


signOutRequested : Model -> ( Model, Cmd OAuthMsg )
signOutRequested model =
    ( { model | flow = Idle }
    , Navigation.load (Url.toString model.redirectUri)
    )


komootButton : Model -> (OAuthMsg -> msg) -> Element msg
komootButton model msgWrapper =
    let
        imgUrl =
            Builder.relative [ "images", "komoot.svg" ] []
    in
    case model.flow of
        Done userInfo _ ->
            column [ padding 10, centerX, spacing 5 ]
                [ text "Connected to Komoot as"
                , text <| userInfo.firstname
                ]

        _ ->
            button
                [ centerX, padding 10 ]
                { onPress = Just <| msgWrapper SignInRequested
                , label =
                    image
                        [ mouseOver [ alpha 0.7 ]
                        , width <| px 250
                        , height <| px 60
                        ]
                        { src = imgUrl
                        , description = "Connect to Komoot"
                        }
                }


getToken : Model -> Maybe OAuth.Token
getToken model =
    case model.flow of
        Done info token ->
            Just token

        Authenticated token ->
            Just token

        _ ->
            Nothing
