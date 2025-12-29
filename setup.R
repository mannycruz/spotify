# Load in client keys from file
file_location <- readline("Key file location:\n")

Sys.setenv(SPOTIFY_CLIENT_ID=readLines(file_location)[1])
Sys.setenv(SPOTIFY_CLIENT_SECRET=readLines(file_location)[2])

# Get access token via SpotifyR function
access_token <- get_spotify_access_token()

# Temporary fix for Spotify's updated API restricting localhost
get_spotify_authorization_code <- function (client_id = Sys.getenv("SPOTIFY_CLIENT_ID"), client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"), 
    scope = scopes()) {
    endpoint <- oauth_endpoint(authorize = "https://accounts.spotify.com/authorize", 
        access = "https://accounts.spotify.com/api/token")
    app <- oauth_app("spotifyr", client_id, client_secret, redirect_uri = "http://127.0.0.1:1410/")
    token <- (purrr::safely(.f = oauth2.0_token))(endpoint = endpoint, 
        app = app, scope = scope)
    if (!is.null(token$error)) {
        token$error
    }
    else {
        token$result
    }
}
