#' @title Get Spotify access token
#' @param client_id - client id as given by the Spotify for Developers app.  Defaults to Sys.getenv('SPOTIFY_CLIENT_ID')
#' @param client_secret - secret id as given by the Spotify for Developers app.  Defaults to Sys.getenv('SPOTIFY_CLIENT_SECRET')
#' @return Spotify access token for the user
#' @examples
#' \dontrun{
#' get_spotify_access_token()
#' }
#' @export
get_spotify_access_token <- function(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'),
                                     client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET')
                                     ) {

    post <- httr::RETRY('POST', 'https://accounts.spotify.com/api/token',
                 httr::accept_json(),
                 httr::authenticate(client_id, client_secret),
                 body = list(grant_type = 'client_credentials'),
                 encode = 'form', httr::config(http_version = 2)) %>%
        httr::content()

    if (!is.null(post$error)) {
        stop(stringr::str_glue('Could not authenticate with given Spotify credentials:\n\t{post$error_description}'))
    }

    access_token <- post$access_token

    access_token
}

#' @title Get Spotify authorization code
#' @param client_id - client id as given by the Spotify for Developers app.  Defaults to Sys.getenv('SPOTIFY_CLIENT_ID')
#' @param client_secret - secret id as given by the Spotify for Developers app.  Defaults to Sys.getenv('SPOTIFY_CLIENT_SECRET')
#' @param scope - string of scopes for authorization access.  Defaults to all scopes
#' @return Spotify authorization code for the user
#' @examples
#' \dontrun{
#' get_spotify_authorization_code()
#' }
#' @export
get_spotify_authorization_code <- function(
        client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
        client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
        scope = scopes()
        ) {

    endpoint <- httr::oauth_endpoint(authorize = 'https://accounts.spotify.com/authorize',
                               access = 'https://accounts.spotify.com/api/token')

    app <- httr::oauth_app('spotifyr', client_id, client_secret)

    token <- purrr::safely(.f = httr::oauth2.0_token)(
        endpoint = endpoint,
        app = app,
        scope = scope)

    if (!is.null(token$error)) {
        token$error
    } else {
      token$result
    }
}