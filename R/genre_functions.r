#' @title Search for Spotify possible genres
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe containing all possible genre inputs for other genre functions
#' @examples
#' \dontrun{
#' get_possible_genres()
#' }
#' @export
get_possible_genres <- function(authorization = get_spotify_access_token()){
    url <- "https://api.spotify.com/v1/recommendations/available-genre-seeds"
    parameters <- list(
        access_token = authorization
    )
    result = httr::RETRY(verb = "GET", url, query = parameters, encode = "json", terminate_on = c(401, 403, 404))
    httr::stop_for_status(result)
    result <- jsonlite::fromJSON(httr::content(result, as = "text", encoding = "UTF-8"), flatten = TRUE)
    result <- as.data.frame(result)
    result
}

#' @title Search for Spotify tracks of a specific genre
#' @param genre - A string of a genre
#' @param limit - Optional.  Number of albums wanted to return.  Valid if between 1 and 50.  Defaults to 20
#' @param offset - Optional.  Index of first album wanted.  Defaults to 0
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of track data, including duration, explicitness, track name and id, track popularity, whether the track was a single or on an album, and artist name and id
#' @examples
#' \dontrun{
#' get_genre_tracks("hip-hop")
#' get_genre_tracks("rap", limit = 50, offset = 2)
#' }
#' @export
get_genre_tracks <- function(genre, limit = 20, offset = 0, authorization = get_spotify_access_token()){
    url <- "https://api.spotify.com/v1/search"
    parameters <- list(
        q = stringr::str_glue('genre:"{genre}"'),
        type = "track",
        market = "US",
        limit = limit,
        offset = offset,
        access_token = authorization
    )

    result <- httr::RETRY("GET", url, query = parameters, encode = "json", terminate_on = c(401, 403, 404))

    httr::stop_for_status(result)

    result <- jsonlite::fromJSON(httr::content(result, as = 'text', encoding = 'UTF-8'),
                    flatten = TRUE)$tracks$items

    result$artists <- purrr::map(result$artists, ~ list(id = .x$id, name = .x$name))
    result <- result %>% 
            dplyr::mutate(
                artist_id = purrr::map_chr(artists, ~ toString(.x$id)),
                artist_name = purrr::map_chr(artists, ~ toString(.x$name))
                ) %>%
            dplyr::select(
                -artists,
                -href,
                -is_local,
                -is_playable,
                -preview_url,
                -disc_number,
                -uri,
                -album.artists,
                -album.href,
                -album.id,
                -album.images,
                -album.is_playable,
                -album.name,
                -album.release_date,
                -album.release_date_precision,
                -album.total_tracks,
                -album.type,
                -album.uri,
                -album.external_urls.spotify,
                -external_ids.isrc,
                -external_urls.spotify
            ) %>%
            dplyr::rename(
                track_id = id,
                track_name = name,
                album_type = album.album_type
            )
    result
}

#' @title Search for Spotify artists of a specific genre
#' @param genre - A string of a genre
#' @param limit - Optional.  Number of albums wanted to return.  Valid if between 1 and 50.  Defaults to 20
#' @param offset - Optional.  Index of first album wanted.  Defaults to 0
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of artist data, including the artist's genre, artist name and id, artist popularity, and the total number of followers
#' @examples
#' \dontrun{
#' get_genre_artists("hip-hop")
#' get_genre_artists("rap", limit = 50, offset = 2)
#' }
#' @export
get_genre_artists <- function(genre, limit = 20, offset = 0, authorization = get_spotify_access_token()){
    url <- "https://api.spotify.com/v1/search"
    parameters <- list(
        q = stringr::str_glue('genre:"{genre}"'),
        type = "artist",
        market = "US",
        limit = limit,
        offset = offset,
        access_token = authorization
    )

    result <- httr::RETRY("GET", url, query = parameters, encode = "json", terminate_on = c(401, 403, 404))

    httr::stop_for_status(result)

    result <- jsonlite::fromJSON(httr::content(result, as = 'text', encoding = 'UTF-8'),
                    flatten = TRUE)$artists$items

    result <- result %>%
              dplyr::select(
                -href,
                -images,
                -uri,
                -external_urls.spotify,
                -followers.href
              ) %>% 
              dplyr::rename(
                artist_id = id,
                artist_name = name,
                followers_total = followers.total
              )
    result
}

#' @title Search for Spotify feature summary of tracks from a specific genre
#' @param genre - A string of a genre
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of genre track summary, including the inputted genre and the means and standard deviations for the following variables:
#'        danceability
#'        energy
#'        loudness
#'        speechiness
#'        acousticness
#'        instrumentalness
#'        liveness
#'        valence
#'        tempo
#'        duration_ms
#'        mode
#' @examples
#' \dontrun{
#' get_genre_summary("hip-hop")
#' }
#' @export
get_genre_summary <- function(genre, authorization = get_spotify_access_token()){
    tracks <- get_genre_tracks(genre, authorization = authorization)

    features <- get_track_audio_features(ids = tracks$track_id, authorization = authorization)

    summary <- features %>%
               dplyr::select(danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, tempo, duration_ms, mode) %>%
               dplyr::summarize(dplyr::across(dplyr::everything(), list(mean = mean, sd = sd), na.rm = TRUE))

    genre <- data.frame(genre = genre)

    result <- dplyr::bind_cols(genre, summary)

    result
}

#' @title Search for Spotify track features of a specific genre
#' @param genre - A string of a genre
#' @param limit - Optional.  Number of tracks wanted to return.  Valid if between 1 and 50.  Defaults to 20
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of track feature data, including explicitness, track name and id, track popularity, track number in its album, artist name and id, and each of the following variables:
#'        danceability
#'        energy
#'        key
#'        loudness
#'        mode
#'        speechiness
#'        acousticness
#'        instrumentalness
#'        liveness
#'        valence
#'        tempo
#'        duration_ms
#'        time_signature
#' @examples
#' \dontrun{
#' get_genre_track_features("hip-hop")
#' }
#' @export
get_genre_track_features <- function(genre, limit = 20, authorization = get_spotify_access_token()){
    tracks <- get_genre_tracks(genre, limit = limit, authorization = authorization)

    features <- get_track_audio_features(ids = tracks$track_id, authorization = authorization)

    result <- dplyr::left_join(tracks, features, by = "track_id") %>%
              dplyr::select(
                -duration_ms.x
              ) %>%
              dplyr::rename(
                duration_ms = duration_ms.y
                )

    result
}