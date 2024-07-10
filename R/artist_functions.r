#' @title Search for Spotify artist information
#' @param queries - A String vector of artist names, taken from the search_spotify() function.  Defaults to NULL.  Only use this or ids
#' @param ids - A vector of Spotify artist ids.  Defaults to NULL.  Only use this or queries
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of artist data, including artist name and id, genres of the artist, artist popularity, and the total number of followers on spotify
#' @examples
#' \dontrun{
#' get_artists(ids = "0du5cEVh5yTK9QJze8zA0C")
#' get_artists(ids = c("0du5cEVh5yTK9QJze8zA0C", "6PvvGcCY2XtUcSRld1Wilr"))
#' get_artists(queries = c("Bruno Mars", "Anderson .Paak"))
#' }
#' @export
get_artists <- function(queries = NULL, ids = NULL, authorization = get_spotify_access_token()){
    if(!is.null(queries)) {
        search <- search_spotify(queries, "artist", authorization = authorization)
        ids <- as.vector(search$id)
    }
    url <- "https://api.spotify.com/v1/artists"
    parameters <- list(
    access_token = authorization
  )
  if (length(ids) > 1) {
    # For multiple IDs
    param <- list(
        ids = paste(ids, collapse = ",")
  )
    parameters <- c(parameters, param)
  } else {
    # For single ID
    url <- stringr::str_glue("{url}/{ids}")
  }
    result <- httr::RETRY(verb = "GET", url, query = parameters, encode = "json", terminate_on = c(401, 403, 404))
    httr::stop_for_status(result)
    result <- jsonlite::fromJSON(httr::content(result, as = "text", encoding = "UTF-8"), flatten = TRUE)
    if (length(ids) > 1) {
    # For multiple IDs
        result <- result$artists
        result <- result %>%
        dplyr::select(
            -images,
            -href,
            -external_urls.spotify,
            -uri,
            -followers.href
        ) %>%
        dplyr::rename(
            artist_id = id,
            artist_name = name,
            followers_total = followers.total
        )
  } else {
    # For single ID
    df <- data.frame(
      genres = paste(result$genres, collapse = ", "),
      artist_id = result$id,
      artist_name = result$name,
      popularity = result$popularity,
      type = result$type,
      followers_total = result$followers$total
    )
    result <- df
  }
  result

}

#' @title Search for Spotify artist audio feature information
#' @param query - A single artist name.  Defaults to NULL.  Only put this or id
#' @param id - A single Spotify artist id.  Defaults to NULL.  Only put this or query
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of artist track data, including the artist name and id, project id and name, release_date, track name and id, key and mode, and each of the following variables:
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
#' @examples
#' \dontrun{
#' get_artist_audio_features(id = "0du5cEVh5yTK9QJze8zA0C")
#' get_artist_audio_features(query = "Bruno Mars")
#' }
#' @export
get_artist_audio_features <- function(query = NULL, id = NULL, authorization = get_spotify_access_token()){
    info <- get_artists(queries = query, ids = id, authorization = authorization)
    artist_id <- info$artist_id
    artist_name <- info$name
    if(is.null(artist_id)){
        stop("No artist found with inputed ID.  Please try again with a different ID.")
    }
    albums <- get_artist_projects(id = artist_id, authorization = authorization)
    if (is.null(albums) || length(albums)==0) {
        stop("No albums found with inputed ID.  Please try again with a different ID.")
    }
    num_loops <- ceiling(nrow(albums) / 50)
    if (num_loops > 1) {
        albums <- purrr::map_df(1:num_loops, function(this_loop) {
            get_artist_projects(id = artist_id,
                              offset = (this_loop - 1) * 50,
                              authorization = authorization)
        })
    }
    albums <- albums %>%
        dplyr::mutate(
          album_release_year = dplyr::case_when(
              release_date_precision == 'year' ~ suppressWarnings(as.numeric(release_date)),
              release_date_precision == 'day' ~ lubridate::year(
                            as.Date(release_date, '%Y-%m-%d',
                                    origin = '1970-01-01')),
              TRUE ~ as.numeric(NA))
        )
        suppressWarnings({
        tracks <- purrr::map_df(albums$album_id, function(album_id) {
            tracks <- get_albums_tracks(ids = album_id,
                                            authorization = authorization)
            num_loops <- ceiling(nrow(tracks) / 20)
                if (num_loops > 1) {
                    tracks <- purrr::map_df(1:num_loops, function(this_loop) {
                        get_albums_tracks(ids = album_id,
                                        offset = (this_loop - 1) * 20,
                                        authorization = authorization)
        })} 
        tracks <- tracks %>%
                dplyr::mutate(
                album_id = album_id,
                album_name = paste(albums$album_name[albums$album_id == album_id], collapse = ", ")
                ) %>%
                dplyr::distinct(track_name, .keep_all = TRUE)
        })
    })
    num_loops_tracks <- ceiling(nrow(tracks) / 100)

    track_audio_features <- purrr::map_df(1:num_loops_tracks, function(this_loop) {
        track_ids <- tracks %>%
            dplyr::slice(((this_loop * 100) - 99):(this_loop * 100)) %>%
            dplyr::pull(track_id)
        get_track_audio_features(ids = track_ids, authorization = authorization)
    }) %>%
        dplyr::left_join(tracks, by = 'track_id') %>%
        dplyr::select(-album_name)

    albums %>%
        dplyr::mutate(
            artist_name = artist_name,
            artist_id = artist_id
    ) %>%
        dplyr::select(
            artist_name,
            artist_id,
            album_id,
            album_release_date = release_date,
            album_release_year,
            album_release_date_precision = release_date_precision,
            album_name
    ) %>%
        dplyr::left_join(track_audio_features, by = 'album_id') %>%
        dplyr::mutate(key_name = pitch_class_lookup[key + 1],
            mode_name = dplyr::case_when(mode == 1 ~ 'major',
                                    mode == 0 ~ 'minor',
                                    TRUE ~ as.character(NA)),
            key_mode = paste(key_name, mode_name)) %>%
        dplyr::select(-artist_name.y,
                      -artist_id.y,
                      -duration_ms.y) %>%
        dplyr::rename(artist_name = artist_name.x,
                      artist_id = artist_id.x,
                      duration_ms = duration_ms.x)
}

#' @title Search for Spotify artist feature summary
#' @param query - A single artist name.  Defaults to NULL.  Only use this or id
#' @param id - A single Spotify artist id.  Defaults to NULL.  Only use this or id
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of artist summary data, including artist name and id, number of songs from the artist, and the mean and standard deviation for:
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
#' get_artist_summary(id = "0du5cEVh5yTK9QJze8zA0C")
#' get_artist_summary(query = "Bruno Mars")
#' }
#' @export
get_artist_summary <- function(query = NULL, id = NULL, authorization = get_spotify_access_token()){
    artist <- get_artists(queries = query, ids = id, authorization = authorization) %>% 
              dplyr::select(artist_name, artist_id)

    features <- get_artist_audio_features(query = query, id = id, authorization = authorization)

    songs <- features %>%
             dplyr::mutate(num_songs = dplyr::n()) %>%
             dplyr::select(artist_name, num_songs)
    
    result <- dplyr::left_join(artist, songs, by = "artist_name") %>%
              dplyr::slice(1)
    
    summary <- features %>%
               dplyr::select(danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, explicit, tempo, duration_ms, mode) %>%
               dplyr::summarize(dplyr::across(dplyr::everything(), list(mean = mean, sd = sd), na.rm = TRUE))

    result <- dplyr::bind_cols(result, summary)
    result
}

#' @title Search for Spotify artists feature summary
#' @param queries - A String vector of artist names, taken from the search_spotify() function.  Defaults to NULL.  Only use this or ids
#' @param ids - A vector of Spotify artist ids.  Defaults to NULL.  Only use this or queries
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of artist summary data, including artist name and id, number of songs from the artist, and the mean and standard deviation for:
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
#' get_artists_summary(ids = c("0du5cEVh5yTK9QJze8zA0C", "6PvvGcCY2XtUcSRld1Wilr"))
#' get_artists_summary(queries = c("Bruno Mars", "Anderson .Paak"))
#' }
#' @export
get_artists_summary <- function(queries = NULL, ids = NULL, authorization = get_spotify_access_token()) {
    if(!is.null(queries)){
        purrr::map_dfr(queries, ~ get_artist_summary(query = .x, id = ids, authorization = authorization))
    } else{
        purrr::map_dfr(ids, ~ get_artist_summary(query = queries, id = .x, authorization = authorization))
    }
}

#' @title Search for Spotify related artists
#' @param query - A single artist name.  Defaults to NULL.  Only use this or id
#' @param id - A single Spotify artist id.  Defaults to NULL.  Only use this or id
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of related artist data, including the artist genres, artist id and name, popularity, and number of followers
#' @examples
#' \dontrun{
#' get_related_artists(id = "0du5cEVh5yTK9QJze8zA0C")
#' get_related_artists(query = "Bruno Mars")
#' }
#' @export
get_related_artists <- function(query = NULL, id = NULL, authorization = get_spotify_access_token()){
    if(!is.null(query)) {
        search <- search_spotify(query, "artist", authorization = authorization)
        id <- as.vector(search$id)
    }
    url <- stringr::str_glue("https://api.spotify.com/v1/artists/{id}/related-artists")

    parameters <- list(
        access_token = authorization
    )
    result <- httr::RETRY(verb = 'GET', url, query = parameters, encode = 'json', terminate_on = c(401, 403, 404))
    httr::stop_for_status(result)

    result <- jsonlite::fromJSON(httr::content(result, as = 'text', encoding = 'UTF-8'), flatten = TRUE)$artists %>%
              dplyr::select(-href,
                            -images,
                            -uri,
                            -external_urls.spotify,
                            -followers.href) %>%
              dplyr::rename(
                artist_id = id,
                artist_name = name
              )

    result
}
