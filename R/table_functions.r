#' @title Search for Spotify album track loudness and tempo
#' @param query - A single of album name, taken from the search_spotify() function.  Defaults to NULL.  Only use this or id
#' @param id - A single Spotify album id.  Defaults to NULL.  Only use this or query
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of album track data, including album name, loudness, instrumentalness, and tempo
#' @examples
#' \dontrun{
#' get_album_track_loudness_instrumentalness_tempo(id = "1uyf3l2d4XYwiEqAb7t7fX")
#' get_album_track_loudness_instrumentalness_tempo(query = "An Evening With Silk Sonic")
#' }
#' @export
get_album_track_loudness_instrumentalness_tempo <- function(query = NULL, id = NULL, authorization = get_spotify_access_token()){
    features <- get_album_track_features(query, id, authorization)
    features <- features %>% 
                dplyr::select(album_name,
                loudness, 
                instrumentalness, 
                tempo)
    features
}

#' @title Search for Spotify artists danceability summary
#' @param queries - A String vector of artist names, taken from the search_spotify() function.  Defaults to NULL.  Only use this or ids
#' @param ids - A vector of Spotify artist ids.  Defaults to NULL.  Only use this or queries
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of artist summary data, including artist id, and the mean and standard deviation for danceability
#' @examples
#' \dontrun{
#' get_artists_danceability(ids = c("0du5cEVh5yTK9QJze8zA0C", "6PvvGcCY2XtUcSRld1Wilr"))
#' get_artists_danceability(queries = c("Bruno Mars", "Anderson .Paak"))
#' }
#' @export
get_artists_danceability <- function(queries = NULL, ids = NULL, authorization = get_spotify_access_token()){
    artists <- get_artists_summary(queries, ids, authorization)
    artists <- artists %>%
               dplyr::select(artist_id,
               danceability_mean,
               danceability_sd)
    artists
}

#' @title Search for Spotify speechiness and duration of tracks from a specific genre
#' @param genre - A string of a genre
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of genre track summary, including the inputted genre, mean tempo, mean speechiness, and mean duration in ms
#' @examples
#' \dontrun{
#' get_genre_key_speechiness_duration("hip-hop")
#' }
#' @export
get_genre_tempo_speechiness_duration <- function(genre = NULL, authorization = get_spotify_access_token()){
    features <- get_genre_summary(genre, authorization)
    features <- features %>%
                dplyr::select(genre,
                tempo_mean,
                speechiness_mean,
                duration_ms_mean)
    features
}

#' @title Search for Spotify album track duration and time signature
#' @param query - A single of album name, taken from the search_spotify() function.  Defaults to NULL.  Only use this or id
#' @param id - A single Spotify album id.  Defaults to NULL.  Only use this or query
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of album track data, including track id, track name, artist id, duration in ms, and time signature
#' @examples
#' \dontrun{
#' get_album_track_duration_signature(id = "1uyf3l2d4XYwiEqAb7t7fX")
#' get_album_track_duration_signature(query = "An Evening With Silk Sonic")
#' }
#' @export
get_album_track_duration_signature <- function(query = NULL, id = NULL, authorization = get_spotify_access_token()){
    features <- get_album_track_features(query, id, authorization)
    features <- features %>%
                dplyr::select(track_id,
                track_name,
                artist_id,
                duration_ms,
                time_signature)
    features
}

#' @title Search for Spotify artists energy summary and number of songs
#' @param queries - A String vector of artist names, taken from the search_spotify() function.  Defaults to NULL.  Only use this or ids
#' @param ids - A vector of Spotify artist ids.  Defaults to NULL.  Only use this or queries
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of artist summary data, including artist name, number of songs, and mean energy
#' @examples
#' \dontrun{
#' get_artists_energy_num_songs(ids = c("0du5cEVh5yTK9QJze8zA0C", "6PvvGcCY2XtUcSRld1Wilr"))
#' get_artists_energy_num_songs(queries = c("Bruno Mars", "Anderson .Paak"))
#' }
#' @export
get_artists_energy_num_songs <- function(queries = NULL, ids = NULL, authorization = get_spotify_access_token()){
    artists <- get_artists_summary(queries, ids, authorization)
    artists <- artists %>%
               dplyr::select(artist_name,
               num_songs,
               energy_mean)
    artists
}