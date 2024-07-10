#' @title Create a histogram of artist's track popularity
#' @param query - A single artist name.  Deafualts to NULL.  Only use this or id
#' @param id - A single Spotify artist id.  Deafualts to NULL.  Only use this or query
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A histogram displaying each tracks' popularity from an artist
#' @examples 
#' \dontrun{
#'  create_artist_track_popularity_histogram(id = "2YZyLoL8N0Wb9xBt1NhZWg")
#'  create_artist_track_popularity_histogram(query = "Bruno Mars")
#' }
#' @export
create_artist_track_popularity_histogram <- function(query = NULL, id = NULL, authorization = get_spotify_access_token()) {
    # Retrieve artist information
    info <- get_artists(queries = query, ids = id, authorization = authorization)
    artist_id <- info$artist_id
    artist_name <- info$artist_name
    
    if (is.null(artist_id)) {
        stop("No artist found with the inputted ID. Please try again with a different ID.")
    }
    
    # Retrieve artist albums
    albums <- get_artist_projects(id = artist_id, authorization = authorization)
    if (is.null(albums) || length(albums) == 0) {
        stop("No albums found for the given artist. Please try again with a different ID.")
    }
    
    # Handle pagination for albums
    num_loops <- ceiling(nrow(albums) / 50)
    if (num_loops > 1) {
        albums <- purrr::map_df(1:num_loops, function(this_loop) {
            get_artist_projects(id = artist_id, offset = (this_loop - 1) * 50, authorization = authorization)
        })
    }
    
    # Extract and process album release years
    albums <- albums %>%
        dplyr::mutate(
            album_release_year = dplyr::case_when(
                release_date_precision == 'year' ~ suppressWarnings(as.numeric(release_date)),
                release_date_precision == 'day' ~ lubridate::year(as.Date(release_date, '%Y-%m-%d', origin = '1970-01-01')),
                TRUE ~ as.numeric(NA)
            )
        )
    
    # Retrieve all tracks from albums
    suppressWarnings({
        tracks <- purrr::map_df(albums$album_id, function(album_id) {
            tracks <- get_albums_tracks(ids = album_id, authorization = authorization)
            num_loops <- ceiling(nrow(tracks) / 20)
            if (num_loops > 1) {
                tracks <- purrr::map_df(1:num_loops, function(this_loop) {
                    get_albums_tracks(ids = album_id, offset = (this_loop - 1) * 20, authorization = authorization)
                })
            }
            tracks <- tracks %>%
                dplyr::mutate(
                    album_id = album_id,
                    album_name = paste(albums$album_name[albums$album_id == album_id], collapse = ", ")
                ) %>%
                dplyr::distinct(track_name, .keep_all = TRUE)
        })
    })
    
    # Extract popularity scores
    track_popularity <- purrr::map_df(tracks$track_id, function(track_id) {
        track_info <- get_tracks(ids = track_id, authorization = authorization)
        dplyr::tibble(track_id = track_id, popularity = track_info$popularity)
    })
    
    # Create histogram
    ggplot2::ggplot(track_popularity, ggplot2::aes(x = popularity)) +
        ggplot2::geom_histogram(binwidth = 5, fill = "#28c4e3", color = "black") +
        ggplot2::labs(title = paste("Popularity Distribution for", artist_name),
             x = "Popularity",
             y = "Frequency") +
        ggplot2::theme_minimal()
}
