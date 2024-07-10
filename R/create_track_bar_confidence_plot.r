#' @title Create a line chart of track bar confidence
#' @param query - A single of track name.  Deafualts to NULL.  Only use this or id
#' @param id - A single Spotify track id.  Deafualts to NULL.  Only use this or query
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A line chart displaying track bar confidence over the duration of the song
#' @examples 
#' \dontrun{
#'  create_track_bar_confidence_plot(id = "6YbhspuOar1D9WSSnfe7ds")
#'  create_track_bar_confidence_plot(query = "Natalie")
#' }
#' @export
create_track_bar_confidence_plot <- function(query = NULL, id = NULL, authorization = get_spotify_access_token()) {
    # Get track bars data
    track_bars <- get_track_bars(query, id, authorization)
    track_bars <- track_bars %>% dplyr::mutate(end = start + duration)
    
    # Create the plot
    ggplot2::ggplot(track_bars, aes(x = start, y = confidence)) +
        ggplot2::geom_smooth(color = "#28c4e3", size = 1) +
        ggplot2::labs(title = paste("Time Analysis of Bars for", track_bars$track_name[1]),
             x = "Time (seconds)",
             y = "Confidence") +
        ggplot2::theme_minimal()
}
