#' @title Create a valenece and energy plot based on an artist's albums
#' @param query - A single artist name.  Defaults to NULL.  Only put this or id
#' @param artist_id - A single Spotify artist id.  Defaults to NULL.  Only put this or query
#' @param limit - Number of albums wanted to return.  Valid if between 1 and 50.  Defaults to 20
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A valence and energy plot taking in a set number of artist's albums
#' @examples 
#' \dontrun{
#'  create_artist_album_valence_energy_visual(artist_id = "0du5cEVh5yTK9QJze8zA0C")
#'  create_artist_album_valence_energy_visual(artist_id = "0du5cEVh5yTK9QJze8zA0C", limit = 5)
#'  create_artist_album_valence_energy_visual(query = "Bruno Mars")
#' }
#' @export
create_artist_album_valence_energy_visual <- function(query = NULL, artist_id = NULL, limit = 5, authorization = get_spotify_access_token()) {
    if (limit > 5){
    stop("Please input only 5 or less albums!")
    }
    album_info <- get_artist_albums(query = query, id = artist_id, limit = limit, authorization = authorization)  
    album_ids <- album_info$album_id
    album_names <- album_info$album_name
    audio_features <- purrr::map(album_ids, ~get_album_summary(id = .x))

    min_length <- min(length(album_ids), length(album_names), length(audio_features))
    artist_name <- rep(get_artists(queries = query, ids = artist_id, authorization = authorization)$artist_name, each = min_length)
    
    valence_means <- purrr::map_dbl(audio_features, ~ purrr::pluck(.x, "valence_mean"))
    energy_means <- purrr::map_dbl(audio_features, ~ purrr::pluck(.x, "energy_mean"))

    artist_data <- data.frame(
        album_id = album_ids[1:min_length],
        album_name = album_names[1:min_length],
        artist_name = artist_name[1:min_length],
        valence_mean = valence_means,
        energy_mean = energy_means,
        stringsAsFactors = FALSE
    )
  colors = c("#FF3131", "green", "#7a05ff", "#f2ff00", "#fffefe")

  if (length(artist_data$album_name) < length(colors)) {
  colors <- colors[1:length(artist_data$album_name)]
  }

  # Associate colors with album names
  artist_colors <- setNames(colors, artist_data$album_name)

  df <- data.frame(x = c(0, 1), y = c(0, 1))
  alpha_score <- 0.2

  p1 <- df %>%
    ggplot2::ggplot(aes(x = x, y = y)) +
    ggplot2::geom_blank() +
    ggplot2::geom_vline(xintercept = 0.5, linewidth = 1) +
    ggplot2::geom_hline(yintercept = 0.5, linewidth = 1) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0),
                       labels = scales::label_number(accuracy = 0.1)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0))  +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 18, face = "bold"),
      axis.text = ggplot2::element_text(size = 12, face = "bold"),
      plot.margin = ggplot2::margin(0.3, 0.5, 0.1, 0.5, "cm")
    ) +
    ggplot2::geom_rect(aes(xmin = 0, xmax = 0.5, ymin = 0.5, ymax = 1),
              alpha = alpha_score, fill = "red1") +
    ggplot2::geom_rect(aes(xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1),
              alpha = alpha_score, fill = "violetred1") +
    ggplot2::geom_rect(aes(xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5),
              alpha = alpha_score, fill = "slategray3") +
    ggplot2::geom_rect(aes(xmin = 0.5, xmax = 1, ymin = 0, ymax = 0.5),
              alpha = alpha_score, fill = "royalblue2") +
    ggplot2::labs(x = "Valence", y = "Energy") +
    # Adjust the font size for annotations
    ggplot2::annotate("text", x = 0.25, y = 0.25, label = "Sad", size = 5,
             color = "white") +
    ggplot2::annotate("text", x = 0.75, y = 0.25, label = "Chill", size = 5,
             color = "white") +
    ggplot2::annotate("text", x = 0.25, y = 0.75, label = "Angry", size = 5,
             color = "white") +
    ggplot2::annotate("text", x = 0.75, y = 0.75, label = "Happy", size = 5,
             color = "white")

  combined_data <- dplyr::bind_rows(artist_data)

  artist_name <- get_artists(queries = query, ids = artist_id, authorization = authorization)$artist_name
  
  p2 <- plotly::ggplotly(p1 + 
    ggplot2::geom_point(data = combined_data,  
               aes(x = valence_mean, y = energy_mean, color = album_name), size = 3) +
    ggplot2::labs(color = "Album Names",
         x = "Valence", y = "Energy") +
    ggplot2::scale_color_manual(values = artist_colors, labels = (combined_data$album_name)) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 15, face = "bold"),  # Adjust the size here
      axis.text = ggplot2::element_text(size = 7, face = "bold"),
      plot.title = ggplot2::element_text(size = 15, face = "bold", hjust = 0.5, margin = margin(b = 20)),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 13, face = "bold"),
      legend.text = ggplot2::element_text(size = 20),
      legend.position = "bottom",
      legend.direction = "vertical",
      plot.margin = ggplot2::margin(t = 50)
    ) +
    ggplot2::ggtitle(paste("Album Valence-Energy Visualization\nfor", artist_name)) +
    ggplot2::theme(plot.title.position = "plot"))
  
  p2
}