#' @title Create a valenece and energy plot based on genre data
#' @param genres - A vector of Spotify genres
#' @param sample_sizes - Number of tracks in each genre to gather data from.  Valid if between 1 and 50.  Defaults to 20
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A valence and energy plot taking in a set number of genres
#' @examples 
#' \dontrun{
#'  create_genre_valence_energy_visual(c("hip-hop", "rap", "jazz"))
#'  create_genre_valence_energy_visual("rock", sample_sizes = 22)
#' }
#' @export
create_genre_valence_energy_visual <- function(genres, sample_sizes = 20, authorization = get_spotify_access_token()) {
  genre_data <- purrr::map2(genres, sample_sizes, ~ {
    genre_info <- get_genre_tracks(.x, limit = .y, authorization = authorization)  
    track_ids <- genre_info$track_id  
    track_names <- genre_info$track_name
    artist_name <- genre_info$artist_name
    audio_features <- purrr::map(track_ids, ~ get_track_audio_features(ids = .x))
    
    min_length <- min(length(track_ids), length(track_names), length(audio_features))
    genre_name <- rep(.x, each = min_length) 

    valence_means <- purrr::map_dbl(audio_features, ~ pluck(.x, "valence"))
    energy_means <- purrr::map_dbl(audio_features, ~ pluck(.x, "energy"))
    
    data.frame(
      track_ids = track_ids[1:min_length],
      genre_name = genre_name[1:min_length],
      artist_name = artist_name[1:min_length],
      track_names = track_names[1:min_length],
      Valence = valence_means,
      Energy = energy_means,
      stringsAsFactors = FALSE
    )
  })
  genre_colors <- setNames(c("red", "blue", "green", "grey", "black"), (genre_data$genre_name))

  combined_data <- dplyr::bind_rows(genre_data)
  
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

  p2 <- p1 + 
  ggplot2::geom_point(data = combined_data,  
             ggplot2::aes(x = Valence, y = Energy,
                 text = paste("Artist:", artist_name,
                              "\nTrack:", track_names),
                 color = genre_name),  
             size = 3) +
  ggplot2::scale_color_manual(values = genre_colors, 
                     labels = unique(combined_data$genre_name),  
                     name = "Genre") +  
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, size = 17, face = "bold"),
    plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 13, face = "bold"),
    legend.text = ggplot2::element_text(size = 20),
    legend.position = "bottom",  
    legend.direction = "horizontal",
    plot.margin = ggplot2::margin(t = 40)
  ) + 
  ggplot2::ggtitle(paste("Genre Valence-Energy Visualization for", paste(str_to_title(genres), collapse = ", ")))

p2_interactive <- plotly::ggplotly(p2)

p2_interactive
}