#' @title Create a radar chart of genre features
#' @param genres - A vector of Spotify genres
#' @param vars - A vector of variables returned from get_genre_summary()
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A radar chart displaying valence, energy, and speechiness, along with any other inputed variables
#' @examples 
#' \dontrun{
#'  create_genre_radar_chart(genres = c("hip-hop", "classical", "rock"), vars = c("instrumentalness", "acousticness"))
#' }
#' @export
create_genre_radar_chart <- function(genres, vars = c(), authorization = get_spotify_access_token()){
  if (length(genres) > 5){
    stop("Please input only 5 or less genres!")
  }
  colors <- c("#6B8E23", "#89A8E0", "#A291B5", "#BCCC9A", "#D3D3D3")
  color_palette <- colors[1:length(genres)]
  create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
    fmsb::radarchart(
      data, axistype = 1,
      # Customize the polygon
      pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
      # Customize the grid
      cglcol = "grey", cglty = 1, cglwd = 0.8,
      # Customize the axis
      axislabcol = "grey", 
      # Variable labels
      vlcex = vlcex, vlabels = vlabels,
      caxislabels = caxislabels, title = title, ...
    )
  }

  min_max <- data.frame(
    row.names = c("max", "min"),
    valence_mean = c(1, 0),
    energy_mean = c(1, 0),
    speechiness_mean = c(1, 0)
  )

  if (length(vars) > 0){
    vars <- paste0(vars, "_mean")
    create_data_frame <- function(var) {
      data.frame(
        var = c(0, 1)  
      )
    }

    combinations <- purrr::map_dfc(vars, create_data_frame) %>% 
                    dplyr::rename_with(~ vars, dplyr::everything())

    min_max <- cbind(min_max, combinations)
  }

  genre_summaries <- purrr::map(genres, ~ get_genre_summary(.x, authorization = authorization))

  final_summary_df <- dplyr::bind_rows(genre_summaries)

  rownames(final_summary_df) <- genres

  final_summary_df <- final_summary_df %>%
                      dplyr::select(
                        valence_mean,
                        energy_mean,
                        speechiness_mean,
                        vars
                      )
                      
  final_summary_df <- rbind(min_max, final_summary_df)  
  
  op <- par(mar = c(1, 0, 0, 0))
  
  create_beautiful_radarchart(
    data = final_summary_df, caxislabels = c(0, 0.25, 0.50, 0.75, 1),
    color = color_palette,
    vlcex = 1
  )
  
  legend(
    x = "bottom", legend = rownames(final_summary_df[-c(1,2),]), horiz = TRUE,
    bty = "n", pch = 20 , col = color_palette,
    text.col = "black", cex = 1.75, pt.cex = 2
    )

  par(op)
}
