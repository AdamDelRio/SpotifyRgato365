#' @title Create a radar chart of artist features
#' @param queries - A vector of artist names.  Deafualts to NULL.  Only use this or ids
#' @param artists - A vector of Spotify artist ids.  Deafualts to NULL.  Only use this or queries
#' @param vars - A vector of variables returned from get_artist_summary()
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A radar chart displaying valence, energy, and speechiness, along with any other inputed variables
#' @examples 
#' \dontrun{
#'  create_average_artists_radar_chart(artists = c("5me0Irg2ANcsgc93uaYrpb", "7hJcb9fa4alzcOq3EaNPoG", "1ZwdS5xdxEREPySFridCfh", "7B4hKK0S9QYnaoqa9OuwgX", "1P8IfcNKwrkQP5xJWuhaOC"), vars = c("acousticness", "danceability"))
#'  create_average_artists_radar_chart(queries = c("Liam Payne", "ZAYN", "Harry Styles", "Niall Horan", "Louis Tomlinson"), vars = c("acousticness", "danceability"))
#' }
#' @export
create_average_artists_radar_chart <- function(queries = NULL, artists = NULL, vars = c(), authorization = get_spotify_access_token()){
  if (length(artists) > 5 || length(queries) > 5){
    stop("Please input only 5 or less artists!")
  }
  colors <- c("#6B8E23", "#89A8E0", "#A291B5", "#BCCC9A", "#D3D3D3")
  if(!is.null(queries)){
    color_palette <- colors[1:length(queries)]
  } else{
    color_palette <- colors[1:length(artists)]
  }
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

  if(!is.null(queries)){
    artist_summaries <- purrr::map(queries, ~ get_artist_summary(query = .x, authorization = authorization))
  } else{
    artist_summaries <- purrr::map(artists, ~ get_artist_summary(id = .x, authorization = authorization))
  }

  final_summary_df <- dplyr::bind_rows(artist_summaries)

  rownames(final_summary_df) <- artists
  
  final_summary_df <- final_summary_df %>%
                      dplyr::select(
                        valence_mean,
                        energy_mean,
                        speechiness_mean,
                        vars
                      )
                      
  final_summary_df <- rbind(min_max, final_summary_df)  
  
  op <- par(mar = c(0, 0, 0, 0))
  
  create_beautiful_radarchart(
    data = final_summary_df, caxislabels = c(0, 0.25, 0.50, 0.75, 1),
    color = color_palette,
    vlcex = 1.5
  )
  
  artists <- get_artists(queries = queries, ids = artists) %>% 
    as.data.frame() %>% 
    dplyr::select(
        dplyr::starts_with("artist_name")
    )
  artists <- tidyr::pivot_longer(artists, cols = dplyr::starts_with("artist_name"), 
                          names_to = "track_names", values_to = "names")
  legend(
    x = "bottom", legend = artists$names, horiz = TRUE,
    bty = "n", pch = 20 , col = color_palette,
    text.col = "black", cex = 1.75
    )

  par(op)
}