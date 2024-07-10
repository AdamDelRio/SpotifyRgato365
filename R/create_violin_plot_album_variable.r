#' @title Create a violin plot based on an artist's albums
#' @param query - A single artist name.  Defaults to NULL.  Only put this or id
#' @param artist_id - A single Spotify artist id.  Defaults to NULL.  Only put this or query
#' @param limit - Number of albums wanted to return.  Valid if between 1 and 50.  Defaults to 7
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @param var - A single variable returned from the get_artist_audio_features() function.  Defaults to "danceability"
#' @return A violin plot displaying a specific number of an artist's albums' danceability
#' @examples 
#' \dontrun{
#'      create_violin_plot_album_variable(query = "Bruno Mars", var = "energy")
#'      create_violin_plot_album_variable(artist_id = "0du5cEVh5yTK9QJze8zA0C", limit = 5)
#' }
#' @export
create_violin_plot_album_variable <- function(query = NULL, artist_id = NULL, limit = 7, var = "danceability", authorization = get_spotify_access_token()) {
    df_artist <- get_artist_audio_features(query = query, id = artist_id, authorization = authorization)
    
    album_names <- unique(df_artist$album_name)[1:limit]
    
    # only keep rows of data frame where album name is one of the names in album_names
    df_artist <- df_artist[df_artist$album_name %in% album_names, ]

    # dynamically set the plot title
    plot_title <- sprintf("%s of %s's Albums", stringr::str_to_title(var), df_artist$artist_name[1])
    
    if (length(album_names) <= 5) {
      if (any(nchar(unique(df_artist$album_name)) > 10)) {
        gg <- ggplot2::ggplot(df_artist, ggplot2::aes_string(x = "album_name", y = var)) + 
                ggplot2::geom_violin(ggplot2::aes_string(fill = "album_name")) + 
                ggplot2::labs(title = plot_title, x = "", y = var, fill = "Album") + 
                ggplot2::theme_bw() +
                ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), 
                      axis.text.x = ggplot2::element_blank(), 
                      axis.ticks.x = ggplot2::element_blank(),
                      legend.text = ggplot2::element_text(size = 20)
                      ) + 
                ggplot2::stat_summary(fun.data = "mean_cl_normal", geom = "point", shape = 20, size = 4, color = "red")
      } else {
        gg <- ggplot2::ggplot(df_artist, ggplot2::aes_string(x = "album_name", y = var)) + 
                ggplot2::geom_violin(ggplot2::aes_string(fill = "album_name")) + 
                ggplot2::labs(title = plot_title, x = "Album", y = var, fill = "Album") +
                ggplot2::theme_bw() + 
                ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                    legend.text = ggplot2::element_text(size = 20)) + 
                ggplot2::stat_summary(fun.data = "mean_cl_normal", geom = "point", shape = 20, size = 4, color = "red")
        }
    } else {
      gg <- ggplot2::ggplot(df_artist, ggplot2::aes_string(x = "album_name", y = var)) + 
              ggplot2::geom_violin(ggplot2::aes_string(fill = "album_name")) + 
              ggplot2::labs(title = plot_title, x = "", y = var, fill = "Album") + 
              ggplot2::theme_bw() +
              ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), 
                    axis.text.x = ggplot2::element_blank(), 
                    axis.ticks.x = ggplot2::element_blank(),
                    legend.text = ggplot2::element_text(size = 20)
                    ) + 
              ggplot2::stat_summary(fun.data = "mean_cl_normal", geom = "point", shape = 20, size = 4, color = "red")
    }
    
    plotly::ggplotly(gg)
}
