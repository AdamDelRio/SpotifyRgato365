#' @title Gain information from the formal name of a artist, album, or track
#' @param query - A single string of an artist name, album name, or track name
#' @param type - A string of "artist", "album", or "track"
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of basic artist, track, or album information
#' @examples
#' \dontrun{
#'  search_helper("Bruno Mars", "artist")
#'  search_helper("Natalie", "track")
#'  search_helper("An Evening With Silk Sonic", "album")
#' }
#' @noRd
search_helper <- function(query, type, authorization = get_spotify_access_token()){
    search_url <- "https://api.spotify.com/v1/search"

    if(length(type) != 1){
        stop("Please input ONE and only ONE type!")
    }

    parameters <- list(
        q = query,
        type = type,
        market = "US",
        limit = 1,
        offset = 0,
        access_token = authorization
    )

    res <- httr::RETRY("GET", search_url, query = parameters, encode = "json", terminate_on = c(401, 403, 404))

    httr::stop_for_status(res)

    res <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"),
                    flatten = TRUE)

    res <- res[[str_glue('{type}s')]]$items %>%
            as_tibble

    res 
}

#' @title Gain information from the formal name of multiple artists, albums, or tracks
#' @param queries - A string vector of an artist names, album names, or track names
#' @param type - A string of "artist", "album", or "track"
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of basic artist, track, or album information
#' @examples
#' \dontrun{
#'  search_spotify(c("Bruno Mars", "Anderson .paak"), "artist")
#'  search_spotify(c("Natalie", "Grenade"), "track")
#'  search_spotify(c("An Evening With Silk Sonic", "Doo-Waps and Hooligans"), "album")
#' }
#' @export
search_spotify <- function(queries, type, authorization = get_spotify_access_token()){
    purrr::map_df(queries, ~search_helper(query = .x, type = type, authorization = authorization)) %>%
    bind_rows
}