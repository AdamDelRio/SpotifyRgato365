#' @title Search for Spotify new releases
#' @param limit - Optional.  Number of projects wanted to return.  Valid if between 1 and 50.  Defaults to 20
#' @param offset - Optional.  Index of first project wanted.  Defaults to 0
#' @param authorization - An access_token generated from the get_spotify_access_token() function
#' @return A dataframe of new relase data, inlcuding the type, project name and id, release date, total number of tracks, and artist names and ids
#' @examples
#' \dontrun{
#' get_new_releases()
#' get_new_releases(limit = 50, offset = 2)
#' }
#' @export
get_new_releases <- function(limit = 20, offset = 0, authorization = get_spotify_access_token()){
    url <- "https://api.spotify.com/v1/browse/new-releases"
    parameters <- list(
        country = "US",
        limit = limit,
        offset = offset,
        access_token = authorization
    )
    result <- httr::RETRY('GET', url, query = parameters, encode = 'json', terminate_on = c(401, 403, 404))

    httr::stop_for_status(result)

    result <- jsonlite::fromJSON(httr::content(result, as = 'text',
                            encoding = 'UTF-8'),
                    flatten = TRUE)

    result <- result$albums$items
    result$artists <- purrr::map(result$artists, ~ list(id = .x$id, name = .x$name))
    result %>%
            dplyr::mutate(
            artist_id = purrr::map_chr(artists, ~ toString(.x$id)),
            artist_name = purrr::map_chr(artists, ~ toString(.x$name))
            ) %>%
            dplyr::select(-artists,
                        -available_markets,
                        -href,
                        -images,
                        -uri,
                        -external_urls.spotify,
                        -type) %>%
            dplyr::rename(
                project_id = id,
                project_name = name
            )
}