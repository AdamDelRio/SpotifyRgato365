#' @importFrom magrittr "%>%"
attributes <- c(
    "Energy", "Valence", "acousticness", "aes", "album.album_type", "album.artists",
    "album.external_urls.spotify", "album.href", "album.id", "album.images",
    "album.is_playable", "album.name", "album.release_date",
    "album.release_date_precision", "album.total_tracks", "album.type", "album.uri",
    "album_group", "album_id", "album_name", "album_release_year", "album_type",
    "analysis_url", "artist", "artist_id", "artist_id.x", "artist_id.y", "artist_name",
    "artist_name.x", "artist_name.y", "artists", "as_tibble", "audio_features.energy",
    "audio_features.valence", "available_markets", "bind_rows", "code_version",
    "codestring", "copyrights", "danceability", "disc_number", "duration_ms",
    "duration_ms.x", "duration_ms.y", "echoprint_version", "echoprintstring",
    "element_text", "energy", "energy_mean", "explicit", "external_ids.isrc",
    "external_ids.upc", "external_urls.spotify", "followers.href", "followers.total",
    "genre", "genre_name", "genres", "href", "id", "images", "instrumentalness",
    "is_local", "is_playable", "key", "key_name", "label", "legend", "liveness",
    "loudness", "margin", "mode_name", "name", "num_songs", "par", "preview_url",
    "release_date", "release_date_precision", "rhythmstring", "sample_md5", "sd",
    "select", "setNames", "speechiness", "speechiness_mean", "str_glue", "str_to_title",
    "synch_version", "synchstring", "tempo", "track_href", "track_id", "track_name",
    "track_names", "tracks.href", "tracks.items", "tracks.limit", "tracks.next",
    "tracks.offset", "tracks.previous", "tracks.total", "type", "uri", "valence",
    "valence_mean", "x", "y"
)
globalVariables(attributes)

#' @title Search for Spotify pitches
#' @return A vector of possible pitches
pitch_class_lookup <- c('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B')

#' @title Search for Spotify possible scopes
#' @return A vector of Spotify possible authorization scopes
#' @examples 
#' \dontrun{
#' scopes()
#' }
#' @export
scopes <- function() {
    xml2::read_html("https://developer.spotify.com/documentation/general/guides/authorization/scopes/") %>%
    rvest::html_elements('code') %>%
    rvest::html_text() %>%
    unique()
}