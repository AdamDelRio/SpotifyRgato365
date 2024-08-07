% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/artist_functions.r
\name{get_artist_summary}
\alias{get_artist_summary}
\title{Search for Spotify artist feature summary}
\usage{
get_artist_summary(
  query = NULL,
  id = NULL,
  authorization = get_spotify_access_token()
)
}
\arguments{
\item{query}{\itemize{
\item A single artist name.  Defaults to NULL.  Only use this or id
}}

\item{id}{\itemize{
\item A single Spotify artist id.  Defaults to NULL.  Only use this or id
}}

\item{authorization}{\itemize{
\item An access_token generated from the get_spotify_access_token() function
}}
}
\value{
A dataframe of artist summary data, including artist name and id, number of songs from the artist, and the mean and standard deviation for:
danceability
energy
loudness
speechiness
acousticness
instrumentalness
liveness
valence
tempo
duration_ms
mode
}
\description{
Search for Spotify artist feature summary
}
\examples{
\dontrun{
get_artist_summary(id = "0du5cEVh5yTK9QJze8zA0C")
get_artist_summary(query = "Bruno Mars")
}
}
