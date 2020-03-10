
#'Need to use the most updated version of package by devtools
#' devtools::install_github('charlie86/spotifyr')

libs <- c('data.table', 'magrittr', 'spotifyr') 
lapply(libs, require, character.only = T)
source(paste0(getwd(), '/Desktop/PersonalProjects/GLOBALS_Spotify.R'))
access_token <- get_spotify_access_token()

timmy_trumpet <- get_artist_audio_features('timmy trumpet')
kygo <- get_discography(artist = 'Kygo')
get_artist_audio_features('Someone You Loved (Future Humans Remix)')
 


get_my_recently_played(limit = 5) %>% View



#'Personal playlists that should be used for analysis and possible prediction 
#'
COLS_playlist <- c('track.name', 'track.popularity', 'danceability', 'energy', 'key', 'loudness', 'mode', 
                   'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'tempo', 
                   'track.id', 'track.duration_ms', 'track.explicit')


my_playlists <- get_user_playlists(my_id) %>% as.data.table

mood_ty <- get_playlist_audio_features(username = my_id, playlist_uris = my_playlists[name %in% 'Mood(T)y']$id) %>%
  as.data.table #%>% .[, ..COLS_playlist]

#Need to make a distance metric and then color by distance 
last1k <- get_playlist_audio_features(username = my_id, playlist_uris = my_playlists[name %in% 'Last1k']$id) %>%
  as.data.table %>% .[, ..COLS_playlist] %>%
  .[, Distance := ((danceability^2)+(energy^2))/2]


library(plotly)
fig <- plot_ly(data = last1k, x = ~danceability, y = ~energy, type = 'scatter',
               color = ~Distance, 
               text = ~paste('</br> Song : ', track.name,
                             '</br> Popularity: ', track.popularity))
fig <- fig %>% layout(title = 'L2-Norm of Danceability vs. Energy', 
                      xaxis = list(title = 'Danceability'),
                      yaxis = list(title = 'Energy'))
fig

#Find Happy Songs + without lyrics


library(genius) #need the artist

set.seed(123)
fig2 <- plot_ly(data = last1k, x = ~danceability, y = ~energy, z = ~tempo)
fig2 
