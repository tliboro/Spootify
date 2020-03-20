
#'Need to use the most updated version of package by devtools
#' devtools::install_github('charlie86/spotifyr')

libs <- c('data.table', 'magrittr', 'spotifyr', 'ggplot2') 
lapply(libs, require, character.only = T)
source(paste0(getwd(), '/Desktop/PersonalProjects/GLOBALS_Spotify.R'))
access_token <- get_spotify_access_token()

# timmy_trumpet <- get_artist_audio_features('timmy trumpet')
# kygo <- get_discography(artist = 'Kygo')
# get_artist_audio_features('Someone You Loved (Future Humans Remix)')
 #get_my_recently_played(limit = 5) %>% View

#'Personal playlists that should be used for analysis and possible prediction 
#'
COLS_playlist <- c('track.name', 'track.popularity', 'danceability', 'energy', 'key', 'loudness', 'mode', 
                   'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'tempo', 
                   'track.id', 'track.duration_ms', 'track.explicit', 'track.album.artists')


my_playlists <- get_user_playlists(my_id) %>% as.data.table

#Need to make a distance metric and then color by distance 
ALLSAVED <- get_playlist_audio_features(username = my_id, playlist_uris = my_playlists[name %in% 'ALL SAVED']$id) %>%
  as.data.table %>%
  .[!is.na(valence)] %>%
  .[, Artist := track.album.artists[[1]]$`name`[1], by =  'track.name'] %>%
  .[, dance_valence := ((danceability^2)+(valence^2))/2] %>% #LOW CORRELATION - PERSONAL HIGH RELATIONSHIP - SAME
  .[, loud_energy := ((energy^2)+(loudness^2))/2] %>% #HIGH CORRELATION - SAME
  .[, acoustic_energy := ((acousticness^2)+(energy^2))/2] %>% #SLIGHT CORRELATION - CROSS
  .[, acoustic_loudness := ((acousticness^2)+(loudness^2))/2] %>% #SLIGHT CORRELATION - CROSS
  .[, dance_energy := ((danceability^2)+(energy^2))/2] %>% #CROSS
  .[, valence_tempo := ((valence^2)+(tempo^2))/2] %>% #SAME
  .[, vibes := ((speechiness^2)+(acousticness^2)+(instrumentalness^2))/2] %>%
  setcolorder(., 'Artist')

#'NORMALIZATION OR STANDARDIZATION
#'Considered manipulating the data; however, since the current goal is to accept any song
#'then it would not be fair to asssume that we are all on the same spectrum for music. I'd 
#'rather acknowledge that I need more experience in another genre than to give them a 
#'recommendation that isn't completely explored.

tri_vars <- c('dance_valence', 'loud_energy', #89.6%
              'track.name')  
tri_vars <- c('dance_valence', 'acoustic_loudness',
              'track.name')  
  
# library(plotly)
# fig <- plot_ly(data = ALLSAVED, x = ~danceability, y = ~energy, type = 'scatter',
#                color = ~dance_energy,
#                text = ~paste('</br> Song : ', track.name,
#                              '</br> Artist : ', Artist,
#                              '</br> Popularity: ', track.popularity))
# fig <- fig %>% layout(title = 'Danceability vs. Energy',
#                       xaxis = list(title = 'Danceability'),
#                       yaxis = list(title = 'Energy'))
# fig


#Need to find correlations for the variabeles
test_correlation <- ALLSAVED[, c('danceability', 'energy', 'key', 'loudness', 'mode', 
                                 'speechiness', 'acousticness', 'instrumentalness',
                                 'liveness', 'valence', 'tempo', 'track.popularity')]
library(corrplot)
M <- cor(x = test_correlation)
col1 <- colorRampPalette(c('#1DB954', '#FFFFFF', '#191414'))

corrplot(M, method = 'circle', order = 'hclust', addrect = 3,  addCoef.col = 'black', tl.col = 'black', 
         tl.srt = 45, col = col1(100))


# model <- lm(valence ~ dance_energy + loudness, data = ALLSAVED)
# summary(model)



#'Clustering based on my most listened to genres
#'(1) - Pop  (2) - EDM (3) - Country
#'(4) - Chill (5) - Rap&HipHop
set.seed(123)
kmeans_DT <- ALLSAVED[, ..tri_vars] 
x <- kmeans(x = kmeans_DT[,-c('track.name')], centers = 5, iter.max = 20)
x$cluster <- as.factor(x$cluster)
x

INFO <- cbind(kmeans_DT, as.character(x$cluster) )
fig <- plot_ly(data = INFO, x = ~dance_valence, y = ~acoustic_loudness, color =  ~V2,
               hovertemplate = '<br><b>Track name</b>: %{dance_valence}')
fig



INFO %>% View



#Need to make a function to test multiple cases



