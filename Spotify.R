
#'Need to use the most updated version of package by devtools
#' devtools::install_github('charlie86/spotifyr')
#' install.packages('httpuv')

libs <- c('data.table', 'magrittr', 'spotifyr', 'ggplot2') 
lapply(libs, require, character.only = T)
source(paste0(getwd(), '/Documents/spotify_credentials.R'))
  
access_token <- get_spotify_access_token()

# timmy_trumpet <- get_artist_audio_features('timmy trumpet')
# kygo <- get_discography(artist = 'Kygo')
# get_artist_audio_features('Someone You Loved (Future Humans Remix)')
 #get_my_recently_played(limit = 5) %>% View

#'Personal playlists that should be used for analysis and possible prediction 
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





#'Data Variable Selection
test_correlation <- ALLSAVED[, c('danceability', 'energy', 'key', 'loudness', 'mode', 
                                 'speechiness', 'acousticness', 'instrumentalness',
                                 'liveness', 'valence', 'tempo', 'track.popularity')]
library(corrplot) ; set.seed(123)
M <- cor(x = test_correlation)
col1 <- colorRampPalette(c('#1DB954', '#FFFFFF', '#191414'))

corrplot(M, method = 'circle', order = 'hclust', addrect = 3,  addCoef.col = 'black', tl.col = 'black', 
         tl.srt = 45, col = col1(100))

tri_vars <- c('dance_valence', 'loud_energy', #89.6%
              'track.name')  
# tri_vars <- c('dance_valence', 'acoustic_loudness',
#               'track.name')  

tri_vars <- c('danceability', 'energy', 'valence', 'track.name')

kmeans_DT <- ALLSAVED[, ..tri_vars] 


#SHOULD ALSO SUPPORT THIS STUDY WITH A PCA OR SVD!!!!!! - also normalize before or after??
#Maybe nont since we don't have THAT many columns. We could see though 
test_correlation_saved <- test_correlation %>% as.matrix
SVD_test <- svd(test_correlation_saved)
singular_values <- SVD_test$d 
plot(x = 1:NROW(singular_values), y = singular_values,
     type = 'o', xlab = 'Dimensions', ylab = 'Singular Values',
     main = 'Alternative Low-Rank Approximation')
lines(x = 1:NROW(singular_values), y = rep(singular_values[2], NROW(singular_values)), 
      type = 'l', col = 'red')
lines(x = 1:NROW(singular_values), y = rep(singular_values[3], NROW(singular_values)), 
      type = 'l', col = 'blue')






#Determining optimal-k using two direct methods and statistical testing analysis
fviz_nbclust(x = kmeans_DT[, -c('track.name')], kmeans, method = 'wss') + 
  geom_vline(xintercept = 3, linetype = 2) +
  labs(subtitle = 'Elbow Method')

fviz_nbclust(x = kmeans_DT[, -c('track.name')], kmeans, method = 'silhouette') + 
  labs(subtitle = 'Silhouette Method')

fviz_nbclust(x = kmeans_DT[, -c('track.name')], kmeans, nstart = 25, nboot = 50, method = 'gap_stat') + 
  labs(subtitle = 'Gap Statistics Method')


#Determining optimal-k using dedrogram from Hierarchical Clustering
#kmeans_DT2 <- ALLSAVED[, c('danceability', 'energy', 'tempo', 'track.name')] 
d <- dist(kmeans_DT[, -c('track.name')])
clusters <- hclust(d, method = 'average')
plot(clusters)

clusterCut <- cutree(clusters, 3)
#'Comment: Noticing the distribution, because I may listen to different types of music
#'then I do not want to adjust the distribution so that an input may be interpretted differently
table(clusterCut)
HIER_INFO <- cbind(kmeans_DT, clusterCut)

#'Visually understand before finding actually applying an optimal-k through different methods
fig <- plot_ly(data = HIER_INFO, x = ~danceability, y = ~energy, z = ~valence, 
               color = clusterCut,
               text = ~track.name,
               hovertemplate = paste('<br><b>Song name</b>: %{text}') )
fig

#'Based on the output I'm really only confident that the smallest cluster carries the most truth
#'The two majority clusters could be broken further. Possible next step would be to use another method.

#'Clustering based on Optimal-k which may be categorizing based on the my most listened to genres
#'(1) - Pop  (2) - EDM (3) - Country
#'(4) - Chill (5) - Rap&HipHop
set.seed(123)
kmeans_DT <- ALLSAVED[, c('dance_valence', 'loud_energy', 'track.name')] 
x <- kmeans(x = kmeans_DT[, -c('track.name')], centers = 4, iter.max = 20)
x$cluster <- as.character(as.factor(x$cluster) )
x #84.8% 

INFO_1 <- cbind(kmeans_DT, x$cluster)

fig <- plot_ly(data = INFO_1, x = ~dance_valence, y = ~loud_energy, color =  ~V2,
               text = ~track.name,
               hovertemplate = paste('<br><b>Song name</b>: %{text}') )
fig %>% layout(title = 'Dancing Valence vs Acoustic Loudness')
INFO_1 %>% View


#'Validation Set
#'To confidently select a clustering algorithm, and important concept is my own subjectivity.
#'For the model to produce good results, I would like for the clustering methods to follow some pattern
#'where my personally related songs are clustered together. This will be used in validating if a 
#'clustering technique proved to work well or not. Accuracy is not the only important metric in this project.  
#'
#'REALLY THINNKING ABOUT PUTTING THE SMALL GROUP FROM THE HIERARCHICAL CLUSTERING IN THIS

#Testing using variables that are not related to another
kmeans_DT2 <- ALLSAVED[, c('danceability', 'energy', 'tempo', 'track.name')] 
x2 <- kmeans(x = kmeans_DT2[, -c('track.name')], centers = 4, iter.max = 25)
x2$cluster <- as.factor(x2$cluster)
x2 #Yields a 90.7%

INFO_2 <- cbind(kmeans_DT2, as.character(x2$cluster) )
fig <- plot_ly(data = INFO_2, x = ~danceability, y = ~energy, z = ~tempo, color =  ~V2,
               text = ~track.name,
               hovertemplate = paste('<br><b>Song name</b>: %{text}') )
fig %>% layout(title = 'Dance vs. Energy vs. Tempo',
               subtitle = 'Kmeans')
INFO_2 %>% View

#PAM: Partitioning around Mediods with k-clusters

library(cluster)
y <- pam(x = kmeans_DT2[, -c('track.name')], k = 4, metric = 'euclidean')
pam_silhouette <- silhouette(y)
mean(pam_silhouette[, 3]) #scale from -[-1,1]. 1 means perfect cluster.

INFO3 <- cbind(kmeans_DT2, y$clustering)
fig <- plot_ly(data = INFO3, x = ~energy, y = ~valence, z = ~tempo, color =  ~V2,
               text = ~track.name,
               hovertemplate = paste('<br><b>Song name</b>: %{text}') )
fig %>% layout(title = 'Energy 2vs. Valence vs. Tempo')


#SVM: Support Vector Machine




#'Noticing that tempo is practically the main classifier to the group, it has reminded me that I need to 
#'normalize the fields. Not just 'tempo' since on the scI believe tempo needs to be standardized because it's on a completely different scale which may be affecting results
#'Act

