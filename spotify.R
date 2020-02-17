
#connect to the API and set these.
Sys.setenv(SPOTIFY_CLIENT_ID = '')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '')

options(scipen = 9999)

library(spotifyr)
library(tidyverse)
library(knitr)

access_token <- get_spotify_access_token()


#get playlist ids
full_playlist <- data.frame()

get_full_playlist <- function(x) {
  playlist <- spotifyr::get_playlist_tracks('5GEf0fJs9xBPr5R4jEQjtw', offset = x)
  full_playlist <<- rbind(full_playlist, playlist)
}

offset <- seq(0,1400,100)
mapply(get_full_playlist, offset)


#get song analysis

analysis <- mapply(full_playlist$track.id, FUN = get_track_audio_features)
analysis_df <- as.data.frame(t(analysis))

#remove lists within df
analysis_df <- lapply(analysis_df, as.character) %>% as.data.frame(stringsAsFactors = FALSE)

#make numeric cols numeric
analysis_df[,1:11] <- lapply(analysis_df[,1:11], as.numeric)
analysis_df[,17:18] <- lapply(analysis_df[,17:18], as.numeric)

#add names of songs while keeping the positions of songs
analysis_df$position <- row.names(analysis_df) %>% as.numeric()
full_playlist_df <- data.frame(id = full_playlist$track.id, name = full_playlist$track.name)
analysis_df <- merge(analysis_df, full_playlist_df, by = 'id')
analysis_df <- arrange(analysis_df, position)

#reverse order
analysis_df <- analysis_df %>% map_df(rev)

#add decades
analysis_df$decade <- NA
analysis_df$decade[1:95] <- '1950s'
analysis_df$decade[96:280] <- '1960s'
analysis_df$decade[281:442] <- '1970s'
analysis_df$decade[443:624] <- '1980s'
analysis_df$decade[625:818] <- '1990s'
analysis_df$decade[819:1087] <- '2000s'
analysis_df$decade[1088:1329] <- '2010s'

analysis_df$decade <- as.factor(analysis_df$decade)

#add colours
cbp1 <- c("#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#plot

ggplot(analysis_df, aes(as.numeric(row.names(analysis_df)), loudness)) +
  geom_point(aes(colour = decade)) +
  geom_smooth() + theme_minimal() + scale_colour_manual(values = cbp1) + labs(title = 'Loudness of Number Ones on Spotify',x = 'Position',y = 'Loudness',caption = 'Source: Spotify API', colour = 'Decade') 

ggplot(analysis_df, aes(as.numeric(row.names(analysis_df)), valence)) +
  geom_point(aes(colour = decade)) +
  geom_smooth() + theme_minimal() + scale_colour_manual(values = cbp1) + labs(title = 'Positivity of Number Ones on Spotify',x = 'Position',y = 'Valence (1 = very happy)',caption = 'Source: Spotify API', colour = 'Decade') 

ggplot(analysis_df, aes(as.numeric(row.names(analysis_df)), energy)) +
  geom_point(aes(colour = decade)) +
  geom_smooth() + theme_minimal() + scale_colour_manual(values = cbp1) + labs(title = 'Energy of Number Ones on Spotify',x = 'Position',y = 'Energy',caption = 'Source: Spotify API', colour = 'Decade') 

ggplot(analysis_df, aes(as.numeric(row.names(analysis_df)), duration_ms)) +
  geom_point(aes(colour = decade)) +
  geom_smooth() + theme_minimal() + scale_colour_manual(values = cbp1) + labs(title = 'Song length of Number Ones on Spotify',x = 'Position',y = 'Length',caption = 'Source: Spotify API', colour = 'Decade') 

ggplot(analysis_df, aes(as.numeric(row.names(analysis_df)), danceability)) +
  geom_point(aes(colour = decade)) +
  geom_smooth() + theme_minimal() + scale_colour_manual(values = cbp1) + labs(title = 'Danceability of Number Ones on Spotify',x = 'Position',y = 'Danceability',caption = 'Source: Spotify API', colour = 'Decade') 
