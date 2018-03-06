library(tidyverse)
library(tuneR)

source("load.R")

if(file.exists("midi_melodies.csv")) {
        midi_melodies <- read.csv("midi_melodies.csv")
}else{ 
##Create track features

skylines <-     midi_db %>%
                get_track_skylines() %>% 
                get_song_skyline() %>% 
                get_track_intervals() %>% 
                get_track_features() %>% 
                get_song_features()


##Summary track statistics

skylines_summary <-     skylines %>% 
                        get_track_stats()

##Include instrument names

track_instruments <-    raw_midi %>% 
                        get_instrument_names() %>% 
                        left_join(midi_program) %>%  #join with reference table
                        right_join(skylines_summary) %>% 
                        mutate_if(is.factor, as.character) %>% 
                        replace_na(list(number = 0,
                                        family = "Drums",
                                        name   = "Drums"))

midi_melodies <-        melodies %>% 
                        select(title, channel, track, ME) %>% 
                        right_join(track_instruments) %>% 
                        replace_na(list(ME = 0))

midi_melodies <-        midi_melodies %>% 
                        unclass() %>% 
                        as.data.frame()

#write.csv(midi_melodies, file = "midi_melodies.csv", row.names = FALSE)
}