library(tidyverse)
library(tuneR)

source("func.R")

###Make or load midi dataset, including meta data
 
file_names <- unzip("data.zip", list = TRUE)$Name
check <- basename(file_names) %in% "raw_midi.csv"

if(any(check)) {
        raw_midi <- read.table(unz("data.zip","data/raw_midi.csv"),
                               header=T,
                               quote="\"",
                               sep=",")
}else{ 
        raw_midi <- build_db(directory = unz("ffmidi.zip", "ffmidi"),
                             FUN = readMidi)
        #write.csv(raw_midi, "raw_midi.csv", row.names = FALSE)
}


###Make or load tidy note dataset, 
###includes only track's note values and durations

file_names <- unzip("data.zip", list = TRUE)$Name
check <- basename(file_names) %in% "midi_db.csv"

if(any(check)) {
        midi_db <- read.table(unz("data.zip","data/midi_db.csv"),
                              header=T,
                              quote="\"",
                              sep=",")
}else{ 
        midi_db <- build_db(unz("ffmidi.zip", "ffmidi"), 
                            function(x) getMidiNotes(readMidi(x)))
        #write.csv(midi_db, "midi_db.csv", row.names = FALSE)
}

midi_program <- read.table(unz("data.zip","data/midi_program.csv"),
                           header=T,
                           quote="\"",
                           sep=",") #midi instrument reference

melodies <- read.table(unz("data.zip","data/midi_melodies.csv"),
                       header=T,
                       quote="\"",
                       sep=",") #manually-tagged melodies
