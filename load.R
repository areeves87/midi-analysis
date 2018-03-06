library(tidyverse)
library(tuneR)

source("func.R")

###Make or load midi dataset, including meta data

if(file.exists("raw_midi.csv")) {
        raw_midi <- read.csv("raw_midi.csv")
}else{ 
        raw_midi <- build_db(directory = "ffmidi", FUN = readMidi)
        write.csv(raw_midi, "raw_midi.csv", row.names = FALSE)
}


###Make or load simplified note dataset, 
###includes only track's note values and durations

if(file.exists("midi_db.csv")) {
        midi_db <- read.csv("midi_db.csv")
}else{ 
        midi_db <- build_db("ffmidi", function(x) getMidiNotes(readMidi(x)))
        write.csv(midi_db, "midi_db.csv", row.names = FALSE)
}

midi_program <- read.csv("midi_program.csv") #midi instrument reference

melodies <- read_csv("melody_guide_program.csv") #manually-tagged melodies
