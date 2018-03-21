library(tidyverse)
library(tuneR)
#library(plotly)

####Functions for Loading Data####

##tries reading in a file with the specified function, creates title column
read_plus <- function(file_loc, read_fun = readMidi){
        filename <- basename(file_loc)
        
        print(filename) #tracks progress during a for loop
        
        ret <- try(read_fun(file_loc) %>% 
                           mutate(title = filename) %>% 
                           select(title, everything())
        )
        
        ret
}

##row-binds list of data.frames produced by read_plus
build_db <- function(directory = "ffmidi", FUN = readMidi) {
        ##List files for dataset
        files_list  <- list.files(directory, full.names = TRUE)
        
        ##Make midi dataset, including meta data
        db <- lapply(files_list, read_plus, FUN)
        
        cond <- lapply(db, function(x) class(x) == "data.frame")
        
        print("The following files did not load as dataframes:")
        print(files_list[!unlist(cond)])
        
        db <- do.call(rbind, db[unlist(cond)])
        
        return(db)      
}

####Functions for Creating Features####

#finds the mode of a vector
Mode <- function(x) {
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
}

#finds the entropy of a vector
entropy_my <- function(x) {
        freqs <- table(x)/length(x)
        -sum(freqs * log2(freqs))
}

##Get each track's skyline, calc track features, then indicate song's skyline

#input is output of tuneR::getMidiNotes
get_track_skylines <- function(midi_notes){
        midi_notes %>% 
        group_by(title, channel, track, time) %>% 
        summarise(
                sky_note = max(note), #skyline within single track
                length  = length[which.max(note)],
                velocity = velocity[which.max(note)],
                dropped_notes = n_distinct(note) - 1
        )
}

#input from get_track_skyline
get_song_skyline <- function(skylines){ 
        skylines %>% 
        group_by(title, time) %>% 
        mutate(is_top = sky_note == max(sky_note)) #skyline across all tracks
}

get_track_intervals <- function(skylines){#use on track skylines
        skylines %>% 
        group_by(title, channel, track) %>% 
        mutate(interval = diff(c(first(sky_note),sky_note)), #track features
               IOI     = diff(c(first(time),time))
        )
}              

get_track_features <- function(skylines){#use after generating intervals
        skylines %>%
        group_by(title, channel, track) %>% 
        mutate(
                #phrasend = Mode(IOI)*4 < lead(IOI, 1, 0),
                longpr   = length > IOI & IOI > 45,
                occupance = pmin(c(IOI[-1],0), length)
                
        )
}

get_song_features <- function(skylines){
        skylines %>% 
        group_by(title) %>% 
        mutate(
                tracks = n_distinct(track),
                channels = n_distinct(channel),
                song_length = max(time)
        )
}

##Summarise each track
get_track_stats <- function(skylines){
        skylines %>%
        group_by(title,channel,track) %>%
        summarise(events = n_distinct(time),
                  track_occ = sum(occupance)/Mode(song_length),
                  tracks = Mode(tracks),
                  channels = Mode(channels),
                  top_rate = mean(is_top),
                  pc_entropy = entropy_my(sky_note),
                  int_entropy = entropy_my(interval),
                  IOI_entropy = entropy_my(IOI[IOI < 1000 & IOI > 60]),
                  longpr   = mean(longpr),
                  med_note = median(sky_note),
                  mad_int = interval %>% abs() %>% median(),
                  frac_poly = mean(dropped_notes > 0)
        )
}

#Pull the instrument name out of the readMidi data

get_instrument_names <-  function(raw_midi){ 
        raw_midi %>% #extract instrument data from output of readMidi
        filter(str_detect(event, "Program Change") , 
               str_detect(parameter1, "[:alnum:]")) %>%
        select(title, channel, track, parameter1) %>% 
        group_by(title,channel,track) %>% 
        summarise(number = first(parameter1) + 1) #convert index
}

####Functions for Plotting Data####

ggpiano <-     function(midi_notes, xlim = c(0,20000)){
                ggplot(midi_notes, aes(colour = as.factor(channel))) +
                geom_segment(aes(x = time, 
                                 xend = time+length, 
                                 y = note, 
                                 yend = note), 
                             size=3) +
                xlim(xlim) +
                xlab("Time (Ticks)")
}

# function to extract binomial confidence levels
get_binCI <- function(x,n){
        as.list(setNames(binom.test(x,n)$conf.int, c("lwr", "upr")))
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        library(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), 
                                                           ncol(layout)
                                                           )
                                      )
                             )
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, 
                                                        arr.ind = TRUE)
                                                  )
                        
                        print(plots[[i]], 
                              vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col)
                              )
                }
        }
}
