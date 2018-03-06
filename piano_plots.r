source("load.R")

ggplot(midi_db %>% filter(title == "FF1airsh.mid"), 
       aes(colour = as.factor(channel))) +
        geom_segment(aes(x = time, 
                         xend = time+length, 
                         y = note, 
                         yend = note), 
                     size=2) +
        xlim(0,20000) +
        xlab("Time (Ticks)")

p1 <- ggplot(midi_db %>% filter(title == "FF1airsh.mid"), 
             aes(colour = as.factor(channel),
                 alpha = 0.5)) +
        geom_segment(aes(x = time, 
                         xend = time+length, 
                         y = note, 
                         yend = note), 
                     size=1) +
        xlim(0,20000) +
        xlab("Airship")+
        theme(legend.position = "none")+
        ylab(NULL)

p2 <- ggplot(midi_db %>% filter(title == "FF1prolo.mid"), 
             aes(colour = as.factor(channel),
                 alpha = 0.5)) +
        geom_segment(aes(x = time, 
                         xend = time+length, 
                         y = note, 
                         yend = note), 
                     size=1) +
        xlim(0,20000) +
        xlab("Prologue")+
        ylab(NULL) +
        theme(legend.position = "none")


p3 <- ggplot(midi_db %>% filter(title == "FF1castl.mid"), 
             aes(colour = as.factor(channel),
                 alpha = 0.5)) +
        geom_segment(aes(x = time, 
                         xend = time+length, 
                         y = note, 
                         yend = note), 
                     size=1) +
        xlim(0,20000) +
        xlab("Castle")+
        ylab(NULL) +
        theme(legend.position = "none")

p4 <- ggplot(midi_db %>% filter(title == "FF1ship.mid"), 
             aes(colour = as.factor(channel),
                 alpha = 0.5)) +
        geom_segment(aes(x = time, 
                         xend = time+length, 
                         y = note, 
                         yend = note), 
                     size=1) +
        xlim(0,20000) +
        xlab("Ship")+
        ylab(NULL) +
        theme(legend.position = "none")


layout <- matrix(1:4,2,2,byrow=TRUE)

multiplot(p1, p2, p3, p4, layout=layout)

p5 <- ggplot(data = midi_db %>% filter(title == "FF1airsh.mid"), 
             aes(x = as.factor(channel),
                 y = note)) +
        geom_boxplot(aes(fill = as.factor(channel))) +
        theme(legend.position = "none")+
        xlab("Airship")+
        ylab(NULL)

p6 <- ggplot(data = midi_db %>% filter(title == "FF1prolo.mid"), 
             aes(x = as.factor(channel),
                 y = note)) +
        geom_boxplot(aes(fill = as.factor(channel))) +
        theme(legend.position = "none")+
        xlab("Prologue")+
        ylab(NULL)


p7 <- ggplot(data = midi_db %>% filter(title == "FF1castl.mid"), 
             aes(x = as.factor(channel),
                 y = note)) +
        geom_boxplot(aes(fill = as.factor(channel))) +
        theme(legend.position = "none")+
        xlab("Castle")+
        ylab(NULL)

p8 <- ggplot(data = midi_db %>% filter(title == "FF1ship.mid"), 
             aes(x = as.factor(channel),
                 y = note)) +
        geom_boxplot(aes(fill = as.factor(channel))) +
        theme(legend.position = "none")+
        xlab("Ship")+
        ylab(NULL)


layout <- matrix(1:4,2,2,byrow=TRUE)

multiplot(p5, p6, p7, p8, layout=layout)

midi_db %>% 
filter(title %in% c("FF1airsh.mid", 
                    "FF1prolo.mid", 
                    "FF1castl.mid", 
                    "FF1ship.mid")) %>% 
group_by(title, channel) %>% 
summarize(mean = mean(note),
          std  = sd(note))
