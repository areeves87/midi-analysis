library(tidyverse)
library(tuneR)

source("clean.R")

######

library(corrplot)

x <-    midi_melodies %>% 
        mutate_all(as.numeric) %>% 
        cor(method = "pearson")

corr.p <- corrplot(x)

x["ME",] %>% sort(decreasing = TRUE)

######Density multiplot

p1 <-   ggplot(midi_melodies, aes(x = channel, fill = as.factor(ME))) +
        geom_density(alpha = 0.5) +
        theme(legend.position = "none")

p2 <-   ggplot(midi_melodies, aes(x = track, fill = as.factor(ME))) +
        geom_density(alpha = 0.5) +
        ylab(NULL) +
        theme(legend.position = "none")

p3 <-   ggplot(midi_melodies, aes(x = track_occ, fill = as.factor(ME))) +
        geom_density(alpha = 0.5) +
        ylab(NULL) +
        theme(legend.position = "none")

p4 <-   ggplot(midi_melodies, aes(x = frac_poly, fill = as.factor(ME))) +
        geom_density(alpha = 0.5) +
        ylab(NULL) +
        theme(legend.position = "none")

p5 <-   ggplot(midi_melodies, aes(x = events, fill = as.factor(ME))) +
        geom_density(alpha = 0.5) +
        scale_x_continuous(limits = c(0, 1000)) +
        theme(legend.position = "none")

p6 <-   ggplot(midi_melodies, aes(x = mad_int, fill = as.factor(ME))) +
        geom_density(alpha = 0.5) +
        ylab(NULL) +
        theme(legend.position = "none")

p7 <-   ggplot(midi_melodies, aes(x = IOI_entropy, fill = as.factor(ME))) +
        geom_density(alpha = 0.5) +
        ylab(NULL) +
        theme(legend.position = "none")

p8 <-   ggplot(midi_melodies, aes(x = pc_entropy, fill = as.factor(ME))) +
        geom_density(alpha = 0.5) +
        ylab(NULL) +
        theme(legend.position = "none")

p9 <-   ggplot(midi_melodies, aes(x = int_entropy, fill = as.factor(ME))) +
        geom_density(alpha = 0.5) +
        theme(legend.position = "none")

p10 <-   ggplot(midi_melodies, aes(x = longpr, fill = as.factor(ME))) +
        geom_density(alpha = 0.5) +
        ylab(NULL) +
        theme(legend.position = "none")

p11 <-   ggplot(midi_melodies, aes(x = top_rate, fill = as.factor(ME))) +
        geom_density(alpha = 0.5) +
        ylab(NULL) +
        theme(legend.position = "none")

p12 <-   ggplot(midi_melodies, aes(x = med_note, fill = as.factor(ME))) +
        geom_density(alpha = 0.5) +
        ylab(NULL) +
        theme(legend.position = "none")

layout <- matrix(1:12,3,4,byrow=TRUE)

multi.p1 <- multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, 
            p11, p12, layout=layout)


####Melody rates for categorical features

#chance of being a melody track
frac_melody <- sum(midi_melodies$ME == 1)/nrow(midi_melodies)*100

#confidence interval
frac_melody_UL <- get_binCI(sum(midi_melodies$ME == 1), 
                            nrow(midi_melodies))$upr * 100
frac_melody_LL <- get_binCI(sum(midi_melodies$ME == 1), 
                            nrow(midi_melodies))$lwr * 100

#refactor name column to random forest limit of 52 categories per variable
midi_melodies <- midi_melodies %>% 
                 mutate(name = fct_lump(name, n = 52, ties.method = "random"))

##Plot fraction melody by instrument name

p13 <-  midi_melodies %>%
        group_by(name, ME) %>%
        count() %>%
        spread(ME, n) %>%
        replace_na(list('0' = 0,
                        '1' = 0)) %>% 
        mutate(frac_claim = `1`/(`1`+`0`)*100,
               lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
               upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
        ) %>%
        ggplot(aes(reorder(name, -frac_claim, FUN = max), frac_claim, fill = name)) +
        geom_col() +
        geom_errorbar(aes(ymin = lwr, ymax = upr), 
                      width = 0.5, size = 0.7, color = "gray30") +
        theme(legend.position = "none") +
        labs(x = "name", y = "Melody [%]") +
        theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        geom_hline(yintercept = frac_melody, linetype = "dashed") #+
        #geom_hline(yintercept = frac_melody_UL, linetype = "dotted") +
        #geom_hline(yintercept = frac_melody_LL, linetype = "dotted")

##Plot fraction melody by instrument family

p14 <-  midi_melodies %>%
        group_by(family, ME) %>%
        count() %>%
        spread(ME, n) %>%
        replace_na(list('0' = 0,
                        '1' = 0)) %>% 
        mutate(frac_claim = `1`/(`1`+`0`)*100,
               lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
               upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
        ) %>%
        ggplot(aes(reorder(family, -frac_claim, FUN = max), frac_claim, fill = family)) +
        geom_col() +
        geom_errorbar(aes(ymin = lwr, ymax = upr), 
                      width = 0.5, size = 0.7, color = "gray30") +
        theme(legend.position = "none") +
        labs(x = "family", y = "Melody [%]") +
        theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        geom_hline(yintercept = frac_melody, linetype = "dotted")

####Dotplots

library('ggridges')

##multiplot

p15 <- ggplot(midi_melodies, aes(x=med_note, y=top_rate)) + 
        geom_jitter(aes(alpha = 0.5,
                        colour = as.factor(ME)))+
        theme(legend.position = "none")

p16 <- ggplot(midi_melodies, aes(x=channel, y=med_note)) + 
        geom_jitter(aes(alpha = 0.5,
                        colour = as.factor(ME)))+
        theme(legend.position = "none")

p17 <- ggplot(midi_melodies, aes(x=channel, y=top_rate)) + 
        geom_jitter(aes(alpha = 0.5,
                        colour = as.factor(ME)))+
        theme(legend.position = "none")

p18 <- ggplot(midi_melodies, aes(x=channel, y=pc_entropy)) + 
        geom_jitter(aes(alpha = 0.5,
                        colour = as.factor(ME)))+
        theme(legend.position = "none")

layout <- matrix(1:4,2,2,byrow=TRUE)

multi.p2 <- multiplot(p15, p16, p17, p18, layout=layout)

#categorical ridgeplots

p19 <- ggplot(midi_melodies, 
       aes(x = med_note, y = factor(family), fill = factor(ME))) +
        geom_density_ridges(scale = 2,alpha = .5,rel_min_height = 0.01) + 
        theme_ridges() 

p20 <- ggplot(midi_melodies, 
       aes(x = med_note, y = factor(name), fill = factor(ME))) +
        geom_density_ridges(scale = 2,alpha = .5,rel_min_height = 0.01) + 
        theme_ridges(font_size = 9) 

pdf(title = "_midi_plots.pdf")

corrplot(x)

layout <- matrix(1:12,3,4,byrow=TRUE)

multi.p1 <- multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, 
                      p11, p12, layout=layout)

p13

p14

layout <- matrix(1:4,2,2,byrow=TRUE)

multi.p2 <- multiplot(p15, p16, p17, p18, layout=layout)

p19

p20

dev.off()


