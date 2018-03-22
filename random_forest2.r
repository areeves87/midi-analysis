library(tidyverse)
library(randomForest)
library(party)
library(pROC)
library(ROCR)
library(PRROC)

source("clean.R")

set.seed(415)

####Test Train Split####

## RF can not handle categorical predictors with more than 53 categories.
## Since instrument name has 128 categories, we create an "other" category

melodies <- melodies %>% #assign low freq factors to "other"
                 mutate(name = fct_lump(name, n = 52, ties.method = "min"),
                        channel = as.factor(channel),
                        track = as.factor(track)) 
                        

## 50% of the songs, not 50% of the tracks
smp_size <- floor(0.50 * n_distinct(melodies$title))

train_ind <- sample(seq_len(n_distinct(melodies$title)), size = smp_size)

train <- melodies %>% filter(as.numeric(title) %in% train_ind)
test <- melodies %>% filter(!(as.numeric(title) %in% train_ind))





####Random Forest####

# rf.fit <- randomForest(as.factor(ME) ~ . - title - track - number,
#                     data = train, 
#                     importance = TRUE, 
#                     ntree = 2000)
# varImpPlot(rf.fit)
# 
# Prediction.rf <- predict(rf.fit, test, type = "prob")
# 
# melody_prediction_rf <- data.frame(title = test$title,
#                                    channel = test$channel,
#                                    track = test$track,
#                                    rf = Prediction.rf)
# 
# model.acc.rf <- melody_prediction_rf %>% 
#                 # mutate(channel = channel %>% as.character %>% as.integer,
#                 #        track   = track %>% as.character %>% as.integer) %>% 
#                 left_join(melodies)%>% 
#                 replace_na(list(is.melody = 0,
#                                 ME     = 0,
#                                 number = 0,
#                                 family = "Drums",
#                                 name   = "Drums"))
# 
# ggplot(model.acc.rf, aes(x = rf.1, fill = as.factor(ME))) +
#         geom_histogram(alpha = 0.5)


####Condtional Forest Model####

fit.cf <- cforest(as.factor(ME) ~ channel + family + name + events +
                                  track_occ + tracks + channels + top_rate +
                                  pc_entropy + int_entropy + IOI_entropy +
                                  longpr + med_note + mad_int + frac_poly,
               data = train, 
               controls = cforest_unbiased(ntree=2000, mtry=3))

#barplot(varimp(fit.cf),las=2)

Prediction.cf <- predict(fit.cf, test, OOB=TRUE, type = "prob")

Prediction.cf <- as.data.frame(do.call(rbind, Prediction.cf))

melody_prediction_cf <- data.frame(title = test$title,
                                   channel = test$channel,
                                   track = test$track,
                                   cf = Prediction.cf[,2])

# model.acc.cf <- melody_prediction_cf %>% 
#         mutate(channel = channel %>% as.character %>% as.integer,
#                track   = track %>% as.character %>% as.integer)

model.acc.cf <- melodies %>% 
                select(title, channel, track, ME) %>% 
                right_join(melody_prediction_cf) %>% 
                replace_na(list(ME = 0)) #drum tracks aren't melody, impute 0

# ggplot(model.acc.cf, aes(x = cf, fill = as.factor(ME))) +
#         geom_histogram(alpha = 0.5)

####Ensembling####

# model.acc <- model.acc.rf %>% left_join(model.acc.cf)
# 
# model.acc <- model.acc %>% mutate(ensemble = rowMeans(.[,c("rf.1","cf")]))
# 
# ggplot(model.acc, aes(x=cf, y=rf.1)) + 
#         geom_jitter(aes(alpha = 0.5,
#                         colour = as.factor(ME)))
# 
# ggplot(model.acc, aes(x = ensemble, fill = as.factor(ME))) +
#         geom_histogram(alpha = 0.5)
# 
# 
# 
# auc(model.acc$ME, model.acc$ensemble)
# model.roc <- roc(model.acc$ME, model.acc$ensemble)
# plot.roc(model.acc$ME, model.acc$ensemble)
# coords(model.roc, "best", ret = "threshold")
# 
# model.acc <- model.acc %>%
#         mutate(channel = channel %>% as.factor,
#                track   = track %>% as.factor) %>%
#              left_join(melodies[,c("title", "channel", "track", "name")])


####Evaluating The Classifier####
auc(model.acc.cf$ME, model.acc.cf$cf)
model.roc <- roc(model.acc.cf$ME, model.acc.cf$cf)
plot.roc(model.acc.cf$ME, model.acc.cf$cf)
coords(model.roc, "best", ret = "threshold")

Prediction.cf.all <- predict(fit.cf, melodies, OOB=TRUE, type = "prob")
Prediction.cf.all <- as.data.frame(do.call(rbind, Prediction.cf.all))

####


fg <- model.acc$cf[model.acc$ME == 1]
bg <- model.acc$cf[model.acc$ME == 0]

fg.null <- rep(0, sum(model.acc$ME == 1))
bg.null <- rep(0, sum(model.acc$ME == 0))

# ROC Curve    
roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(roc)

# PR Curve
pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(pr)

###

library (ROCR);


y <- model.acc$ME
predictions <- model.acc$cf

pred <- prediction(predictions, y);

# # Recall-Precision curve             
# RP.perf <- performance(pred, "prec", "rec");
# 
# plot (RP.perf);
# 
# # ROC curve
# ROC.perf <- performance(pred, "tpr", "fpr");
# plot (ROC.perf);
# 
# # ROC area under the curve
# auc.tmp <- performance(pred,"auc");
# auc <- as.numeric(auc.tmp@y.values)

F1.perf <- performance(pred,"f")
plot(F1.perf)

