# DoorGablingEffort - behavioral analyses

# Author: Davide Gheza
# Date: 5th May 2018

# Initial stuff
# dev.off() # clear plots
rm(list=ls()) # clear environment
cat("\014") # clear console
set.seed(42) # specify seed for RNG and ensure reproducible results
#load packages and install them if they're not
if (!require("pacman")) install.packages("pacman")
pacman::p_load(plyr, reshape2, Rmisc,ggplot2, ez, schoRsch, pastecs, MBESS,BayesFactor)
if (!require("gapminder")) install.packages("gapminder")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("Hmisc")) install.packages("Hmisc")
library(gapminder)
library(dplyr)
library(tidyverse)
library(Hmisc)

## Import data

wd <- ("C:/Users/gdavide/Documents/MATLAB/dg_workspace18/DoorGamblingEffort/Behavioral exp/")
setwd(wd) # set work directory
raw <- read.csv2("23.csv",header=TRUE,na.strings="NaN") # read data


as.factor(raw$Running.Trial.)
as.factor(raw$Subject)
as.factor(raw$Condition)


# filter out practice and instructions

expdata = raw[(raw$Running.Trial. %in% "ExperimentList"),]




## EffortResponse.ACC

#filter EffortTaskList

expdata.acc = expdata[(expdata$Running.SubTrial. %in% "EffortTaskList"),]

# sort by sbj and cond
expdata.acc = expdata.acc[order(expdata.acc$Subject, expdata.acc$Condition),] 

# subset 
expdata.acc = subset(expdata.acc, select = c(Subject, Condition, EffortResponse.ACC))

# set up data frame for by-group processing 
expdata.acc = group_by(expdata.acc, Condition)


# calculate the summary metrics
acc.sum = summarise(expdata.acc, 
#                     acc.count = count(EffortResponse.ACC),
                       acc.mean = mean(EffortResponse.ACC))


# plot accuracy with prob density

expdata.acc = expdata.acc[!(expdata.acc$Condition %in% c("32", "52")),] # filter out Condition = 32 52 (exclude conditions where effort task followed the rating)

acc.subj = group_by(expdata.acc, Condition, Subject) # set up data frame for by-group processing 

acc.subj = summarise(acc.subj,                      # calculate the summary metrics - mean for Subject*Condition
                    ACC.mean = mean(EffortResponse.ACC))

# plot expdata.acc

acc.subj$Condition = as.factor(acc.subj$Condition)

acc.subj$label = factor(acc.subj$Condition, 
                       labels=c("Easy", "Hard"))   


acc.subj %>%
  ggplot(aes(x = label, y = ACC.mean)) +
  geom_point() +
  geom_violin(mapping = NULL, data = NULL, stat = "ydensity",
              position = "dodge", draw_quantiles = NULL, trim = TRUE,
              scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=5) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))


## EffortResponse.RT


#filter EffortTaskList

expdata.RT = expdata[(expdata$Running.SubTrial. %in% "EffortTaskList"),]

# filter EffortResponse.ACC != 0 (exclude mistakes)

expdata.RT = expdata.RT[!(expdata.RT$EffortResponse.ACC %in% "0"),]


# sort by sbj and cond
expdata.RT = expdata.RT[order(expdata.RT$Subject, expdata.RT$Condition),] 

# subset 
expdata.RT = subset(expdata.RT, select = c(Subject, Condition, EffortResponse.RT))

# filter out Condition = 32 52 (exclude conditions where effort task followed the rating)
expdata.RT = expdata.RT[!(expdata.RT$Condition %in% c("32", "52")),]

# set up data frame for by-group processing 
RT.subj = group_by(expdata.RT, Condition, Subject)

# calculate the summary metrics - mean for Subject*Condition
RT.subj = summarise(RT.subj,
                         RT.mean = mean(EffortResponse.RT))

# calculate the summary metrics - mean for Condition (on within Subject average)
RT.sum = group_by(RT.subj, Condition)
RT.sum = summarise(RT.sum,
                   RT.mean = mean(RT.mean))


# plot RT

RT.subj$Condition = as.factor(RT.subj$Condition)

RT.subj$label = factor(RT.subj$Condition, 
                labels=c("Easy", "Hard"))  # excluded:  "EasyRating" , "HardRating"


RT.subj %>%
  ggplot(aes(x = label, y = RT.mean)) +
  geom_point() +
  geom_violin(mapping = NULL, data = NULL, stat = "ydensity",
            position = "dodge", draw_quantiles = NULL, trim = TRUE,
            scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) +
  stat_summary(fun.y=mean, geom="point", shape=15, size=5, color="red") +
  geom_jitter(shape=16, position=position_jitter(0.02), size=2) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))


## FB ratings

#filter FbRateProc

expdata.FbRate = expdata[(expdata$Procedure.SubTrial. %in% "FbRateProc"),]

# sort by sbj and cond
expdata.FbRate = expdata.FbRate[order(expdata.FbRate$Subject, expdata.FbRate$Condition),] 

# subset 
expdata.FbRate = subset(expdata.FbRate, select = c(Subject, Condition, FbRateXa, FbRateXf, FbRateXr))

# compute VAS as percentage (relative to pixel range)
expdata.FbRate$VASa = (expdata.FbRate$FbRateXa-316)/396*100
expdata.FbRate$VASf = (expdata.FbRate$FbRateXf-316)/396*100
expdata.FbRate$VASr = (expdata.FbRate$FbRateXr-316)/396*100

# long format: RateType as condition

expdata.FbRate = gather(expdata.FbRate, key = "RateType", value = "percent", VASa:VASr)


# set up data frame for by-group processing 
FbRate.subj = group_by(expdata.FbRate, Condition, Subject, RateType)

# calculate the summary metrics - mean for Subject*Condition*RateType
FbRate.subj = summarise(FbRate.subj,
                        VAS.mean = mean(percent))


# plot FB rating 

FbRate.subj$Condition = as.factor(FbRate.subj$Condition)

FbRate.subj$RateType = as.factor(FbRate.subj$RateType)

FbRate.subj$label = paste0(FbRate.subj$Condition, FbRate.subj$RateType)

FbRate.subj$label = factor(FbRate.subj$label, 
                       levels = c("31VASa", "51VASa", "31VASf", "51VASf", "31VASr", "51VASr","32VASa", "52VASa", "32VASf", "52VASf", "32VASr", "52VASr"),            # specify levels in the order to be plotted
                       labels=c("Easy reward A", "Hard reward A", "Easy reward F", "Hard reward F", "Easy reward R", "Hard reward R", "Easy no-reward A", "Hard no-reward A", "Easy no-reward F", "Hard no-reward F", "Easy no-reward R", "Hard no-reward R"))  


# reward

FbRate.subj.reward = FbRate.subj[(FbRate.subj$Condition %in% c("31", "51")),]

FbRate.subj.reward = group_by(FbRate.subj.reward, Condition, Subject, RateType)

FbRate.subj.reward %>%
  ggplot(aes(x = label, y = VAS.mean)) +
  geom_point() +
  geom_violin(mapping = NULL, data = NULL, stat = "ydensity",
              position = "dodge", draw_quantiles = NULL, trim = TRUE,
              scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) +
  stat_summary(fun.y=mean, geom="point", shape=4, size=10, color="black") +
  geom_jitter(shape=16, position=position_jitter(0.02), size=2) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))


# no-reward

FbRate.subj.reward = FbRate.subj[(FbRate.subj$Condition %in% c("32", "52")),]

FbRate.subj.reward = group_by(FbRate.subj.reward, Condition, Subject, RateType)

FbRate.subj.reward %>%
  ggplot(aes(x = label, y = VAS.mean)) +
  geom_point() +
  geom_violin(mapping = NULL, data = NULL, stat = "ydensity",
              position = "dodge", draw_quantiles = NULL, trim = TRUE,
              scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) +
  stat_summary(fun.y=mean, geom="point", shape=4, size=10, color="black") +
  geom_jitter(shape=16, position=position_jitter(0.02), size=2) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))



# stats on FBratings (single rating level)           IN PROGRESS - CREATE FACTOR EFFORT AND FACTOR VALENCE (POS NEG)

library(brms)
theme_set(theme_default())

fit_compact <- brm(power ~ 1 + trialsN + (1 + trialsN | snG), data = expdata.FbRate, cores = 8)
