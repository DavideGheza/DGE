# DoorGablingEffort - behavioral analyses

# Author: Davide Gheza
# Date: 25th October 2018

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
if (!require("BayesFactor")) install.packages("BayesFactor")
if (!require("reshape")) install.packages("reshape")
library(gapminder)
library(dplyr)
library(tidyverse)
library(Hmisc)
library(reshape)

## Import data

wd <- ("C:/Users/gdavide/Documents/MATLAB/dg_workspace18/DoorGamblingEffort/Behavioral exp/")
setwd(wd) # set work directory
raw <- read.csv2("23.csv",header=TRUE,na.strings="NaN") # read data


as.factor(raw$Running.Trial.)
as.factor(raw$Subject)
as.factor(raw$Condition)


# filter out practice and instructions

expdata = raw[(raw$Running.Trial. %in% "ExperimentList"),]




## Doors.RESP - Door selection

# filter single entries of Doors.RESP
expdata.doorresp = expdata[!(expdata$SubTrial %in% c(2:10)),]

# set up data frame for by-group processing 
doorresp.subj = group_by(expdata.doorresp, Subject, Doors.RESP)

# calculate the summary metrics: count of door selection by sbj and door
doorresp.subj = summarise(doorresp.subj, 
                    doorresp.count = n()
                    )
# wide format
doorresp.subj = cast(doorresp.subj, Subject ~ Doors.RESP, value = "doorresp.count")




## RateX - Effort Task ratings

# filter RatingList
expdata.ETR = expdata[(expdata$Running.SubTrial. %in% "RatingList"),]

# compute VAS as percentage (relative to pixel range)
expdata.ETR$RateX = (expdata.ETR$RateX-316)/396*100

# set up data frame for by-group processing (RatingList = list of questions rated)
ETR.subj = group_by(expdata.ETR, Subject, RatingList)

# calculate the summary metrics: average of RateX by sbj and question
ETR.subj = summarise(ETR.subj, 
                         RateX.mean = mean(RateX)
                    )
# wide format
ETR.wide = cast(ETR.subj, Subject ~ RatingList, value = "RateX.mean")


# plot ETR

# whole question: labels=expdata.ETR$text[1:8]

# create variables defining question type and effort level
ETR.subj$qtype = rep(c("moeilik", "aangenaam", "gedreven om correct", "leuk om correct"), each = 2)
ETR.subj$qtype = factor(ETR.subj$qtype,                                                              # force order
                        levels = c("moeilik", "aangenaam", "gedreven om correct", "leuk om correct"))
ETR.subj$effort = rep(c("easy", "hard"))

# plot violin
ETR.subj %>%
  ggplot(aes(x = effort, y = RateX.mean)) +
  geom_point() +
  geom_violin(mapping = NULL, data = NULL, stat = "ydensity",
              position = "dodge", draw_quantiles = NULL, trim = TRUE,
              scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=5) +
  geom_jitter(shape=16, position=position_jitter(0.02), size=1) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16)) +
  facet_grid(. ~ qtype)

# plot geom line between effort levels for each sbj
ETR.subj %>%
  ggplot(aes(x = effort, y = RateX.mean, group = Subject)) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16)) +
  geom_line() +
  facet_grid(. ~ qtype)



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

# add FBrate repetition n. (note: 12 rating in behavioral exp. only 8 in EEG exp)

expdata.FbRate$rateRep = rep((1:12), times = ((length(unique(expdata.FbRate$Subject)))*
                                               (length(unique(expdata.FbRate$Condition))))
                             )

# compute VAS as percentage (relative to pixel range)
expdata.FbRate$VASa = (expdata.FbRate$FbRateXa-316)/396*100
expdata.FbRate$VASf = (expdata.FbRate$FbRateXf-316)/396*100
expdata.FbRate$VASr = (expdata.FbRate$FbRateXr-316)/396*100

# invert score for Frustrerend

expdata.FbRate$VASf = 100-expdata.FbRate$VASf

# long format: RateType as condition

expdata.FbRate = gather(expdata.FbRate, key = "RateType", value = "percent", VASa:VASr)
expdata.FbRate = subset(expdata.FbRate, select = c(Subject, Condition, rateRep, RateType, percent)) # drop raw rates

# create factor outcome

expdata.FbRate$outcome[(expdata.FbRate$Condition %in% c("31","51"))] = "reward"
expdata.FbRate$outcome[(expdata.FbRate$Condition %in% c("32","52"))] = "noreward"

expdata.FbRate$outcome = as.factor(expdata.FbRate$outcome)

# create factor effortlevel

expdata.FbRate$efflev[(expdata.FbRate$Condition %in% c("31","32"))] = "low"
expdata.FbRate$efflev[(expdata.FbRate$Condition %in% c("51","52"))] = "high"

expdata.FbRate$efflev = as.factor(expdata.FbRate$efflev)




### inspect single subject ratings ###

expdata.FbRate %>% 
  mutate(label = paste0(expdata.FbRate$Condition, expdata.FbRate$RateType)) %>%  # create label defining single levels by RateType*Outcome*Effort
  filter(Subject == 1) %>%                                                        # filter sbj n
  ggplot(aes(x = label, y = percent)) +
    geom_point() +
    geom_violin(mapping = NULL, data = NULL, stat = "ydensity",
                position = "dodge", draw_quantiles = NULL, trim = TRUE,
                scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) +
    stat_summary(fun.y=mean, geom="point", shape=4, size=10, color="black") +
    geom_jitter(shape=16, position=position_jitter(0.02), size=2) +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 16))  





############# Exporting FB rating - sbj level AVERAGES by Condition and RateType #############

# set up data frame for by-group processing 
FbRate.subj = group_by(expdata.FbRate, Condition, Subject, RateType)

# calculate the summary metrics - mean for Subject*Condition*RateType
FbRate.subj = summarise(FbRate.subj,
                        VAS.mean = mean(percent))

# save out expdata.FbRate (with inverted score for Frustrerend)
  # wide format
  FbRate.subj$widecond = paste0(FbRate.subj$Condition, FbRate.subj$RateType)
  FbRate.subj.wide = cast(FbRate.subj, Subject ~ widecond, value = "VAS.mean")
  # write.csv
  # write.csv(FbRate.subj.wide, file = "23_behavioral_FBrate_summarised_invertedFrustrerend.csv")

# log transform for non-normal distributed data

# ## set negative values as 0.1 (out of scale ratings)
# FbRate.subj$VAS.mean[FbRate.subj$VAS.mean<0] = 0.1
# ## log transform
# FbRate.subj$VAS.mean = log(FbRate.subj$VAS.mean,base= exp(10))


############# plotting FB rating - sbj level averages #############

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

FbRate.subj.noreward = FbRate.subj[(FbRate.subj$Condition %in% c("32", "52")),]

FbRate.subj.noreward = group_by(FbRate.subj.noreward, Condition, Subject, RateType)

FbRate.subj.noreward %>%
  ggplot(aes(x = label, y = VAS.mean)) +
  geom_point() +
  geom_violin(mapping = NULL, data = NULL, stat = "ydensity",
              position = "dodge", draw_quantiles = NULL, trim = TRUE,
              scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) +
  stat_summary(fun.y=mean, geom="point", shape=4, size=10, color="black") +
  geom_jitter(shape=16, position=position_jitter(0.02), size=2) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

########################################################################################


# stats on FBratings (single rating level)           IN PROGRESS - CREATE FACTOR EFFORT AND FACTOR reward ///VALENCE (POS NEG)


########## Bayesian model comparison ##############

library(BayesFactor)

num.iter=10000 # number of MonteCarlo iterations (default: 10000)

# as factor
expdata.FbRate$Subject = as.factor(expdata.FbRate$Subject)
expdata.FbRate$rateRep = as.factor(expdata.FbRate$rateRep)
expdata.FbRate$RateType = as.factor(expdata.FbRate$RateType)

# Assuming a medium Cauchy prior d~Cauchy(0,.707):
m.null=lmBF(percent ~ 1 + Subject + RateType + rateRep,
            data=expdata.FbRate,iterations=num.iter,whichRandom=c("Subject","RateType","rateRep"),rscaleRandom="nuisance",rscaleFixed=.707)
m.outcome=lmBF(percent ~ Subject + RateType + rateRep + outcome,
               data=expdata.FbRate,iterations=num.iter,whichRandom=c("Subject","RateType","rateRep"),rscaleRandom="nuisance",rscaleFixed=.707)
m.efflev=lmBF(percent ~ Subject + RateType + rateRep + efflev,
                data=expdata.FbRate,iterations=num.iter,whichRandom=c("Subject","RateType","rateRep"),rscaleRandom="nuisance",rscaleFixed=.707)
m.maineffects=lmBF(percent ~ Subject + RateType + rateRep + outcome + efflev,
                   data=expdata.FbRate,iterations=num.iter,whichRandom=c("Subject","RateType","rateRep"),rscaleRandom="nuisance",rscaleFixed=.707)
m.interaction=lmBF(percent ~ Subject + RateType + rateRep + outcome * efflev,
                   data=expdata.FbRate,iterations=num.iter,whichRandom=c("Subject","RateType","rateRep"),rscaleRandom="nuisance",rscaleFixed=.707)

m.outcome/m.null
m.efflev/m.null
m.maineffects/m.null
m.interaction/m.null

m.interaction/m.maineffects
m.interaction/m.efflev
m.interaction/m.outcome
m.interaction/m.null

# --> the interaction model is the best one. This interaction can be interpreted as 
# "Participant rated the reward FB as more pleasant when they anticipated high vs low cognitive effort, 
# while they rated the no-reward FB as more pleasant when they anticipated low vs high cognitive effort

# --actually, for claims at each level or Reward, we need to first run a t-test within reward level...

chains = posterior(m.interaction, iter=10000)
summary(chains)

plot(m.interaction, include1=FALSE, addDenom = FALSE)
?plot

# same as lmBF 
BFbehav = anovaBF(percent~outcome*efflev + Subject + RateType + rateRep, data=expdata.FbRate, whichRandom=c("Subject", "RateType", "rateRep"))
# here we add Subject:RateType interaction as Nuisance
BFbehav2 = anovaBF(percent~outcome*efflev + Subject + RateType + Subject:RateType + rateRep, data=expdata.FbRate, whichRandom=c("Subject", "RateType", "Subject:RateType" , "rateRep"))
# here we specify Subject:rateRep:RateType interaction as Nuisance
BFbehav3 = anovaBF(percent~outcome*efflev + Subject + RateType + Subject:RateType + Subject:RateType:rateRep, data=expdata.FbRate, whichRandom=c("Subject", "RateType", "Subject:RateType", "Subject:RateType:rateRep"))

BayesFactor::plot()





## Specifying more random effects
# 1) Subject*RateType = random intercept for Subject, random effect of RateType, random effect of RateType for each sbj
# 2) Subject:RateType:rateRep = random variability of rate across RateType, for each sbj


# Assuming a medium Cauchy prior d~Cauchy(0,.707):
m.null=lmBF(percent ~ 1 + Subject*RateType + Subject:RateType:rateRep,
            data=expdata.FbRate,iterations=num.iter,whichRandom=c("Subject*RateType","Subject:RateType:rateRep"),rscaleRandom="nuisance",rscaleFixed=.707)
m.outcome=lmBF(percent ~ Subject*RateType + Subject:RateType:rateRep + outcome,
               data=expdata.FbRate,iterations=num.iter,whichRandom=c("Subject*RateType","Subject:RateType:rateRep"),rscaleRandom="nuisance",rscaleFixed=.707)
m.efflev=lmBF(percent ~ Subject*RateType + Subject:RateType:rateRep + efflev,
              data=expdata.FbRate,iterations=num.iter,whichRandom=c("Subject*RateType","Subject:RateType:rateRep"),rscaleRandom="nuisance",rscaleFixed=.707)
m.maineffects=lmBF(percent ~ Subject*RateType + Subject:RateType:rateRep + outcome + efflev,
                   data=expdata.FbRate,iterations=num.iter,whichRandom=c("Subject*RateType","Subject:RateType:rateRep"),rscaleRandom="nuisance",rscaleFixed=.707)
m.interaction=lmBF(percent ~ Subject*RateType + Subject:RateType:rateRep + outcome * efflev,
                   data=expdata.FbRate,iterations=num.iter,whichRandom=c("Subject*RateType","Subject:RateType:rateRep"),rscaleRandom="nuisance",rscaleFixed=.707)

m.outcome/m.null
m.efflev/m.null
m.maineffects/m.null
m.interaction/m.null

m.interaction/m.maineffects
m.interaction/m.efflev
m.interaction/m.outcome
m.interaction/m.null

# --> the interaction model is the best one. 
