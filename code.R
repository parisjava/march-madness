################################################
######## MARCH MADNESS MACHINE LEARNING ########
################################################

#### Prep ####

#Load packages
library(dplyr)
library(ggplot2)
library(neuralnet) #You may need to install this
library(randomForest) #You may need to install this

#Set working directory
#setwd("your working directory")

#Load data
reg = read.csv("RegularSeasonDetailedResults.csv")
sample = read.csv("sample_submission.csv")
seasons = read.csv("Seasons.csv")
teams = read.csv("Teams.csv")
tourneyresults = read.csv("TourneyDetailedResults.csv")
seeds = read.csv("TourneySeeds.csv")
slots = read.csv("TourneySlots.csv")

#Add columns
seeds$seedNum = as.numeric(substring(seeds$Seed,2,3))

tourneyresults = tourneyresults %>% 
  inner_join(seeds, by=c("Season"="Season","Wteam"="Team")) %>% 
  rename(WSeed = Seed, WseedNum = seedNum) %>% 
  inner_join(seeds, by=c("Season"="Season","Lteam"="Team")) %>% 
  rename(LSeed = Seed, LseedNum = seedNum)

reg[["WReboundPct"]] = round(with(reg,(Wor+Wdr)/(Wor+Wdr+Lor+Ldr)),4)
reg[["LReboundPct"]] = round(with(reg,(Lor+Ldr)/(Wor+Wdr+Lor+Ldr)),4)

reg[["WPctofShots3pt"]] = round(with(reg,Wfga3/(Wfga+Wfga3)),4)
reg[["LPctofShots3pt"]] = round(with(reg,Lfga3/(Lfga+Lfga3)),4)

reg[["W3pPct"]] = round(with(reg,Wfgm3/Wfga3),4)
reg[["L3pPct"]] = round(with(reg,Lfgm3/Lfga3),4)

reg[["WPtsPerFGA"]] = round(with(reg,(Wfgm*2+Wfgm3)/Wfga),2)
reg[["LPtsPerFGA"]] = round(with(reg,(Lfgm*2+Lfgm3)/Lfga),2)

reg[["WAstTurnRatio"]] = round(with(reg,Wast/Wto),4)
reg[["LAstTurnRatio"]] = round(with(reg,Last/Lto),4)

reg[["Wstlblk"]] = with(reg,Wstl+Wblk)
reg[["Lstlblk"]] = with(reg,Lstl+Lblk)

reg[["Wdiff"]] = with(reg,Wscore-Lscore)
reg[["Ldiff"]] = with(reg,Lscore-Wscore)



#Plot relations between stats and point differential
rel = ggplot() + 
  geom_smooth(data=ptdiff.r, aes(x=ReboundPct,y=Diff,color="black")) + 
  geom_smooth(data=ptdiff.r, aes(x=Pct3p,y=Diff,color="blue")) + 
  geom_smooth(data=ptdiff.r, aes(x=PtsPerFGA,y=Diff,color="orange")) + 
  geom_smooth(data=ptdiff.r, aes(x=AstTurnRatio,y=Diff,color="red")) + 
  coord_cartesian(xlim=c(0,2)) + 
  theme(panel.background = element_rect(fill="white")) +
  labs(x="Statistic",
       y="Point differential",
       title="Correlation of Various Metrics to Point Differential") + 
  scale_colour_manual(name = "Metric", 
                      values =c("black"="black","blue"="blue","orange"="orange","red"="red"), 
                      labels = c("Rebound%","3-point%","Points/FGA","Assists:Turnovers"))


