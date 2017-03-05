#Load packages
library(dplyr)
library(neuralnet)
library(randomForest)

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