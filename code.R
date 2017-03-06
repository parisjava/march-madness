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

#Define functions
lloc = function(wloc) {
  if (wloc == "N") {
    l="N"
  } else if (wloc == "H") {
    l="A"
  } else {
    l="H"
  }
  return(l)
}

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

reg[["Lloc"]] = lapply(reg[["Wloc"]], lloc)

reg[["Wwin"]] = 1
reg[["Lwin"]] = 0

reg[["Wreb"]] = with(reg,Wor+Wdr)
reg[["Lreb"]] = with(reg,Lor+Ldr)

#Reorganize data
ptdiff.w = reg %>% 
  select(Season, Team = Wteam, Opp = Lteam, Daynum, Loc = Wloc, OT = Numot,
         Diff = Wdiff, W = Wwin, Score = Wscore, OppScore = Lscore,
         FGM = Wfgm, FGA = Wfga, FGM3 = Wfgm3, FGA3 = Wfga3, FTM = Wftm, FTA = Wfta,
         Reb = Wreb, OReb = Wor, DReb = Wdr, Assists = Wast, Turnovers = Wto, 
         Steals = Wstl, Blocks = Wblk, Fouls = Wpf,
         ReboundPct = WReboundPct, PctofShots3pt = WPctofShots3pt, Pct3p = W3pPct, PtsPerFGA = WPtsPerFGA, 
         AstTurnRatio = WAstTurnRatio, StlBlk = Wstlblk,
         OppFGM = Lfgm, OppFGA = Lfga, OppFGM3 = Lfgm3, OppFGA3 = Lfga3, OppFTM = Lftm, OppFTA = Lfta, 
         OppReb = Lreb, OppOReb = Lor, OppDReb = Ldr, OppAssists = Last, OppTurnovers = Lto, 
         OppSteals = Lstl, OppBlocks = Lblk, OppFouls = Lpf,
         OppReboundPct = LReboundPct, OppPctofShots3pt = LPctofShots3pt, OppPct3p = L3pPct, OppPtsPerFGA = LPtsPerFGA, 
         OppAstTurnRatio = LAstTurnRatio, OppStlBlk = Lstlblk)
ptdiff.l = reg %>% 
  select(Season, Team = Lteam, Opp = Wteam, Daynum, Loc = Lloc, OT = Numot,
         Diff = Ldiff, W = Lwin, Score = Lscore, OppScore = Wscore,
         FGM = Lfgm, FGA = Lfga, FGM3 = Lfgm3, FGA3 = Lfga3, FTM = Lftm, FTA = Lfta,
         Reb = Lreb, OReb = Lor, DReb = Ldr, Assists = Last, Turnovers = Lto, 
         Steals = Lstl, Blocks = Lblk, Fouls = Lpf,
         ReboundPct = LReboundPct, PctofShots3pt = LPctofShots3pt, Pct3p = L3pPct, PtsPerFGA = LPtsPerFGA, 
         AstTurnRatio = LAstTurnRatio, StlBlk = Lstlblk,
         OppFGM = Wfgm, OppFGA = Wfga, OppFGM3 = Wfgm3, OppFGA3 = Wfga3, OppFTM = Wftm, OppFTA = Wfta, 
         OppReb = Wreb, OppOReb = Wor, OppDReb = Wdr, OppAssists = Wast, OppTurnovers = Wto, 
         OppSteals = Wstl, OppBlocks = Wblk, OppFouls = Wpf,
         OppReboundPct = WReboundPct, OppPctofShots3pt = WPctofShots3pt, OppPct3p = W3pPct, OppPtsPerFGA = WPtsPerFGA, 
         OppAstTurnRatio = WAstTurnRatio, OppStlBlk = Wstlblk)

ptdiff = rbind(ptdiff.w, ptdiff.l)
ptdiff.r = filter(ptdiff,Season>=2014) #Only recent data

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


