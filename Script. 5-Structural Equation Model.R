## Load Libraries
library(lavaan)
library(lavaanPlot)
library(dplyr) 
library(tidyr)
library(knitr)
library(mvnormalTest)
library(tidySEM)
library(piecewiseSEM)
library(tidyverse)
library(readxl)
library(lme4)
library(performance)#for checking models
library(lmerTest)

#
##
###
#reading data
seeddata22 <- read_excel("data/2022-Beech_seedfate.xlsx")
str(seeddata22)
seeddata21 <- read_excel("data/2021-Beech_seedfate.xlsx")
str(seeddata21)

# 
# ##PREPARING DATA-----
#rescalling variables
seeddata21$Understory_scaled<- scale(seeddata21$Understory_coverage)[,1]
seeddata21$Mice_Plot_density_scaled <-  scale(seeddata21$Mice_Plot_density)[,1]
seeddata21$Vole_density_scaled <- scale(seeddata21$Vole_density)[,1]
seeddata21$Beech_ba_scaled <- scale(seeddata21$Beech_absolute_basal_area)[,1]
seeddata22$Understory_scaled<- scale(seeddata22$Understory_coverage)[,1]
seeddata22$Mice_Plot_density_scaled <-  scale(seeddata22$Mice_Plot_density)[,1]
seeddata22$Vole_density_scaled <- scale(seeddata22$Vole_density)[,1]
seeddata22$Beech_ba_scaled <- scale(seeddata22$Beech_absolute_basal_area)[,1]

#Creating column and datasets for removal, survival and dispersal analyses
remseeddata21<- seeddata21 %>% mutate(Removed=Fate!="I" & Fate!="PI")
surseeddata21 <- seeddata21 %>% filter(Fate!="M"& Fate!="PI") %>% 
  mutate(Survived=Fate!="P")
disseeddata21 <- seeddata21 %>% filter(Fate!="M"& Fate!="PI") %>% 
  mutate(Dispersed=Fate=="D")

remseeddata22<- seeddata22 %>% mutate(Removed=Fate!="I" & Fate!="PI")
surseeddata22 <- seeddata22 %>% filter(Fate!="M"& Fate!="PI") %>% 
  mutate(Survived=Fate!="P")
disseeddata22 <- seeddata22 %>% filter(Fate!="M"& Fate!="PI") %>% 
  mutate(Dispersed=Fate=="D")


##filtering only last date
remseeddata21_ld <- remseeddata21 %>% filter(Day_number==8)
surseeddata21_ld <- surseeddata21 %>% filter(Day_number==8)
disseeddata21_ld <- disseeddata21 %>% filter(Day_number==8)

remseeddata22_ld <- remseeddata22 %>% filter(Day_number==8)
surseeddata22_ld <- surseeddata22 %>% filter(Day_number==8)
disseeddata22_ld <- disseeddata22 %>% filter(Day_number==8)


#REMOVED SEEDS
rem2years <- rbind(remseeddata21_ld,remseeddata22_ld)
rem2years$Beech_seeds_perm2 <- as.numeric(rem2years$Beech_seeds_perm2)
rem2years$Beech_seeds_perm2[is.na(rem2years$Beech_seeds_perm2)] <- 0


remtable<- rem2years %>% #%>%filter(Removed=="TRUE") 
  group_by( Mice_Plot_density,Mice_Plot_density_scaled, Vole_density,Vole_density_scaled,
            Beech_seeds_perm2, Beech_absolute_basal_area, Beech_ba_scaled,
            Understory_coverage,Understory_scaled, Plot, Year, Site,
            id_seed_pile, .drop = FALSE ) %>% tally(Removed=="TRUE")

remtable2 <- (remtable) %>% mutate(N_removed_seeds=n) %>% 
  select(N_removed_seeds=n, Mice_Plot_density,Mice_Plot_density_scaled, Vole_density,
         Vole_density_scaled, Beech_seeds_perm2, Beech_absolute_basal_area, Beech_ba_scaled,
         Understory_coverage,Understory_scaled, Plot, Year, Site, id_seed_pile)


## SEED SURVIVAL

sur2years <- rbind(surseeddata21_ld,surseeddata22_ld)

surtable<- sur2years %>% #%>%filter(suroved=="TRUE") 
  group_by( Mice_Plot_density,Mice_Plot_density_scaled, Vole_density,Vole_density_scaled,
            Beech_seeds_perm2, Beech_absolute_basal_area, Beech_ba_scaled,
            Understory_coverage,Understory_scaled, Plot, Year, Site,
            id_seed_pile, .drop = FALSE ) %>% tally(Survived=="TRUE")

surtable2 <- (surtable) %>% mutate(N_alive_seeds=n) %>%  
  select(N_alive_seeds=n, Mice_Plot_density,Mice_Plot_density_scaled, Vole_density,
         Vole_density_scaled, Beech_seeds_perm2, Beech_absolute_basal_area, Beech_ba_scaled,
         Understory_coverage,Understory_scaled, Plot, Year, Site, id_seed_pile)

## SEED Dispersal

dis2years <- rbind(disseeddata21_ld,disseeddata22_ld)

distable<- dis2years %>% #%>%filter(Disoved=="TRUE") 
  group_by( Mice_Plot_density,Mice_Plot_density_scaled, Vole_density,Vole_density_scaled,
            Beech_seeds_perm2, Beech_absolute_basal_area, Beech_ba_scaled,
            Understory_coverage,Understory_scaled, Plot, Year, Site,
            id_seed_pile, .drop = FALSE ) %>% tally(Dispersed=="TRUE")

distable2 <- (distable) %>% mutate(N_Dispersed_seeds=n) %>%
  select(N_Dispersed_seeds=n, Mice_Plot_density,Mice_Plot_density_scaled, Vole_density,
         Vole_density_scaled, Beech_seeds_perm2, Beech_absolute_basal_area, Beech_ba_scaled,
         Understory_coverage,Understory_scaled, Plot, Year, Site, id_seed_pile)

###MEGA GENERAL MODEL

Allfatetable <- as.data.frame(remtable2) %>% mutate(N_alive_seeds=surtable2$N_alive_seeds)%>%
  mutate (N_Dispersed_seeds=distable2$N_Dispersed_seeds)

#non-scaled varaibles

# ##testing data for assumptions
# # mvnout <-mardia(Allfatetable)
# # mvnout$uv.shapiro
# # mvnout$mv.test
# 
# #fitting model
# ## Model specification
GGmodel <- '
# regressions
    N_removed_seeds ~ Understory_coverage + Mice_Plot_density + Beech_absolute_basal_area + Vole_density +  Year
    N_alive_seeds ~ Understory_coverage + Mice_Plot_density + Beech_absolute_basal_area + Vole_density +  Year + N_removed_seeds + N_Dispersed_seeds
    N_Dispersed_seeds ~ Understory_coverage + Mice_Plot_density + Beech_absolute_basal_area + Vole_density +  Year + N_removed_seeds
    Mice_Plot_density ~ Beech_absolute_basal_area + Understory_coverage + Year + Vole_density
    Vole_density ~ Beech_absolute_basal_area + Understory_coverage + Year
    Beech_absolute_basal_area ~ Understory_coverage
# variance (optional)
N_removed_seeds ~~ id_seed_pile
N_alive_seeds ~~ id_seed_pile
N_Dispersed_seeds ~~ id_seed_pile
Vole_density ~~ Site
Mice_Plot_density ~~ Site'
GGfit.mod <- sem(GGmodel, data=Allfatetable, std.lv = TRUE, #estimator = "LM"
)
summary(GGfit.mod)
standardizedsolution(GGfit.mod) %>% filter(op == "~")
# parameterEstimates(GGfit.mod)


lavaanPlot(name = "plot", model=GGfit.mod, edge_options = list(color = "grey"), coefs = T, stand=T, sig=0.1, stars="regress")


## Model specification
GGmodel2 <- '
# regressions
    N_removed_seeds ~ Understory_scaled + Mice_Plot_density_scaled + Beech_ba_scaled + Vole_density_scaled +  Year
    N_alive_seeds ~ Understory_scaled + Mice_Plot_density_scaled + Beech_ba_scaled + Vole_density_scaled +  Year
    N_Dispersed_seeds ~ Understory_scaled + Mice_Plot_density_scaled + Beech_ba_scaled + Vole_density_scaled +  Year
    Mice_Plot_density_scaled ~ Beech_ba_scaled + Understory_scaled  + Vole_density_scaled
    Vole_density_scaled ~ Beech_ba_scaled + Understory_scaled
    Understory_scaled ~Beech_ba_scaled 
    # N_alive_seeds ~ N_removed_seeds
    # N_Dispersed_seeds ~ N_removed_seeds
# variance (optional)
N_removed_seeds ~~ id_seed_pile
N_alive_seeds ~~ id_seed_pile
N_Dispersed_seeds ~~ id_seed_pile
Vole_density_scaled ~~ Site
Mice_Plot_density_scaled ~~ Site'
GGfit.mod2 <- sem(GGmodel2, data=Allfatetable, std.lv = TRUE, #estimator = "LM"
)
summary(GGfit.mod2)
standardizedsolution(GGfit.mod2) %>% filter(op == "~")
parameterEstimates(GGfit.mod2)


lavaanPlot(name = "plot", model=GGfit.mod2, edge_options = list(color = "grey"), coefs = T, stand=T, sig=0.06, stars="regress")

#creating model for plotting only with significant results p<0.1
  
  GGplotmodel <- '
  # regressions
      N_removed_seeds ~ Understory_scaled + Mice_Plot_density_scaled + Beech_ba_scaled +  Year
      N_alive_seeds ~   Understory_scaled + Mice_Plot_density_scaled + Beech_ba_scaled +  Year
      N_Dispersed_seeds ~                   Mice_Plot_density_scaled + Beech_ba_scaled +  Year
      Vole_density_scaled ~ Understory_scaled 
      #Beech_ba_scaled ~ Understory_scaled
  # variance (optional)
  N_removed_seeds ~~ id_seed_pile
  N_alive_seeds ~~ id_seed_pile
  N_Dispersed_seeds ~~ id_seed_pile
  Vole_density_scaled ~~ Site
  Mice_Plot_density_scaled ~~ Site
  Vole_density_scaled ~~ Year
  Mice_Plot_density_scaled ~~ Year
  Understory_scaled ~~ Site
  '
  GGplotmodel.fit <- sem(GGplotmodel, data=Allfatetable, std.lv = TRUE, estimator = "MLM"
  )
  summary(GGplotmodel.fit)
  standardizedsolution(GGplotmodel.fit) %>% filter(op == "~")
  #parameterEstimates(GGplotmodel.fit)
  lavaanPlot(name = "plot", model=GGplotmodel.fit, edge_options = list(color = "grey"), coefs = T, stand=T, sig=0.05, stars="regress")


