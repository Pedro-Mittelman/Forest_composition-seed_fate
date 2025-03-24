#SEED FATE GENERAL MODELS
#
##
###
#packages
library(tidyverse)
library(readxl)
library(lme4)
library(performance)#for checking models
library(lmerTest)
library(ggpubr)
library(modelsummary)
library(DHARMa)


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


##Setting  variables that are factors
seeddata21$Year <- as_factor(seeddata21$Year)
seeddata21$Plot <- as_factor(seeddata21$Plot)
seeddata21$Site <- as_factor(seeddata21$Site)
seeddata21$Seed_Pile <- as_factor(seeddata21$Seed_Pile)
seeddata21$id_seed_pile <- as_factor(seeddata21$id_seed_pile)
seeddata21$plot_type <- as.factor(seeddata21$plot_type)

seeddata22$Year <- as_factor(seeddata22$Year)
seeddata22$Plot <- as_factor(seeddata22$Plot)
seeddata22$Site <- as_factor(seeddata22$Site)
seeddata22$Seed_Pile <- as_factor(seeddata22$Seed_Pile)
seeddata22$id_seed_pile <- as_factor(seeddata22$id_seed_pile)
seeddata22$plot_type <- as.factor(seeddata22$plot_type)

#Creating northern and Southern Sites colun
seeddata21 <- seeddata21 %>%  mutate (Site_location= if_else(seeddata21$Site=="4"|seeddata21$Site=="3","Southern","Northern"))
seeddata22 <- seeddata22 %>%  mutate (Site_location= if_else(seeddata22$Site=="4"|seeddata22$Site=="3","Southern","Northern"))



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



#RUNNING GENERAL MODELS

#HARVESTING----
#putting the two years together
rem2years <- rbind(remseeddata21_ld,remseeddata22_ld)

#checking data
remtable<- rem2years %>% #%>%filter(Removed=="TRUE") 
  group_by( Mice_Plot_density, Beech_absolute_basal_area, 
  Understory_coverage, Plot, Year, .drop = FALSE ) %>% tally(Removed=="TRUE")
REMTABLE2X <-rem2years %>% group_by(  plot_type, .drop = FALSE ) %>% tally(Removed=="TRUE")


### RUNNING SEED HARVESTING MODELS

#null model without any forest composition variable
nullremmodel<- glmer(Removed ~ Understory_coverage+ Mice_Plot_density+ Year +
                       (1|Site) + (1|id_seed_pile),
                     data=rem2years, family=binomial)
summary(nullremmodel)

#with beech basal area as forest composition variable
mremgen.be<- glmer(Removed ~ Beech_absolute_basal_area + Understory_coverage+
                        Mice_Plot_density+ Year +(1|Site) + (1|id_seed_pile),
                      data=rem2years, family=binomial)
summary(mremgen.be)
##RESULT:
#ALL VARIABLES INFLUENCED SEED FATE

##verifying model
testResiduals(mremgen.be)#everything ok

#exporting results
modelsummary::modelsummary(mremgen.be, statistic = c("std.error", "statistic", "p.value"), shape=term ~ model + statistic, "rem_model.xlsx")


#with Norway Spruce basal area as variable
mremaba2yaxby2<- glmer(Removed ~ Beech_absolute_basal_area + Norway_spruce_basal_area + 
                         Understory_coverage+ Mice_Plot_density+ Year +(1|Site) + (1|id_seed_pile),
                     data=rem2years, family=binomial)
summary(mremaba2yaxby2)

#with Douglas-fir basal area as variable
mremaba2yaxby3<- glmer(Removed ~ Beech_absolute_basal_area + Douglas_fir_basal_area + 
                         Understory_coverage+ Mice_Plot_density+ Year +(1|Site) + (1|id_seed_pile),
                     data=rem2years, family=binomial)
summary(mremaba2yaxby3)
##RESULT:
#Neither DOuglas fir or Norway spruce basal areas improved the models


anova(nullremmodel,mremgen.be,mremaba2yaxby2,mremaba2yaxby3 )
capture.output(anova(nullremmodel,mremgen.be,mremaba2yaxby2,mremaba2yaxby3 ), file="anovaremmodels.txt")



#SURVIVAL-----
#putting years together
sur2years <- rbind(surseeddata21_ld,surseeddata22_ld)

#checking data
surtable<- sur2years %>% #%>%filter(Removed=="TRUE") 
  group_by( Mice_Plot_density, Beech_absolute_basal_area, 
            Understory_coverage, Plot, Year, .drop = FALSE ) %>% tally(Survived=="TRUE")

## RUNNING SEED SURVIVAL MODELS

#null model without any forest composition variable
nullsurmodel<- glmer(Survived ~ Understory_coverage+ Mice_Plot_density+ Year +
                       (1|Site) + (1|id_seed_pile),
                     data=sur2years, family=binomial)
summary(nullsurmodel)

#with beech basal area as forest composition variable
msurgen.be<- glmer(Survived ~ Beech_absolute_basal_area + Understory_coverage+
                     Mice_Plot_density+ Year +(1|Site) + (1|id_seed_pile),
                   data=sur2years, family=binomial)
summary(msurgen.be)
##RESULT:
#ALL VARIABLES INFLUENCED SEED FATE

##verifying model
testResiduals(msurgen.be)#everything ok

#exporting results
modelsummary::modelsummary(msurgen.be, statistic = c("std.error", "statistic", "p.value"), shape=term ~ model + statistic, "sur_model.xlsx")


#with Norway Spruce basal area as variable
msuraba2yaxby2<- glmer(Survived ~ Beech_absolute_basal_area + Norway_spruce_basal_area + 
                         Understory_coverage+ Mice_Plot_density+ Year +(1|Site) + (1|id_seed_pile),
                       data=sur2years, family=binomial)
summary(msuraba2yaxby2)

#with Douglas-fir basal area as variable
msuraba2yaxby3<- glmer(Survived ~ Beech_absolute_basal_area + Douglas_fir_basal_area + 
                         Understory_coverage+ Mice_Plot_density+ Year +(1|Site) + (1|id_seed_pile),
                       data=sur2years, family=binomial)
summary(msuraba2yaxby3)
##RESULT:
#Neither DOuglas fir or Norway spruce basal areas improved the models


anova(nullsurmodel,msurgen.be,msuraba2yaxby2,msuraba2yaxby3)
#exporting last results
capture.output(anova(nullsurmodel,msurgen.be,msuraba2yaxby2,msuraba2yaxby3), file="anovasurmodels.txt")




#CACHING -----
dis2years <- rbind(disseeddata21_ld,disseeddata22_ld)

#checking data
distable<- dis2years %>% #%>%filter(Disoved=="TRUE") 
  group_by( Mice_Plot_density, Beech_absolute_basal_area, 
            Understory_coverage, Plot, Year, Site, id_seed_pile, .drop = FALSE ) %>% tally(Dispersed=="TRUE")

## RUNNING SEED CACHING MODELS

#null model without any forest composition variable
nulldismodel<- glmer(Dispersed ~ Understory_coverage+ Mice_Plot_density+ Year +
                        +(1|Site/Seed_Pile),
                     data=dis2years, family=binomial)
summary(nulldismodel)

#with beech basal area as forest composition variable
mdisgen.be<- glmer(Dispersed ~ Beech_absolute_basal_area + Understory_coverage+
                     Mice_Plot_density+ Year +(1|Site/Seed_Pile),
                   data=dis2years, family=binomial)
summary(mdisgen.be)
##RESULT:
#ALL VARIABLES INFLUENCED SEED FATE, except Understory

##verifying model
testResiduals(mdisgen.be)#everything ok

#exporting results
modelsummary::modelsummary(mdisgen.be, statistic = c("std.error", "statistic", "p.value"), shape=term ~ model + statistic, "dis_model.xlsx")


#with Norway Spruce basal area as variable
mdisaba2yaxby2<- glmer(Dispersed ~ Beech_absolute_basal_area + Norway_spruce_basal_area + 
                         Understory_coverage+ Mice_Plot_density+ Year +(1|Site/Seed_Pile),
                       data=dis2years, family=binomial)
summary(mdisaba2yaxby2)

#with Douglas-fir basal area as variable
mdisaba2yaxby3<- glmer(Dispersed ~ Beech_absolute_basal_area + Douglas_fir_basal_area + 
                         Understory_coverage+ Mice_Plot_density+ Year +(1|Site/Seed_Pile),
                       data=dis2years, family=binomial)
summary(mdisaba2yaxby3)
##RESULT:
#Neither DOuglas fir or Norway spruce basal areas improved the models


anova(nulldismodel,mdisgen.be,mdisaba2yaxby2,mdisaba2yaxby3)
#exporting last results
capture.output(anova(nulldismodel,mdisgen.be,mdisaba2yaxby2,mdisaba2yaxby3), file="anovadismodels.txt")


##CHECKING EFFECT OF VOLE DENSITIES ON SEED FATE----

##adding vole densities
#seed harvesting

mremaba2yaxv<- glmer(Removed ~ Beech_absolute_basal_area + Understory_coverage+ Mice_Plot_density+ Vole_density + (1|Year/Site)+(1|id_seed_pile),
                    data=rem2years, family=binomial)
veffecrem<- summary(mremaba2yaxv)
veffecrem$coefficients["Vole_density",]

## seed survival

msuraba2yav<- glmer(Survived ~ Beech_absolute_basal_area  +  Understory_coverage +Mice_Plot_density + Vole_density +(1|Year/Site)+(1|id_seed_pile),
                   data=sur2years, family=binomial)
veffecsur<-summary(msuraba2yav)
veffecsur$coefficients["Vole_density",]
## seed caching

mdisaba2yav<- glmer(Dispersed ~ Beech_absolute_basal_area  +  Understory_coverage +Mice_Plot_density + Vole_density + (1|Year/Site)+(1|id_seed_pile),
                   data=dis2years, family=binomial)
veffecdis<-summary(mdisaba2yav)
veffecdis$coefficients["Vole_density",]
#NO EFFECTS OF VOLES ON SEED FATE