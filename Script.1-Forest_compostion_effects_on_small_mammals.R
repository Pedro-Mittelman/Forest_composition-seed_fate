#BEECH BASAL AREA ANALYSES 2021 AND 2022
#Loading and cleaning Data
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
covariables_plot <- read_excel("Data/Plot_info_and_covariables.xlsx")
str(covariables_plot)

#Putting variables as factors
covariables_plot$Plot <- as.factor(covariables_plot$Plot)
covariables_plot$Plot_type <- as.factor(covariables_plot$Plot_type)
covariables_plot$Site <- as.factor(covariables_plot$Site)
covariables_plot$Year<- as.factor(covariables_plot$Year)
covariables_plot$Session<- as.factor(covariables_plot$Session)
covariables_plot$Site_location<- as.factor(covariables_plot$Site_location)




####Effect of forest composition area on mice and voles densities----
#####using mixed linear mixed models

###MOUSE DENSITY MODELS----
#first only using beech basal area as a forest composition
#taking into account year and understory effects
mdens.mbeech<- lmer(Mice_Plot_Density~  Beech_absolute_basal_area + Understory_scaled+ Year +(1|Site),
                  data=covariables_plot)
summary(mdens.mbeech)
##RESULT: 
#No effect of beech basal area nor understory on mouse densities found
#Year influenced mouse densities

#Checking if adding Basal area of the other conifer species improve the model or
#any relationship can be found
mdens.mbedfns<- lmer(Mice_Plot_Density~ Norway_spruce_basal_area + Douglas_fir_basal_area + Beech_absolute_basal_area + 
                       Understory_coverage+ Year + (1|Site),
                    data=covariables_plot)
summary(mdens.mbedfns)
#re-scaling Understory
mdens.mbedfns_sc<-lmer(Mice_Plot_Density~ Norway_spruce_basal_area + Douglas_fir_basal_area + Beech_absolute_basal_area + 
       Understory_scaled+ Year + (1|Site),
     data=covariables_plot)
summary(mdens.mbedfns_sc)
##RESULT: 
#No effect of any tree species basal area nor understory on mouse densities found
#Year influenced mouse densities

#verifying model
DHARMa::testResiduals(mdens.mbedfns_sc)#all good

#exporting last model's results
modelsummary(mdens.mbedfns, statistic = c("std.error", "statistic", "p.value"), 
             shape=term ~ model + statistic, "dens_model_mouse.xlsx")



###VOLE DENSITY MODELS
#first only using beech basal area as a forest composition
#taking into account year and understory effects
mdens.vbeech<- lmer(Vole_density~  Beech_absolute_basal_area + Understory_scaled+ Year +(1|Site),
                    data=covariables_plot)
summary(mdens.vbeech)
##RESULT: 
#No effect of beech basal area nor Year on vole densities found
#Understory positively influenced vole densities

#Checking if adding Basal area of the other conifer species improve the model or
#any relationship can be found
mdens.vbedfns<- lmer(Vole_density~ Norway_spruce_basal_area + Douglas_fir_basal_area + Beech_absolute_basal_area + 
                       Understory_coverage+ Year + (1|Site),
                     data=covariables_plot)
summary(mdens.vbedfns)
#re-scaling Understory
mdens.vbedfns_sc <- lmer(Vole_density~ Norway_spruce_basal_area + Douglas_fir_basal_area + Beech_absolute_basal_area + 
               Understory_scaled+ Year + (1|Site),
             data=covariables_plot)
summary(mdens.vbedfns_sc )

##RESULT: 
#No effect of any tree species basal area nor Year on vole densities found 
#Understory positively influenced vole densities

#verifying model
DHARMa::testResiduals(mdens.vbedfns_sc )#all good


#exporting last model's results
modelsummary(mdens.vbedfns, statistic = c("std.error", "statistic", "p.value"), 
             shape=term ~ model + statistic, "dens_model_vole.xlsx")


#DENSITY vs BASAL AREA GRAPHS----

##GRAPHS FOR MOUSE DENSITIES ON DIFFERENT PLOTS


pddm <- ggplot(data = covariables_plot, aes(x =jitter(Douglas_fir_basal_area,80),
                                   y = jitter(Mice_Plot_Density,0.05) ,col=Year )) + 
  geom_point( size=5, alpha=0.5) +          
  geom_smooth(se=F, linewidth=2, linetype="longdash", method="lm")+
  ylab(bquote("Mouse Density "~(ha^-1)))+
  xlab(bquote("Douglas-fir Basal Area " (m^2~ha^-1)))+
theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 16));pddm

pdnm <- ggplot(data = covariables_plot, aes(x =jitter(Norway_spruce_basal_area,80),
                                   y = jitter(Mice_Plot_Density,0.05) ,col=Year )) + 
  geom_point( size=5, alpha=0.5) +          
  geom_smooth(se=F, linewidth=2, linetype="longdash", method="lm")+
  ylab(bquote("Mouse Density "~(ha^-1)))+
  xlab(bquote("Norway spruce Basal Area " (m^2~ha^-1)))+
    theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 16));pdnm

pdbm <- ggplot(data = covariables_plot, aes(x =jitter(Beech_absolute_basal_area,80),
                                            y = jitter(Mice_Plot_Density,0.05) ,col=Year )) + 
  geom_point( size=5, alpha=0.5) +          
  geom_smooth(se=F, linewidth=2, linetype="longdash", method="lm")+
  ylab(bquote("Mouse Density "~(ha^-1)))+
  xlab(bquote("Beech Basal Area " (m^2~ha^-1)))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 16));pdbm

##GRAPHS FOR VOLE DENSITIES ON DIFFERENT PLOTS
pddv <- ggplot(data = covariables_plot, aes(x =jitter(Douglas_fir_basal_area,80),
                                            y = jitter(Vole_density,0.05) ,col=Year )) + 
  geom_point( size=5, alpha=0.5) +          
  geom_smooth(se=F, linewidth=2, linetype="longdash", method="lm")+
  ylab(bquote("Vole Density "~(ha^-1)))+
  xlab(bquote("Douglas-fir Basal Area " (m^2~ha^-1)))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 16));pddv

pdnv <- ggplot(data = covariables_plot, aes(x =jitter(Norway_spruce_basal_area,80),
                                            y = jitter(Vole_density,0.05) ,col=Year )) + 
  geom_point( size=5, alpha=0.5) +          
  geom_smooth(se=F, linewidth=2, linetype="longdash", method="lm")+
  ylab(bquote("Vole Density "~(ha^-1)))+
  xlab(bquote("Norway spruce Basal Area " (m^2~ha^-1)))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 16));pdnv

pdbv <- ggplot(data = covariables_plot, aes(x =jitter(Beech_absolute_basal_area,80),
                                            y = jitter(Vole_density,0.05) ,col=Year )) + 
  geom_point( size=5, alpha=0.5) +          
  geom_smooth(se=F, linewidth=2, linetype="longdash", method="lm")+
  ylab(bquote("Vole Density "~(ha^-1)))+
  xlab(bquote("Beech Basal Area " (m^2~ha^-1)))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 16));pdbv



#COMMPARRING RICHNESS AND DIVERSITY WITH BASAL AREAS----

#RICHNESS----

#Only using beech basal area

mrich.be<- lmer(Richness~  Beech_absolute_basal_area + Understory_scaled+ Year +(1|Site),
                  data=covariables_plot)
summary(mrich.be)
#RESULT:
#NO effect of beech basal area nor Year nor Understory on Richness

#Using all three species basal areas

mrich.bedfns<- lmer(Richness~Douglas_fir_basal_area + Norway_spruce_basal_area +
                  Beech_absolute_basal_area + Understory_scaled+ Year +(1|Site),
                data=covariables_plot)
summary(mrich.bedfns)
modelsummary(mrich.bedfns, statistic = c("std.error", "statistic", "p.value"), shape=term ~ model + statistic, "richness.xlsx")
#RESULT:
#NO effect of any species' basal area nor Year nor Understory on Richness

pdr <- ggplot(data = covariables_plot, aes(x =jitter(Douglas_fir_basal_area,80),
                                   y = jitter(Richness,0.05) ,col=Year )) + 
  geom_point( size=5, alpha=0.5) +          
  geom_smooth(se=F, linewidth=2, linetype="longdash", method="lm")+
  ylab("Species Richness")+
  xlab(bquote("Douglas-fir Basal Area " (m^2~ha^-1)))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 16));pdr


pnr <-ggplot(data = covariables_plot, aes(x =jitter(Norway_spruce_basal_area,80),
                                          y = jitter(Richness,0.05) ,col=Year )) + 
  geom_point( size=5, alpha=0.5) +          
  geom_smooth(se=F, linewidth=2, linetype="longdash", method="lm")+
  ylab("Species Richness")+
  xlab(bquote("Norway Spruce Basal Area " (m^2~ha^-1)))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 16));pnr


pbr <- ggplot(data = covariables_plot, aes(x =jitter(Beech_absolute_basal_area,80),
                                           y = jitter(Richness,0.05) ,col=Year )) + 
  geom_point( size=5, alpha=0.5) +          
  geom_smooth(se=F, linewidth=2, linetype="longdash", method="lm")+
  ylab("Species Richness")+
  xlab(bquote("Beech Basal Area " (m^2~ha^-1)))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 16)); pbr

######
#Diversity with Shannon index-----

#Only Beech Basal area
mdivi.be<- lmer(Shannon_diversity~ Beech_absolute_basal_area + Understory_scaled+ Year +(1|Site),
                  data=covariables_plot)
summary(mdivi.be)
#RESULT:
#NO effect of beech basal area nor Year nor Understory on Diversity
#one model with all basal areas

#all three tree species basal Area
mdivi.bedfns<- lmer(Shannon_diversity~ Douglas_fir_basal_area + Norway_spruce_basal_area +
                    Beech_absolute_basal_area + Understory_scaled+ Year +(1|Site),
                  data=covariables_plot)
summary(mdivi.bedfns)
modelsummary(mdivi.bedfns, statistic = c("std.error", "statistic", "p.value"), shape=term ~ model + statistic, "diversity_results.xlsx")

#RESULT:
#NO effect of beech basal area nor Year nor Understory on Diversity
#one model with all basal areas

## DIVERSITY GRAPHS

pdd <- ggplot(data = covariables_plot, aes(x =jitter(Douglas_fir_basal_area,80),
                                           y = jitter(Shannon_diversity,0.05) ,col=Year )) + 
  geom_point( size=5, alpha=0.5) +          
  geom_smooth(se=F, linewidth=2, linetype="longdash", method="lm")+
  ylab("Species Diversity\n(Shannon index)")+
  xlab(bquote("Douglas-fir Basal Area " (m^2~ha^-1)))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 16));pdd

pnd <- ggplot(data = covariables_plot, aes(x =jitter(Norway_spruce_basal_area,80),
                                           y = jitter(Shannon_diversity,0.05) ,col=Year )) + 
  geom_point( size=5, alpha=0.5) +          
  geom_smooth(se=F, linewidth=2, linetype="longdash", method="lm")+
  ylab("Species Diversity\n(Shannon index)")+
  xlab(bquote("Norway Spruce Basal Area " (m^2~ha^-1)))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 16));pnd

pbd <- ggplot(data = covariables_plot, aes(x =jitter(Beech_absolute_basal_area,80),
                                           y = jitter(Shannon_diversity,0.05) ,col=Year )) + 
  geom_point( size=5, alpha=0.5) +          
  geom_smooth(se=F, linewidth=2, linetype="longdash", method="lm")+
  ylab("Species Diversity\n(Shannon index)")+
  xlab(bquote("Beech Basal Area " (m^2~ha^-1)))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 16));pbd

#CHANGE PLOTS PARAMETERS AND PUTTING THEM ALLTOGETHER----

pdr2 <-pdr+
  xlab(" ")+
  theme(
    axis.text = element_text(size = 12),
    axis.title.y = element_text( size=16),
    legend.position = "none")

pnr2 <-pnr+
  xlab(" ")+
  ylab(" ")+
  theme(
    axis.text = element_text(size = 12),
    axis.title.y = element_text( size=15),
    legend.position = "none")

pbr2 <-pbr+
  xlab(" ")+
  ylab(" ")+
  theme(
    axis.text = element_text(size = 12),
    axis.title.y = element_text( size=15),
    legend.position = c(0.75, 0.85))

pdd2 <-pdd+
  theme(
    axis.text = element_text(size = 12),
    axis.title.y = element_text( size=16),
    axis.title.x = element_text(size=15),
    legend.position = "none")

pnd2 <-pnd+
  ylab(" ")+
  theme(
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size=15),
    axis.title.x = element_text(size=15),
    legend.position = "none")

pbd2 <-pbd+
  ylab(" ")+
  theme(
    axis.text = element_text(size = 12),
    axis.title.y = element_text( size=15),
    axis.title.x = element_text(size=15),
    legend.position = c(0.75, 0.85))

diri.fig <- ggarrange(pdr2, pnr2, pbr2, pdd2, pnd2, pbd2, ncol = 3, nrow = 2);diri.fig 



#fixing for ploting

pddm2 <-pddm+
  xlab(" ")+
  theme(
    axis.text = element_text(size = 12),
    axis.title.y = element_text( size=16),
    legend.position = "none")

pdnm2 <-pdnm+
  xlab(" ")+
  ylab(" ")+
  theme(
    axis.text = element_text(size = 12),
    axis.title.y = element_text( size=15),
    legend.position = "none")

pdbm2 <-pdbm+
  xlab(" ")+
  ylab(" ")+
  theme(
    axis.text = element_text(size = 12),
    axis.title.y = element_text( size=15),
    legend.position = c(0.35, 0.85))

pddv2 <-pddv+
  theme(
    axis.text = element_text(size = 12),
    axis.title.y = element_text( size=16),
    axis.title.x = element_text(size=15),
    legend.position = "none")

pdnv2 <-pdnv+
  ylab(" ")+
  theme(
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size=15),
    axis.title.x = element_text(size=15),
    legend.position = "none")

pdbv2 <-pdbv+
  ylab(" ")+
  theme(
    axis.text = element_text(size = 12),
    axis.title.y = element_text( size=15),
    axis.title.x = element_text(size=15),
    legend.position = c(0.75, 0.85))

densi.fig <- ggarrange(pddm2, pdnm2, pdbm2, pddv2, pdnv2, pdbv2, ncol = 3, nrow = 2);densi.fig 
