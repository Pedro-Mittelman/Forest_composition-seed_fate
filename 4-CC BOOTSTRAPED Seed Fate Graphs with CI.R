#ANALYSES OF SEED FATE ACCORDING TO BEECH BASAL AREA WITH
#CONFIDENCE INTERVAS BASED ON BOOTSTRAP

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
#
##
###
#GLMMMs confidence interval on the graph----
#
##
###2021 REMOVED SEEDS----

#
##compared to ABSOLUTE BEECH BASAL AREA in the plots
mremaba21ax<- glmer(Removed ~ Beech_absolute_basal_area + Understory_coverage+ Mice_Plot_density+ (1|Site/Seed_Pile), #(1|Site)+(1|id_seed_pile),
                    data=remseeddata21_ld, family=binomial)
summary(mremaba21ax)

# General prediction
rem.grid<-expand.grid(Beech_absolute_basal_area = seq(0, 31, 0.05),Understory_coverage=median(remseeddata21_ld$Understory_coverage),
                      Mice_Plot_density=median(remseeddata21_ld$Mice_Plot_density))
pred.remaba<-predict(mremaba21ax, newdata = rem.grid, type="response", re.form=NA)
df.predict.remaba <- data.frame(pred = pred.remaba, rem.grid)

#Plotting the graph

ggplot(data = remseeddata21_ld, aes(x = jitter(Beech_absolute_basal_area, 30),
                                    y = jitter(as.numeric(Removed),0.175),)) + 
  geom_point( size=3, alpha=0.4, col="lightgreen") +          
  geom_line(data = df.predict.remaba, aes(y = pred, x = Beech_absolute_basal_area),
             col= "black", linewidth=3, alpha=0.65, show.legend = FALSE) +
  labs(y="Seed Removal Probability", x = "Beech Basal Area")+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 13))

##MAKING CONFIDENCE INTERVAL REM 2021----

# 0 -Reducing dataset to only essential data in order to lighten the proccessing
remseeddata21_ldlight <-  remseeddata21_ld %>% select(Site, Plot,Fate, id_seed_pile, Mice_Plot_density, Seed_Pile, Understory_coverage,
                                                      Beech_absolute_basal_area, Removed)

#1-resampling lines
#listdfseeds <- list()
for (i in 1:1000) {
  listdfseeds[[i]] <-  sample_n(remseeddata21_ldlight, nrow(remseeddata21_ldlight), replace = T)
  print(i)}


#2-creating models  for each resample
#listmodels <- list()
for (i in 1:1000) {
  listmodels[[i]]  <- glmer(Removed ~ Beech_absolute_basal_area + Understory_coverage+ Mice_Plot_density+ (1|Site/Seed_Pile), 
                            data=listdfseeds[[i]], family=binomial)
  print(i)
}

#  Singularvector <- vector()
# for (i in 1:length(listmodels)) {
#   Singularvector[i] <-isSingular(listmodels[[i]])
# }
# listb<- listmodels[!Singularvector]
# listb<- listb[1:1000]
# 
# Singularvector2 <- vector()
# for (i in 1:length(listb)) {
#   Singularvector2[i] <-isSingular(listb[[i]])
# }



#3-predicting values of Y(seed removal probability) for different values of X (beech basal area)
listpredictmodels <- list()
#listb has no singular models

removal.grid<-expand.grid(Beech_absolute_basal_area = seq(0, 31, 0.05),Understory_coverage=median(remseeddata21_ld$Understory_coverage),
                          Mice_Plot_density=median(remseeddata21_ld$Mice_Plot_density))
for (i in 1:1000){
  
  pred.removalx<-predict(listmodels[[i]], newdata = removal.grid, type="response", re.form=NA)
  listpredictmodels[[i]] <- data.frame(pred.removalx, removal.grid)
  print(i)
}


#putting all predictions for each value of X in the same column
predicrionspern_tree <- data.frame()
xpred <- length(rownames( listpredictmodels[[1]]))#number of x predictions per model
for (j in 1:xpred) {
  for (i in 1:1000) {
    predicrionspern_tree[i,j] <- listpredictmodels[[i]][j,1]
    
  } 
  print(j)}

## sorting in ascendig order and selecting only tHe values between the 95% CONFIDENCE INTERVAL
predicrionspern_tree95 <- data.frame(row.names = 1:950)
for (i in 1:xpred) {
  predicrionspern_tree95 <- cbind(predicrionspern_tree95, sort(predicrionspern_tree[,i])[26:975])
}
colnames(predicrionspern_tree95) <- NULL
low <- as_vector(predicrionspern_tree95[1,])
high <- as_vector(predicrionspern_tree95[950,])


##PReparing to plot


##lines of the 95 %interval
df.predict.removallow <- data.frame(pred =low, removal.grid)
df.predict.removalhigh <- data.frame(pred = high, removal.grid)
ribonlh<- cbind(df.predict.removallow, high)
#Plotting the graph


plot_boot_sh2021<- ggplot() + 
  geom_point(data = remseeddata21_ld, aes(x = jitter(Beech_absolute_basal_area, 30),
                                          y = jitter(as.numeric(Removed),0.175)), size=3, alpha=0.4, col="lightgreen") +          
  geom_line(data = df.predict.remaba, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=2, alpha=0.65, show.legend = FALSE) +
  labs(y="Seed Harvesting Probability", x=(bquote("Beech Basal Area " (m^2~ha^-1))))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 13))+
  geom_line(data = df.predict.removallow, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=1, alpha=0.2) + 
  geom_line(data = df.predict.removalhigh, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=1, alpha=0.2) + 
  geom_ribbon(data=ribonlh, aes(ymin = low, ymax = high, x=Beech_absolute_basal_area), alpha = 0.1);plot_boot_sh2021

#removing models from the working environment as they aer very heavy memory wise
rm(listmodels)

###2021 SEED SURVIVAL-----

#
##compared to ABSOLUTE BEECH BASAL AREA in the plots
msuraba21ax<- glmer(Survived ~ Beech_absolute_basal_area + Understory_coverage+ Mice_Plot_density+ (1|Site/Seed_Pile), #(1|Site)+(1|id_seed_pile),
                    data=surseeddata21_ld, family=binomial)
summary(msuraba21ax)



#Creating prediction and plotting graph
# General prediction
sur.grid<-expand.grid(Beech_absolute_basal_area = seq(0, 31, 0.05),Understory_coverage=median(surseeddata21_ld$Understory_coverage),
                      Mice_Plot_density=median(surseeddata21_ld$Mice_Plot_density))
pred.suraba<-predict(msuraba21ax, newdata = sur.grid, type="response", re.form=NA)
df.predict.suraba <- data.frame(pred = pred.suraba, sur.grid)


#Plotting the graph

ggplot(data = surseeddata21_ld, aes(x = jitter(Beech_absolute_basal_area, 30),
                                    y = jitter(as.numeric(Survived),0.175),)) + 
  geom_point( size=3, alpha=0.4, col="lightgreen") +          
  # geom_line(data = df.predict.suraba.rfsouth, aes(y = pred, x = Beech_absolute_basal_area,
  #                                                 group=Site),
  #           size=1.5, alpha=0.7,col="red", show.legend = FALSE)+
  # geom_line(data = df.predict.suraba.rfnorth, aes(y = pred, x = Beech_absolute_basal_area,
  #                                                 group=Site),
  #           size=1.5, alpha=0.7,col="deepskyblue4", show.legend = FALSE)+
  geom_line(data = df.predict.suraba, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=3, alpha=0.65, show.legend = FALSE) +
  labs(y="Seed Survival Probability", x = "Beech Basal Area")+
  # scale_x_reverse()+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 13))

##MAKING CONFIDENCE INTERVAL SUR 2021----

# 0 -Reducing dataset to only essential data in order to lighten the proccessing
surseeddata21_ldlight <-  surseeddata21_ld %>% select(Site, Plot,Fate, id_seed_pile, Mice_Plot_density, Seed_Pile, Understory_coverage,
                                                      Beech_absolute_basal_area, Survived)

#1-resampling lines
listdfseeds_sur <- list()
for (i in 1:1000) {
  listdfseeds_sur[[i]] <-  sample_n(surseeddata21_ldlight, nrow(surseeddata21_ldlight), replace = T)
  print(i)}


#2-creating models  for each resample
listmodels_sur <- list()
for (i in 1:1000) {
  listmodels_sur[[i]]  <- glmer(Survived ~ Beech_absolute_basal_area + Understory_coverage+ Mice_Plot_density+ (1|Site/Seed_Pile), 
                            data=listdfseeds_sur[[i]], family=binomial)
  print(i)
}

#  Singularvector <- vector()
# for (i in 1:length(listmodels)) {
#   Singularvector[i] <-isSingular(listmodels[[i]])
# }
# listb<- listmodels[!Singularvector]
# listb<- listb[1:1000]
# 
# Singularvector2 <- vector()
# for (i in 1:length(listb)) {
#   Singularvector2[i] <-isSingular(listb[[i]])
# }

#3-predicting values of Y(seed Survival probability) for different values of X (beech basal area)
listpredictmodels_sur <- list()
#listb has no singular models

survival.grid<-expand.grid(Beech_absolute_basal_area = seq(0, 31, 0.05),Understory_coverage=median(surseeddata21_ld$Understory_coverage),
                          Mice_Plot_density=median(surseeddata21_ld$Mice_Plot_density))
for (i in 1:1000){
  
  pred.survivalx<-predict(listmodels_sur[[i]], newdata = survival.grid, type="response", re.form=NA)
  listpredictmodels_sur[[i]] <- data.frame(pred.survivalx, survival.grid)
  print(i)
}

#putting all predictions for each value of X in the same column
predicrionspern_tree_sur <- data.frame()
xpred_sur <- length(rownames( listpredictmodels_sur[[1]]))#number of x predictions per model
for (j in 1:xpred_sur) {
  for (i in 1:1000) {
    predicrionspern_tree_sur[i,j] <- listpredictmodels_sur[[i]][j,1]
    
  } 
  print(j)}

## sorting in ascending order and selecting only tHe values between the 95% CONFIDENCE INTERVAL
predicrionspern_tree95_sur <- data.frame(row.names = 1:950)
for (i in 1:xpred_sur) {
  predicrionspern_tree95_sur <- cbind(predicrionspern_tree95_sur, sort(predicrionspern_tree_sur[,i])[26:975])
}
colnames(predicrionspern_tree95_sur) <- NULL
low_sur <- as_vector(predicrionspern_tree95_sur[1,])
high_sur <- as_vector(predicrionspern_tree95_sur[950,])


##PReparing to plot


##lines of the 95 %interval
df.predict.survivallow <- data.frame(pred =low_sur, survival.grid)
df.predict.survivalhigh <- data.frame(pred = high_sur, survival.grid)
ribonlh_sur<- cbind(df.predict.survivallow, high_sur)
#Plotting the graph


plot_boot_ss2021<- ggplot() + 
  geom_point(data = surseeddata21_ld, aes(x = jitter(Beech_absolute_basal_area, 30),
                                          y = jitter(as.numeric(Survived),0.175)), size=3, alpha=0.4, col="lightgreen") +          

  geom_line(data = df.predict.suraba, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=2, alpha=0.65, show.legend = FALSE) +
  labs(y="Seed Survival Probability", x = (bquote("Beech Basal Area " (m^2~ha^-1))))+

  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 13))+
  geom_line(data = df.predict.survivallow, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=1, alpha=0.2) + 
  geom_line(data = df.predict.survivalhigh, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=1, alpha=0.2) + 
  geom_ribbon(data=ribonlh_sur, aes(ymin = low_sur, ymax = high_sur, x=Beech_absolute_basal_area), alpha = 0.1);plot_boot_ss2021

#removing models from the working environment as they aer very heavy memory wise
rm(listmodels_sur)


###2021 SEED CACHING----

#
##compared to ABSOLUTE BEECH BASAL AREA in the plots

#scaling Understory because it is failing to converge
#disseeddata21_ld$Understory_scaled <- scale(disseeddata21_ldlight$Understory_coverage)



mdisaba21ax<- glmer(Dispersed ~ Beech_absolute_basal_area + Understory_coverage+ Mice_Plot_density+  (1|Site/Seed_Pile),
                    data=disseeddata21_ld, family=binomial)
summary(mdisaba21ax)



#Creating prediction and plotting graph
# General prediction
dis.grid<-expand.grid(Beech_absolute_basal_area = seq(0, 31, 0.05),Understory_coverage=median(disseeddata21_ld$Understory_coverage),
                      Mice_Plot_density=median(disseeddata21_ld$Mice_Plot_density))
pred.disaba<-predict(mdisaba21ax, newdata = dis.grid, type="response", re.form=NA)
df.predict.disaba <- data.frame(pred = pred.disaba, dis.grid)


#Plotting the graph

p<- ggplot(data = disseeddata21_ld, aes(x = jitter(Beech_absolute_basal_area, 30),
                                    y = jitter(as.numeric(Dispersed),0.175),)) + 
  geom_point( size=3, alpha=0.4, col="lightgreen") +  
  geom_line(data = df.predict.disaba, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=3, alpha=0.65, show.legend = FALSE) +
  labs(y="Seed Caching Probability", x = "Beech Basal Area")+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 13))


##MAKING CONFIDENCE INTERVAL Caching 2021----

# 0 -Reducing dataset to only essential data in order to lighten the proccessing
disseeddata21_ldlight <-  disseeddata21_ld %>% select(Site, Plot,Fate, id_seed_pile, Mice_Plot_density, Seed_Pile, Understory_coverage,
                                                      Beech_absolute_basal_area, Dispersed)
#running more than 1000 models because models are failing to converge
#1-resampling lines
set.seed(25)
listdfseeds_dis <- list()
for (i in 1:1200) {
  listdfseeds_dis[[i]] <-  sample_n(disseeddata21_ldlight, nrow(disseeddata21_ldlight), replace = T)
  print(i)}


#2-creating models  for each resample
#listmodels_dis <- list()
#running more than 1000 models because models are failing to converge
for (i in 1:1200) { tryCatch({
  listmodels_dis[[i]]  <- glmer(Dispersed ~ Beech_absolute_basal_area + scale(Understory_coverage)+ Mice_Plot_density+  (1|Site/Seed_Pile),
                                data=listdfseeds_dis[[i]], family=binomial)
  print(i) }, error=function(e){cat("ERROR")})
}
#cleaning error models
listmodels_dis<- listmodels_dis[lengths(listmodels_dis) != 0]
#selecting the 1000 first models
listmodels_dis<- listmodels_dis[1:1000]


#  Singularvector <- vector()
# for (i in 1:length(listmodels)) {
#   Singularvector[i] <-isSingular(listmodels[[i]])
# }
# listb<- listmodels[!Singularvector]
# listb<- listb[1:1000]
# 
# Singularvector2 <- vector()
# for (i in 1:length(listb)) {
#   Singularvector2[i] <-isSingular(listb[[i]])
# }

#3-predicting values of Y(seed dispersal probability) for different values of X (beech basal area)
listpredictmodels_dis <- list()
#listb has no singular models

dispersal.grid<-expand.grid(Beech_absolute_basal_area = seq(0, 31, 0.05),Understory_coverage=median(disseeddata21_ld$Understory_coverage),
                           Mice_Plot_density=median(disseeddata21_ld$Mice_Plot_density))
for (i in 1:1000){
  
  pred.dispersalx<-predict(listmodels_dis[[i]], newdata = dispersal.grid, type="response", re.form=NA)
  listpredictmodels_dis[[i]] <- data.frame(pred.dispersalx, dispersal.grid)
  print(i)
}

#putting all predictions for each value of X in the same column
predicrionspern_tree_dis <- data.frame()
xpred_dis <- length(rownames( listpredictmodels_dis[[1]]))#number of x predictions per model
for (j in 1:xpred_dis) {
  for (i in 1:1000) {
    predicrionspern_tree_dis[i,j] <- listpredictmodels_dis[[i]][j,1]
    
  } 
  print(j)}

## sorting in ascending order and selecting only tHe values between the 95% CONFIDENCE INTERVAL
predicrionspern_tree95_dis <- data.frame(row.names = 1:950)
for (i in 1:xpred_dis) {
  predicrionspern_tree95_dis <- cbind(predicrionspern_tree95_dis, sort(predicrionspern_tree_dis[,i])[26:975])
}
colnames(predicrionspern_tree95_dis) <- NULL
low_dis <- as_vector(predicrionspern_tree95_dis[1,])
high_dis <- as_vector(predicrionspern_tree95_dis[950,])


##PReparing to plot


##lines of the 95 %interval
df.predict.dispersallow <- data.frame(pred =low_dis, dispersal.grid)
df.predict.dispersalhigh <- data.frame(pred = high_dis, dispersal.grid)
ribonlh_dis<- cbind(df.predict.dispersallow, high)
#Plotting the graph
disseeddata21_ldT <- disseeddata21_ld %>% filter(Dispersed=="TRUE")
disseeddata21_ldF <- disseeddata21_ld %>% filter(Dispersed=="FALSE")

plot_boot_sd2021<- ggplot() + 
  geom_point(data = disseeddata21_ld, aes(x = jitter(Beech_absolute_basal_area, 30),
                                          y = jitter(as.numeric(Dispersed),0.095)), size=3, alpha=0.4, col="lightgreen") +
  geom_point(data=disseeddata21_ldT, aes(x = jitter(Beech_absolute_basal_area, 30), y=jitter(rep(0.55,length(as.numeric(disseeddata21_ldT$Dispersed))),0.095)), 
             size=4, alpha=0.4, col="lightgreen")+
  scale_y_continuous(limits = c(-0.02,0.575), breaks = seq(0, 0.5, 0.1))+
  geom_line(data = df.predict.disaba, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=2, alpha=0.65, linetype = "longdash", show.legend = FALSE) +
  labs(y="Seed Caching Probability", x = (bquote("Beech Basal Area " (m^2~ha^-1))))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 13))+
  geom_line(data = df.predict.dispersallow, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=1, alpha=0.2) + 
  geom_line(data = df.predict.dispersalhigh, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=1, alpha=0.2) + 
  geom_ribbon(data=ribonlh_dis, aes(ymin = low_dis, ymax = high_dis, x=Beech_absolute_basal_area), alpha = 0.1);plot_boot_sd2021

#removing models from the working environment as they are very heavy memory wise
rm(listmodels_dis)


###2022 HARVESTED SEEDS-----
#scaling understory as model it is failing to converge
remseeddata22_ld$Understory_scaled <-  scale(remseeddata22_ld$Understory_coverage)
#
##compared to ABSOLUTE BEECH BASAL AREA in the plots
mremaba22ax<- glmer(Removed ~ Beech_absolute_basal_area + Understory_scaled+ Mice_Plot_density+ (1|Site/Seed_Pile),#(1|Site)+(1|id_seed_pile),
                    data=remseeddata22_ld, family=binomial)
summary(mremaba22ax)


#Creating prediction and plotting graph
# General prediction
rem.grid<-expand.grid(Beech_absolute_basal_area = seq(0, 31, 0.05),Understory_scaled=median(remseeddata22_ld$Understory_scaled),
                      Mice_Plot_density=median(remseeddata22_ld$Mice_Plot_density))
pred.remaba<-predict(mremaba22ax, newdata = rem.grid, type="response", re.form=NA)
df.predict.remaba <- data.frame(pred = pred.remaba, rem.grid)

#Plotting the graph

ggplot(data = remseeddata22_ld, aes(x = jitter(Beech_absolute_basal_area, 30),
                                    y = jitter(as.numeric(Removed),0.175),)) + 
  geom_point( size=3, alpha=0.4, col="lightgreen") +          
  geom_line(data = df.predict.remaba, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=3, alpha=0.65, show.legend = FALSE) +
  labs(y="Seed Removal Probability", x = (bquote("Beech Basal Area " (m^2~ha^-1))))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 13))

##MAKING CONFIDENCE INTERVAL HARVESTING 2022----

# 0 -Reducing dataset to only essential data in order to lighten the proccessing
remseeddata22_ldlight <-  remseeddata22_ld %>% select(Site, Plot,Fate, id_seed_pile, Mice_Plot_density, Seed_Pile, Understory_scaled,
                                                      Beech_absolute_basal_area, Removed)

#1-resampling lines
listdfseeds22 <- list()
for (i in 1:1000) {
  listdfseeds22[[i]] <-  sample_n(remseeddata22_ldlight, nrow(remseeddata22_ldlight), replace = T)
  print(i)}


#2-creating models  for each resample
listmodels22 <- list()
for (i in 1:1000) {
  listmodels22[[i]]  <- glmer(Removed ~ Beech_absolute_basal_area + Understory_scaled+ Mice_Plot_density+ (1|Site/Seed_Pile), 
                            data=listdfseeds22[[i]], family=binomial)
  print(i)
}

#  Singularvector <- vector()
# for (i in 1:length(listmodels)) {
#   Singularvector[i] <-isSingular(listmodels[[i]])
# }
# listb<- listmodels[!Singularvector]
# listb<- listb[1:1000]
# 
# Singularvector2 <- vector()
# for (i in 1:length(listb)) {
#   Singularvector2[i] <-isSingular(listb[[i]])
# }

#3-predicting values of Y(seed removal probability) for different values of X (beech basal area)
listpredictmodels22 <- list()
#listb has no singular models

removal.grid22<-expand.grid(Beech_absolute_basal_area = seq(0, 31, 0.05),Understory_scaled=median(remseeddata22_ld$Understory_scaled),
                          Mice_Plot_density=median(remseeddata22_ld$Mice_Plot_density))
for (i in 1:1000){
  
  pred.removalx22<-predict(listmodels22[[i]], newdata = removal.grid22, type="response", re.form=NA)
  listpredictmodels22[[i]] <- data.frame(pred.removalx22, removal.grid22)
  print(i)
}

#putting all predictions for each value of X in the same column
predicrionspern_tree22 <- data.frame()
xpred22 <- length(rownames( listpredictmodels22[[1]]))#number of x predictions per model
for (j in 1:xpred22) {
  for (i in 1:1000) {
    predicrionspern_tree22[i,j] <- listpredictmodels22[[i]][j,1]
    
  } 
  print(j)}


## sorting in ascendig order and selecting only tHe values between the 95% CONFIDENCE INTERVAL
predicrionspern_tree9522 <- data.frame(row.names = 1:950)
for (i in 1:xpred22) {
  predicrionspern_tree9522 <- cbind(predicrionspern_tree9522, sort(predicrionspern_tree22[,i])[26:975])
}
colnames(predicrionspern_tree9522) <- NULL
low22 <- as_vector(predicrionspern_tree9522[1,])
high22 <- as_vector(predicrionspern_tree9522[950,])


##PReparing to plot


##lines of the 95 %interval
df.predict.removallow22 <- data.frame(pred =low22, removal.grid22)
df.predict.removalhigh22 <- data.frame(pred = high22, removal.grid22)
ribonlh22<- cbind(df.predict.removallow22, high22)
#Plotting the graph


plot_boot_sh2022<- ggplot() + 
  geom_point(data = remseeddata22_ld, aes(x = jitter(Beech_absolute_basal_area, 30),
                                          y = jitter(as.numeric(Removed),0.175)), size=3, alpha=0.4, col="lightgreen") +          
  geom_line(data = df.predict.remaba, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=2, alpha=0.65, show.legend = FALSE) +
  labs(y="Seed Harvesting Probability", x = (bquote("Beech Basal Area " (m^2~ha^-1))))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 13))+
  geom_line(data = df.predict.removallow22, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=1, alpha=0.2) + 
  geom_line(data = df.predict.removalhigh22, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=1, alpha=0.2) + 
  geom_ribbon(data=ribonlh22, aes(ymin = low22, ymax = high22, x=Beech_absolute_basal_area), alpha = 0.1);plot_boot_sh2022

#removing models from the working environment as they aer very heavy memory wise
rm(listmodels22)
rm(listdfseeds22)


###2022 SEED SURVIVAL----

#
##compared to ABSOLUTE BEECH BASAL AREA in the plots
#scaling understory as model is nearly unidentifiable: very large eigenvalue
surseeddata22_ld$Understory_scaled <-  scale(surseeddata22_ld$Understory_coverage)

msuraba22ax<- glmer(Survived ~ Beech_absolute_basal_area + Understory_scaled+ Mice_Plot_density+ (1|Site/Seed_Pile), #(1|Site)+(1|id_seed_pile),
                    data=surseeddata22_ld, family=binomial)
summary(msuraba22ax)



#Creating prediction and plotting graph
# General prediction
sur.grid22<-expand.grid(Beech_absolute_basal_area = seq(0, 31, 0.05),Understory_scaled=mean(surseeddata22_ld$Understory_scaled),
                      Mice_Plot_density=mean(seeddata22$Mice_Plot_density))
pred.suraba22<-predict(msuraba22ax, newdata = sur.grid22, type="response", re.form=NA)
df.predict.suraba22 <- data.frame(pred = pred.suraba22, sur.grid22)

#Plotting the graph

ggplot(data = surseeddata22_ld, aes(x = jitter(Beech_absolute_basal_area, 30),
                                    y = jitter(as.numeric(Survived),0.175),)) + 
  geom_point( size=3, alpha=0.4, col="lightgreen") +          
  geom_line(data = df.predict.suraba22, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=3, alpha=0.65, show.legend = FALSE) +
  labs(y="Seed Survival Probability", x = (bquote("Beech Basal Area " (m^2~ha^-1))))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 13))

##MAKING CONFIDENCE INTERVAL SUR 2022----

# 0 -Reducing dataset to only essential data in order to lighten the proccessing
surseeddata22_ldlight <-  surseeddata22_ld %>% select(Site, Plot,Fate, id_seed_pile, Mice_Plot_density, Seed_Pile, Understory_scaled,
                                                      Beech_absolute_basal_area, Survived)

#1-resampling lines
listdfseeds_sur22 <- list()
for (i in 1:1000) {
  listdfseeds_sur22[[i]] <-  sample_n(surseeddata22_ldlight, nrow(surseeddata22_ldlight), replace = T)
  print(i)}


#2-creating models  for each resample
listmodels_sur22 <- list()
for (i in 1:1000) {
  listmodels_sur22[[i]]  <- glmer(Survived ~ Beech_absolute_basal_area + Understory_scaled+ Mice_Plot_density+ (1|Site/Seed_Pile), 
                                data=listdfseeds_sur22[[i]], family=binomial)
  print(i)
}

#  Singularvector <- vector()
# for (i in 1:length(listmodels)) {
#   Singularvector[i] <-isSingular(listmodels[[i]])
# }
# listb<- listmodels[!Singularvector]
# listb<- listb[1:1000]
# 
# Singularvector2 <- vector()
# for (i in 1:length(listb)) {
#   Singularvector2[i] <-isSingular(listb[[i]])
# }

#3-predicting values of Y(seed Survival probability) for different values of X (beech basal area)
listpredictmodels_sur22 <- list()
#listb has no singular models

survival.grid22<-expand.grid(Beech_absolute_basal_area = seq(0, 31, 0.05),Understory_scaled=mean(surseeddata22_ld$Understory_scaled),
                           Mice_Plot_density=mean(surseeddata22_ld$Mice_Plot_density))
for (i in 1:1000){
  
  pred.survivalx22<-predict(listmodels_sur22[[i]], newdata = survival.grid22, type="response", re.form=NA)
  listpredictmodels_sur22[[i]] <- data.frame(pred.survivalx22, survival.grid22)
  print(i)
}

#putting all predictions for each value of X in the same column
predicrionspern_tree_sur22 <- data.frame()
xpred_sur22 <- length(rownames( listpredictmodels_sur22[[1]]))#number of x predictions per model
for (j in 1:xpred_sur22) {
  for (i in 1:1000) {
    predicrionspern_tree_sur22[i,j] <- listpredictmodels_sur22[[i]][j,1]
    
  } 
  print(j)}

## sorting in ascending order and selecting only tHe values between the 95% CONFIDENCE INTERVAL
predicrionspern_tree95_sur22 <- data.frame(row.names = 1:950)
for (i in 1:xpred_sur22) {
  predicrionspern_tree95_sur22 <- cbind(predicrionspern_tree95_sur22, sort(predicrionspern_tree_sur22[,i])[26:975])
}
colnames(predicrionspern_tree95_sur22) <- NULL
low_sur22 <- as_vector(predicrionspern_tree95_sur22[1,])
high_sur22 <- as_vector(predicrionspern_tree95_sur22[950,])


##PReparing to plot


##lines of the 95 %interval
df.predict.survivallow22 <- data.frame(pred =low_sur22, survival.grid22)
df.predict.survivalhigh22 <- data.frame(pred = high_sur22, survival.grid22)
ribonlh_sur22<- cbind(df.predict.survivallow22, high_sur22)
#Plotting the graph


plot_boot_ss2022<- ggplot() + 
  geom_point(data = surseeddata22_ld, aes(x = jitter(Beech_absolute_basal_area, 30),
                                          y = jitter(as.numeric(Survived),0.175)), size=3, alpha=0.4, col="lightgreen") +          
  geom_line(data = df.predict.suraba22, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=2, alpha=0.65, show.legend = FALSE) +
  labs(y="Seed Survival Probability", x = (bquote("Beech Basal Area " (m^2~ha^-1))))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 13))+
  geom_line(data = df.predict.survivallow22, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=1, alpha=0.2) + 
  geom_line(data = df.predict.survivalhigh22, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=1, alpha=0.2) + 
  geom_ribbon(data=ribonlh_sur22, aes(ymin = low_sur22, ymax = high_sur22, x=Beech_absolute_basal_area), alpha = 0.1);plot_boot_ss2022

#removing models from the working environment as they aer very heavy memory wise
rm(listmodels_sur22)
rm(listdfseeds_sur22)

###2022 SEED CACHING----

#
##compared to ABSOLUTE BEECH BASAL AREA in the plots
disseeddata22_ld$Understory_scaled <- scale(disseeddata22_ld$Understory_coverage)

mdisaba22axb<- glmer(Dispersed ~ Beech_absolute_basal_area + Understory_scaled+ Mice_Plot_density+ (1|Site)+(1|id_seed_pile), #(1|Site/Seed_Pile),
                     data=disseeddata22_ld, family=binomial)
summary(mdisaba22axb)

#Creating prediction and plotting graph
# General prediction
dis.grid22<-expand.grid(Beech_absolute_basal_area = seq(0, 31, 0.05),Understory_scaled=median(disseeddata22_ld$Understory_scaled),
                      Mice_Plot_density=mean(seeddata22$Mice_Plot_density))
pred.disaba22<-predict(mdisaba22axb, newdata = dis.grid22, type="response", re.form=NA)
df.predict.disaba22 <- data.frame(pred = pred.disaba22, dis.grid22)



#Plotting the graph

ggplot(data = disseeddata22_ld, aes(x = jitter(Beech_absolute_basal_area, 30),
                                    y = jitter(as.numeric(Dispersed),0.175),)) + 
  geom_point( size=3, alpha=0.4, col="lightgreen") +          
  geom_line(data = df.predict.disaba22, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=3, alpha=0.65, show.legend = FALSE) +
  labs(y="Seed Caching Probability", x = (bquote("Beech Basal Area " (m^2~ha^-1))))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 13))

##MAKING CONFIDENCE INTERVAL CACHING 2022----

# 0 -Reducing dataset to only essential data in order to lighten the proccessing
disseeddata22_ldlight <-  disseeddata22_ld %>% select(Site, Plot,Fate, id_seed_pile, Mice_Plot_density, Seed_Pile, Understory_scaled,
                                                      Beech_absolute_basal_area, Dispersed)

#1-resampling lines
listdfseeds_dis22 <- list()
for (i in 1:1000) {
  listdfseeds_dis22[[i]] <-  sample_n(disseeddata22_ldlight, nrow(disseeddata22_ldlight), replace = T)
  print(i)}


#2-creating models  for each resample
listmodels_dis22 <- list()
for (i in 1:1000) {
  listmodels_dis22[[i]]  <- glmer(Dispersed ~ Beech_absolute_basal_area + Understory_scaled+ Mice_Plot_density+  (1|Site)+(1|id_seed_pile), #(1|Site/Seed_Pile),
                                data=listdfseeds_dis22[[i]], family=binomial)
  print(i)
}

#  Singularvector <- vector()
# for (i in 1:length(listmodels)) {
#   Singularvector[i] <-isSingular(listmodels[[i]])
# }
# listb<- listmodels[!Singularvector]
# listb<- listb[1:1000]
# 
# Singularvector2 <- vector()
# for (i in 1:length(listb)) {
#   Singularvector2[i] <-isSingular(listb[[i]])
# }

#3-predicting values of Y(seed dispersal probability) for different values of X (beech basal area)
listpredictmodels_dis22 <- list()
#listb has no singular models

dispersal.grid22<-expand.grid(Beech_absolute_basal_area = seq(0, 31, 0.05),Understory_scaled=median(disseeddata22_ld$Understory_scaled),
                            Mice_Plot_density=median(disseeddata22_ld$Mice_Plot_density))
for (i in 1:1000){
  
  pred.dispersalx22<-predict(listmodels_dis22[[i]], newdata = dispersal.grid22, type="response", re.form=NA)
  listpredictmodels_dis22[[i]] <- data.frame(pred.dispersalx22, dispersal.grid22)
  print(i)
}

#putting all predictions for each value of X in the same column
predicrionspern_tree_dis22 <- data.frame()
xpred_dis22 <- length(rownames( listpredictmodels_dis22[[1]]))#number of x predictions per model
for (j in 1:xpred_dis22) {
  for (i in 1:1000) {
    predicrionspern_tree_dis22[i,j] <- listpredictmodels_dis22[[i]][j,1]
    
  } 
  print(j)}

## sorting in ascending order and selecting only tHe values between the 95% CONFIDENCE INTERVAL
# predicrionspern_tree95_dis22 <- data.frame(row.names = 1:950)
# for (i in 1:xpred_dis22) {
#   predicrionspern_tree95_dis22 <- cbind(predicrionspern_tree95_dis22, sort(predicrionspern_tree_dis22[,i])[26:975])
# }
# colnames(predicrionspern_tree95_dis22) <- NULL
# low_dis22 <- as_vector(predicrionspern_tree95_dis22[1,])



##PReparing to plot


##lines of the 95 %interval
df.predict.dispersallow22 <- data.frame(pred =low_dis22, dispersal.grid22)
df.predict.dispersalhigh22 <- data.frame(pred = high_dis22, dispersal.grid22)
ribonlh_dis22<- cbind(df.predict.dispersallow22, high_dis22)
#Plotting the graph

#filtering for geom_rug
disseeddata22_ldT <- disseeddata22_ld %>% filter(Dispersed=="TRUE")
disseeddata22_ldF <- disseeddata22_ld %>% filter(Dispersed=="FALSE")
plot_boot_sd2022<- ggplot() + 
  geom_point(data = disseeddata22_ld, aes(x = jitter(Beech_absolute_basal_area, 50),
                                          y = jitter(as.numeric(Dispersed),0.095)), size=3, alpha=0.4, col="lightgreen") +
  geom_point(data=disseeddata22_ldT, aes(x = jitter(Beech_absolute_basal_area, 30), y=jitter(rep(0.55,length(as.numeric(disseeddata22_ldT$Dispersed))),0.095)), 
             size=4, alpha=0.4, col="lightgreen")+
  scale_y_continuous(limits = c(-0.02,0.575), breaks = seq(0, 0.5, 0.1))+
  geom_line(data = df.predict.disaba22, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=2, alpha=0.65, show.legend = FALSE) +
  labs(y="Seed Caching Probability", x = (bquote("Beech Basal Area " (m^2~ha^-1))))+
  #scale_x_reverse()+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 13))+
  geom_line(data = df.predict.dispersallow22, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=1, alpha=0.2) + 
  geom_line(data = df.predict.dispersalhigh22, aes(y = pred, x = Beech_absolute_basal_area),
            col= "black", size=1, alpha=0.2) + 
  geom_ribbon(data=ribonlh_dis22, aes(ymin = low_dis22, ymax = high_dis22, x=Beech_absolute_basal_area), alpha = 0.1);plot_boot_sd2022


#removing models from the working environment as they aer very heavy memory wise
rm(listmodels22_dis)

##adjusting plots for one one figure-----

plot_boot_sh2021_to<- plot_boot_sh2021 +
  xlab(" ")+
  theme(
    axis.text = element_text(size = 11),
  axis.title.y = element_text( size=16)
)
plot_boot_sh2022_to<- plot_boot_sh2022 +
  xlab(" ") +
  ylab(" ") +
  theme(
    axis.text = element_text(size = 11),
    axis.title.y = element_text( size=16)
  #axis.title.y = element_blank()
)
plot_boot_ss2021_to<- plot_boot_ss2021 + 
  xlab(" ")+
  theme(
    axis.text = element_text(size = 11),
  axis.title.y = element_text( size=16)
)
plot_boot_ss2022_to<- plot_boot_ss2022 + 
  xlab(" ")+
  ylab(" ") +
  theme(
    axis.text = element_text(size = 11),
    axis.title.y = element_text( size=16)
  #axis.title.y = element_blank()
)
plot_boot_sd2021_to<- plot_boot_sd2021 +
  theme(
    axis.text = element_text(size = 11),
  axis.title.x = element_text( size=18),
  axis.title.y = element_text( size=16)
) #+ scale_y_continuous(limits = c(-0.02,0.51))
plot_boot_sd2022_to<- plot_boot_sd2022 +
  ylab(" ") + 
  theme(
  axis.text = element_text(size = 11),
  axis.title.x = element_text( size=18),
  axis.title.y = element_text( size=16)
  #axis.title.y = element_blank()
) #+scale_y_continuous(limits = c(-0.02,0.51))
figure_boot <- ggarrange(plot_boot_sh2021_to, plot_boot_sh2022_to,
                    plot_boot_ss2021_to, plot_boot_ss2022_to,
                    plot_boot_sd2021_to, plot_boot_sd2022_to,ncol = 2, nrow = 3);figure_boot

ggsave("test4.jpg", plot= figure_boot, width = 8*3, height = 10.5*3, units = "cm", dpi= 300 )
