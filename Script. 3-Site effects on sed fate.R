#SITE EFFECTS ON SEED FATE

#comparing northern sites (lots of mice) with southern sites (few mice)
#without using animals densities 

#Run script 2 first


##HARVESTING####
##Harvesting models and graphs with site location as covariable----

#scaling variables
rem2years$sUnderstory_coverage <- scale(rem2years$Understory_coverage)

mremgen.beNS<- glmer(Removed ~ Beech_absolute_basal_area + sUnderstory_coverage+
                     Year + Site_location + (1|id_seed_pile),
                   data=rem2years, family=binomial)
summary(mremgen.beNS)

##REM 2021

# General prediction for 2021 SOuthern Sites
rem.gridS21<-expand.grid(Beech_absolute_basal_area = seq(0, 33, 0.05),sUnderstory_coverage=median(rem2years$sUnderstory_coverage),
                       Year="2021", Site_location="Southern")
pred.remS21<-predict(mremgen.beNS, newdata = rem.gridS21, type="response", re.form=NA)
df.predict.remS21<- data.frame(pred = pred.remS21, rem.gridS21)

# General prediction for 2021 Northern Sites
rem.gridN21<-expand.grid(Beech_absolute_basal_area = seq(0, 33, 0.05),sUnderstory_coverage=median(rem2years$sUnderstory_coverage),
                         Year="2021", Site_location="Northern")
pred.remN21<-predict(mremgen.beNS, newdata = rem.gridN21, type="response", re.form=NA)
df.predict.remN21<- data.frame(pred = pred.remN21, rem.gridN21)


df.predict.remNS21 <- rbind(df.predict.remS21,df.predict.remN21)

plotremNS21 <- ggplot() + 
  geom_point(data = remseeddata21_ld, aes(x = jitter(Beech_absolute_basal_area, 60),
                                          y = jitter(as.numeric(Removed),0.125)), size=3, alpha=0.3, col="lightgreen") +
  geom_line(data = df.predict.remNS21, aes(y = pred, x = Beech_absolute_basal_area, col=Site_location),
             linewidth=3, alpha=0.95) +
  ylab("Seed Harvesting Probability")+
  xlab(bquote("Beech Basal Area " (m^2~ha^-1)))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 13));plotremNS21

##REM 2022

# General prediction for 2022 Southern Sites
rem.gridS22<-expand.grid(Beech_absolute_basal_area = seq(0, 33, 0.05),sUnderstory_coverage=median(rem2years$sUnderstory_coverage),
                          Year="2022", Site_location="Southern")
pred.remS22<-predict(mremgen.beNS, newdata = rem.gridS22, type="response", re.form=NA)
df.predict.remS22<- data.frame(pred = pred.remS22, rem.gridS22)

# General prediction for 2022 Northern Sites
rem.gridN22<-expand.grid(Beech_absolute_basal_area = seq(0, 33, 0.05),sUnderstory_coverage=median(rem2years$sUnderstory_coverage),
                          Year="2022", Site_location="Northern")
pred.remN22<-predict(mremgen.beNS, newdata = rem.gridN22, type="response", re.form=NA)
df.predict.remN22<- data.frame(pred = pred.remN22, rem.gridN22)


df.predict.remNS22 <- rbind(df.predict.remS22,df.predict.remN22)

plotremNS22 <- ggplot() + 
  geom_point(data = remseeddata22_ld, aes(x = jitter(Beech_absolute_basal_area, 60),
                                          y = jitter(as.numeric(Removed),0.125)), size=3, alpha=0.3, col="lightgreen") +
  geom_line(data = df.predict.remNS22, aes(y = pred, x = Beech_absolute_basal_area, col=Site_location),
            linewidth=3, alpha=0.95) +
  ylab("Seed Harvesting Probability")+
  xlab(bquote("Beech Basal Area " (m^2~ha^-1)))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 13));plotremNS22



### SURVIVAL####
##Survival models and graphs with site location as covariable----
sur2years$sUnderstory_coverage <- scale(sur2years$Understory_coverage)

msurgen.beNS<- glmer(Survived ~ Beech_absolute_basal_area + sUnderstory_coverage+
                       Year + Site_location + (1|id_seed_pile),
                     data=sur2years, family=binomial)
summary(msurgen.beNS)

##SUR 2021

# General prediction for 2021 SOuthern Sites
sur.gridS21<-expand.grid(Beech_absolute_basal_area = seq(0, 33, 0.05),sUnderstory_coverage=median(sur2years$sUnderstory_coverage),
                          Year="2021", Site_location="Southern")
pred.surS21<-predict(msurgen.beNS, newdata = sur.gridS21, type="response", re.form=NA)
df.predict.surS21<- data.frame(pred = pred.surS21, sur.gridS21)

# General prediction for 2021 Northern Sites
sur.gridN21<-expand.grid(Beech_absolute_basal_area = seq(0, 33, 0.05),sUnderstory_coverage=median(sur2years$sUnderstory_coverage),
                          Year="2021", Site_location="Northern")
pred.surN21<-predict(msurgen.beNS, newdata = sur.gridN21, type="response", re.form=NA)
df.predict.surN21<- data.frame(pred = pred.surN21, sur.gridN21)


df.predict.surNS21 <- rbind(df.predict.surS21,df.predict.surN21)

plotsurNS21 <- ggplot() + 
  geom_point(data = surseeddata21_ld, aes(x = jitter(Beech_absolute_basal_area, 60),
                                          y = jitter(as.numeric(Survived),0.125)), size=3, alpha=0.3, col="lightgreen") +
  geom_line(data = df.predict.surNS21, aes(y = pred, x = Beech_absolute_basal_area, col=Site_location),
            linewidth=3, alpha=0.95) +
  ylab("Seed Survival Probability")+
  xlab(bquote("Beech Basal Area " (m^2~ha^-1)))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 13));plotsurNS21

##SUR 2022

# General prediction for 2022 Southern Sites
sur.gridS22<-expand.grid(Beech_absolute_basal_area = seq(0, 33, 0.05),sUnderstory_coverage=median(sur2years$sUnderstory_coverage),
                          Year="2022", Site_location="Southern")
pred.surS22<-predict(msurgen.beNS, newdata = sur.gridS22, type="response", re.form=NA)
df.predict.surS22<- data.frame(pred = pred.surS22, sur.gridS22)

# General prediction for 2022 Northern Sites
sur.gridN22<-expand.grid(Beech_absolute_basal_area = seq(0, 33, 0.05),sUnderstory_coverage=median(sur2years$sUnderstory_coverage),
                          Year="2022", Site_location="Northern")
pred.surN22<-predict(msurgen.beNS, newdata = sur.gridN22, type="response", re.form=NA)
df.predict.surN22<- data.frame(pred = pred.surN22, sur.gridN22)


df.predict.surNS22 <- rbind(df.predict.surS22,df.predict.surN22)

plotsurNS22 <- ggplot() + 
  geom_point(data = surseeddata22_ld, aes(x = jitter(Beech_absolute_basal_area, 60),
                                          y = jitter(as.numeric(Survived),0.125)), size=3, alpha=0.3, col="lightgreen") +
  geom_line(data = df.predict.surNS22, aes(y = pred, x = Beech_absolute_basal_area, col=Site_location),
            linewidth=3, alpha=0.95) +
  ylab("Seed Survival Probability")+
  xlab(bquote("Beech Basal Area " (m^2~ha^-1)))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 13));plotsurNS22



###CACHING####
##Caching models and graphs with site location as covariable----


dis2years$sUnderstory_coverage <- scale(dis2years$Understory_coverage)

mdisgen.beNS<- glmer(Dispersed ~ Beech_absolute_basal_area + sUnderstory_coverage+
                       Year + Site_location + (1|id_seed_pile),
                     data=dis2years, family=binomial)
summary(mdisgen.beNS)

##dis 2021

# General prediction for 2021 SOuthern Sites
dis.gridS21<-expand.grid(Beech_absolute_basal_area = seq(0, 33, 0.05),sUnderstory_coverage=median(dis2years$sUnderstory_coverage),
                         Year="2021", Site_location="Southern")
pred.disS21<-predict(mdisgen.beNS, newdata = dis.gridS21, type="response", re.form=NA)
df.predict.disS21<- data.frame(pred = pred.disS21, dis.gridS21)

# General prediction for 2021 Northern Sites
dis.gridN21<-expand.grid(Beech_absolute_basal_area = seq(0, 33, 0.05),sUnderstory_coverage=median(dis2years$sUnderstory_coverage),
                         Year="2021", Site_location="Northern")
pred.disN21<-predict(mdisgen.beNS, newdata = dis.gridN21, type="response", re.form=NA)
df.predict.disN21<- data.frame(pred = pred.disN21, dis.gridN21)


df.predict.disNS21 <- rbind(df.predict.disS21,df.predict.disN21)

#adjusting axis to put dispersed seeds lower in graph
disseeddata21_ldDIS <- disseeddata21_ld %>% filter(Dispersed=="TRUE")
set.seed(25) 

#plot
plotdisNS21 <- ggplot() + 
  geom_point(data = disseeddata21_ld, aes(x = jitter(Beech_absolute_basal_area, 60),
                                          y = jitter(as.numeric(Dispersed),0.095)), size=3, alpha=0.3, col="lightgreen") +
  geom_line(data = df.predict.disNS21, aes(y = pred, x = Beech_absolute_basal_area, col=Site_location),
            linewidth=3, alpha=0.95) +
  geom_point(data=disseeddata21_ldDIS, aes(x = jitter(Beech_absolute_basal_area, 60), y=jitter(rep(0.55,length(as.numeric(Dispersed))),0.095)), 
             size=4, alpha=0.3, col="lightgreen")+
  scale_y_continuous(limits = c(-0.02,0.575), breaks = seq(0, 0.5, 0.1))+
  ylab("Seed Caching Probability")+
  xlab(bquote("Beech Basal Area " (m^2~ha^-1)))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 13));plotdisNS21

##dis 2022

# General prediction for 2022 Southern Sites
dis.gridS22<-expand.grid(Beech_absolute_basal_area = seq(0, 33, 0.05),sUnderstory_coverage=median(dis2years$sUnderstory_coverage),
                         Year="2022", Site_location="Southern")
pred.disS22<-predict(mdisgen.beNS, newdata = dis.gridS22, type="response", re.form=NA)
df.predict.disS22<- data.frame(pred = pred.disS22, dis.gridS22)

# General prediction for 2022 Northern Sites
dis.gridN22<-expand.grid(Beech_absolute_basal_area = seq(0, 33, 0.05),sUnderstory_coverage=median(dis2years$sUnderstory_coverage),
                         Year="2022", Site_location="Northern")
pred.disN22<-predict(mdisgen.beNS, newdata = dis.gridN22, type="response", re.form=NA)
df.predict.disN22<- data.frame(pred = pred.disN22, dis.gridN22)


df.predict.disNS22 <- rbind(df.predict.disS22,df.predict.disN22)

#adjusting axis
disseeddata22_ldDIS <- disseeddata22_ld %>% filter(Dispersed=="TRUE")
set.seed(27) 

#plot
plotdisNS22 <- ggplot() + 
  geom_point(data = disseeddata22_ld, aes(x = jitter(Beech_absolute_basal_area, 60),
                                          y = jitter(as.numeric(Dispersed),0.095)), size=3, alpha=0.3, col="lightgreen") +
  geom_point(data=disseeddata22_ldDIS, aes(x = jitter(Beech_absolute_basal_area, 60), y=jitter(rep(0.55,length(as.numeric(Dispersed))),0.095)), 
             size=4, alpha=0.3, col="lightgreen")+
  scale_y_continuous(limits = c(-0.02,0.575), breaks = seq(0, 0.5, 0.1))+
  geom_line(data = df.predict.disNS22, aes(y = pred, x = Beech_absolute_basal_area, col=Site_location),
            linewidth=3, alpha=0.95) +
  ylab("Seed Caching Probability")+
  xlab(bquote("Beech Basal Area " (m^2~ha^-1)))+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 13));plotdisNS22

#
#####
##Adjusting graphs and plotting all together in 3 by 2----- 

##adjusting plots for one one figure

plotremNS21 <- plotremNS21 +
  xlab(" ")+
  scale_colour_discrete(name = "Site Location", labels = c("South\n(vole dominated)\n", "North\n(mouse dominated)"))+
  theme(
    axis.text = element_text(size = 11),
    axis.title.y = element_text( size=13),
    legend.position = c(0.3, 0.35)
  )
plotremNS22 <- plotremNS22 +
  xlab(" ") +
  ylab(" ") +
  scale_colour_discrete(name = "Site Location", labels = c("South\n(vole dominated)\n", "North\n(mouse dominated)"))+
  theme(
    axis.text = element_text(size = 11),
    legend.position = c(0.75, 0.65)
  )
plotsurNS21 <-  plotsurNS21 + 
  xlab(" ")+
  theme(
    axis.text = element_text(size = 11),
    axis.title.y = element_text( size=13),
    legend.position = "none"
  )
plotsurNS22 <- plotsurNS22 + 
  xlab(" ")+
  ylab(" ") +
  theme(
    axis.text = element_text(size = 11),
    legend.position = "none"
  )
plotdisNS21 <- plotdisNS21 +
  theme(
    axis.text = element_text(size = 11),
    axis.title.x = element_text( size=14),
    axis.title.y = element_text( size=13),
    legend.position = "none"
  ) 

plotdisNS22 <- plotdisNS22 +
  ylab(" ") + 
  theme(
    axis.text = element_text(size = 11),
    axis.title.x = element_text( size=14),
    legend.position = "none"
  ) 
figureNS <- ggarrange(plotremNS21, plotremNS22, 
                      plotsurNS21, plotsurNS22,
                      plotdisNS21, plotdisNS22,
                      ncol= 2, nrow = 3);figureNS

#warning messages of missing points are dispersed/cached seeds that we are now
#showing lower along the Y axis, in order to shorten the axis and better visualise
# the graph


