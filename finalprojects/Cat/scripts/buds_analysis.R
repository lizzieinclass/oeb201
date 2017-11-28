### WORKING ON BUDS DATA
## 25 SEPTEMBER 2017 - CAT


# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(lme4)
library(arm)
library(gridExtra)
library( data.table )
library(rstan)
library(rstanarm)



# Set Working Directory
setwd("~/Documents/git/freezingexperiment/analyses")
d <-read.csv("input/Buds_clean.csv", header=TRUE, check.names=FALSE)

################ Cleaning data ##################################
# remove individuals that were frosted twice
error.inds<-d%>%dplyr::select(NEW,Freeze)
error.inds<-error.inds[!duplicated(error.inds),]
#error.inds$Freeze<-ifelse(error.inds$Freeze=="", NA, error.inds$Freeze)
#error.inds<-na.omit(error.inds)
error.inds<-as.data.frame(table(error.inds$NEW))
error.inds<-filter(error.inds, Freq!=3)
keep<-unique(error.inds$Var1)

## Clean dataframe for analysis
dx<-gather(d, key=doy, value=bbch, -NEW, -TX, -Freeze, -Bud)
dx<-filter(dx, NEW %in% keep)
dx$day<-substr(dx$doy, 1, 2)
dx$month<-substr(dx$doy, 4, 5)
dx$year<-substr(dx$doy, 7,10)
x<-paste(dx$year, dx$day, dx$month)
dx$date<-as.Date(strptime(x, format="%Y %d %m"))
dx$doy<-yday(dx$date)

### DOY should now be adjusted for start of experiment rather than actual calendar doy
# Start of Experiment was: 24 March 2017
start<-yday("2017/03/24")
dx$doy.adjusted<-dx$doy-start
dx$bud <- ave(dx$Bud, dx$NEW, dx$date, FUN = seq_along)

# Quick check...
dvr<-dx[!is.na(dx$bbch),]
done<-c("DONE!", "DONE", "DONE!!", "missed","(missed)", "")
dvr<-dvr[!dvr$bbch%in%done,]
dead<-c("stem snapped", "x", "dead")
dvr$bbch<-ifelse(dvr$bbch%in%dead, 0, dvr$bbch)
dvr$bud<-as.numeric(dvr$bud)
dvr$ID<-paste(dvr$NEW, dvr$bud, sep="_")
dvr<-dvr%>%dplyr::select(ID, doy.adjusted, bbch)
last<-aggregate(dvr$doy.adjusted, by = list(dvr$ID), max)
last<-last%>%rename(ID=Group.1)%>%rename(doy.adjusted=x)
last$leaf<-NA
for(i in c(1:nrow(last))) {
  for(j in c(1:nrow(dvr)))
    if(last$ID[i]==dvr$ID[j] & last$doy.adjusted[i]==dvr$doy.adjusted[j])
      last$leaf[i]<-dvr$bbch[j]
}

first<-aggregate(dvr$doy.adjusted, by = list(dvr$ID), min)
first<-first%>%rename(ID=Group.1)%>%rename(doy.adjusted=x)
first$bb<-NA
for(i in c(1:nrow(first))) {
  for(j in c(1:nrow(dvr)))
    if(first$ID[i]==dvr$ID[j] & first$doy.adjusted[i]==dvr$doy.adjusted[j])
      first$bb[i]<-dvr$bbch[j]
}


############## Determine Duration of Vegetative Risk #################
##### Need to clean data a lot - remove early incorrectly entered data 
## and edit errors from when I was away
## then determine dead buds and if any reached budburst and then died
## If so to above, need to recalculate percent budburst

first<-first%>%rename(budburst=doy.adjusted)%>%rename(bbch.first=bb)
last<-last%>%rename(leafout=doy.adjusted)%>%rename(bbch.last=leaf)
risk<-full_join(first, last)
risk$species<-substr(risk$ID, 1,6)
risk$individ<-substr(risk$ID, 1, 10)
risk$bud<-substr(risk$ID, 12, 13)



###### Now integrate FS ###########
frz<-subset(dx,TX=="B")
frz<-dplyr::select(frz, NEW, Freeze, bud)
frz$day<-substr(frz$Freeze, 1, 2)
frz$month<-substr(frz$Freeze, 4, 5)
frz$year<-substr(frz$Freeze, 7,10)
x<-paste(frz$year, frz$day, frz$month)
frz$date<-as.Date(strptime(x, format="%Y %d %m"))
frz$frz<-yday(frz$date)
frz<-dplyr::select(frz, NEW, frz)
frz<-na.omit(frz)
frz<-frz[!duplicated(frz),]
risk$frz<-NA
for(i in c(1:nrow(risk))) {
  for(j in c(1:nrow(frz)))
    if(risk$individ[i]==frz$NEW[j])
      risk$frz[i]<-frz$frz[j]
}
risk$frz<- risk$frz-start
risk$tx<-ifelse(is.na(risk$frz), "A", "B")

risk$dvr<-ifelse(risk$bbch.last==15, (risk$leafout-risk$budburst), NA)
risk$frost<-ifelse(risk$bbch.first<=risk$frz, 1, 0)
risk$frost<-ifelse(risk$tx=="A", 0, risk$frost)

risk$bud<-as.numeric(risk$bud)
mod1<-lmer(dvr~tx+(1|species), data=risk)
display(mod1)
betula<-c("BETPOP", "BETPAP")
birch<-subset(risk, risk$species%in%betula)

birch<-read.csv("output/birches_buddata.csv", header=TRUE)
mod2<-glm(dvr~bud+species+frost, data=birch)
display(mod2)
mod3<-glm(dvr~species+bud*tx, data=birch)
display(mod3)
mod4<-lmer(dvr~tx + (1|species), data=birch)
display(mod4)

birch$bud<-as.numeric(birch$bud)
birch<-birch[!is.na(birch$dvr),]
birch<-birch[birch$dvr>0,]
bpap<-ggplot(birch, aes(x=bud, y=dvr, color=as.factor(tx))) + geom_point() + geom_smooth(method="lm") + facet_wrap(~species)


dvr_bb<-risk[!is.na(risk$dvr),]
dvr_bb<-risk[risk$dvr>0,]
#write.csv(risk, file="~/Documents/git/freezingexperiment/analyses/output/budsdvr_all.csv", row.names = FALSE)
ggplot(dvr_bb, aes(x=bud, y=dvr, color=tx)) + geom_point() + geom_smooth(method="lm") + facet_wrap(~species)
mod<-lmer(dvr~tx+species+(1|individ), data=risk)
display(mod)
mod1<-lmer(dvr~as.factor(frost)+species+(1|individ), data=birch)
display(mod1)

birch$bud
birch$z_bud<-(birch$bud - mean(birch$bud))/sd(birch$bud)
lmod1<-lmer(dvr~tx+bud+(1|species), data=birch)
display(lmod1)
simp<-lm(dvr~frost+bud+species+frost:bud, data=birch)
display(simp)


birch.mean<-birch[!is.na(birch$dvr),]
birch.mean$avg.rate<-ave(birch.mean$dvr, birch.mean$individ)
hist(birch.mean$avg.rate)
M1<-lmer(avg.rate~tx+(1|species), data=birch.mean)
display(M1)
qplot(species, dvr, data = birch, 
      geom = "boxplot", color=tx) + 
  xlab("Species")+ylab("Mean DVR")

###### Re-evaluated % budburst #######
burst<-risk[!is.na(risk$dvr),]
burst<-distinct(burst, ID,individ)
burst<-as.data.frame(table(burst$individ))
burst<-burst%>%rename(individ=Var1)%>%rename(burst=Freq)
burst$individ<-as.character(burst$individ)

total<-risk
total<-distinct(total, ID,individ)
total<-as.data.frame(table(total$individ))
total<-total%>%rename(individ=Var1)%>%rename(total=Freq)
total$individ<-as.character(total$individ)

percent<-full_join(total, burst)
percent$perc.bb<-(percent$burst/percent$total)*100
percent$species<-substr(percent$individ, 1,6)
percent$tx<-NA
percent$tx<-as.character(percent$tx)
for(i in c(1:nrow(percent))){
  for(j in c(1:nrow(risk)))
    if(percent$individ[i]==risk$individ[j])
      percent$tx[i]<-risk$tx[j]
}

percent$species<-substr(percent$individ, 1, 6)
#percent<-subset(percent, species %in% betula)
mod<-lm(perc.bb~tx+species, data=percent)
display(mod)

#percent$ind<-as.numeric(as.factor(percent$individ))
p<-percent
p<-dplyr::select(p,species, individ, tx)
p<-p[!duplicated(p),]
pt <- data.table(p)
pt<-pt[,ind := 1:.N , by = c("species" , "tx") ]
percent<-dplyr::select(percent, species, individ, tx, perc.bb)
px<-left_join(percent, pt)

#percent$individ<-as.numeric(as.factor(percent$individ))
ggplot(px, aes(x=ind, y=perc.bb, color=tx)) + geom_point() + geom_smooth(method="lm") + facet_wrap(~species)

qplot(species, perc.bb, data = px, 
      geom = "boxplot", color=tx) + 
  xlab("Species")+ylab("Percent Budburst")


fit1<-stan_glm(perc.bb~tx+species, data=px)
fit1<-stan_glm(perc.bb~tx+species+tx*species, data=px)
fit1

sp<-c("BETPAP", "BETPOP")
pb<-filter(px, species %in% sp)
#write.csv(pb, file="~/Documents/git/freezingexperiment/analyses/output/percentBB_betula.csv", row.names = FALSE)
fit.b<-stan_glm(perc.bb~tx+species+tx*species, data=pb)
fit.b

pb$mean<-ave(pb$perc.bb, pb$species, pb$tx)
pb$sd<-ave(pb$perc.bb, pb$species, pb$tx, FUN=sd)

ggplot(pb, aes(x=perc.bb, y=ind)) + 
  geom_linerange(aes(ymin=perc.bb-sd, ymax=perc.bb+sd, color=tx, shape=species), alpha=0.3) + 
  geom_point(aes(shape=species, color=tx)) + ylab("Percent Budburst") +
  xlab("Individual") + 
  geom_hline(yintercept=0, linetype=2) + coord_flip() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 
plot(fit.b)
abline(v=0)
pp_check (fit.b, nreps=30)

birch$ind<-substr(birch$individ, 9, 10)
new.mod<-lmer(dvr~frost+bud+(1|species), data=birch)
display(new.mod)

#write.csv(birch, file=("~/Documents/git/freezingexperiment/analyses/output/birches_buddata.csv"), row.names=FALSE)
#write.csv(birch, file=("~/Documents/git/freezingexperiment/analyses/output/birches_speciesdata.csv"), row.names=FALSE)

birch$ind<-as.numeric(as.factor(birch$ind))
bb<-birch
bb<-dplyr::select(bb,species, individ, tx)
bb<-bb[!duplicated(bb),]
bt <- data.table(bb)
bt<-bt[,ind := 1:.N , by = c("species" , "tx") ]
birch<-dplyr::select(birch, species, individ, bud, tx, dvr, frost)
bx<-left_join(birch, bt)


#write.csv(bx, file="~/Documents/git/freezingexperiment/analyses/output/birches_clean.csv", row.names = FALSE)
stan_glmer(dvr~tx+species+(1|individ), data=birch)



