library(mise)
mise()

##################
### Setup data ###
##################

### load libraries
library(drc)
library(devtools) # loader devtools to be abelt to install drc and bmd
library(readxl)

### load data
# dat <- read.delim("C:/Lokaldata/JEF/Chemtox/1år/blok 3/pesticid use/pesticid/examdata.csv",sep = ";")
dat <- read_excel("C:/Lokaldata/JEF/KANDIDAT_SPECIALE_2022/DATA/BEARBEJDET DATA/SAMLET DATA/JEF oversigt planter-v10.xlsx",skip = 3)
#View(dat)

### check data
dim(dat)
# summary(dat$Harvest1_leaf.area)
str(dat)
str(dat$Harvest1_leaf.area)
dat$Harvest1_leaf.area

### change variables type formats 
dat$Harvest1_leaf.area <- as.numeric(dat$Harvest1_leaf.area)
dat$Harvest2_leaf.area <- as.numeric(dat$Harvest2_leaf.area)
dat$Harvest3_leaf.area <- as.numeric(dat$Harvest3_leaf.area)

dat$Herbicide <- as.factor(dat$Herbicide)

# new dose variable
n <- dim(dat)[1]
dat$Dose2 <- numeric(n)

dat$Dose2[dat$Herbicide=="Glyphosate" & dat$Dose==0] <- 0
dat$Dose2[dat$Herbicide=="Glyphosate" & dat$Dose==1] <- 4.37
dat$Dose2[dat$Herbicide=="Glyphosate" & dat$Dose==2] <- 8.75
dat$Dose2[dat$Herbicide=="Glyphosate" & dat$Dose==3] <- 17.5
dat$Dose2[dat$Herbicide=="Glyphosate" & dat$Dose==4] <- 35
dat$Dose2[dat$Herbicide=="Glyphosate" & dat$Dose==5] <- 70
dat$Dose2[dat$Herbicide=="Glyphosate" & dat$Dose==6] <- 210
dat$Dose2[dat$Herbicide=="Glyphosate" & dat$Dose==7] <- 630
dat$Dose2[dat$Herbicide=="Betanal" & dat$Dose==0] <- 0
dat$Dose2[dat$Herbicide=="Betanal" & dat$Dose==1] <- 4.44
dat$Dose2[dat$Herbicide=="Betanal" & dat$Dose==2] <- 8.89
dat$Dose2[dat$Herbicide=="Betanal" & dat$Dose==3] <- 17.78
dat$Dose2[dat$Herbicide=="Betanal" & dat$Dose==4] <- 35.56
dat$Dose2[dat$Herbicide=="Betanal" & dat$Dose==5] <- 71.1
dat$Dose2[dat$Herbicide=="Betanal" & dat$Dose==6] <- 213.3
dat$Dose2[dat$Herbicide=="Betanal" & dat$Dose==7] <- 640

##########################
### Plot data chl,dose ###
##########################

#SPECIES#
unique(dat$Species)
dat2 <- dat[dat$Species=="Valmue",]
# dat2 <- dat[dat$Species=="Bloed Storkenaeb",]
# dat2 <- dat[dat$Species=="Ager sennep",]
# dat2 <- dat[dat$Species=="Kornblomst",]

#HERBICIDE#
# dat2 <- dat2[dat2$Herbicide=="Glyphosate",]
dat2 <- dat2[dat2$Herbicide=="Betanal",]

par(mfrow=c(2,2))
plot(dat2$Dose,dat2$Harvest1_leaf.area, ylim = c(0,30),col="black")
plot(dat2$Dose,dat2$Harvest2_leaf.area, ylim = c(0,30),col="blue")
plot(dat2$Dose,dat2$Harvest3_leaf.area, ylim = c(0,30),col="red")

par(mfrow=c(1,1))

summary(dat2$Harvest1_leaf.area)
summary(dat2$Harvest2_leaf.area)
summary(dat2$Harvest3_leaf.area)

#Fitting a dose-response model to data
drm_Harvest1_leaf.area <- drm(Harvest1_leaf.area~ Dose2, data=dat2,fct=LL.4())
drm_Harvest2_leaf.area <- drm(Harvest2_leaf.area~ Dose2, data=dat2,fct=LL.4())
drm_Harvest3_leaf.area <- drm(Harvest3_leaf.area~ Dose2, data=dat2,fct=LL.4())


###############################
#####plotting the model########
###############################

plot(drm_Harvest4_leaf.area, broken= TRUE)

par(mfrow=c(2,2))
#plotting the model with all data
plot(drm_Harvest1_leaf.area,broken=TRUE,type = "all", xlab="Phenmedipharm (g a.i.ha^-1)", ylab="leaf.area ")
title(main="Papaver rhoeas", sub="(day 8)")
plot(drm_Harvest2_leaf.area,broken=TRUE,type = "all", xlab="Phenmedipharm (g a.i.ha^-1)", ylab="leaf.area ")
title(main="Papaver rhoeas", sub="(day 15)")
plot(drm_Harvest3_leaf.area,broken=TRUE,type = "all", xlab="Phenmedipharm (g a.i.ha^-1)", ylab="leaf.area ")
title(main="Papaver rhoeas", sub="(day 21)")


par(mfrow=c(1,1))

#summary of the modelfit
summary(drm_Harvest1_leaf.area)
summary(drm_Harvest2_leaf.area)
summary(drm_Harvest3_leaf.area)



#########################################
#####plotting the model in same plot#####
#########################################

#  Jeg synes du skal forsøge at få alle målinger for alle 3 datoer ind i samme plot.
# Du kan enten lægge det nye plot oveni det gamle med add=TRUE eller lave en model hvor hver dato får sin egen kurve (med curveID=dato i drm()).
# Sidstnævnte giver så også mulighed for nemt at kunne sammenligne parametre over datoer.


par(mfrow=c(1,1))
#plotting the model with all data
plot(drm_Harvest1_leaf.area,broken=TRUE,type = "all", xlab="Phenmedipharm (g a.i.ha^-1)", ylab="leaf.area  ",ylim = c(0,200),lty=1)
title(main="Papaver rhoeas")
plot(drm_Harvest2_leaf.area,broken=TRUE,type = "all", xlab="Phenmedipharm (g a.i.ha^-1)", ylab="leaf.area  ",add = TRUE,col="red")
plot(drm_Harvest3_leaf.area,broken=TRUE,type = "all", xlab="Phenmedipharm (g a.i.ha^-1)", ylab="leaf.area  ",add = TRUE,col="blue")

grid()

#Fitting a dose-response model to data
par(mfrow=c(1,1))
plot(drm_Harvest1_leaf.area,broken=TRUE,type = "all", xlab="Phenmedipharm (g a.i.ha^-1)", ylab="leaf.area  ",ylim = c(0,200),lty=1,pch = 20)
title(main="Papaver rhoeas")
plot(drm_Harvest3_leaf.area,broken=TRUE,type = "all", xlab="Phenmedipharm (g a.i.ha^-1)", ylab="leaf.area  ",add = TRUE,col="blue",pch = 20)
legend("topleft",col = c("black","blue"),legend = c("day 8","day 21"),lty = c(1,1),pch = c(20,20))


#summary of the modelfit
table1 <-summary(drm_Harvest1_leaf.area)
table2 <-summary(drm_Harvest2_leaf.area)
table3 <-summary(drm_Harvest3_leaf.area)


write.csv2(table1$coefficients, "leaf.area_phen_KV1_Parameter estimates.csv")
write.csv2(table2$coefficients, "leaf.area_phen_KV2_Parameter estimates.csv")
write.csv2(table3$coefficients, "leaf.area_phen_KV3_Parameter estimates.csv")


#B-
#C-
#d-
#E- EC fx 3,05 EC50  

#model control, residual plot ens plot p? begge sider
drm_Harvest1_leaf.area$df.residual
#varians hommogenitet  varians den samme fo dem alle

plot(residuals(drm_Harvest1_leaf.area) ~ fitted(drm_Harvest1_leaf.area))
plot(residuals(drm_Harvest2_leaf.area) ~ fitted(drm_Harvest2_leaf.area))
plot(residuals(drm_Harvest3_leaf.area) ~ fitted(drm_Harvest3_leaf.area))


#Effective dose, ED10, ED 20 og Ed50
ED(drm_Harvest1_leaf.area, c(10,20,50))
ED(drm_Harvest2_leaf.area, c(10,20,50))
ED(drm_Harvest3_leaf.area, c(10,20,50))


#ED-value with confidence intervals
table1 <- ED(drm_Harvest1_leaf.area, c(10,20,50), interval = "delta")
table2 <- ED(drm_Harvest2_leaf.area, c(10,20,50), interval = "delta")
table3 <- ED(drm_Harvest3_leaf.area, c(10,20,50), interval = "delta")


write.csv2(table1, "leaf.area_phen_KV1_Effective dose w confint.csv")
write.csv2(table2, "leaf.area_phen_KV2_Effective dose w confint.csv")
write.csv2(table3, "leaf.area_phen_KV3_Effective dose w confint.csv")

