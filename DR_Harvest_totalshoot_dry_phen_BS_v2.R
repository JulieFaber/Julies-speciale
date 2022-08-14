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
# summary(dat$Harvest1_Total_shoot_l.s.b.f)
str(dat)
str(dat$Harvest1_Total_shoot_l.s.b.f)
dat$Harvest1_Total_shoot_l.s.b.f

### change variables type formats 
dat$Harvest1_Total_shoot_l.s.b.f <- as.numeric(dat$Harvest1_Total_shoot_l.s.b.f)
dat$Harvest2_Total_shoot_l.s.b.f <- as.numeric(dat$Harvest2_Total_shoot_l.s.b.f)
dat$Harvest3_Total_shoot_l.s.b.f <- as.numeric(dat$Harvest3_Total_shoot_l.s.b.f)

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
# dat2 <- dat[dat$Species=="Valmue",]
dat2 <- dat[dat$Species=="Bloed Storkenaeb",]
# dat2 <- dat[dat$Species=="Ager sennep",]
# dat2 <- dat[dat$Species=="Kornblomst",]

#HERBICIDE#
# dat2 <- dat2[dat2$Herbicide=="Glyphosate",]
dat2 <- dat2[dat2$Herbicide=="Betanal",]

par(mfrow=c(2,2))
plot(dat2$Dose,dat2$Harvest1_Total_shoot_l.s.b.f, ylim = c(0,30),col="black")
plot(dat2$Dose,dat2$Harvest2_Total_shoot_l.s.b.f, ylim = c(0,30),col="blue")
plot(dat2$Dose,dat2$Harvest3_Total_shoot_l.s.b.f, ylim = c(0,30),col="red")

par(mfrow=c(1,1))

summary(dat2$Harvest1_Total_shoot_l.s.b.f)
summary(dat2$Harvest2_Total_shoot_l.s.b.f)
summary(dat2$Harvest3_Total_shoot_l.s.b.f)

#Fitting a dose-response model to data
drm_Harvest1_Total_shoot_l.s.b.f <- drm(Harvest1_Total_shoot_l.s.b.f~ Dose2, data=dat2,fct=LL.4())
drm_Harvest2_Total_shoot_l.s.b.f <- drm(Harvest2_Total_shoot_l.s.b.f~ Dose2, data=dat2,fct=LL.4())
drm_Harvest3_Total_shoot_l.s.b.f <- drm(Harvest3_Total_shoot_l.s.b.f~ Dose2, data=dat2,fct=LL.4())


###############################
#####plotting the model########
###############################

plot(drm_Harvest4_Total_shoot_l.s.b.f, broken= TRUE)

par(mfrow=c(2,2))
#plotting the model with all data
plot(drm_Harvest1_Total_shoot_l.s.b.f,broken=TRUE,type = "all", xlab="Phenmedipharm (g a.i.ha^-1)", ylab="Total_shoot_l.s.b.f ")
title(main="Geranium molle", sub="(day 8)")
plot(drm_Harvest2_Total_shoot_l.s.b.f,broken=TRUE,type = "all", xlab="Phenmedipharm (g a.i.ha^-1)", ylab="Total_shoot_l.s.b.f ")
title(main="Geranium molle", sub="(day 15)")
plot(drm_Harvest3_Total_shoot_l.s.b.f,broken=TRUE,type = "all", xlab="Phenmedipharm (g a.i.ha^-1)", ylab="Total_shoot_l.s.b.f ")
title(main="Geranium molle", sub="(day 21)")


par(mfrow=c(1,1))

#summary of the modelfit
summary(drm_Harvest1_Total_shoot_l.s.b.f)
summary(drm_Harvest2_Total_shoot_l.s.b.f)
summary(drm_Harvest3_Total_shoot_l.s.b.f)



#########################################
#####plotting the model in same plot#####
#########################################

#  Jeg synes du skal forsøge at få alle målinger for alle 3 datoer ind i samme plot.
# Du kan enten lægge det nye plot oveni det gamle med add=TRUE eller lave en model hvor hver dato får sin egen kurve (med curveID=dato i drm()).
# Sidstnævnte giver så også mulighed for nemt at kunne sammenligne parametre over datoer.


par(mfrow=c(1,1))
#plotting the model with all data
plot(drm_Harvest1_Total_shoot_l.s.b.f,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="Total_shoot_l.s.b.f  ",ylim = c(0,20),lty=1)
title(main="Geranium molle")
plot(drm_Harvest2_Total_shoot_l.s.b.f,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="Total_shoot_l.s.b.f  ",add = TRUE,col="red")

plot(drm_Harvest3_Total_shoot_l.s.b.f,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="Total_shoot_l.s.b.f  ",add = TRUE,col="blue")

grid()

#Fitting a dose-response model to data
par(mfrow=c(1,1))
plot(drm_Harvest1_Total_shoot_l.s.b.f,broken=TRUE,type = "all", xlab="Phenmedipharm (g a.i.ha^-1)", ylab="Total_shoot_l.s.b.f  ",ylim = c(0,20),lty=1,pch = 20)
title(main="Geranium molle")
plot(drm_Harvest3_Total_shoot_l.s.b.f,broken=TRUE,type = "all", xlab="Phenmedipharm(g a.i.ha^-1)", ylab="Total_shoot_l.s.b.f  ",add = TRUE,col="blue",pch = 20)
legend("topleft",col = c("black","blue"),legend = c("day 8","day 21"),lty = c(1,1),pch = c(20,20))


#summary of the modelfit
table1 <-summary(drm_Harvest1_Total_shoot_l.s.b.f)
table2 <-summary(drm_Harvest2_Total_shoot_l.s.b.f)
table3 <-summary(drm_Harvest3_Total_shoot_l.s.b.f)


write.csv2(table1$coefficients, "Total_shoot_l.s.b.f_phen_BS1_Parameter estimates.csv")
write.csv2(table2$coefficients, "Total_shoot_l.s.b.f_phen_BS2_Parameter estimates.csv")
write.csv2(table3$coefficients, "Total_shoot_l.s.b.f_phen_BS3_Parameter estimates.csv")


#B-
#C-
#d-
#E- EC fx 3,05 EC50  

#model control, residual plot ens plot p? begge sider
drm_Harvest1_Total_shoot_l.s.b.f$df.residual
#varians hommogenitet  varians den samme fo dem alle

plot(residuals(drm_Harvest1_Total_shoot_l.s.b.f) ~ fitted(drm_Harvest1_Total_shoot_l.s.b.f))
plot(residuals(drm_Harvest2_Total_shoot_l.s.b.f) ~ fitted(drm_Harvest2_Total_shoot_l.s.b.f))
plot(residuals(drm_Harvest3_Total_shoot_l.s.b.f) ~ fitted(drm_Harvest3_Total_shoot_l.s.b.f))


#Effective dose, ED10, ED 20 og Ed50
ED(drm_Harvest1_Total_shoot_l.s.b.f, c(10,20,50))
ED(drm_Harvest2_Total_shoot_l.s.b.f, c(10,20,50))
ED(drm_Harvest3_Total_shoot_l.s.b.f, c(10,20,50))


#ED-value with confidence intervals
table1 <- ED(drm_Harvest1_Total_shoot_l.s.b.f, c(10,20,50), interval = "delta")
table2 <- ED(drm_Harvest2_Total_shoot_l.s.b.f, c(10,20,50), interval = "delta")
table3 <- ED(drm_Harvest3_Total_shoot_l.s.b.f, c(10,20,50), interval = "delta")


write.csv2(table1, "Total_shoot_l.s.b.f_phen_BS1_Effective dose w confint.csv")
write.csv2(table2, "Total_shoot_l.s.b.f_phen_BS2_Effective dose w confint.csv")
write.csv2(table3, "Total_shoot_l.s.b.f_phen_BS3_Effective dose w confint.csv")

