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
# summary(dat$Dua1_Flav)
str(dat)
str(dat$Dua1_Flav)
dat$Dua1_Flav

### change variables type formats 
dat$Dua1_Flav <- as.numeric(dat$Dua1_Flav)
dat$Dua2_Flav <- as.numeric(dat$Dua2_Flav)
dat$Dua3_Flav <- as.numeric(dat$Dua3_Flav)
dat$Dua4_Flav <- as.numeric(dat$Dua4_Flav)
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
### make avg  ###
##########################

dat$Dua1_Flav_avg <- numeric(n) #ret her
g <- unique(dat$`Pot ID`)
for (i in g) {
  pot_val <- dat$Dua1_Flav[dat$`Pot ID`==i] #ret her
  pot_val <- as.numeric(pot_val)
  avg <- mean(pot_val,na.rm = TRUE)
  dat$Dua1_Flav_avg[dat$`Pot ID`==i] <- avg #ret her
}

dat$Dua4_Flav_avg <- numeric(n) #ret her
g <- unique(dat$`Pot ID`)
for (i in g) {
  pot_val <- dat$Dua4_Flav[dat$`Pot ID`==i] #ret her
  pot_val <- as.numeric(pot_val)
  avg <- mean(pot_val,na.rm = TRUE)
  dat$Dua4_Flav_avg[dat$`Pot ID`==i] <- avg #ret her
}

dat$Dua2_Flav_avg <- numeric(n) #ret her
g <- unique(dat$`Pot ID`)
for (i in g) {
  pot_val <- dat$Dua2_Flav[dat$`Pot ID`==i] #ret her
  pot_val <- as.numeric(pot_val)
  avg <- mean(pot_val,na.rm = TRUE)
  dat$Dua2_Flav_avg[dat$`Pot ID`==i] <- avg #ret her
}

dat$Dua3_Flav_avg <- numeric(n) #ret her
g <- unique(dat$`Pot ID`)
for (i in g) {
  pot_val <- dat$Dua3_Flav[dat$`Pot ID`==i] #ret her
  pot_val <- as.numeric(pot_val)
  avg <- mean(pot_val,na.rm = TRUE)
  dat$Dua3_Flav_avg[dat$`Pot ID`==i] <- avg #ret her
}



##########################
### Plot data chl,dose ###
##########################

#SPECIES#
unique(dat$Species)
# dat2 <- dat[dat$Species=="Valmue",]
# dat2 <- dat[dat$Species=="Bloed Storkenaeb",]
dat2 <- dat[dat$Species=="Ager sennep",]
# dat2 <- dat[dat$Species=="Kornblomst",]

#HERBICIDE#
dat2 <- dat2[dat2$Herbicide=="Glyphosate",]
# dat2 <- dat2[dat2$Herbicide=="Betanal",]

par(mfrow=c(2,2))
plot(dat2$Dose,dat2$Dua1_Flav, ylim = c(0,30),col="black")
plot(dat2$Dose,dat2$Dua2_Flav, ylim = c(0,30),col="blue")
plot(dat2$Dose,dat2$Dua3_Flav, ylim = c(0,30),col="red")
plot(dat2$Dose,dat2$Dua4_Flav, ylim = c(0,30),col="green")
par(mfrow=c(1,1))

summary(dat2$Dua1_Flav)
summary(dat2$Dua2_Flav)
summary(dat2$Dua3_Flav)
summary(dat2$Dua4_Flav)

#Fitting a dose-response model to data
drm_Dua1_Flav <- drm(Dua1_Flav~ Dose2, data=dat2,fct=LL.4())
drm_Dua2_Flav <- drm(Dua2_Flav~ Dose2, data=dat2,fct=LL.4())
drm_Dua3_Flav <- drm(Dua3_Flav~ Dose2, data=dat2,fct=LL.4())
drm_Dua4_Flav <- drm(Dua4_Flav~ Dose2, data=dat2,fct=LL.4())


#Fitting a dose-response model to data
dat3 <- dat2[dat2$`PotPlant ID`==1,]
drm_Dua1_Flav_avg <- drm(Dua1_Flav_avg~ Dose2, data=dat3,fct=LL.4())
drm_Dua2_Flav_avg <- drm(Dua2_Flav_avg~ Dose2, data=dat3,fct=LL.4())
drm_Dua3_Flav_avg <- drm(Dua3_Flav_avg~ Dose2, data=dat3,fct=LL.4())
drm_Dua4_Flav_avg <- drm(Dua4_Flav_avg~ Dose2, data=dat3,fct=LL.4())


###############################
#####plotting the model########
###############################

plot(drm_Dua4_Flav, broken= TRUE)

par(mfrow=c(2,2))
#plotting the moldel with all data
plot(drm_Dua1_Flav,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="Flavonoids ")
title(main="Sinapis arvensis", sub="(day 2)")
plot(drm_Dua2_Flav,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="Flavonoids ")
title(main="Sinapis arvensis", sub="(day 6)")
plot(drm_Dua3_Flav,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="Flavonoids ")
title(main="Sinapis arvensis", sub="(day 13)")
plot(drm_Dua4_Flav,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="Flavonoids ")
title(main="Sinapis arvensis", sub="(day 20)")

par(mfrow=c(1,1))

#summary of the modelfit
summary(drm_Dua1_Flav)
summary(drm_Dua2_Flav)
summary(drm_Dua3_Flav)
summary(drm_Dua4_Flav)


#########################################
#####plotting the model in same plot#####
#########################################

#  Jeg synes du skal forsøge at få alle målinger for alle 3 datoer ind i samme plot.
# Du kan enten lægge det nye plot oveni det gamle med add=TRUE eller lave en model hvor hver dato får sin egen kurve (med curveID=dato i drm()).
# Sidstnævnte giver så også mulighed for nemt at kunne sammenligne parametre over datoer.


par(mfrow=c(1,1))
#plotting the model with all data
plot(drm_Dua1_Flav_avg,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="Flavocyanins ",ylim = c(0.4,400),lty=1)
title(main="Sinapis arvensis", sub="(day 2)")
plot(drm_Dua2_Flav_avg,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="Flavocyanins ",add = TRUE,col="red")
plot(drm_Dua3_Flav_avg,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="Flavocyanins ",add = TRUE,col="blue")
plot(drm_Dua4_Flav_avg,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="Flavocyanins ",add = TRUE,col="green")
grid()

#Fitting a dose-response model to data
par(mfrow=c(1,1))
plot(drm_Dua1_Flav_avg,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="Flavocyanins ",ylim = c(0.4,1),lty=1,pch = 20)
title(main="Sinapis arvensis")
plot(drm_Dua4_Flav_avg,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="Flavocyanins ",add = TRUE,col="green",pch = 20)
legend("topleft",col = c("black","green"),legend = c("day 2","day 20"),lty = c(1,1),pch = c(20,20))



#Fitting a dose-response model to data
par(mfrow=c(1,1))
par(mar=c(5.1, 5.1, 4.1, 2.1), mgp=c(3.75, 1, 0), las=0)
plot(drm_Dua1_Flav_avg,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="Flavocyanins ",ylim = c(0.1,1.5),lty=1, lwd=3,pch = 20,cex.axis = 1.5, cex.lab=1.5)
title(main=list("Sinapis arvensis",cex = 2.5))
plot(drm_Dua4_Flav_avg,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="Flavocyanins ",add = TRUE,col="blue",pch = 2,lwd=3)
legend("topleft",col = c("black","blue"),legend = c("day 2","day 20"),lty = c(1,1),pch = c(20,2))

#summary of the modelfit
table1 <-summary(drm_Dua1_Flav_avg)
table2 <-summary(drm_Dua2_Flav_avg)
table3 <-summary(drm_Dua3_Flav_avg)
table4 <-summary(drm_Dua4_Flav_avg)

write.csv2(table1$coefficients, "Flav_gly_AS1_Parameter estimates.csv")
write.csv2(table2$coefficients, "Flav_gly_AS2_Parameter estimates.csv")
write.csv2(table3$coefficients, "Flav_gly_AS3_Parameter estimates.csv")
write.csv2(table4$coefficients, "Flav_gly_AS4_Parameter estimates.csv")

#B-
#C-
#d-
#E- EC fx 3,05 EC50  

#model control, residual plot ens plot p? begge sider
drm_Dua1_Chl$df.residual
#varians hommogenitet  varians den samme fo dem alle

plot(residuals(drm_Dua1_Flav_avg) ~ fitted(drm_Dua1_Flav_avg))
plot(residuals(drm_Dua2_Flav_avg) ~ fitted(drm_Dua2_Flav_avg))
plot(residuals(drm_Dua3_Flav_avg) ~ fitted(drm_Dua3_Flav_avg))
plot(residuals(drm_Dua4_Flav_avg) ~ fitted(drm_Dua4_Flav_avg))

#Effective dose, ED10, ED 20 og Ed50
ED(drm_Dua1_Flav_avg, c(10,20,50))
ED(drm_Dua2_Flav_avg, c(10,20,50))
ED(drm_Dua3_Flav_avg, c(10,20,50))
ED(drm_Dua4_Flav_avg, c(10,20,50))

#ED-value with confidence intervals
table1 <- ED(drm_Dua1_Flav_avg, c(10,20,50), interval = "delta")
table2 <- ED(drm_Dua2_Flav_avg, c(10,20,50), interval = "delta")
table3 <- ED(drm_Dua3_Flav_avg, c(10,20,50), interval = "delta")
table4 <- ED(drm_Dua4_Flav_avg, c(10,20,50), interval = "delta")

write.csv2(table1, "Flav_gly_AS1_Effective dose w confint.csv")
write.csv2(table2, "Flav_gly_AS2_Effective dose w confint.csv")
write.csv2(table3, "Flav_gly_AS3_Effective dose w confint.csv")
write.csv2(table4, "Flav_gly_AS4_Effective dose w confint.csv")


