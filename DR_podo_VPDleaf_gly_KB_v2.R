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
# summary(dat$Podo1_VPDleaf)
str(dat)
str(dat$Podo1_VPDleaf)
dat$Podo1_VPDleaf

### change variables type formats 
dat$Podo1_VPDleaf <- as.numeric(dat$Podo1_VPDleaf)
dat$Podo2_VPDleaf <- as.numeric(dat$Podo2_VPDleaf)
dat$Podo3_VPDleaf <- as.numeric(dat$Podo3_VPDleaf)
dat$Podo4_VPDleaf <- as.numeric(dat$Podo4_VPDleaf)
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

dat$Podo1_VPDleaf_avg <- numeric(n) #ret her
g <- unique(dat$`Pot ID`)
for (i in g) {
  pot_val <- dat$Podo1_VPDleaf[dat$`Pot ID`==i] #ret her
  pot_val <- as.numeric(pot_val)
  avg <- mean(pot_val,na.rm = TRUE)
  dat$Podo1_VPDleaf_avg[dat$`Pot ID`==i] <- avg #ret her
}

dat$Podo4_VPDleaf_avg <- numeric(n) #ret her
g <- unique(dat$`Pot ID`)
for (i in g) {
  pot_val <- dat$Podo4_VPDleaf[dat$`Pot ID`==i] #ret her
  pot_val <- as.numeric(pot_val)
  avg <- mean(pot_val,na.rm = TRUE)
  dat$Podo4_VPDleaf_avg[dat$`Pot ID`==i] <- avg #ret her
}

dat$Podo2_VPDleaf_avg <- numeric(n) #ret her
g <- unique(dat$`Pot ID`)
for (i in g) {
  pot_val <- dat$Podo2_VPDleaf[dat$`Pot ID`==i] #ret her
  pot_val <- as.numeric(pot_val)
  avg <- mean(pot_val,na.rm = TRUE)
  dat$Podo2_VPDleaf_avg[dat$`Pot ID`==i] <- avg #ret her
}

dat$Podo3_VPDleaf_avg <- numeric(n) #ret her
g <- unique(dat$`Pot ID`)
for (i in g) {
  pot_val <- dat$Podo3_VPDleaf[dat$`Pot ID`==i] #ret her
  pot_val <- as.numeric(pot_val)
  avg <- mean(pot_val,na.rm = TRUE)
  dat$Podo3_VPDleaf_avg[dat$`Pot ID`==i] <- avg #ret her
}


##########################
### Plot data chl,dose ###
##########################

#SPECIES#
unique(dat$Species)
# dat2 <- dat[dat$Species=="Valmue",]
# dat2 <- dat[dat$Species=="Bloed Storkenaeb",]
# dat2 <- dat[dat$Species=="Ager sennep",]
dat2 <- dat[dat$Species=="Kornblomst",]

#HERBICIDE#
dat2 <- dat2[dat2$Herbicide=="Glyphosate",]
# dat2 <- dat2[dat2$Herbicide=="Betanal",]

par(mfrow=c(2,2))
plot(dat2$Dose,dat2$Podo1_VPDleaf, ylim = c(0,30),col="black")
plot(dat2$Dose,dat2$Podo2_VPDleaf, ylim = c(0,30),col="blue")
plot(dat2$Dose,dat2$Podo3_VPDleaf, ylim = c(0,30),col="red")
plot(dat2$Dose,dat2$Podo4_VPDleaf, ylim = c(0,30),col="green")
par(mfrow=c(1,1))

summary(dat2$Podo1_VPDleaf)
summary(dat2$Podo2_VPDleaf)
summary(dat2$Podo3_VPDleaf)
summary(dat2$Podo4_VPDleaf)

#Fitting a dose-response model to data
drm_Podo1_VPDleaf <- drm(Podo1_VPDleaf~ Dose2, data=dat2,fct=LL.4())
drm_Podo2_VPDleaf <- drm(Podo2_VPDleaf~ Dose2, data=dat2,fct=LL.4())
drm_Podo3_VPDleaf <- drm(Podo3_VPDleaf~ Dose2, data=dat2,fct=LL.4())
drm_Podo4_VPDleaf <- drm(Podo4_VPDleaf~ Dose2, data=dat2,fct=LL.4())


#Fitting a dose-response model to data
dat3 <- dat2[dat2$`PotPlant ID`==1,]
drm_Podo1_VPDleaf_avg <- drm(Podo1_VPDleaf_avg~ Dose2, data=dat3,fct=LL.4())
drm_Podo2_VPDleaf_avg <- drm(Podo2_VPDleaf_avg~ Dose2, data=dat3,fct=LL.4())
drm_Podo3_VPDleaf_avg <- drm(Podo3_VPDleaf_avg~ Dose2, data=dat3,fct=LL.4())
drm_Podo4_VPDleaf_avg <- drm(Podo4_VPDleaf_avg~ Dose2, data=dat3,fct=LL.4())


###############################
#####plotting the model########
###############################

plot(drm_Podo4_Flav, broken= TRUE)

par(mfrow=c(2,2))
#plotting the model with all data
plot(drm_Podo1_VPDleaf,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="VPDleaf ")
title(main="Centaurea cyanus", sub="(day 2)")
plot(drm_Podo2_VPDleaf,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="VPDleaf ")
title(main="Centaurea cyanus", sub="(day 6)")
plot(drm_Podo3_VPDleaf,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="VPDleaf ")
title(main="Centaurea cyanus", sub="(day 13)")
plot(drm_Podo4_VPDleaf,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="VPDleaf ")
title(main="Centaurea cyanus", sub="(day 20)")

par(mfrow=c(1,1))


#########################################
#####plotting the model in same plot#####
#########################################

#  Jeg synes du skal forsøge at få alle målinger for alle 3 datoer ind i samme plot.
# Du kan enten lægge det nye plot oveni det gamle med add=TRUE eller lave en model hvor hver dato får sin egen kurve (med curveID=dato i drm()).
# Sidstnævnte giver så også mulighed for nemt at kunne sammenligne parametre over datoer.


par(mfrow=c(1,1))
#plotting the model with all data
plot(drm_Podo1_VPDleaf_avg,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="VPDleaf ",ylim = c(0,2.5),lty=1)
title(main="Centaurea cyanus", sub="(day 2)")
plot(drm_Podo2_VPDleaf_avg,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="VPDleaf ",add = TRUE,col="red")
plot(drm_Podo3_VPDleaf_avg,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="VPDleaf ",add = TRUE,col="blue")
plot(drm_Podo4_VPDleaf_avg,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="VPDleaf ",add = TRUE,col="green")
grid()

#Fitting a dose-response model to data
par(mfrow=c(1,1))
plot(drm_Podo1_VPDleaf_avg,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="VPDleaf ",ylim = c(0,2.5),lty=1,pch = 20)
title(main="Centaurea cyanus")
plot(drm_Podo4_VPDleaf_avg,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="VPDleaf ",add = TRUE,col="green",pch = 20)
legend("topleft",col = c("black","green"),legend = c("day 2","day 20"),lty = c(1,1),pch = c(20,20))


#Fitting a dose-response model to data
par(mfrow=c(1,1))
par(mar=c(5.1, 5.1, 4.1, 2.1), mgp=c(3.75, 1, 0), las=0)
plot(drm_Podo1_VPDleaf_avg,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="VPDleaf (kPa) ",ylim =c(0,3),lty=1, lwd=3,pch = 20,cex.axis = 1.5, cex.lab=1.5)
title(main=list("Centaurea cyanus",cex = 2.5))
plot(drm_Podo4_VPDleaf_avg,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="VPDleaf (kPa) ",add = TRUE,col="blue",pch = 2,lwd=3)
legend("topleft",col = c("black","blue"),legend = c("day 3","day 21"),lty = c(1,1),pch = c(20,2))

#summary of the modelfit
table1 <-summary(drm_Podo1_VPDleaf_avg)
table2 <-summary(drm_Podo2_VPDleaf_avg)
table3 <-summary(drm_Podo3_VPDleaf_avg)
table4 <-summary(drm_Podo4_VPDleaf_avg)

write.csv2(table1$coefficients, "VPDleaf_gly_KB1_Parameter estimates.csv")
write.csv2(table2$coefficients, "VPDleaf_gly_KB2_Parameter estimates.csv")
write.csv2(table3$coefficients, "VPDleaf_gly_KB3_Parameter estimates.csv")
write.csv2(table4$coefficients, "VPDleaf_gly_KB4_Parameter estimates.csv")

#B-
#C-
#d-
#E- EC fx 3,05 EC50  

#model control, residual plot ens plot p? begge sider
drm_Podo1_Chl$df.residual
#varians hommogenitet  varians den samme fo dem alle

plot(residuals(drm_Podo1_VPDleaf_avg) ~ fitted(drm_Podo1_VPDleaf_avg))
plot(residuals(drm_Podo2_VPDleaf_avg) ~ fitted(drm_Podo2_VPDleaf_avg))
plot(residuals(drm_Podo3_VPDleaf_avg) ~ fitted(drm_Podo3_VPDleaf_avg))
plot(residuals(drm_Podo4_VPDleaf_avg) ~ fitted(drm_Podo4_VPDleaf_avg))

#Effective dose, ED10, ED 20 og Ed50
ED(drm_Podo1_VPDleaf_avg, c(10,20,50))
ED(drm_Podo2_VPDleaf_avg, c(10,20,50))
ED(drm_Podo3_VPDleaf_avg, c(10,20,50))
ED(drm_Podo4_VPDleaf_avg, c(10,20,50))

#ED-value with confidence intervals
table1 <- ED(drm_Podo1_VPDleaf_avg, c(10,20,50), interval = "delta")
table2 <- ED(drm_Podo2_VPDleaf_avg, c(10,20,50), interval = "delta")
table3 <- ED(drm_Podo3_VPDleaf_avg, c(10,20,50), interval = "delta")
table4 <- ED(drm_Podo4_VPDleaf_avg, c(10,20,50), interval = "delta")

write.csv2(table1, "VPDleaf_gly_KB1_Effective dose w confint.csv")
write.csv2(table2, "VPDleaf_gly_KB2_Effective dose w confint.csv")
write.csv2(table3, "VPDleaf_gly_KB3_Effective dose w confint.csv")
write.csv2(table4, "VPDleaf_gly_KB4_Effective dose w confint.csv")

