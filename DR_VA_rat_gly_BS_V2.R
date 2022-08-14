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
# summary(dat$VA1_number)
str(dat)
str(dat$VA1_number)
dat$VA1_number

### change variables type formats 
dat$VA1_number <- as.numeric(dat$VA1_number)
dat$VA2_number <- as.numeric(dat$VA2_number)
dat$VA3_number <- as.numeric(dat$VA3_number)
dat$VA4_number <- as.numeric(dat$VA4_number)
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
dat2 <- dat2[dat2$Herbicide=="Glyphosate",]
# dat2 <- dat2[dat2$Herbicide=="Betanal",]

par(mfrow=c(2,2))
plot(dat2$Dose,dat2$VA1_number, ylim = c(0,30),col="black")
plot(dat2$Dose,dat2$VA2_number, ylim = c(0,30),col="blue")
plot(dat2$Dose,dat2$VA3_number, ylim = c(0,30),col="red")
plot(dat2$Dose,dat2$VA4_number, ylim = c(0,30),col="green")
par(mfrow=c(1,1))

summary(dat2$VA1_number)
summary(dat2$VA2_number)
summary(dat2$VA3_number)
summary(dat2$VA4_number)

#Fitting a dose-response model to data
drm_VA1_number <- drm(VA1_number~ Dose2, data=dat2,fct=LL.4())
drm_VA2_number <- drm(VA2_number~ Dose2, data=dat2,fct=LL.4())
drm_VA3_number <- drm(VA3_number~ Dose2, data=dat2,fct=LL.4())
drm_VA4_number <- drm(VA4_number~ Dose2, data=dat2,fct=LL.4())
summary(drm_VA1_number)
summary(drm_VA3_number)
AIC(drm_VA1_number)
AIC(drm_VA3_number)

###############################
#####plotting the model########
###############################

plot(drm_Dua4_Flav, broken= TRUE)

par(mfrow=c(2,2))
#plotting the model with all data
plot(drm_VA1_number,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="VA_number ")
title(main="Geranium molle", sub="(day 6)")
plot(drm_VA2_number,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="VA_number ")
title(main="Geranium molle", sub="(day 13)")
plot(drm_VA3_number,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="VA_number ")
title(main="Geranium molle", sub="(day 20)")
plot(drm_VA4_number,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="VA_number ")
title(main="Geranium molle", sub="(day 44)")

par(mfrow=c(1,1))

#summary of the modelfit
summary(drm_VA1_number)
summary(drm_VA2_number)
summary(drm_VA3_number)
summary(drm_VA4_number)



#########################################
#####plotting the model in same plot#####
#########################################

#  Jeg synes du skal forsøge at få alle målinger for alle 3 datoer ind i samme plot.
# Du kan enten lægge det nye plot oveni det gamle med add=TRUE eller lave en model hvor hver dato får sin egen kurve (med curveID=dato i drm()).
# Sidstnævnte giver så også mulighed for nemt at kunne sammenligne parametre over datoer.


par(mfrow=c(1,1))
#plotting the model with all data
plot(drm_VA1_number,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="VA_number  ",ylim = c(0,10),lty=1)
title(main="Geranium molle")
plot(drm_VA2_number,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="VA_number  ",add = TRUE,col="red")
plot(drm_VA3_number,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="VA_number  ",add = TRUE,col="blue")
plot(drm_VA4_number,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="VA_number  ",add = TRUE,col="green")
grid()

#Fitting a dose-response model to data
par(mfrow=c(1,1))
plot(drm_VA1_number,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="VA_number  ",ylim = c(0,10),lty=1,pch = 20)
title(main="Geranium molle")
plot(drm_VA3_number,broken=TRUE,type = "all", xlab="Glyphosate (g a.i.ha^-1)", ylab="VA_number  ",add = TRUE,col="blue",pch = 20)
legend("topleft",col = c("black","blue"),legend = c("day 6","day 20"),lty = c(1,1),pch = c(20,20))


#Fitting a dose-response model to data
par(mfrow=c(1,1))
par(mar=c(5.1, 5.1, 4.1, 2.1), mgp=c(3.75, 1, 0), las=0)
plot(drm_VA1_number,broken=TRUE,type = "all", xlab="Phenmedipharm (g a.i.ha^-1)", ylab="VA_number ",ylim =c(0,5),lty=1, lwd=3,pch = 20,cex.axis = 1.5, cex.lab=1.5)
title(main=list("Geranium molle",cex = 2.5))
plot(drm_VA3_number,broken=TRUE,type = "all", xlab="Phenmedipharm (g a.i.ha^-1)", ylab="VA_number ",add = TRUE,col="blue",pch = 2,lwd=3)
legend("topleft",col = c("black","blue"),legend = c("day 6","day 20"),lty = c(1,1),pch = c(20,2))
summary(drm_Podo1_v_temp_avg)
summary(drm_Podo1_v_temp_avg)



#summary of the modelfit
table1 <-summary(drm_VA1_number)
table2 <-summary(drm_VA2_number)
table3 <-summary(drm_VA3_number)
table4 <-summary(drm_VA4_number)

write.csv2(table1$coefficients, "number_gly_BS1_Parameter estimates.csv")
write.csv2(table2$coefficients, "number_gly_BS2_Parameter estimates.csv")
write.csv2(table3$coefficients, "number_gly_BS3_Parameter estimates.csv")
write.csv2(table4$coefficients, "number_gly_BS4_Parameter estimates.csv")

#B-
#C-
#d-
#E- EC fx 3,05 EC50  

#model control, residual plot ens plot p? begge sider
drm_VA1_number$df.residual
#varians hommogenitet  varians den samme fo dem alle

plot(residuals(drm_VA1_number) ~ fitted(drm_VA1_number))
plot(residuals(drm_VA2_number) ~ fitted(drm_VA2_number))
plot(residuals(drm_VA3_number) ~ fitted(drm_VA3_number))
plot(residuals(drm_VA4_number) ~ fitted(drm_VA4_number))

#Effective dose, ED10, ED 20 og Ed50
ED(drm_VA1_number, c(10,20,50))
ED(drm_VA2_number, c(10,20,50))
ED(drm_VA3_number, c(10,20,50))
ED(drm_VA4_number, c(10,20,50))

#ED-value with confidence intervals
table1 <- ED(drm_VA1_number, c(10,20,50), interval = "delta")
table2 <- ED(drm_VA2_number, c(10,20,50), interval = "delta")
table3 <- ED(drm_VA3_number, c(10,20,50), interval = "delta")
table4 <- ED(drm_VA4_number, c(10,20,50), interval = "delta")

write.csv2(table1, "number_gly_BS1_Effective dose w confint.csv")
write.csv2(table2, "number_gly_BS2_Effective dose w confint.csv")
write.csv2(table3, "number_gly_BS3_Effective dose w confint.csv")
write.csv2(table4, "number_gly_BS4_Effective dose w confint.csv")

