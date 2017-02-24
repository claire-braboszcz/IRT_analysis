
setwd('/home/claire/DATA/Etude Benjamin/')

data48 <- read.csv('n48_PrePostSuivi.csv')

patient<-1:48
data48$patient<-as.factor(patient)


#-------------------
## Data Centering
#-------------------

# PTSD
data48$IERSIntrusionSouvenirs.PRE<-data48$IERSIntrusionSouvenirs.PRE-mean(data48$IERSIntrusionSouvenirs.PRE)
data48$IERSEvitement.PRE<-data48$IERSEvitement.PRE - mean(data48$IERSEvitement.PRE)
data48$IERSActivationNeurovégétative.PRE <-data48$IERSActivationNeurovégétative.PRE - mean(data48$IERSActivationNeurovégétative.PRE)
data48$IERS.Global.PRE<-data48$IERS.Global.PRE-mean(data48$IERS.Global.PRE)
data48$PCLS.PRE<-data48$PCLS.PRE-mean(data48$PCLS.PRE)

# Sommeil
data48$Epworth.PRE<-data48$Epworth.PRE-mean(data48$Epworth.PRE)
data48$ISI.PRE<-data48$ISI.PRE-mean(data48$ISI.PRE)
data48$PSQI.PRE<-data48$PSQI.PRE-mean(data48$PSQI.PRE)
data48$QualitéSommeilSur10.PRE<-data48$QualitéSommeilSur10.PRE-mean(data48$QualitéSommeilSur10.PRE)

# Imagerie mentale
data48$VVIQYeuxFermé.PRE<-data48$VVIQYeuxFermé.PRE-mean(data48$VVIQYeuxFermé.PRE)
data48$VVIQYeuxOuvert.PRE<-data48$VVIQYeuxOuvert.PRE-mean(data48$VVIQYeuxOuvert.PRE)

# Cauchermars
data48$NombreCauchemarParMois.POST<-data48$NombreCauchemarParMois.POST-mean(data48$NombreCauchemarParMois.POST)
data48$NombreCauchemarParMois.PRE<-data48$NombreCauchemarParMois.PRE-mean(data48$NombreCauchemarParMois.PRE)
data48$Amelioration.NbCauchemars.PrePost <- data48$NombreCauchemarParMois.POST - data48$NombreCauchemarParMois.PRE

#-------------------------------------------------
# Create variable fpr améliorations
#-------------------------------------------------

data48$AmeliorationSleep50<- data48$Sleep50.PRE-data48$Sleep50.POST
data48$AmeliorationDetresse<- data48$DétresseSur10.PRE-data48$DétresseSur10.POST
data48$AmeliorationIERS<- data48$IERS.Global.PRE-data48$IERS.Global.POST
data48$AmeliorationPCLS<- data48$PCLS.PRE-data48$PCLS.POST
data48$AmeliorationQualiteSommeil <- data48$QualitéSommeilSur10.POST-data48$QualitéSommeilSur10.PRE
data48$AmeliorationPSQI<-data48$PSQI.PRE-data48$PSQI.POST
data48$AmeliorationISI<- data48$ISI.PRE-data48$ISI.POST
data48$AmeliorationEpworth <-data48$Epworth.PRE-data48$Epworth.POST
data48$AmeliorationNuit <- data48$NuitCauchemarParMois.PRE-data48$NuitCauchemarParMois.POST
data48$Amelioration.NbCauchemars.PrePost <- data48$NombreCauchemarParMois.PRE - data48$NombreCauchemarParMois.POST

#-----------------------------------------------
# Confirmatory Factor Analysis
#----------------------------------------------


library(lavaan)
library(semPlot)

Improv_Nightmares =~ data48$AmeliorationNuit+ data48$AmeliorationSleep50 + data48$Amelioration.NbCauchemars.PrePost+data48$AmeliorationDetresse
Improv_PTSD =~ data48$AmeliorationIERS + data48$AmeliorationPCLS
Score_imagerie =~ data48$GordonTotal.PRE + data48$VVIQYeuxFermé.PRE + data48$VVIQYeuxOuvert.PRE+data48$TestdeRotation.PRE

# sepcify model
nightmare.mod <- 'Improv_Nightmares =~ NuitCauchemarParMois.POST + NombreCauchemarParMois.POST 
Improv_PTSD =~ IERSIntrusionSouvenirs.POST + IERSActivationNeurovégétative.POST + IERSEvitement.POST
Score_imagerie =~ GordonTotal.PRE + VVIQYeuxFermé.PRE + VVIQYeuxOuvert.PRE+ TestdeRotation.PRE'

nightmare.mod2 <- 'Improv_Nightmares =~ NuitCauchemarParMois.POST + NombreCauchemarParMois.POST 
Improv_PTSD =~ IERSIntrusionSouvenirs.POST + IERSActivationNeurovégétative.POST + IERSEvitement.POST
Score_imagerie =~ GordonTotal.PRE + VVIQYeuxFermé.PRE + VVIQYeuxOuvert.PRE+ TestdeRotation.PRE'

# fit model cfa
fit<-cfa(nightmare.mod, data=data48)

summary(fit, fit.measures=TRUE)


#-----------------------------------------------
# SEM Analysis
#----------------------------------------------


library(lavaan)
library(semPlot)

Improv_Nightmares =~ data48$AmeliorationNuit+ data48$AmeliorationSleep50 + data48$Amelioration.NbCauchemars.PrePost+data48$AmeliorationDetresse
Score_suppression =~ data48$IERSEvitement.PRE + data48$WBSIsuppression.PRE
Score_Intrusion =~ data48$IERSIntrusionSouvenirs.PRE + data48$WBSIintrusion.PRE
Score_imagerie =~ data48$GordonTotal.PRE + data48$VVIQYeuxFermé.PRE + data48$VVIQYeuxOuvert.PRE+data48$TestdeRotation.PRE


Improv_Nightmares =~ data48$AmeliorationNuit+ data48$AmeliorationSleep50 + data48$Amelioration.NbCauchemars.PrePost+data48$AmeliorationDetresse
Improv_PTSD =~ data48$AmeliorationIERS + data48$AmeliorationPCLS
Score_imagerie =~ data48$GordonTotal.PRE + data48$VVIQYeuxFermé.PRE + data48$VVIQYeuxOuvert.PRE+data48$TestdeRotation.PRE

# sepcify model
mod1 <- '
        # measurement model
        Improv_Nightmares =~ AmeliorationNuit+ AmeliorationSleep50 + Amelioration.NbCauchemars.PrePost+ AmeliorationDetresse
        Score_Intrusion =~ IERSIntrusionSouvenirs.PRE +  WBSIintrusion.PRE
        Score_imagerie =~ GordonTotal.PRE + VVIQYeuxFermé.PRE + VVIQYeuxOuvert.PRE + TestdeRotation.PRE

        # REGRESIONS  
        Improv_Nightmares ~ Score_Intrusion + Score_imagerie
        Score_Intrusion ~ Score_imagerie
'



fit <- sem(mod1, data=data48)
summary(fit, standardized=TRUE)

semPaths(fit, "std", "est" )
semPaths(fit)