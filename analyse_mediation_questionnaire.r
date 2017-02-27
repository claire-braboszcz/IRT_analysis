
setwd('/home/claire/DATA/Etude Benjamin/')

data48 <- read.csv('n48_PrePostSuivi.csv')

patient<-1:48
data48$patient<-as.factor(patient)


#-------------------
## Data Centering
#-------------------

# Sommeil
data48$Epworth.PRE<-data48$Epworth.PRE-mean(data48$Epworth.PRE)
data48$ISI.PRE<-data48$ISI.PRE-mean(data48$ISI.PRE)
data48$PSQI.PRE<-data48$PSQI.PRE-mean(data48$PSQI.PRE)
data48$QualitéSommeilSur10.PRE<-data48$QualitéSommeilSur10.PRE-mean(data48$QualitéSommeilSur10.PRE)
data48$Sleep50.PRE  <- data48$Sleep50.PRE - mean(data48$Sleep50.PRE )
data48$DétresseSur10.PRE <- data48$DétresseSur10.PRE -mean(data48$DétresseSur10.PRE)
data48$NuitCauchemarParMois.PRE <- data48$NuitCauchemarParMois.PRE-mean(data48$NuitCauchemarParMois.PRE)
data48$NombreCauchemarParMois.PRE <- data48$NombreCauchemarParMois.PRE - mean(data48$NombreCauchemarParMois.PRE)

data48$Epworth.POST<-data48$Epworth.POST-mean(data48$Epworth.POST)
data48$ISI.POST<-data48$ISI.POST-mean(data48$ISI.POST)
data48$PSQI.POST<-data48$PSQI.POST-mean(data48$PSQI.POST)
data48$QualitéSommeilSur10.POST<-data48$QualitéSommeilSur10.POST-mean(data48$QualitéSommeilSur10.POST)
data48$Sleep50.POST  <- data48$Sleep50.POST - mean(data48$Sleep50.POST )
data48$DétresseSur10.POST <- data48$DétresseSur10.POST -mean(data48$DétresseSur10.POST)
data48$NuitCauchemarParMois.POST <- data48$NuitCauchemarParMois.POST-mean(data48$NuitCauchemarParMois.POST)
data48$NombreCauchemarParMois.POST <- data48$NombreCauchemarParMois.POST - mean(data48$NombreCauchemarParMois.POST)

# Imagerie
data48$VVIQYeuxFermé.PRE<-data48$VVIQYeuxFermé.PRE-mean(data48$VVIQYeuxFermé.PRE)
data48$VVIQYeuxOuvert.PRE<-data48$VVIQYeuxOuvert.PRE-mean(data48$VVIQYeuxOuvert.PRE)
data48$GordonTotal.PRE <- data48$GordonTotal.PRE - mean(data48$GordonTotal.PRE)
data48$TestdeRotation.PRE <- data48$TestdeRotation.PRE - mean(data48$TestdeRotation.PRE)

data48$VVIQYeuxFermé.POST<-data48$VVIQYeuxFermé.POST-mean(data48$VVIQYeuxFermé.POST)
data48$VVIQYeuxOuvert.POST<-data48$VVIQYeuxOuvert.POST-mean(data48$VVIQYeuxOuvert.POST)
data48$GordonTotal.POST <- data48$GordonTotal.POST - mean(data48$GordonTotal.POST)
data48$TestdeRotation.POST <- data48$TestdeRotation.POST - mean(data48$TestdeRotation.POST)

# PTSD
data48$IERSIntrusionSouvenirs.PRE<-data48$IERSIntrusionSouvenirs.PRE-mean(data48$IERSIntrusionSouvenirs.PRE)
data48$IERSEvitement.PRE<-data48$IERSEvitement.PRE - mean(data48$IERSEvitement.PRE)
data48$IERSActivationNeurovégétative.PRE <-data48$IERSActivationNeurovégétative.PRE - mean(data48$IERSActivationNeurovégétative.PRE)
data48$IERS.Global.PRE<-data48$IERS.Global.PRE-mean(data48$IERS.Global.PRE)
data48$PCLS.PRE<-data48$PCLS.PRE-mean(data48$PCLS.PRE)

# WBSI
data48$WBSIintrusion.PRE <- data48$WBSIintrusion.PRE - mean(data48$WBSIintrusion.PRE)
data48$WBSIsuppression.PRE <-data48$WBSIsuppression.PRE - mean(data48$WBSIsuppression.PRE)
data48$SentimentControleGénéral.PRE <- data48$SentimentControleGénéral.PRE - mean(data48$SentimentControleGénéral.PRE)


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



# Causal mediation

data48$Improv_Nightmares = data48$AmeliorationNuit+ data48$AmeliorationSleep50 + data48$Amelioration.NbCauchemars.PrePost+data48$AmeliorationDetresse

data48$Score_suppression = data48$IERSEvitement.PRE + data48$WBSIsuppression.PRE
data48$Score_Intrusion = data48$IERSIntrusionSouvenirs.PRE + data48$WBSIintrusion.PRE
data48$Score_imagerie = data48$GordonTotal.PRE + data48$VVIQYeuxFermé.PRE + data48$VVIQYeuxOuvert.PRE+data48$TestdeRotation.PRE

# Y~X
Model1 = lm(data48$Improv_Nightmares~data48$Score_Intrusion)
summary(Model1)$coef

# Y ~M + X
Model2 = lm(data48$Improv_Nightmares~data48$Score_imagerie  + data48$Score_Intrusion )
summary(Model2)$coef

# M ~ X
Model3 = lm(data48$Score_imagerie ~data48$Score_Intrusion)
summary(Model3)$coef

source('~/SCRIPTS/IRT_analysis/mediate.R')


data48$PostCauchemars <- data48$NuitCauchemarParMois.POST + data48$NombreCauchemarParMois.POST + data48$Sleep50.POST + data48$DétresseSur10.POST
data48$PreCauchemars <- data48$NuitCauchemarParMois.PRE + data48$NombreCauchemarParMois.PRE + data48$Sleep50.PRE + data48$DétresseSur10.PRE

data48$PreScore_imagerie = data48$GordonTotal.PRE + data48$VVIQYeuxFermé.PRE + data48$VVIQYeuxOuvert.PRE+data48$TestdeRotation.PRE

with(data48, mediate(PreCauchemars,PostCauchemars, PreScore_imagerie, names=c("Cauchemars Pre", "Cauchemars Post", "Score Imagerie")))


with(data48, mediate(PreScore_imagerie,AmeĺiorationEvaluationCauchemars, Score_Intrusion, names=c("PreScore_imagerie","AmeĺiorationEvaluationCauchemars", "Score_Intrusion")))



# SEM

library(lavaan)
library(semPlot)

data48$Improv_Nightmares = data48$AmeliorationNuit+ data48$AmeliorationSleep50 + data48$Amelioration.NbCauchemars.PrePost+data48$AmeliorationDetresse

data48$Score_suppression = data48$IERSEvitement.PRE + data48$WBSIsuppression.PRE
data48$Score_Intrusion = data48$IERSIntrusionSouvenirs.PRE + data48$WBSIintrusion.PRE
data48$Score_imagerie = data48$GordonTotal.PRE + data48$VVIQYeuxFermé.PRE + data48$VVIQYeuxOuvert.PRE+data48$TestdeRotation.PRE


model <- '
# measurement model
  Nightmares =~ AmeliorationNuit+ AmeliorationSleep50 +AmeliorationDetresse
Suppression =~ IERSEvitement.PRE + WBSIsuppression.PRE
Imagerie =~ GordonTotal.PRE + VVIQYeuxFermé.PRE + VVIQYeuxOuvert.PRE+TestdeRotation.PRE

# regressions
  Suppression ~ Imagerie
  Nightmares ~ Imagerie + Suppression

'

fit <- sem(model, data=data48)
summary(fit, standardized=TRUE)
semPaths(fit, "sem")
