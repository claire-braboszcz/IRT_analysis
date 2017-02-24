
setwd('/home/claire/DATA/Etude Benjamin/')

orig <-read.csv('n48_PrePostSuivi.csv')
orig$Amelioration.NbCauchemars.PrePost <- orig$NombreCauchemarParMois.POST - orig$NombreCauchemarParMois.PRE




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


#-----------------------------------------------
# Create general score for criteria of interest
#----------------------------------------------

data48$AllPTSD.PRE<- data48$IERSIntrusionSouvenirs.PRE + data48$IERSEvitement.PRE + data48$IERSActivationNeurovégétative.PRE+data48$PCLS.PRE


#-------------------------------------------------
# effet sur amaéliorations
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


xmdl<-lm(data48$AmeliorationNuit~data48$QualitéSommeilSur10.PRE)
summary(xmdl)

plot(xmdl)

plot(data48$QualitéSommeilSur10.PRE, data48$AmeliorationNuit)
abline(a=coef(xmdl)[1], coef(xmdl)[2])
abline(xmdl)

xmdl.null<-lm(data48$AmeliorationNuit~1)
summary(xmdl.null)

summary(xmdl)$r.squared
summary(xmdl.null)$r.squared

AIC(xmdl)
AIC(xmdl.null)

#-----------------
library(lavaan)

Effect_IRT =~ data48$Amelioration.Impact + data48$Amelioration.Detresse + data48$Amélioration.PRE.SUIVI.moyenne.cauchemars
Imagery =~ data48$PRE.Gordon.total + data48$PRE.VVIQ.Yeux.ouvert + data48$PRE.VVIQ.Yeux.fermé + data48$PRE.Test.de.rotation
Controle =~ data48$PRE.Total.Sentiment.de.contrôle.général + data48$PRE.Total.sentiment.de.contrôle.spécifique
Exposition =~ data48$Temps.total.des.entrainements.sur.la.période 

model <- ' # latent variables definition
            Effect_IRT =~ Amelioration.Impact + Amelioration.Detresse + Amélioration.PRE.SUIVI.moyenne.cauchemars
            Imagery =~ PRE.Gordon.total + PRE.VVIQ.Yeux.ouvert + PRE.VVIQ.Yeux.fermé + PRE.Test.de.rotation
            Controle =~ PRE.Total.Sentiment.de.contrôle.général + PRE.Total.sentiment.de.contrôle.spécifique
            Exposition =~ Temps.total.des.entrainements.sur.la.période 
        
        # regression
            Effect_IRT ~ Imagery+Controle+Exposition

'



fit <- sem(model, data=data48)
summary(fit, standardized = TRUE) ## trouver le pb !!


Y <- data48$POST.Moyenne.cauchemars..semaine
X <- data48$POST.ControleReve#data48$BASELINE.Moyenne.cauchemars..semaine
M <- data48$PRE.VVIQ.Yeux.fermé

mod<-lm(Y~X*M)
summary(mod)

mod2<-lm(Y~X)
summary(mod2)

# variance décrite par chaque modele
summary(mod)$r.squared
summary(mod2)$r.squared


data48 <- read.csv('n48_PrePostSuivi.csv')


mod<-lm(data48$PSQI.POST~data48$NombreCauchemarParMois.POST+data48$VVIQYeuxFermé.PRE)
summary(mod)
