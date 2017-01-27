
setwd('/home/claire/DATA/Etude Benjamin/')

data48 <- read.csv('n48_PrePostSuivi.csv')

patient<-1:48
data48$patient<-as.factor(patient)


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



# effet sir amaélioiration nuit avec cauchemars
xmdl<-lm(data48$Amélioration.PRE.SUIVI.moyenne.nuits.avec.cauchemars~data48$PRE.IERS.Global)
summary(xmdl)

# effet sur nombre de cauchemars

xmdl<-lm(data48$Amélioration.PRE.SUIVI.moyenne.cauchemars~data48$PRE.IERS.Global)
summary(xmdl)

xmdl<-lm(data48$Amélioration.PRE.SUIVI.moyenne.nuits.avec.cauchemars~data48$PRE.PCLS.Total)
summary(xmdl)

xmdl<-lm(data48$Amélioration.PRE.SUIVI.moyenne.cauchemars~data48$PRE.PCLS.Total)
summary(xmdl)

## Gordon

xmdl<-lm(data48$Amélioration.PRE.SUIVI.moyenne.cauchemars~data48$PRE.Gordon.total)
summary(xmdl)

xmdl<-lm(data48$Amélioration.PRE.SUIVI.moyenne.nuits.avec.cauchemars~data48$PRE.Gordon.total)
summary(xmdl)

## VVIQ

xmdl<-lm(data48$Amélioration.PRE.SUIVI.moyenne.cauchemars~data48$PRE.VVIQ.Yeux.ouvert)
summary(xmdl)

xmdl<-lm(data48$Amélioration.PRE.SUIVI.moyenne.nuits.avec.cauchemars~data48$PRE.VVIQ.Yeux.ouvert)
summary(xmdl)


xmdl<-lm(data48$Amélioration.PRE.SUIVI.moyenne.cauchemars~data48$PRE.VVIQ.Yeux.fermé)
summary(xmdl)

xmdl<-lm(data48$Amélioration.PRE.SUIVI.moyenne.nuits.avec.cauchemars~data48$PRE.VVIQ.Yeux.fermé)
summary(xmdl)

## Test Rotation
rm(xmdl)
xmdl<-lm(data48$Amélioration.PRE.SUIVI.moyenne.cauchemars~data48$PRE.Test.de.rotation)
summary(xmdl)
rm(xmdl)
xmdl<-lm(data48$Amélioration.PRE.SUIVI.moyenne.nuits.avec.cauchemars~data48$PRE.Test.de.rotation)
summary(xmdl)

plot(data48$Amélioration.PRE.SUIVI.moyenne.cauchemars~data48$PRE.Total.Sentiment.de.contrôle.général)

rm(xmdl)
xmdl<-lm(data48$Amélioration.PRE.SUIVI.moyenne.cauchemars~data48$PRE.Total.Sentiment.de.contrôle.général)
summary(xmdl)
rm(xmdl)
xmdl<-lm(data48$Amélioration.PRE.SUIVI.moyenne.nuits.avec.cauchemars~data48$PRE.Total.Sentiment.de.contrôle.général)
summary(xmdl)

# ajout colonne pour amélioration détresse
Amelioration.Detresse <- data48$SUIVI.Détresse - data48$BASELINE.Détresse

data48$Amelioration.Detresse<-Amelioration.Detresse

data48$Amelioration.Detresse

xmdl<-lm(data48$Amelioration.Detresse~data48$PRE.IERS.Global)
summary(xmdl)

rm(xmdl)
xmdl<-lm(data48$Amelioration.Detresse~data48$PRE.PCLS.Total)
summary(xmdl)


xmdl<-lm(data48$Amelioration.Detresse~data48$PRE.Gordon.total)
summary(xmdl)

xmdl<-lm(data48$Amelioration.Detresse~data48$PRE.VVIQ.Yeux.ouvert)
summary(xmdl)

xmdl<-lm(data48$Amelioration.Detresse~data48$PRE.VVIQ.Yeux.fermé)
summary(xmdl)

rm(xmdl)
xmdl<-lm(data48$Amelioration.Detresse~data48$PRE.Test.de.rotation)
summary(xmdl)


xmdl<-lm(data48$Amelioration.Detresse ~data48$PRE.Qualité.sommeil)
summary(xmdl)

# test du model

xmdl.Null <-lm(data48$Amelioration.Detresse ~1)

summary(xmdl.Null)$r.squared
summary(xmdl)$r.squared

AIC(xmdl.Null)
AIC(xmdl)

anova(xmdl, xmdl.Null, test='F')

rm(xmdl)
xmdl<-lm(data48$Amelioration.Detresse~data48$PRE.Total.Sentiment.de.contrôle.général)
summary(xmdl)

data48$Amelioration.Impact<-data48$BASELINE.Impact-data48$SUIVI.Impact

xmdl<-lm(data48$Amelioration.Impact~data48$PRE.IERS.Global)
summary(xmdl)

xmdl<-lm(data48$Amelioration.Impact~data48$PRE.PCLS.Total)
summary(xmdl)


xmdl<-lm(data48$Amelioration.Impact~data48$PRE.Gordon.total)
summary(xmdl)

xmdl<-lm(data48$Amelioration.Impact~data48$PRE.VVIQ.Yeux.ouvert)
summary(xmdl)

xmdl<-lm(data48$Amelioration.Impact~data48$PRE.VVIQ.Yeux.fermé)
summary(xmdl)

rm(xmdl)
xmdl<-lm(data48$Amelioration.Impact~data48$PRE.Test.de.rotation)
summary(xmdl)

xmdl<-lm(data48$Amelioration.Impact ~data48$SUIVI.Qualité.sommeil)
summary(xmdl)

rm(xmdl)
xmdl<-lm(data48$Amelioration.Impact~data48$PRE.Total.Sentiment.de.contrôle.général)
summary(xmdl)

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
