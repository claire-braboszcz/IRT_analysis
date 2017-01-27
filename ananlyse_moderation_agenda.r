
setwd('/home/claire/DATA/Etude Benjamin/')

data38 <- read.csv('n38_Agenda_Pre_Post_Suivi.csv')
patient<-1:38
data38$patient<-as.factor(patient)


# centering covariate of interests
data38$PRE.IERS.Global <- data38$PRE.IERS.Global-mean(data38$PRE.IERS.Global)
data38$PRE.PCLS.Total <- data38$PRE.PCLS.Total-mean(data38$PRE.PCLS.Total)
data38$PRE.Gordon.total <- data38$PRE.Gordon.total-mean(data38$PRE.Gordon.total)
data38$PRE.VVIQ.Yeux.ouvert <- data38$PRE.VVIQ.Yeux.ouvert-mean(data38$PRE.VVIQ.Yeux.ouvert)
data38$PRE.VVIQ.Yeux.fermé <- data38$PRE.VVIQ.Yeux.fermé-mean(data38$PRE.VVIQ.Yeux.fermé)
data38$PRE.Test.de.rotation <- data38$PRE.Test.de.rotation-mean(data38$PRE.Test.de.rotation)
data38$PRE.Total.Sentiment.de.contrôle.général<-data38$PRE.Total.Sentiment.de.contrôle.général-mean(data38$PRE.Total.Sentiment.de.contrôle.général)
data38$SUIVI.Qualité.sommeil<-data38$SUIVI.Qualité.sommeil-mean(data38$SUIVI.Qualité.sommeil)



# effet sir amaélioiration nuit avec cauchemars
xmdl<-lm(data38$Amélioration.PRE.SUIVI.moyenne.nuits.avec.cauchemars~data38$PRE.IERS.Global)
summary(xmdl)

# effet sur nombre de cauchemars

xmdl<-lm(data38$Amélioration.PRE.SUIVI.moyenne.cauchemars~data38$PRE.IERS.Global)
summary(xmdl)

xmdl<-lm(data38$Amélioration.PRE.SUIVI.moyenne.nuits.avec.cauchemars~data38$PRE.PCLS.Total)
summary(xmdl)

xmdl<-lm(data38$Amélioration.PRE.SUIVI.moyenne.cauchemars~data38$PRE.PCLS.Total)
summary(xmdl)

## Gordon

xmdl<-lm(data38$Amélioration.PRE.SUIVI.moyenne.cauchemars~data38$PRE.Gordon.total)
summary(xmdl)

xmdl<-lm(data38$Amélioration.PRE.SUIVI.moyenne.nuits.avec.cauchemars~data38$PRE.Gordon.total)
summary(xmdl)

## VVIQ

xmdl<-lm(data38$Amélioration.PRE.SUIVI.moyenne.cauchemars~data38$PRE.VVIQ.Yeux.ouvert)
summary(xmdl)

xmdl<-lm(data38$Amélioration.PRE.SUIVI.moyenne.nuits.avec.cauchemars~data38$PRE.VVIQ.Yeux.ouvert)
summary(xmdl)


xmdl<-lm(data38$Amélioration.PRE.SUIVI.moyenne.cauchemars~data38$PRE.VVIQ.Yeux.fermé)
summary(xmdl)

xmdl<-lm(data38$Amélioration.PRE.SUIVI.moyenne.nuits.avec.cauchemars~data38$PRE.VVIQ.Yeux.fermé)
summary(xmdl)

## Test Rotation
rm(xmdl)
xmdl<-lm(data38$Amélioration.PRE.SUIVI.moyenne.cauchemars~data38$PRE.Test.de.rotation)
summary(xmdl)
rm(xmdl)
xmdl<-lm(data38$Amélioration.PRE.SUIVI.moyenne.nuits.avec.cauchemars~data38$PRE.Test.de.rotation)
summary(xmdl)

plot(data38$Amélioration.PRE.SUIVI.moyenne.cauchemars~data38$PRE.Total.Sentiment.de.contrôle.général)

rm(xmdl)
xmdl<-lm(data38$Amélioration.PRE.SUIVI.moyenne.cauchemars~data38$PRE.Total.Sentiment.de.contrôle.général)
summary(xmdl)
rm(xmdl)
xmdl<-lm(data38$Amélioration.PRE.SUIVI.moyenne.nuits.avec.cauchemars~data38$PRE.Total.Sentiment.de.contrôle.général)
summary(xmdl)

# ajout colonne pour amélioration détresse
Amelioration.Detresse <- data38$SUIVI.Détresse - data38$BASELINE.Détresse

data38$Amelioration.Detresse<-Amelioration.Detresse

data38$Amelioration.Detresse

xmdl<-lm(data38$Amelioration.Detresse~data38$PRE.IERS.Global)
summary(xmdl)

rm(xmdl)
xmdl<-lm(data38$Amelioration.Detresse~data38$PRE.PCLS.Total)
summary(xmdl)


xmdl<-lm(data38$Amelioration.Detresse~data38$PRE.Gordon.total)
summary(xmdl)

xmdl<-lm(data38$Amelioration.Detresse~data38$PRE.VVIQ.Yeux.ouvert)
summary(xmdl)

xmdl<-lm(data38$Amelioration.Detresse~data38$PRE.VVIQ.Yeux.fermé)
summary(xmdl)

rm(xmdl)
xmdl<-lm(data38$Amelioration.Detresse~data38$PRE.Test.de.rotation)
summary(xmdl)


xmdl<-lm(data38$Amelioration.Detresse ~data38$PRE.Qualité.sommeil)
summary(xmdl)

# test du model

xmdl.Null <-lm(data38$Amelioration.Detresse ~1)

summary(xmdl.Null)$r.squared
summary(xmdl)$r.squared

AIC(xmdl.Null)
AIC(xmdl)

anova(xmdl, xmdl.Null, test='F')

rm(xmdl)
xmdl<-lm(data38$Amelioration.Detresse~data38$PRE.Total.Sentiment.de.contrôle.général)
summary(xmdl)

data38$Amelioration.Impact<-data38$BASELINE.Impact-data38$SUIVI.Impact

xmdl<-lm(data38$Amelioration.Impact~data38$PRE.IERS.Global)
summary(xmdl)

xmdl<-lm(data38$Amelioration.Impact~data38$PRE.PCLS.Total)
summary(xmdl)


xmdl<-lm(data38$Amelioration.Impact~data38$PRE.Gordon.total)
summary(xmdl)

xmdl<-lm(data38$Amelioration.Impact~data38$PRE.VVIQ.Yeux.ouvert)
summary(xmdl)

xmdl<-lm(data38$Amelioration.Impact~data38$PRE.VVIQ.Yeux.fermé)
summary(xmdl)

rm(xmdl)
xmdl<-lm(data38$Amelioration.Impact~data38$PRE.Test.de.rotation)
summary(xmdl)

xmdl<-lm(data38$Amelioration.Impact ~data38$SUIVI.Qualité.sommeil)
summary(xmdl)

rm(xmdl)
xmdl<-lm(data38$Amelioration.Impact~data38$PRE.Total.Sentiment.de.contrôle.général)
summary(xmdl)

library(lavaan)

Effect_IRT =~ data38$Amelioration.Impact + data38$Amelioration.Detresse + data38$Amélioration.PRE.SUIVI.moyenne.cauchemars
Imagery =~ data38$PRE.Gordon.total + data38$PRE.VVIQ.Yeux.ouvert + data38$PRE.VVIQ.Yeux.fermé + data38$PRE.Test.de.rotation
Controle =~ data38$PRE.Total.Sentiment.de.contrôle.général + data38$PRE.Total.sentiment.de.contrôle.spécifique
Exposition =~ data38$Temps.total.des.entrainements.sur.la.période 

model <- ' # latent variables definition
            Effect_IRT =~ Amelioration.Impact + Amelioration.Detresse + Amélioration.PRE.SUIVI.moyenne.cauchemars
            Imagery =~ PRE.Gordon.total + PRE.VVIQ.Yeux.ouvert + PRE.VVIQ.Yeux.fermé + PRE.Test.de.rotation
            Controle =~ PRE.Total.Sentiment.de.contrôle.général + PRE.Total.sentiment.de.contrôle.spécifique
            Exposition =~ Temps.total.des.entrainements.sur.la.période 
        
        # regression
            Effect_IRT ~ Imagery+Controle+Exposition

'



fit <- sem(model, data=data38)
summary(fit, standardized = TRUE) ## trouver le pb !!


Y <- data38$POST.Moyenne.cauchemars..semaine
X <- data38$POST.ControleReve#data38$BASELINE.Moyenne.cauchemars..semaine
M <- data38$PRE.VVIQ.Yeux.fermé

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
