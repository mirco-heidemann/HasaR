## ----------------------------------------------------------------------------
##  Hagelschaeden vs Radardaten: Modellierung der Schadensumme

## Die Modellierung besteht aus zwei Teilen (bedingte Wahrscheinlichkeit):
## Im ersten Teil wird unterschieden, ob ein Schaden eintritt (Schaden ja/nein)
##  - Logistische Regression

## Tritt ein Schaden ein, so wird im zweiten Teil der erwartete Schaden
## pro Gitterzellen mit einer Gamma likelihood (link=log) modelliert.
##  - GLM mit Gamma Verteilung
##  - GLM mit Poisson Verteilung fuer die geschaetzte Anzahl Schaeden
##
##  Diplomarbeit WBL Statistik
##  Mirco Heidemann, 08/2014
##  Modified 06/2016
## ----------------------------------------------------------------------------

## Arbeitsverzeichnis definieren
setwd('J:/Naturgefahren/FG2_Datenanalysen/Ereignisanalysen/Hagel/HasaR/data/Rdata/')
require(MASS)

# ## Ordner-Pfad angeben
# '.' <- 'J:/Naturgefahren/Datenanalysen/Ereignisanalysen/Hagel/HasaR/data/Rdata/'

## Gegitterte Schadendaten laden...
## ... mit meteoschweiz radardaten
# in.file.meteoschweiz <- paste('J:/Naturgefahren/Datenanalysen/Ereignisanalysen/',
#                               '/Hagel/HasaR/data/Rdata/',
#                               'events.schadtabelle.meteoschweiz.Rdata', sep='')
load('events.schadtabelle.meteoschweiz.Rdata')

dat.meteo <- grid.data
dat.meteo$Anzahl <- as.numeric(dat.meteo$Anzahl)

dat.meteo <- cbind(dat.meteo,
                   f.poh = as.factor(dat.meteo$poh),
                   f.meshs = as.factor(dat.meteo$meshs),
                   nonzero = ifelse(dat.meteo$Schaden.SchadSum > 0, 1, 0))

## Hagelereignis auswaehlen
in.files <- list.files('.', '-Gitter.Rdata')
event.datum <- paste('20', sub('-Gitter.Rdata', '',
                               sub('hagelschad', '',
                                       in.files)), sep = '')

## SCHADEN - RADAR MODELL
## ---------------------------------------------------------------------------
## Deviance is a measure of goodness of fit of a generalized linear model.
## Or rather, it's a measure of badness of fit-higher numbers indicate worse fit.

## Grosse Hagelereigniss: hagel.event = 1
## Kleine Hagelereigniss: hagel.event = 2
hagel.event = 1


## betrachte nur Gitterzellen (Gebaeude) wo POH mindestens 20%
ind <- which(dat.meteo$poh > 0.1)
dat.meteo <- dat.meteo[ind,]

# ## A) Modell fuer grosse Hagelereignisse
# ## Fuer die Modellentwicklung werden folgende Hagelzuege gewaehlt:
# ## 20040812 und 20120701
# ind <- which(dat.meteo$event == event.datum[4] | 
#                dat.meteo$event == event.datum[9])
# dat <- dat.meteo[ind,]
# 
# # Modell mit POH und MESHS
# # m.logit <- glm(nonzero ~ f.poh + f.meshs + VersSumme, family = binomial(link="logit"),
# #                data = dat)
# 
# m.logit <- glm(nonzero ~ f.poh + VersSumme, family = binomial(link="logit"),
#                data = dat)
# 
# m.gamma <- glm(Schaden.SchadSum ~ f.meshs + Anzahl, family=Gamma(link="log"),
#                data = subset(dat, nonzero == 1))
# 
# m.pois <- glm(Schaden.Anzahl ~ f.meshs + Anzahl, family=poisson(link="log"),
#               data = subset(dat, nonzero == 1))
# 
# # anova(m.logit, test="Chisq")
# # anova(m.gamma, test="Chisq")
# # anova(m.pois, test="Chisq")
# 
# # ## save the models
# # save(m.logit, file = 'm.logit.gr.rda')
# # save(m.gamma, file = 'm.gamma.gr.rda')
# # save(m.pois, file = 'm.pois.gr.rda')

# B) Modell fuer kleine Hagelereignisse
# Fuer die Modellentwicklung werden folgende Hagelzuege gewaehlt:
# 20030614 und 20080701
ind <- which(dat.meteo$event == event.datum[2] |
                dat.meteo$event == event.datum[5])
dat <- dat.meteo[ind,]

## Modell mit POH und MESHS
m.logit <- glm(nonzero ~ f.poh + f.meshs + VersSumme + Anzahl, family = binomial(link="logit"),
               data = dat)

# m.logit <- glm(nonzero ~ f.poh + f.meshs + Anzahl, family = binomial(link="logit"),
#                data = dat)

m.gamma <- glm(Schaden.SchadSum ~ f.meshs + VersSumme, family=Gamma(link="log"),
               data = subset(dat, nonzero == 1))

m.pois <- glm(Schaden.Anzahl ~ f.meshs + Anzahl, family=poisson(link="log"),
              data = subset(dat, nonzero == 1))

# anova(m.logit, test="Chisq")
# anova(m.gamma, test="Chisq")
# anova(m.pois, test="Chisq")

# ## save the models
# save(m.logit, file = 'm.logit.kl.rda')
# save(m.gamma, file = 'm.gamma.kl.rda')
# save(m.pois, file = 'm.pois.kl.rda')


## Vergleich der beobachteten Werte und den modellierten
## ---------------------------------------------------------------------------
## fitted(schadsum.glm): predicted values on the original scale
##                       (scale of response)
## predict(schadsum.glm, type='response'): same as 'fitted(schadsum.glm)'
## predict(schadsum.glm, type='link'): predicted values on the transformed
##                                     scale (scale of linear predictor)

## Differenzierung zwischen grossen und kleinen Hagelereignissen
## Als gross werden Ereignisse betrachtet mit ueber 1'000 Schadenmeldungen
## Grosse Ereignisse: 20040708, 20040812, 20110707, 20110712, 20120701, 20130618
event.datum.gr <- c(event.datum[3], event.datum[4], event.datum[7],
                    event.datum[8], event.datum[9], event.datum[10])
event.datum.kl <- c(event.datum[1], event.datum[2], event.datum[5])
                    #event.datum[6])

schadSum.list <- list()
schadAnzahl.list <- list()

if(hagel.event==1) event.datum <- event.datum.gr  else
  event.datum <- event.datum.kl

for(j in 1:length(event.datum)){
  ind.event <- which(dat.meteo$event == event.datum[j])

  newDat <- data.frame(f.meshs = dat.meteo$f.meshs[ind.event],
                       f.poh = dat.meteo$f.poh[ind.event],
                       Anzahl = dat.meteo$Anzahl[ind.event],
                       VersSumme = dat.meteo$VersSumme[ind.event])
  
  ## wie gut passt das modell auf den event?
  
#   ## updating the model with all factor levels ##
#   m.gamma$xlevels[["f.poh"]] <- union(m.gamma$xlevels[["f.poh"]], levels(newDat$f.poh))
#   m.pois$xlevels[["f.poh"]] <- union(m.pois$xlevels[["f.poh"]], levels(newDat$f.poh))
  
  pred.m.logit <- predict(m.logit, newdata=newDat, type="response")
  pred.m.gamma <- predict(m.gamma, newdata=newDat, type="response")
  pred.pois <- predict(m.pois, newdata=newDat, type="response")
  
  # ## %-Abweichung von Modell-Fit zu Beobachtung
  # diff <- round((obs.sum-fit.sum)/obs.sum*100)
  # print(paste(diff,'% Abweichung von Modell-Fit zu Beobachtung', sep=""))
  
  ## Gesamtschaden beobachtet
  obs.sum <- sum(dat.meteo$Schaden.SchadSum[dat.meteo$event==event.datum[j]])
  ## Schadenanzahl beobachtet
  obs.anzahl <- sum(dat.meteo$Schaden.Anzahl[dat.meteo$event==event.datum[j]])
  ## Gesamtschaden Modell fit
  fit.sum <- sum(pred.m.gamma * pred.m.logit)
  ## Schadenanzahl Modell fit
  fit.anzahl<- round(sum(pred.pois * pred.m.logit))
  
  schadSum.list[[j]] <- c(obs.sum, fit.sum)
  schadAnzahl.list[[j]] <- c(obs.anzahl, fit.anzahl)
}

fcst.schadSum <- as.data.frame(do.call("rbind", schadSum.list))
fcst.schadSum$V3 <- round((fcst.schadSum$V2 - fcst.schadSum$V1) /
                            fcst.schadSum$V1 * 100, 1)
names(fcst.schadSum) <- c("beobachtete SchadSum", "modellierte SchadSum",
                          "Abweichung %")
rownames(fcst.schadSum) <- event.datum
fcst.schadSum

fcst.schadAnzahl <- as.data.frame(do.call("rbind", schadAnzahl.list))
fcst.schadAnzahl$V3 <- round((fcst.schadAnzahl$V2 - fcst.schadAnzahl$V1) /
                               fcst.schadAnzahl$V1 * 100, 1)
names(fcst.schadAnzahl) <- c("beobachtete Schadenanzahl",
                             "modellierte Schadenanzahl", "Abweichung %")
rownames(fcst.schadAnzahl) <- event.datum
fcst.schadAnzahl
