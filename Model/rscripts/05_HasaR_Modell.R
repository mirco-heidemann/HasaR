## ----
##  Hagelschaeden vs Radardaten: Modellierung der Schadensumme
##  (Fuer Ereignisse mit ueber 1'000 Schadenmeldungen)

## Die Modellierung besteht aus zwei Teilen (bedingte Wahrscheinlichkeit):
## Im ersten Teil wird unterschieden, ob ein Schaden eintritt (Schaden ja/nein)
##  - Logistische Regression

## Tritt ein Schaden ein, so wird im zweiten Teil der erwartete Schaden
## pro Gitterzellen mit einer Gamma likelihood (link=log) modelliert.
##  - GLM mit Gamma Verteilung
##  - GLM mit Poisson Verteilung fuer die geschaetzte Anzahl Schaeden

## Es werden nur Gitterzellen mit Anzahl Geb > 0 
## und einer POH von mindestens 80% verwendet

##  Mirco Heidemann, Diplomarbeit WBL Statistik
##  08/2014

##  Modified 06/2016
##  HasaR 2.0: 11/2017
##  HasaR 4.0: 05/2022
## ----

library(MASS)
library(dplyr)
library(tidyverse)
library(here)

pth_func <- here("Data/rfunctions/")
pth_rdata <- here("Data/rdata/")

## Funktion um neue Faktor-Levels predict-Datensatz auf NA zu setzen
source(here(pth_func, 'RemoveMissingLevels.R'))

## Gegitterte Schadendaten laden: grid.data
load(here(pth_rdata, '2022_Lossevents_Grid.Rdata'))

## Datensatz fuer die modellierung und validierung:
## poh, meshs als faktoren, nonzero binaer
dat.meteo <- grid.data %>%
  mutate(fact_poh = as.factor(poh),
         fact_meshs = as.factor(meshs),
         nonzero = ifelse(schad.SchadSum > 0, 1, 0)) %>% 
  filter(Anzahl > 0) %>%  ## nur gitterzellen mit mindestens einem gebaeude
  filter(poh >= 0.8)## nur gitterzellen mit mindestens poh 80%

## Hagelereignis auswaehlen
in.files <- list.files(pth_rdata, '_Gitter.Rdata')
event.datum <- sub('_Gitter.Rdata', '', sub('Eventloss_', '', in.files))

## ---- SCHADEN - RADAR MODELL
## Deviance is a measure of goodness of fit of a generalized linear model.
## Or rather, it's a measure of badness of fit. Higher numbers indicate worse fit.

## Datensatz fuer die modellierung
## folgende Hagelzuege fuer HasaR 4.0
dat <- dat.meteo %>%
  filter(event == 20040812 |
           event == 20110707 |
           event == 20120701 |
           event == 20170801)
  
# BESTES MODELL
m_logit <- glm(nonzero ~ fact_poh + fact_meshs + VersSumme,
               family = binomial(link="logit"),
               data = dat)

m_gamma <- glm(schad.SchadSum ~ fact_meshs + Anzahl,
               family=Gamma(link="log"), data = subset(dat, nonzero == 1))

m_pois <- glm(schad.Anzahl ~ fact_meshs + fact_poh + VersSumme,
              family=poisson(link="log"),
              data = subset(dat, nonzero == 1))

# anova(m_logit, test="Chisq")
# anova(m_gamma, test="Chisq")
# anova(m_pois, test="Chisq")

# summary(m_logit)
# summary(m_gamma)
# summary(m_pois)


# ## save the models
# save(m_logit, file = here(pth_rdata, 'mLogit.rda'))
# save(m_gamma, file = here(pth_rdata, 'mGamma.rda'))
# save(m_pois, file = here(pth_rdata, 'mPois.rda'))

## ----
## Vergleich der beobachteten Werte und den modellierten

## fitted(schadsum.glm): predicted values on the original scale
##                       (scale of response)
## predict(schadsum.glm, type='response'): same as 'fitted(schadsum.glm)'
## predict(schadsum.glm, type='link'): predicted values on the transformed
##                                     scale (scale of linear predictor)

schadSum.list <- list()
schadAnzahl.list <- list()

for(j in 1:length(event.datum)){
  
  newDat <- dat.meteo %>% filter(event == event.datum[j])%>% 
    dplyr::select(fact_meshs, fact_poh, Anzahl, VersSumme)

  ## wie gut passt das modell auf den event?
  pred.m.logit <- predict(m_logit, newdata=newDat, type="response")
  
  ## mit der funktion 'RemoveMissingLevels' werden alle faktor levels
  ## die in den original daten (dat) nicht vorkommen auf NA gesetzt
  pred.m.gamma <- predict(m_gamma,
                          newdata=RemoveMissingLevels(fit=m_gamma, test_data=newDat),
                          type="response")

  pred.pois <- predict(m_pois,
                          newdata=RemoveMissingLevels(fit=m_pois, test_data=newDat),
                          type="response")
  
  # pred.m.gamma <- predict(m.gamma, newdata=newDat, type="response")
  # pred.pois <- predict(m.pois, newdata=newDat, type="response")

  ## Gesamtschaden beobachtet
  obs.sum <- sum(grid.data$schad.SchadSum[grid.data$event==event.datum[j]])
  ## Schadenanzahl beobachtet
  obs.anzahl <- sum(grid.data$schad.Anzahl[grid.data$event==event.datum[j]])
  ## Gesamtschaden Modell fit
  fit.sum <- round(sum((pred.m.gamma * pred.m.logit), na.rm = TRUE), 0)
  ## Schadenanzahl Modell fit
  fit.anzahl<- round(sum((pred.pois * pred.m.logit), na.rm = TRUE), 0)
  
  schadSum.list[[j]] <- c(obs.sum, fit.sum)
  schadAnzahl.list[[j]] <- c(obs.anzahl, fit.anzahl)
}

tbl.schadSum <- data.frame(do.call("rbind", schadSum.list))
tbl.schadAnzahl <- data.frame(do.call("rbind", schadAnzahl.list))

(fcst <- data.frame(event=event.datum,
                   beob.schadSum=tbl.schadSum$X1,
                   mod.schadSum=tbl.schadSum$X2,
                   diffSum=round((tbl.schadSum$X2 - tbl.schadSum$X1) /
                                   tbl.schadSum$X1 * 100, 1),
                   beob.anzSchad=tbl.schadAnzahl$X1,
                   mod.anzSchad=tbl.schadAnzahl$X2,
                   diffAnz=round((tbl.schadAnzahl$X2 - tbl.schadAnzahl$X1) /
                                   tbl.schadAnzahl$X1 * 100, 1)))

