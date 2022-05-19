## ----
## Selektion Hagelschaden ausgewaehlter Ereignisse
## Modellentwicklung HasaR, Version 2.0
##
## Mirco Heidemann, Nov 2017
## ----

## Arbeitsverzeichnis
setwd('I:/Statistical Computing/HasaR 2.0/data')

## Funktion fuer die Aufbereitung der Elementarschaeden laden
source('f.schadPrepGemDatExport.R')
library(stringr)
library(MASS)

# Definiere Anfangs- und End-Datum (TT.MM.JJJJ) der Ereignisauswertung
## Folgende Hagelereignisse:
## 08.05.2003, 14.06.2003, 08.07.2004, 12.08.2004, 01.07.2008
## 26.05.2009, 07.07.2011, 12.07.2011, 01.07.2012, 18.06.2013

eventDate <- '02.08.2017'
## definiere schadenart
eventArt <- 'Hagel'

## ----
## Daten laden und aufbereiten
schad.file <- 'A_Schadendatenper22.11.2017.csv'
index.file <- 'versicherungsindex.gvz.csv'

## Aufbereitung der Elementarschaeden
schad <- schadPrepGemDat(schad.file, index.file)

## Datum der Schadenstatistik
eventDate <- as.Date(eventDate, '%d.%m.%Y')
eventEnd <- eventDate #+ 1

## Datenaufbereitung mit dplyr
## ... nur hagelschaeden UND DANN ...
schad <- schad %>% filter(str_detect(artcode, eventArt)) %>%
  ## ... nur  am schadendatum
  filter(schadat >= eventDate & schadat <= eventEnd)
## ----

summary(schad$indexSchad)
truehist(schad$indexSchad, xlim=c(0, quantile(schad$indexSchad, 0.99)),
         main="indexierte Hagelschaden")

## ----
## allenfalls noch georeferenzieren?
gebBestand <- read.csv2('gebaeudebestand.201701.georef - CHECKED gem schaetzkr.csv',
                        stringsAsFactors = FALSE, sep = ',')

options(digits = 11) ## um die nach-kommastellen nicht zu verliehren
schad$geox <- gebBestand$geoxLV03[match(schad$gebnr,
                                        gebBestand$gebNr)]
schad$geoy <- gebBestand$geoyLV03[match(schad$gebnr,
                                        gebBestand$gebNr)]
schad <- schad %>% filter(!is.na(geox))

## "turn off" scientific notation in write.csv 
schad$geox <- format(schad$geox, scientific = FALSE)
schad$geoy <- format(schad$geoy, scientific = FALSE)
## ----

charFile <- paste('hagelschad.', format(eventDate, '%y%m%d'), '.csv', sep='')
(outFile <- paste('.', '/Rdata/', charFile, sep=''))

write.csv2(schad, outFile, row.names = FALSE)





