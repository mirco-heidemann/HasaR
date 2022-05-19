## schadenfile aus gemdatExport mit koordinaten 
## aus dem gebBestands File verknuepfen

library(dplyr)
library(stringr)

## Arbeitsverzeichnis definieren
setwd('I:/Statistical Computing/HasaR 2.0/data')

## um die nach-kommastellen nicht zu verliehren
options(digits = 11)

## schadendaten einlesen
schadat <- read.csv2('./tables/A_Schadendatenper05.12.2017_135135.csv', stringsAsFactors = F)

## join schadenfile mit den korrdinaten aus dem gebBestand
schageo <- left_join(schadat, gemdat, by=c('GebaeudeId' = 'strGebNr'))

## nur hagelschaeden mit dem entsprechenden datum
schageo <- schageo %>% filter(str_detect(schageo$CodTextDt, 'Hagel')) %>% 
  mutate(SchadenDatum = as.Date(SchadenDatum, format='%d.%m.%Y'),
         ## turn off scientific notification
         geox = format(geox, scientific = FALSE),
         geoy = format(geoy, scientific = FALSE)) %>% 
  filter(SchadenDatum == as.Date('01.08.2017',format='%d.%m.%Y'))

write.csv2(schageo, 'schadenmeldung.20170801.csv', row.names = FALSE)
