####################################################
#### Script als Vorlage für App
####################################################



##### BASICS ######################################

### Pakete laden
library("shinythemes")
library("ggplot2")
library("dplyr")
library("shiny")
library("RSwissMaps")
library("plotly")
library("googlesheets4")

### Initial
source("R/global.R", encoding = "UTF-8")




##### INPUTS ###################################

gemeindeInput <- "Lachen"






##### OUTPUTS ##################################

### DATENBASIS

# Daten laden (pro Server-Session)
daten_raw <- loadData("Daten")

# Daten anreichern (pro Server-Session)
daten <- datenAnreichern(daten_raw)

# Aktuellste Daten
daten_aktuell <- aktuelleDaten(daten)

# Vergleichsdatum
vergleichsdatum <- vergleichsDatum(daten)

### KARTEN-PLOT
kartenPlot(daten_aktuell)


### KARTEN-TEXT
daten_aktuell_rang <- aktuelleDatenRang(daten_aktuell)

# Gemeinden identifizieren
gemeinde_absolut <- gemeindeAbsolut(daten_aktuell_rang)
gemeinde_relativ <- gemeindeRelativ(daten_aktuell_rang)
gemeinde_ohneCases <- gemeindeOhneCases(daten_aktuell_rang)

# Text generieren
str1 <- paste0("In der Gemeinde ", strong(gemeinde_absolut$Gemeinde), " liegen mit ", strong(gemeinde_absolut$Anzahl), " positiv getesteten Personen derzeit die meisten Fälle vor.")
str2 <- paste0("Unter Berücksichtigtung der Einwoherzahl ist die Gemeinde ", strong(gemeinde_relativ$Gemeinde), " mit ", strong(round(gemeinde_relativ$AnzahlRelativ, 1)), " aktiven Fällen pro 1'000 Einwohner aktuell am stärksten betroffen.")
str3 <- paste0("Folgende Gemeinden weisen aktuell weniger als 5 Fälle auf: ", strong(paste(gemeinde_ohneCases, collapse = ", ")))

HTML(paste0("<ul><li>",str1, "</li><li>", str2, "</li><li>", str3, "</li></ul>"))


### BARPLOT
gemeindePlot(daten_aktuell, gemeindeInput)
gemeindePlotRelativ(daten_aktuell, gemeindeInput)


### BARPLOT-TEXT

# Datenbasis
daten_gemeinde <- datenGemeinde(daten_aktuell_rang, gemeindeInput)

# Text für aktuelle Situation
gemeindeText(daten_gemeinde, gemeindeInput)

# Text für Vergleich mit vergangenen Daten
gemeindeTextHistorie(daten, gemeindeInput, vergleichsdatum)


#### ZEIT
  
daten_gemeinde <- daten[(daten$Gemeinde=="Lachen"),]

daten_gemeinde$Datum <- as.Date(daten_gemeinde$Datum, "%d.%m.%Y")
daten$Datum <- as.Date(daten$Datum, "%d.%m.%Y")


ggplot(daten, aes(x=Datum, y=Anzahl, group=Gemeinde)) +
  geom_line(aes(color=Gemeinde))
  
  
# create data
xValue <- 1:10
yValue <- cumsum(rnorm(10))
data <- data.frame(xValue,yValue)

# Plot
ggplot(data, aes(x=xValue, y=yValue)) +
  geom_line()
  


