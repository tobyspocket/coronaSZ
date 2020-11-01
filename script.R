#### IGNORE GIT

### Initial
source("initial.R")

### Daten laden
daten_raw <- read_sheet(sheet_url, sheet="Daten")




### Daten ergänzen
daten <- datenAnreichern(daten_raw)



### Aktuellste Situation

# Daten aufbereiten
daten_aktuell <- group_by(daten, Gemeinde) %>% slice(which.max(as.Date(Datum, '%d.%m.%Y')))
daten_aktuell$Gemeinde <- factor(daten_aktuell$Gemeinde, levels = daten_aktuell$Gemeinde[order(daten_aktuell$Anzahl)])


kartenPlot <- function(daten_aktuell){
  mun.plot(daten_aktuell$bfs, daten_aktuell$Anzahl, 2016, 
           cantons = c("SZ"),
           lakes =c("Sihlsee"),
           title = "Aktuell positiv getestete Personen", 
           subtitle = paste0("Stand: ", daten_aktuell$Datum[1]), 
           caption = "Datenquelle: sz.ch", 
           legend_title = "Anzahl Fälle")
}


# Grafik: Map
kartenPlot(daten_aktuell)


# Grafik: Barplot


  ggplot(daten_aktuell, aes(x=Gemeinde, y=Anzahl)) +
  geom_segment( aes(xend=Gemeinde, yend=0)) +
  geom_point( size=4, color="red") +
  scale_y_continuous(breaks = round(seq(0, max(daten_aktuell$Anzahl), by = 10),1)) +
  coord_flip() +
  theme_bw() +
  xlab("")
  
  


