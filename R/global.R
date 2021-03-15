###########################################################
#### global.R
###########################################################

### Google Credentials Authentifizierung
# App --> via gecachte .secrets in app.R
# readData.R --> via Browser


### Google Sheet Url
sheet_url <- "https://docs.google.com/spreadsheets/d/1hN8M6crArMqLE6jycQNf1Q85OJB3DbQdg-mOeV_tHmo/edit#gid=0"


### LINK SZ.ch
link_sz <- "https://www.sz.ch/behoerden/information-medien/medienmitteilungen/coronavirus.html/72-416-412-1379-6948"



### DATEN ######################################################################

### BEVÖLKERUNG KANTON SZ
# https://www.sz.ch/public/upload/assets/44900/Wohnbev%C3%B6lkerung%202019.pdf

gemeinden <- c("Schwyz", "Arth", "Ingenbohl", "Muotathal", "Steinen", "Sattel", "Rothenthurm", "Oberiberg", "Unteriberg", "Lauerz", "Steinerberg", "Morschach", "Alpthal", "Illgau", "Riemenstalden",
               "Gersau", "Lachen", "Altendorf", "Galgenen", "Vorderthal", "Innerthal", "Schübelbach", "Tuggen", "Wangen (SZ)", "Reichenburg", "Einsiedeln", "Küssnacht (SZ)", "Wollerau", "Freienbach", "Feusisberg")
einwohnerzahl <- c(15133,12056,8880,3475,3591,1910,2449,861,2385,1100,931,1126,618,789,86,2314,8830,6998,5208,991,177,9144,3260,5114,3806,16027,13270,7274,16543,5306)
einwohner <- data.frame(gemeinden,einwohnerzahl)
colnames(einwohner) <- c("Gemeinde", "Einwohner")




### FUNKTIONEN #################################################################

### DATEN VON PDF LESEN
readDataFromPDF <- function() {

  # PDF lesen
  PDF.raw <- pdf_text(pdf_link) %>% readr::read_lines() 
  
  # Datum auslesen
  PDF.datum <- PDF.raw[4]
  PDF.datum <- gsub("Stand ", "", PDF.datum)
  
  # Relevante Zeilen
  daten_pdf <- PDF.raw[-c(1:6,16,21,39,40)]
  
  # Bezirke mit nur einer Gemeinde korrigieren
  daten_pdf <- sub("Bezirk", "", daten_pdf)
  
  # Leere Spaces removen
  daten_pdf <- daten_pdf %>%
    str_squish() %>%
    strsplit(split = " ") 
  
  # Datenframe generieren
  daten_pdf <- plyr::ldply(daten_pdf)
  daten_pdf$Datum <- PDF.datum
  colnames(daten_pdf) <- c("Gemeinde", "Anzahl", "Datum")
  
  # Bereinigen <5 als 0 für besseres Handling
  daten_pdf$Anzahl <- as.numeric(daten_pdf$Anzahl) # ersetzt die <5 Fälle durch NA
  daten_pdf[is.na(daten_pdf)] <- 0 # NA durch 0 ersetzen
  
  # Daten zurückgeben
  return(daten_pdf)
}

### DATEN LADEN VON GOOGLESHEETS
loadData <- function (sheet_name) {
  daten <- read_sheet(sheet_url, sheet=sheet_name)
  daten$Datum <- as.Date(daten$Datum, "%d.%m.%Y")
  return (daten)
}


### AKTUELLSTE DATEN
aktuelleDaten <- function (daten) {
  daten_aktuell <- group_by(daten, Gemeinde) %>% slice(which.max(as.Date(Datum, '%d.%m.%Y')))
  return(daten_aktuell)
}

### DATEN VON GEMEINDE
datenGemeinde <- function (daten, gemeinde) {
  daten_gemeinde <- daten[(daten$Gemeinde %in% gemeinde),]
  return(daten_gemeinde)
}


#### DATEN ERGÄNZEN
datenAnreichern <- function(daten_raw) {
  
  # Daten Grundlage
  daten <- daten_raw
  
  # Wangen und Küssnacht gemäss BFS umbenennen
  daten$Gemeinde <- gsub("Wangen", "Wangen (SZ)", daten$Gemeinde)
  daten$Gemeinde <- gsub("Küssnacht", "Küssnacht (SZ)", daten$Gemeinde)
  
  # BFS - Datum
  bfs <- mun.template(year = 2016)[,c(1,2)]
  colnames(bfs) <- c("bfs", "Gemeinde")
  daten <- left_join(daten, bfs, by="Gemeinde")
  
  # Bevölkerung
  daten <- left_join(daten, einwohner, by="Gemeinde")
  
  # Fälle relativ zur Bevölkerung
  daten$AnzahlRelativ <- daten$Anzahl/daten$Einwohner*1000
  
  
  # Daten zurückgeben
  return(daten)
  
}


##### AKTUELLE DATEN RANGLISTE
aktuelleDatenRang <- function(daten_aktuell) {
  
  # Datenbasis
  daten_text <- daten_aktuell
  
  # Anteil Fälle am Total (auf Gemeindebasis, d.h. Abweichung zu BAG möglich)
  total_cases <- sum(daten_text$Anzahl)
  daten_text$Anteil <- round(daten_text$Anzahl/total_cases*100,1)
  
  # Rang erstellen für absolute Fälle
  order.scores <- order(daten_text$Anzahl, decreasing = T)
  daten_text <- daten_text[(order.scores),]
  daten_text$Rang <- rank(-daten_text$Anzahl, ties.method = "min")
  
  # Rang erstellen für relative Fälle
  order.scores.rel <- order(daten_text$AnzahlRelativ, decreasing = T)
  daten_text <- daten_text[(order.scores.rel),]
  daten_text$RangRelativ <- rank(-daten_text$AnzahlRelativ, ties.method = "min")
  
  return(daten_text)
  
}

#### GEMEINDEN IDENTIFIZIEREN
gemeindeAbsolut <- function (daten_aktuell_rang) {
 daten_aktuell_rang[(daten_aktuell_rang$Rang==1),][1,] # falls mehrer gewählt sind wird zufällig eine gewählt
}

gemeindeRelativ <- function (daten_aktuell_rang) {
  daten_aktuell_rang[(daten_aktuell_rang$RangRelativ==1),][1,] # falls mehrer gewählt sind wird zufällig eine gewählt
}

gemeindeOhneCases <- function (daten_aktuell_rang) {
  daten_aktuell_rang[(daten_aktuell_rang$Anzahl<5),]$Gemeinde
}

#### Vergleichsdatum finden
vergleichsDatum <- function(daten) {
  
  alleDaten <- daten
  alleDaten <- unique(daten$Datum)
  aktuellesDatum <- max(alleDaten, na.rm = TRUE)
  letztesDatum <- aktuellesDatum - 7
  
  letztesDatum <- which((abs(alleDaten-letztesDatum)) == min(abs(alleDaten-letztesDatum)))
  letztesDatum <- alleDaten[letztesDatum]
  
  return(letztesDatum)
}


#### PLOT: KARTE

kartenPlot <- function(daten_aktuell){
  mun.plot(daten_aktuell$bfs, daten_aktuell$Anzahl, 2016, 
        cantons = c("SZ"),
         lakes =c("Sihlsee"),
         caption = "Datenquelle: sz.ch", 
         legend_title = "Anzahl aktuelle Fälle")
  }



#### BARPLOT ABSOLUT
gemeindePlot <- function(daten_aktuell, gemeindeInput) {
  
  # Datenbasis
  daten_aktuell_highlight <- daten_aktuell
  
  # Gemeinden sortieren
  daten_aktuell_highlight$Gemeinde <- factor(daten_aktuell_highlight$Gemeinde, levels = daten_aktuell_highlight$Gemeinde[order(daten_aktuell_highlight$Anzahl)])
  
  # Selected Gemeinde markieren
  daten_aktuell_highlight$Highlight <- ifelse(daten_aktuell$Gemeinde == gemeindeInput, "yes", "no" )
  
  # Plot generieren
  ggplot(daten_aktuell_highlight, aes(x=Gemeinde, y=Anzahl, fill=Highlight)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c( "yes"="#FF0011", "no"="#f4c6b1" ), guide = FALSE ) +
    coord_flip() +
    theme_minimal() +
    xlab("Gemeinde") +
    ylab ("Anzahl aktuelle Fälle") +
    scale_y_continuous(breaks = round(seq(0, max(daten_aktuell$Anzahl)+10, by = 20),1))
}


#### BARPLOT RELATIV
gemeindePlotRelativ <- function(daten_aktuell, gemeindeInput) {
  
  # Datenbasis
  daten_aktuell_highlight <- daten_aktuell
  
  # Gemeinden sortieren
  daten_aktuell_highlight$Gemeinde <- factor(daten_aktuell_highlight$Gemeinde, levels = daten_aktuell_highlight$Gemeinde[order(daten_aktuell_highlight$AnzahlRelativ)])
  
  # Selected Gemeinde markieren
  daten_aktuell_highlight$Highlight <- ifelse(daten_aktuell$Gemeinde == gemeindeInput, "yes", "no" )
  
  #Plot generieren
  ggplot(daten_aktuell_highlight, aes(x=Gemeinde, y=AnzahlRelativ, fill=Highlight)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c( "yes"="#FF0011", "no"="#f4c6b1" ), guide = FALSE ) +
    coord_flip() +
    theme_minimal() +
    xlab("Gemeinde") +
    ylab ("Anzahl aktuelle Fälle pro 1000 Einwohner") +
    scale_y_continuous(breaks = round(seq(0, max(daten_aktuell$AnzahlRelativ)+2, by = 2),1))
}


#### TEXT PRO GEMEINDE AKTUELL
gemeindeText <- function(daten_gemeinde, gemeindeInput) {
  
  # Passagen generieren
  str1 <- paste0("In ", gemeindeInput, " gibt es derzeit <strong>", daten_gemeinde$Anzahl, " aktive Fälle.</strong>")
  str2 <- paste0("Damit liegt ", gemeindeInput, " aktuell <strong>", ifelse(daten_gemeinde$Rang<15, "über", "unter"), " dem Schwyzer Durchschnitt</strong> (Median).")
  str3 <- paste0("Ca. ", strong(daten_gemeinde$Anteil), "% aller aktuellen Fälle im Kanton Schwyz liegen in ", gemeindeInput)
  str4 <- paste0("Pro 1'000 Einwohner gibt es ", strong(round(daten_gemeinde$AnzahlRelativ,1)), " aktive Fälle (liegt ", ifelse(daten_gemeinde$RangRelativ<15, "über", "unter"), " dem Durchschnitt)." )
  
  # HTML generieren
  HTML(paste0("<ul><li>",str1, "</li><li>", str2, "</li><li>", str3, "</li><li>", str4, "</li></ul>"))
}


#### TEXT PRO GEMEINDE AKTUELL
gemeindeTextHistorie <- function(daten, gemeindeInput, vergleichsdatum) {
  
  daten_historie <- daten
  
  # Daten filtern nach Gemeinde
  daten_historie <- daten_historie[(daten_historie$Gemeinde == gemeindeInput),]
  
  # Daten filtern nach Vergleichsdatum
  anzahl_vorwoche <- daten_historie[(daten_historie$Datum==vergleichsdatum),]
  
  # Passagen generieren
  str1 <- paste0("Vor ca. einer Woche (", format(vergleichsdatum, "%d.%m.%Y"), ") hatte ", gemeindeInput, " <strong>", anzahl_vorwoche$Anzahl, " aktive Fälle.</strong>")
  
  # HTML generieren
  HTML(paste0("<ul><li>",str1, "</li></ul>"))
  
}


