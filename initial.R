#### INITIAL LOAD #######################################################

# Package names
packages <- c("ggplot2", "dplyr", "tidyverse", "pdftools", "shiny", "googlesheets4", "rvest", "RSwissMaps", "plotly")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))




### Google Credentials
sheets_auth(
  cache = ".secrets",
  email = "tobydiethelm@gmail.com"
)

### Google Sheet Url
sheet_url <- "https://docs.google.com/spreadsheets/d/1hN8M6crArMqLE6jycQNf1Q85OJB3DbQdg-mOeV_tHmo/edit#gid=0"
sheet_name <- "Daten"

### LINK SZ.ch
link_sz <- "https://www.sz.ch/behoerden/information-medien/medienmitteilungen/coronavirus.html/72-416-412-1379-6948"

### Link-Liste mit importierten Files
linkliste <- read_sheet(sheet_url, sheet="Links")



### FUNKTIONEN #################################################################

### DATEN VON PDF LESEN
readDataFromPDF <- function() {

  # PDF lesen
  PDF.raw <- pdf_text(pdf_link) %>% readr::read_lines() 
  
  # Datum auslesen
  PDF.datum <- PDF.raw[3]
  PDF.datum <- gsub("Stand ", "", PDF.datum)
  
  # Relevante Zeilen
  daten_pdf <- PDF.raw[-c(1:5,15,20,38,39)]
  
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
  # TO DO
  
  # Daten zurückgeben
  return(daten)
  
}



#### PLOT: KARTE

kartenPlot <- function(daten_aktuell){
  mun.plot(daten_aktuell$bfs, daten_aktuell$Anzahl, 2016, 
        cantons = c("SZ"),
         lakes =c("Sihlsee"),
         title = "Aktuell positiv getestete Personen", 
         subtitle = paste0("Stand: ", daten_aktuell$Datum[1]), 
         caption = "Datenquelle: sz.ch", 
         legend_title = "Anzahl Fälle")
  }



#### PLOT: Gemeinde
gemeindePlot <- function(daten_aktuell) {
  
ggplot(daten_aktuell, aes(x=Gemeinde, y=Anzahl)) +
  geom_segment( aes(xend=Gemeinde, yend=0)) +
  geom_point( size=4, color="red") +
  coord_flip() +
  theme_bw() +
  xlab("")
}
