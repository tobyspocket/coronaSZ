##########################################
### DATEN VON SZ.CH abrufen und speichern
##########################################


###### BASICS ############################

### Initial Load
source("initial.R")



#### PDF-LINK FESTLEGEN #################

### Linkliste generieren (Credits to https://gist.github.com/paulrougieux)
scraplinks <- function(url){ 
  # Create an html document from the url
  webpage <- xml2::read_html(url)
  # Extract the URLs
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  # Extract the link text
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  return(tibble(link = link_, url = url_))
}

linkliste_sz <- scraplinks(link_sz)


### Link mit Fälle pro Gemeinde finden
pdf_link <- linkliste_sz[(grepl("pro Gemeinde", linkliste_sz$link, fixed = TRUE)),][1,] # Falls mehrere Links, wird der erste genommen
pdf_link <- pdf_link$url
# pdf_link <- "Corona_Fallzahlen_22_10_2020.pdf"




#### CHECK OB DATEN BEREITS VERARBEITET #############

# Link bereits in Linkliste?
link_exists <- pdf_link %in% linkliste$Link

# Importdatum bereits in Linkliste?
datum_exists <- format(Sys.Date(), format="%d.%m.%Y") %in% linkliste$Datum.Import

# Daten aktualisieren?
daten_aktualisieren <- ifelse(datum_exists==F & link_exists==F, T, F)

# Link bereits eingelesen
if (link_exists==T) {
  print("File wurde bereits eingelesen. Keine weiteren Aktionen.")
  } else { print("File noch nicht eingelesen.") }

# Link nicht eingelesen, aber schon ein anderes File an diesem Tag
if (link_exists==F & datum_exists==T) { print("Fehler: Zu diesem Datum wurde bereits ein File eingelesen")}

# Check Eintrag
daten_check <- data.frame("Datum"=Sys.time(), "Update"=daten_aktualisieren)



##### PDF ZU DATEN VERARBEITEN ##########

if (daten_aktualisieren==T) {

  # Daten von PDF auslesen
  daten_pdf <- readDataFromPDF()
  
  # Linkeintrag mit Datum generieren
  daten_pdf_link <- data.frame("Datum.File"= daten_pdf$Datum[1], "Datum.Import"=format(Sys.Date(), format="%d.%m.%Y"), "Link"=pdf_link)
  

}




#### DATEN AUF GOOGLE ERGÄNZEN #########################

### Falldaten
if (daten_aktualisieren==T)
  sheet_append(sheet_url, daten_pdf, sheet="Daten")

### Linkliste
if (daten_aktualisieren==T)
  sheet_append(sheet_url, daten_pdf_link, sheet="Links")

### Check
sheet_append(sheet_url, daten_check, sheet="Check")


