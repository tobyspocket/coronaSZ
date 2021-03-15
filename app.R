##########################################################
#### Shiny App File
##########################################################


##### BASICS #############################

### Pakete laden
library("shinythemes")
library("shinyWidgets")
library("ggplot2")
library("dplyr")
library("shiny")
library("RSwissMaps")
library("plotly")
library("googlesheets4")

### Google Credentials - secrets wird benötigt für Zugriff der App auf Google
sheets_auth(
  cache = ".secrets",
  email = "tobydiethelm@gmail.com"
)


### Globale Objekte laden
source("R/global.R", encoding = "UTF-8")





##### UI #################################

ui <- fluidPage(theme = shinytheme("united"),
  titlePanel(
    div(h1("Corona in den Schwyzer Gemeinden", img(src='coronaSZ.png', width="75px"))), windowTitle = "Corona-Situation im Kanton Schwyz"),
  fluidRow(
    column(12, h2(textOutput("titelSituation"), style = "border-bottom: 5px solid red;display: inline-block;"))),
  fluidRow(
    column(6, plotOutput("kartenPlot")),
    column(6, h3("Hot- und Coldspots"), htmlOutput("text"))),
  fluidRow(
    column(6, plotlyOutput("gemeindePlot"), materialSwitch(inputId = "switchRelativ", label = "Anzahl Fälle pro 1000 Einwohner", status = "danger")),
    column(6, h3(textOutput("titelGemeinde")), selectInput("gemeinde", "Gemeinde ändern", choices = "", multiple = F), htmlOutput("textGemeindeAktuell"),  htmlOutput("textGemeindeHistorie"))),
  fluidRow(
    column(12, h2("Entwicklung der Zahlen", style = "border-bottom: 5px solid red;display: inline-block;"))),
  fluidRow(
    column(8, plotOutput("zeit")),
    column(4, fluidRow(column(12, selectInput("gemeinden", "Vergleichen", choices = gemeinden, multiple = T)))))
)




##### SERVER #############################

server <- function(input, output, session) {
  
  
  ### Daten laden
  daten <- datenAnreichern(loadData("Daten"))
  
  ### Vergleichsdatum
  vergleichsdatum <- vergleichsDatum(daten)
  
  ### Aktuellstes Datum
  daten_aktuell <- aktuelleDaten(daten)
  
  ### Aktuelle Daten mit Rangliste
  daten_aktuell_rang <- aktuelleDatenRang(daten_aktuell)
  
  ### Aktuelle Daten einer einzelnen Gemeinde
  daten_aktuell_gemeinde <- reactive({
    req(input$gemeinde)
    datenGemeinde(daten_aktuell_rang, input$gemeinde)
  })
  
  ### Daten filtern von mehreren Gemeinde
  daten_gemeinde <- reactive({
    req(input$gemeinden)
    # gemeindenFilter <- unique(c(input$gemeinde, input$gemeinden))
    datenGemeinde(daten, input$gemeinden)
  })
  
  
  ### Titel "Aktuelle Situation vom Datum"
  output$titelSituation <-  renderText({
    paste0("Aktuelle Situation vom ", format(unique(daten_aktuell$Datum),  "%d.%m.%Y"))
  })
  
  ### Titel "Aktuelle Sitation von Gemeinde XY"
  output$titelGemeinde <-  renderText({
    req(input$gemeinde)
    paste0("Situation in ", input$gemeinde)
  })
  
  
  
  # KartenPlot
  output$kartenPlot <- renderPlot({
      kartenPlot(daten_aktuell)
  })
  
  
  ### HTML OUTPUT
  
  # Gemeinden identifizieren
  gemeinde_absolut <- gemeindeAbsolut(daten_aktuell_rang)
  gemeinde_relativ <- gemeindeRelativ(daten_aktuell_rang)
  gemeinde_ohneCases <- gemeindeOhneCases(daten_aktuell_rang)
  
  output$text <- renderUI({
    str1 <- paste0("In der Gemeinde ", strong(gemeinde_absolut$Gemeinde), " liegen mit ", strong(gemeinde_absolut$Anzahl), " positiv getesteten Personen derzeit die meisten Fälle vor.")
    str2 <- paste0("Unter Berücksichtigtung der Einwoherzahl ist die Gemeinde ", strong(gemeinde_relativ$Gemeinde), " mit ", strong(round(gemeinde_relativ$AnzahlRelativ, 1)), " aktiven Fällen pro 1'000 Einwohner aktuell am stärksten betroffen.")
    str3 <- paste0("Folgende Gemeinden weisen aktuell weniger als 5 Fälle auf: ", strong(paste(gemeinde_ohneCases, collapse = ", ")))
    
    HTML(paste0("<ul><li>",str1, "</li><li>", str2, "</li><li>", str3, "</li></ul>"))
    
  })
  
  
  ### TEXT OUTPUT GEMEINDE AKTUELL
  
  output$textGemeindeAktuell <- renderUI({
    gemeindeText(daten_aktuell_gemeinde(), input$gemeinde)
    
  })
  
  
  ### TEXT OUTPUT GEMEINDE HISTORIE
  
  output$textGemeindeHistorie <- renderUI({
    req(input$gemeinde)
    gemeindeTextHistorie(daten, input$gemeinde, vergleichsdatum)
    
  })
  

  # GemeindePlot
  output$gemeindePlot <- renderPlotly({
    req(input$gemeinde)
    input_gemeinde <- input$gemeinde
    
    if(input$switchRelativ==T) {
      ggplotly(gemeindePlotRelativ(daten_aktuell, input_gemeinde))
    }
    else {
      ggplotly(gemeindePlot(daten_aktuell, input_gemeinde))
    }
  })
  
  ### Update Vergleich wenn neue Gemeinde gewählt wird
  observeEvent(input$gemeinde,{
    updateSelectInput(session, "gemeinden", selected = input$gemeinde)
  })


  
  gemeindeAuswahl <- reactive({unique(daten$Gemeinde)})  
  
  observeEvent(gemeindeAuswahl(), {
    choices <- gemeindeAuswahl()
    updateSelectInput(session, "gemeinde", choices = choices, selected = gemeinde_absolut$Gemeinde)
  })
    
    
    # zeit
    output$zeit <- renderPlot({
      req(daten_gemeinde())
      ggplot(daten_gemeinde(), aes(x=Datum, y=Anzahl, group=Gemeinde)) +
        geom_line(aes(color=Gemeinde)) +
        geom_point(aes(color=Gemeinde))
      
    })
    
    
  }




###### RUN APP ############################################

shinyApp(ui, server)