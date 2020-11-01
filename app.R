### Daten laden
source("initial.R", encoding = "UTF-8")

### Daten laden
daten_raw <- read_sheet(sheet_url, sheet="Daten")
daten <- datenAnreichern(daten_raw)

### Aktuellstes Datum
daten_aktuell <- group_by(daten, Gemeinde) %>% slice(which.max(as.Date(Datum, '%d.%m.%Y')))
daten_aktuell$Gemeinde <- factor(daten_aktuell$Gemeinde, levels = daten_aktuell$Gemeinde[order(daten_aktuell$Anzahl)])


# Define UI for app that draws a histogram
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel(div(img(src='coronaSZ.png', width="100px"),"Corona SZ // Gemeinden")),
  plotOutput("kartenPlot"),
  plotlyOutput("gemeindePlot"),
  actionButton("buttonId", "Prüfen, ob es neue Daten auf sz.ch gibt"),
  "Beta-Version, tobias.diethelm@bluewin.ch"
 
)





# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
      observeEvent(input$buttonId, {
        message("running script.R")
        source("readData.R")
      })

    # KartenPlot
    output$kartenPlot <- renderPlot({
      kartenPlot(daten_aktuell)
    })
    
    # GemeindePlot
    output$gemeindePlot <- renderPlotly({
      ggplotly(gemeindePlot(daten_aktuell))
    })
    
    
  }


shinyApp(ui, server)




# shinyApp(
#   fluidPage(
#     actionButton("buttonId", "run script")
#   ),
#   function(input, output, session) {
#     observeEvent(input$buttonId, {
#       message("running script.R")
#       source("readData.R")
#     })
#   }
# )