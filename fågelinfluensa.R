library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(ggplot2)

# Läs in data
url <- "https://raw.githubusercontent.com/SVA-SE/data-viz-quiz/master/data/Avian_Influenza.csv"
data <- read.csv(url, sep = ";", header = TRUE)

# Definiera User interface
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Tester", 
              dateRangeInput("daterange", "Välj datumintervall:", 
              start = min(data$Ankomstdatum), 
                      end = max(data$Ankomstdatum)),
             
              selectInput("Djurslag", "Välj djurslag:", 
                      choices = c("All", unique(data$Djurslag))),
             
              leafletOutput("map_markers"),
              HTML("<br><h4>Testning för fågelinfluensa</h4>")
    ),
    tabPanel("Heatmap", 
              leafletOutput("map_heatmap"),
              HTML("<br><h4>Fördelning av fall av fågelinfluensa</h4>")
    ),
    tabPanel("Bar plot",
             plotOutput("bar_plot")
    )
  )
)

# Definiera server
server <- function(input, output) {
  
  # Filtrera data med avseende på datumintervall och djurslag
  filtered_data <- reactive({
    if (input$Djurslag == "All") {
      data[data$Ankomstdatum >= input$daterange[1] & 
             data$Ankomstdatum <= input$daterange[2], ]
    } else {
      data[data$Ankomstdatum >= input$daterange[1] & 
             data$Ankomstdatum <= input$daterange[2] &
             data$Djurslag == input$Djurslag, ]
    }
  })
  
  # Skapa karta med markeringar 
  output$map_markers <- renderLeaflet({
    leaflet(filtered_data()) %>%
      addTiles() %>%
      addCircleMarkers(
        lat = ~coords.x2,
        lng = ~coords.x1,
        color = ifelse(filtered_data()$Resultat == 0, "blue", "red"),
        group = "Tester"
      ) %>%
      addLegend(
        position = "topright",
        colors = c("blue", "red"),
        labels = c("Ej influensa", "Influensa"),
        title = "Resultat" 
      ) 
  })
  
  # Skapa heatmap
  output$map_heatmap <- renderLeaflet({
    data_heatmap <- filtered_data()[filtered_data()$Resultat == 1 & 
                                    filtered_data()$Ankomstdatum >= input$daterange[1] &
                                    filtered_data()$Ankomstdatum <= input$daterange[2],]
    map <- leaflet(data_heatmap) %>%
      addTiles() 
    map <- map %>%
      addHeatmap(
        lng = ~coords.x1,
        lat = ~coords.x2,
        blur=70
      )
    map
  })
  
  # Skapa bar plot på fördelning av andel funna fall av influensa per månad
  output$bar_plot <- renderPlot({
    # Filtrera data med avseende på djurslag
    filtered <- filtered_data()
    if (input$Djurslag != "All") {
      filtered <- filtered[filtered$Djurslag == input$Djurslag, ]
    }
    
    # Lägg till kolumn med månad
    filtered$month <- format(as.Date(filtered$Ankomstdatum), "%m")
    
    # Beräkna andel fall av influensa per månad
    month_weights <- filtered %>%
      group_by(month) %>%
      summarize(weight = sum(Resultat == 1) / n())
    
    # Skapa en bar plot 
    ggplot(month_weights, aes(x=month, y=weight)) + 
      geom_bar(stat="identity") +
      xlab("Månad") +
      ylab("Andel fall per månad") +
      ggtitle("Fördelning av andel fall av fågelinfluensa per månad") +
      theme(plot.title = element_text(size = 20))
  })
}

# Skapp app
shinyApp(ui, server)

