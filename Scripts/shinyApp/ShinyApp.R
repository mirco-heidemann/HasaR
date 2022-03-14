# Load packages ----
library(shiny)
library(shinythemes)
library(plotrix)
library(DT)

## start shiny app ----

## User interface
ui <- fluidPage(
  
  navbarPage("GVZ", id="nav",
             
             # ## tab1
             # tabPanel("Hagelereignis",
             #          titlePanel("Datum des Hagelereignisses"),
             #          column(4, wellPanel(
             #            dateInput('date',
             #                      label = 'Date input: yyyy-mm-dd',
             #                      value = Sys.Date(),
             #                      language = "de"
             #            )
             #          ), ## END wellPanel
             #          actionButton("go", "Go"),
             #          br(),
             #          br(),
             #          verbatimTextOutput("dateText")
             #          ) ## END Column
             # ), # END tab1

             ## tab HasaR
             tabPanel("HasaR",
                      div(class="outer",
                          tags$head(
                            # Include custom CSS
                            includeCSS("styles.css")
                          ),
                          
                          leafletOutput("map", width = "100%", height = "100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        fixed = TRUE, draggable = TRUE, top = 60,
                                        left = "auto", right = 20, bottom = "auto",
                                        width = 250, height = "auto",
                                        
                                        h4("Hagelereignis vom", eventDat),
                                        h5("Der geschaetzte betraegt CHF", strong(BE,"Mio.")),
                                        br(),
                                        
                                        # Input: Select separator ----
                                        radioButtons("imag", "Layer",
                                                     choices = c(Schaden = "schad",
                                                                 Hagelradar = 'meshs'),
                                                     # selected = "schad"
                                                     selected = ""
                                        ) ## END RADIOBUTTONS
                          )
                      ) # END div
             ), ## tab HasaR
             
             ## tab INTERPOL
             tabPanel("Interpol",
                      mainPanel(
                        h3('INTERPOL Methode'),
                        wellPanel("Der geschaetzte Schaden nach INTERPOL Methode vom ",
                          eventDat,"betraegt", strong(format(tbl$schadHagel/1e6, big.mark="'"),
                                                      "Mio.")),
                        br(),
                        p("Die INTERPOL Methode rechnet mit empirischen Betroffenheitsgraden 
                          je Baujahrskategorie und einem Durchschnittsschaden von CHF 5'000.
                          Die Hagelkorngroesse wird dabei nicht beruecksichtigt.
                          Jedem Gebaeude wird anhand des Radarbildes eine 
                          Hagelwahrscheinlichkeit zugeordnet (interpoliert).
                          Die Gebaeude mit einer POH von mindestens 80% werden selektiert 
                          und je nach Baujahrskategorie unterschiedliche Betroffenheitsgrade 
                          angewandt. Betroffenheitsgrad mal die Anzahl selektierter 
                          Gebaeude pro Baujahrskategorie ergibt die Anzahl betroffener
                          Gebaeude pro Baujahrskategorie."),
                        br(),
                        h4("Betroffenheitsgrade:"),
                        wellPanel(p('Gebaeude mit Baujahr aelter als 1960: 0.03'),
                        p('Gebaeude mit Baujahr zwischen 1960 und 2002: 0.06'),
                        p('Gebaeude mit Baujahr juenger als 2002: 0.08')),
                        br(),
                        br(),
                        tableOutput("view"),
                        br(),
                        br(),
                        em("Quelle fuer die Betroffenheitsgrade und 
                           Durchschnittsschaden:", 
                           strong("analyse.hagelevents.xlsx,", style = "color:black"), 
                           "MHE, Nov 2017")
                        # br(),
                        # strong("strong() makes bold text."),
                        # br(),
                        # em("em() creates italicized (i.e, emphasized) text."),
                        # br(),
                        # code("code displays your text similar to computer code"),
                        # br(),
                        # div("div creates segments of text with a similar style. 
                        #     This division of text is all blue because I passed the 
                        #     argument 'style = color:blue' to div", style = "color:blue"),
                        # br(),
                        # p("span does the same thing as div, but it works with",
                        #   span("groups of words", style = "color:blue"),
                        #   "that appear inside a paragraph.")
                        
                      ) ## END MAINPANEL
             ) ## tab INTERPOL
  ) # END navbarPage
) # END UI

## server function ----
server <- function(input, output, session){

  ## tab Datum
  ## select date after action button
  d <- eventReactive(input$go, {
    input$date
  })
  # input$date and others are Date objects. When outputting
  # text, we need to convert to character; otherwise it will
  # print an integer rather than a date.
  output$dateText  <- renderText({
    paste("input$date is", as.character(d()))
  })

    ## tab INTERPOL
  output$view <- renderTable({
    tblOut
  })

  ## tab HasaR
  ## render base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      setView(lng = 8.652261, lat = 47.424643, zoom = 11) %>% 
      addPolygons(data = shp.kanton,
                  color = "#444444", weight = 1.5, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 1.0,
                  fillColor = "transparent")
    
  }) ## END RENDER LEAFLET

  # Use an observer to recreate the raster imag
  observeEvent(input$imag,{

    map <- leafletProxy("map") %>% clearControls() %>% clearImages()
    if (input$imag == 'schad')
    {map <- map %>% addRasterImage(rEstLoss, colors = cbL, opacity = 0.4, project=F) %>% 
      addLegend(position = "bottomright", pal = cbL,
                values = values(rEstLoss),
                title = "Schaden", className = "info legend",
                labFormat = labelFormat(big.mark = "'"))}
    
    else if (input$imag == 'meshs')
    {map <- map %>% addRasterImage(rMeshs, colors = cbR.f, opacity = 0.4, project=F) %>% 
      addLegend(position = "bottomright", pal = cbR.f,
                values = values(rMeshs),
                title = "MESHS",
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  p = c("< 2", "2.5", "3", "3.5",
                        "4", "4.5", "5", "5.5", "6")})}
    
  }) ## END OBSERVER
  
  # ## activate "HasaR" tabPanel
  
} ## END SERVER

shinyApp(ui, server)
