# by A. Marcia Barbosa (https://modtools.wordpress.com/barbosa/)
# used for https://atlas-mamiferos.uevora.pt/index.php/mapas/
# last updated 30 Jun 2022

# RStudio: Session -> Set Working Directory -> To Source File Location

library(terra)
library(leaflet)
library(leaflet.extras)  # 'addSearchOSM' function
library(shiny)

# carregar objectos necessarios tanto para 'ui' como para 'server':
source('global.R')


ui <- fluidPage(
  
  fluidPage(
    titlePanel("Atlas of Mammals in Portugal")
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId = "ordem", label = "Order", choices = c("ALL", as.character(sort(unique(mam_pt$ordem))))),
      selectInput(inputId = "especie", label = "Species", choices = "", selectize = FALSE),
      hr(),
      strong(textOutput(outputId = "source")),
      hr(),
      textOutput(outputId = "datainfo")
    ),
    
    mainPanel(
      leafletOutput(outputId = "mapa", height = "80vh"),
      textOutput(outputId = "moreinfo"),
      textOutput(outputId = "lic")
    )
  )
)


server <- function(input, output, session) {
  
  output$linebreak <- renderText("________")
  
  seleccionar_especies <- reactive({
    ord <- input$ordem
    if (ord == "ALL")  especies <- sort(unique(mam_pt$especie))
    else  especies <- sort(unique(mam_pt[mam_pt$ordem == ord, "especie"]))
    especies
  })
  
  observe({
    updateSelectInput(session, "especie", choices = seleccionar_especies()
    )})
  
  seleccionar_dados <- reactive({
    dados <- mam_pt[mam_pt$especie == input$especie, ]
    dados
  })
  
  # limites <- bbox(ptgal)
  limites <- ext(ptgal)  # 'terra' pkg
  
  cor_grelha <- "darkgrey"
  cor_semdata <- "grey"
  cor_antigo <- "orange"
  cor_recente <- "darkred"
  cor_confirm <- "#FF6347"
  
  # https://stackoverflow.com/questions/37862467/leaflet-legend-for-custom-markers-in-r
  atlas_legend <- "<img src='https://github.com/AMBarbosa/AtlasMamPor/blob/master/imagens/legenda_mapas_interact/legenda_semdata.png?raw=true'>No date<br/>
<img src='https://github.com/AMBarbosa/AtlasMamPor/blob/master/imagens/legenda_mapas_interact/legenda_antigo.png?raw=true'>Old (1990-1999)<br/>
  <img src='https://github.com/AMBarbosa/AtlasMamPor/blob/master/imagens/legenda_mapas_interact/legenda_recente.png?raw=true'>Recent (2000-2018)<br/>
  <img src='https://github.com/AMBarbosa/AtlasMamPor/blob/master/imagens/legenda_mapas_interact/legenda_confirmado.png?raw=true'>  confirmed<br/>
  <img src='https://github.com/AMBarbosa/AtlasMamPor/blob/master/imagens/legenda_mapas_interact/legenda_plausivel.png?raw=true'>  credible<br/>
  <img src='https://github.com/AMBarbosa/AtlasMamPor/blob/master/imagens/legenda_mapas_interact/legenda_inquerito.png?raw=true'>  interview<br/>"
  
  output$mapa <- renderLeaflet({
    leaflet(data = ptgal) %>%
      fitBounds(lng1 = as.numeric(limites[1]), lat1 = as.numeric(limites[3]), lng2 = as.numeric(limites[2]), lat2 = as.numeric(limites[4]))  %>%
      addTiles(options = providerTileOptions(minZoom = 4, maxZoom = 15), group = "OpenStreetMap") %>%
      addProviderTiles(options = providerTileOptions(minZoom = 4, maxZoom = 15), providers$OpenTopoMap, group = "OpenTopoMap") %>%
      addProviderTiles(options = providerTileOptions(minZoom = 4, maxZoom = 15), providers$Stamen.Terrain, group = "Stamen.Terrain") %>%
      addControl(html = atlas_legend, position = "bottomright")
  })
  
  observe({
    
    dados <- seleccionar_dados()
    
    # library(raster)
    # ptgal <- as(ptgal, "Spatial")  # not needed with Hijmans's branch of 'leaflet' which implements 'terra' inputs
    
    leafletProxy("mapa", data = ptgal) %>%
      clearShapes() %>%
      
      addPolygons(data = ptgal[ptgal$utm10 %in% dados[which(is.na(dados$recente)), "utm10"], ], fillColor = cor_semdata, fillOpacity = 0.5, stroke = FALSE, group = "No date") %>%
      addPolygons(data = ptgal[ptgal$utm10 %in% dados[!is.na(dados$recente) & dados$recente == 0, "utm10"], ], fillColor = cor_antigo, fillOpacity = 0.6, stroke = FALSE, group = "Old") %>%
      addPolygons(data = ptgal[ptgal$utm10 %in% dados[!is.na(dados$recente) & dados$recente == 1, "utm10"], ], fillColor = cor_recente, fillOpacity = 0.6, stroke = FALSE, group = "Recent") %>%
      
      addCircles(data = ptgal[ptgal$utm10 %in% dados[!is.na(dados$confirmado) & dados$confirmado == 1, "utm10"], c("centr_x", "centr_y")], ~centr_x, ~centr_y, radius = 3000, stroke = FALSE, fill = TRUE, fillColor = cor_confirm, fillOpacity = 0.5, group = "Reliability") %>%
      addCircles(data = ptgal[ptgal$utm10 %in% dados[!is.na(dados$confirmado) & dados$confirmado == 0, "utm10"], c("centr_x", "centr_y")], ~centr_x, ~centr_y, radius = 3000, stroke = TRUE, fill = FALSE, color = cor_confirm, weight = 2, opacity = 0.5, group = "Reliability") %>%
      addRectangles(data = ptgal[ptgal$utm10 %in% dados[!is.na(dados$confirmado) & dados$confirmado == -1, "utm10"], c("centr_x", "centr_y")], lng1 = ~centr_x - 0.03, lat1 = ~centr_y - 0.008, lng2 = ~centr_x + 0.03, lat2 = ~centr_y + 0.008, stroke = FALSE, fill = TRUE, fillColor = cor_confirm, fillOpacity = 0.5, group = "Reliability") %>%
      
      addPolygons(color = cor_grelha, fillColor = NULL, fillOpacity = 0, weight = 1, label = ~utm10, labelOptions = labelOptions(noHide = FALSE, textOnly = TRUE, opacity = 0.8, textsize = "16px", style = list("color" = "darkgreen"))) %>%
      
      addLayersControl(overlayGroups = c("No date", "Old", "Recent", "Reliability"), baseGroups = c("OpenStreetMap", "OpenTopoMap", "Stamen.Terrain"), options = layersControlOptions(collapsed = TRUE)) %>%
      
      addSearchOSM()
  })
  
  output$source <- renderText("SOURCE: Bencatel J., Sabino-Marques H., Alvares F., Moura A.E. & Barbosa A.M. (2019) Atlas de Mamiferos de Portugal (2nd edition). Universidade de Evora, Portugal")
  
  output$datainfo <- renderText("This atlas gathers mammal occurrence records made available (in publications, theses, reports, online photos or direct contributions) over the last three decades. Mind that survey effort was uneven: as in most atlases, the data reflect spatial and taxonomic bias, as some species and areas were more intensively surveyed than others. The atlas is a picture of the knowledge made available up to the time of publication.")
  
  output$moreinfo <- renderText("See http://atlas-mamiferos.uevora.pt/ for more information on the data.")
  
  output$lic <- renderText("Available under a Creative Commons Attribution-ShareAlike license (CC BY-SA 4.0).")
  
}


shinyApp(ui = ui, server = server)
