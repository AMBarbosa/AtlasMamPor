# RStudio: Session -> Set Working Directory -> To Source File Location

library(rgdal)
library(leaflet)
library(shiny)

# carregar objectos necessarios tanto para 'ui' como para 'server':
source('global.R')


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "ordem", label = "Order", choices = c("ALL", as.character(sort(unique(mam_pt$ordem))))),
      selectInput(inputId = "especie", label = "Species", choices = "", selectize = FALSE),
      textOutput(outputId = "fonte")
    ),

    mainPanel(
      leafletOutput(outputId = "mapa"),
      textOutput(outputId = "info"),
      textOutput(outputId = "lic")
    )
  )
)


server <- function(input, output, session) {

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

  limites <- bbox(ptgal)
  limites["x", "min"] <- limites["x", "min"] + 16
  limites["y", "min"] <- limites["y", "min"] + 3
  cor_grelha <- "darkgrey"
  cor_semdata <- "grey"
  cor_antigo <- "orange"
  cor_recente <- "darkred"
  cor_confirm <- "#FF6347"

  # https://stackoverflow.com/questions/37862467/leaflet-legend-for-custom-markers-in-r
  atlas_legend <- "<img src='http://atlas-mamiferos.uevora.pt/wp-content/uploads/2019/03/legenda_semdata.png'>No date<br/>
<img src='http://atlas-mamiferos.uevora.pt/wp-content/uploads/2019/03/legenda_antigo.png'>Old (1990-1999)<br/>
  <img src='http://atlas-mamiferos.uevora.pt/wp-content/uploads/2019/03/legenda_recente.png'>Recent (2000-2018)<br/>
  <img src='http://atlas-mamiferos.uevora.pt/wp-content/uploads/2019/03/legenda_confirmado.png'>  confirmed<br/>
  <img src='http://atlas-mamiferos.uevora.pt/wp-content/uploads/2019/03/legenda_plausivel.png'>  credible<br/>
  <img src='http://atlas-mamiferos.uevora.pt/wp-content/uploads/2019/03/legenda_inquerito.png'>  interview<br/>"

  output$mapa <- renderLeaflet({
    leaflet(data = ptgal) %>%
      fitBounds(lng1 = limites["x", "min"], lat1 = limites["y", "min"], lng2 = limites["x", "max"], lat2 = limites["y", "max"])  %>%
      addTiles(options = providerTileOptions(minZoom = 4, maxZoom = 15), group = "OpenStreetMap") %>%
      addProviderTiles(options = providerTileOptions(minZoom = 4, maxZoom = 15), providers$OpenTopoMap, group = "OpenTopoMap") %>%
      addControl(html = atlas_legend, position = "bottomright")
  })

  observe({
    dados <- seleccionar_dados()

    leafletProxy("mapa", data = ptgal) %>%
      clearShapes() %>%

      addPolygons(data = ptgal[ptgal$utm10 %in% dados[which(is.na(dados$recente)), "utm10"], ], fillColor = cor_semdata, fillOpacity = 0.5, stroke = FALSE, group = "No date") %>%
      addPolygons(data = ptgal[ptgal$utm10 %in% dados[!is.na(dados$recente) & dados$recente == 0, "utm10"], ], fillColor = cor_antigo, fillOpacity = 0.6, stroke = FALSE, group = "Old") %>%
      addPolygons(data = ptgal[ptgal$utm10 %in% dados[!is.na(dados$recente) & dados$recente == 1, "utm10"], ], fillColor = cor_recente, fillOpacity = 0.6, stroke = FALSE, group = "Recent") %>%

      addCircles(data = ptgal[ptgal$utm10 %in% dados[!is.na(dados$confirmado) & dados$confirmado == 1, "utm10"], c("centr_x", "centr_y")], ~centr_x, ~centr_y, radius = 3000, stroke = FALSE, fill = TRUE, fillColor = cor_confirm, fillOpacity = 0.5, group = "Reliability") %>%
      addCircles(data = ptgal[ptgal$utm10 %in% dados[!is.na(dados$confirmado) & dados$confirmado == 0, "utm10"], c("centr_x", "centr_y")], ~centr_x, ~centr_y, radius = 3000, stroke = TRUE, fill = FALSE, color = cor_confirm, weight = 2, opacity = 0.5, group = "Reliability") %>%
      addRectangles(data = ptgal[ptgal$utm10 %in% dados[!is.na(dados$confirmado) & dados$confirmado == -1, "utm10"], c("centr_x", "centr_y")], lng1 = ~centr_x - 0.03, lat1 = ~centr_y - 0.008, lng2 = ~centr_x + 0.03, lat2 = ~centr_y + 0.008, stroke = FALSE, fill = TRUE, fillColor = cor_confirm, fillOpacity = 0.5, group = "Reliability") %>%

      addPolygons(color = cor_grelha, fillColor = NULL, fillOpacity = 0, weight = 1, label = ~utm10, labelOptions = labelOptions(noHide = FALSE, textOnly = TRUE, opacity = 0.8, textsize = "16px", style = list("color" = "darkgreen"))) %>%

      addLayersControl(overlayGroups = c("No date", "Old", "Recent", "Reliability"), baseGroups = c("OpenStreetMap", "OpenTopoMap"), options = layersControlOptions(collapsed = TRUE))
  })

  output$fonte <- renderText({paste("SOURCE: Bencatel J., Sabino-Marques H., Alvares F., Moura A.E. & Barbosa A.M. (2019) Atlas de Mamiferos de Portugal (2nd edition). Universidade de Evora, Portugal.")})

  output$info <- renderText({paste("See http://atlas-mamiferos.uevora.pt/ for more information on the data.")})

  output$lic <- renderText({paste("Available under a Creative Commons Attribution-ShareAlike license (CC BY-SA 4.0).")})

}


shinyApp(ui = ui, server = server)
