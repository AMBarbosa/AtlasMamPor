# by A. Marcia Barbosa (https://modtools.wordpress.com/barbosa/)
# used for https://atlas-mamiferos.uevora.pt/index.php/mapas/
# last updated 31 Jul 2023

# RStudio: Session -> Set Working Directory -> To Source File Location

library(terra)
library(leaflet)
library(leaflet.extras)  # 'addSearchOSM' function
library(shiny)

# carregar objectos necessarios tanto para 'ui' como para 'server':
source('global.R')


ui <- fluidPage(

  fluidPage(
    h1("Atlas of Mammals in Portugal")  # or titlePanel()
  ),

  sidebarLayout(

    sidebarPanel(
      selectInput(inputId = "ordem", label = "Order", choices = c("ALL", as.character(sort(unique(atlas$ordem))))),
      selectInput(inputId = "especie", label = "Species", choices = "", selectize = FALSE),

      # hr(),  # horizontal line

      p("Note that", tags$i("Microtus agrestis"), "populations in Portugal were recently reassigned to the new species", tags$i("Microtus rozianus", .noWS = "after"), "."),

      br(),

      p(tags$strong("SOURCES:")),
      p("Bencatel J., Sabino-Marques H., Alvares F., Moura A.E. & Barbosa A.M. (2019)", a(href = "https://atlasmamiferosportugal.wordpress.com/", "Atlas de Mamiferos de Portugal (2nd edition)", .noWS = "after"), ". Universidade de Evora, Portugal"),
      p("Grilo et al. (2022)", a(href = "https://doi.org/10.1002/ecy.3654", "Mammals in Portugal: A data set of terrestrial, volant, and marine mammal occurrences in Portugal", .noWS = "after"), ".", tags$i("Ecology,"), "103:e3654"),

      br(),

      p("Note Grilo et al. (2022) provide point occurrences at a higher resolution; here they were converted to the Atlas grid, but see the original paper for more detail."),
    ),  # end sidebar panel

    mainPanel(
      leafletOutput(outputId = "mapa", height = "80vh"),

      p("The data, maps and code behind this webpage are available at", a(href = "https://github.com/AMBarbosa/AtlasMamPor", "https://github.com/AMBarbosa/AtlasMamPor"), "under a Creative Commons Attribution-ShareAlike license (CC BY-SA 4.0)."),

      p("The Atlas of Mammals in Portugal gathered mammal occurrence records on UTM 10x10-km2 grid cells, made available (in publications, theses, reports, online photos or direct contributions) over approximately three decades up to 2018. Mind that survey effort was uneven: as in most atlases, the data reflect spatial and taxonomic bias, as some species and areas were more intensively surveyed than others. The atlas is a picture of the knowledge made available up to the time of publication."),

      p("Grid cell presences extracted from major datasets published after the Atlas were added to the interactive maps, and are now also cited under 'SOURCES' on the left panel"),

      p("Note that the 'confirmed', 'credible' and 'interview' labels refer only to the Atlas records, in dark red. Note also that marine mammals are not in the interactive maps due to data privacy, but they have plenty of records in the Atlas. See the PDF book (freely available", a(href = "https://atlasmamiferosportugal.wordpress.com", "here", .noWS = "after"), ") for more info on the data!")
    )  # end mainPanel
  )  # end sidebarLayout
)  # end fluidPage


server <- function(input, output, session) {

  seleccionar_especies <- reactive({
    ord <- input$ordem
    if (ord == "ALL")  especies <- sort(unique(atlas$especie))
    else  especies <- sort(unique(atlas[atlas$ordem == ord, "especie"]))
    especies
  })

  observe({
    updateSelectInput(session, "especie", choices = seleccionar_especies()
    )})

  seleccionar_dados <- reactive({
    dados <- atlas[atlas$especie == input$especie, ]
    dados <- rbind(dados, datapaper[datapaper$especie == input$especie, ])  # data paper add
    dados
  })

  limites <- ext(ptgal)

  cor_grelha <- "darkgrey"
  cor_semdata <- "grey"
  cor_antigo <- "orange"
  cor_recente <- "darkred"
  cor_confirm <- "#FF6347"
  cor_datapaper <- "darkblue"

  # https://stackoverflow.com/questions/37862467/leaflet-legend-for-custom-markers-in-r
  # images need to be online or stored in a folder named 'www'
  atlas_legend <- "<img src='legenda_semdata.png'>No date<br/>
<img src='legenda_antigo.png'>Old (1990-1999)<br/>
  <img src='legenda_recente.png'>Recent (2000-2018)<br/>
  <img src='legenda_confirmado.png'>  confirmed<br/>
  <img src='legenda_plausivel.png'>  credible<br/>
  <img src='legenda_inquerito.png'>  interview<br/>
  <img src='legenda_datapaper.png'>  Grilo et al. (2022)<br/>"

  output$mapa <- renderLeaflet({
    leaflet(data = ptgal) |>
      fitBounds(lng1 = as.numeric(limites[1]), lat1 = as.numeric(limites[3]), lng2 = as.numeric(limites[2]), lat2 = as.numeric(limites[4]))  |>
      addTiles(options = providerTileOptions(minZoom = 4, maxZoom = 15), group = "OpenStreetMap") |>
      addProviderTiles(options = providerTileOptions(minZoom = 4, maxZoom = 15), providers$OpenTopoMap, group = "OpenTopoMap") |>
      addProviderTiles(options = providerTileOptions(minZoom = 4, maxZoom = 15), providers$Stamen.Terrain, group = "Stamen.Terrain") |>
      addControl(html = atlas_legend, position = "bottomleft")
  })  # end renderLeaflet

  observe({

    dados <- seleccionar_dados()

    leafletProxy("mapa", data = ptgal) |>
      clearShapes() |>

      # https://search.r-project.org/cran/refmans/leaflet/html/addMapPane.html
      addMapPane("back", zIndex = 410) |>  # always below
      addMapPane("front", zIndex = 440) |>  # always on top, for grid utm labels to show

      addPolygons(data = ptgal[ptgal$utm10 %in% dados[which(is.na(dados$recente) & dados$source == "atlas"), "utm10"], ], fillColor = cor_semdata, fillOpacity = 0.5, stroke = FALSE, group = "No date", options = pathOptions(pane = "back")) |>
      addPolygons(data = ptgal[ptgal$utm10 %in% dados[!is.na(dados$recente) & dados$recente == 0 & dados$source == "atlas", "utm10"], ], fillColor = cor_antigo, fillOpacity = 0.6, stroke = FALSE, group = "Old", options = pathOptions(pane = "back")) |>
      addPolygons(data = ptgal[ptgal$utm10 %in% dados[!is.na(dados$recente) & dados$recente == 1 & dados$source == "atlas", "utm10"], ], fillColor = cor_recente, fillOpacity = 0.6, stroke = FALSE, group = "Recent", options = pathOptions(pane = "back")) |>
      addPolygons(data = ptgal[ptgal$utm10 %in% dados[dados$source == "datapaper", "utm10"], ], fillColor = cor_datapaper, fillOpacity = 0.6, stroke = FALSE, group = "Grilo et al. (2022)", options = pathOptions(pane = "back")) |>

      addCircles(data = ptgal[ptgal$utm10 %in% dados[!is.na(dados$confirmado) & dados$confirmado == 1, "utm10"], c("centr_x", "centr_y")], ~centr_x, ~centr_y, radius = 3000, stroke = FALSE, fill = TRUE, fillColor = cor_confirm, fillOpacity = 0.5, group = "Reliability", options = pathOptions(pane = "back")) |>
      addCircles(data = ptgal[ptgal$utm10 %in% dados[!is.na(dados$confirmado) & dados$confirmado == 0, "utm10"], c("centr_x", "centr_y")], ~centr_x, ~centr_y, radius = 3000, stroke = TRUE, fill = FALSE, color = cor_confirm, weight = 2, opacity = 0.5, group = "Reliability", options = pathOptions(pane = "back")) |>
      addRectangles(data = ptgal[ptgal$utm10 %in% dados[!is.na(dados$confirmado) & dados$confirmado == -1, "utm10"], c("centr_x", "centr_y")], lng1 = ~centr_x - 0.03, lat1 = ~centr_y - 0.008, lng2 = ~centr_x + 0.03, lat2 = ~centr_y + 0.008, stroke = FALSE, fill = TRUE, fillColor = cor_confirm, fillOpacity = 0.5, group = "Reliability", options = pathOptions(pane = "back")) |>

      addPolygons(color = cor_grelha, fillColor = NULL, fillOpacity = 0, weight = 1, label = ~utm10, labelOptions = labelOptions(noHide = FALSE, textOnly = FALSE, opacity = 0.8, textsize = "16px", style = list("color" = "darkgreen")), options = pathOptions(pane = "front")) |>

      addLayersControl(overlayGroups = c("No date", "Old", "Recent", "Reliability", "Grilo et al. (2022)"), baseGroups = c("OpenStreetMap", "OpenTopoMap", "Stamen.Terrain"), options = layersControlOptions(collapsed = TRUE)) |>

      leaflet.extras::addSearchOSM()
  })  # end observe
}


shinyApp(ui = ui, server = server)
