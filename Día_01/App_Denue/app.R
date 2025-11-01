library(shiny)
library(bslib)
library(leaflet)
library(echarts4r)

scian= read_rds("scian.rds")
# De acá puedes sacar el DENUE
# https://www.inegi.org.mx/servicios/api_denue.html#


ui = div(
  tags$head(
    tags$style(HTML("
      * {
        margin: 0;
        padding: 0;
        box-sizing: border-box;
      }
      html, body {
        height: 100%;
        width: 100%;
        overflow: hidden;
      }
      body {
        margin: 0 !important;
        padding: 0 !important;
      }
      .container-fluid {
        padding: 0 !important;
        margin: 0 !important;
      }
      /* Estilos para el panel minimizable */
      .panel-minimizable {
        transition: all 0.3s ease;
      }
      .panel-header {
        background-color: #f8f9fa;
        padding: 8px 12px;
        border-bottom: 1px solid #dee2e6;
        cursor: pointer;
        display: flex;
        justify-content: space-between;
        align-items: center;
        border-radius: 10px 10px 0 0;
      }
      .panel-header:hover {
        background-color: #e9ecef;
      }
      .panel-content {
        transition: all 0.3s ease;
        overflow: hidden;
      }
      .panel-minimized .panel-content {
        height: 0 !important;
        padding: 0;
      }
      .panel-title {
        font-weight: bold;
        font-size: 14px;
        margin: 0;
      }
      .minimize-icon {
        font-size: 12px;
        transition: transform 0.3s ease;
      }
      .panel-minimized .minimize-icon {
        transform: rotate(180deg);
      }
      /* Estilos para el navbar en el absolutePanel */
      .navbar {
        margin-bottom: 0;
        border-radius: 0;
        min-height: 40px;
      }
      .navbar-nav > li > a {
        padding-top: 10px;
        padding-bottom: 10px;
      }
      .tab-content {
        padding: 10px;
        max-height: 585px; /* Aumentado de 450px a 585px (30% más) */
        overflow-y: auto;
      }
    "))
  ),
  
  # JavaScript para manejar el minimizado
  tags$script(HTML("
    $(document).ready(function() {
      $('#panel-header').click(function() {
        $('#analysis-panel').toggleClass('panel-minimized');
        
        if ($('#analysis-panel').hasClass('panel-minimized')) {
          $('#analysis-panel').css('height', '80px');
        } else {
          $('#analysis-panel').css('height', '715px'); /* Aumentado de 550px a 715px (30% más) */
        }
      });
    });
  ")),
  
  style = "position: fixed; top: 0; left: 0; right: 0; bottom: 0; margin: 0; padding: 0;",
  
  # Mapa que cubre toda la pantalla
  leafletOutput("mapa_parkour", width = "100%", height = "100%"),
  
  # Panel absoluto con controles superpuestos - centrado
  absolutePanel(
    top = 10, left = "50%",
    style = "background-color: rgba(255, 255, 255, 0.9); padding: 15px; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.3); transform: translateX(-50%); z-index: 1000;",
    sliderInput(
      "id_radio",
      "Selecciona un valor:",
      min = 100,
      max = 1000,
      value = 500,
      step = 100,
      width = "350px"
    ),
    actionButton(
      inputId = "id_buscar",
      label = "Buscar",
      class = "btn-primary",
      style = "margin-top: 5px; width: 100%;"
    ),
    div(
      style = "margin-top: 10px; font-size: 12px; text-align: center;",
      HTML("Por <strong>Parkour</strong> | "),
      tags$a(
        href = "https://twitter.com/noeosoriopk", 
        target = "_blank",
        style = "color: #1da1f2; text-decoration: none; margin-right: 10px;",
        tags$i(class = "fab fa-twitter", style = "margin-right: 3px;"),
        "@noeosoriopk"
      ),
      HTML(" | "),
      tags$a(
        href = "https://github.com/noeosoriopk", 
        target = "_blank",
        style = "color: #333; text-decoration: none; margin-left: 10px;",
        tags$i(class = "fab fa-github", style = "margin-right: 3px;"),
        "código"
      )
    )
  ),
  
  absolutePanel(
    bottom = 10, left = 10,
    width = "400px",
    height = "40px",  # Altura inicial minimizada
    id = "analysis-panel",
    class = "panel-minimizable panel-minimized",  # Agregar panel-minimized desde el inicio
    style = "background-color: rgba(255, 255, 255, 0.98); border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.3); z-index: 1000; overflow: hidden;",
    
    
    div(
      id = "panel-header",
      class = "panel-header",
      div(
        class = "panel-title",
        "Análisis de Datos"
      ),
      div(
        class = "minimize-icon",
        HTML("▲")  
      )
    ),
    
    
    div(
      class = "panel-content",
      style = "height: 650px;", 
      navbarPage(
        title = "",
        id = "navbar_analisis",
        fluid = TRUE,
        tabPanel(
          title = "Resumen",
          icon = icon("chart-bar"),
          
          gt::gt_output(outputId = "plot_scian")
        )
      )
    )
  )
)

server = function(input, output, session) {
  
  output$mapa_parkour = renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 5,maxZoom = 18)) %>%
      # Grupo INEGI
      addWMSTiles(baseUrl = "http://gaiamapas1.inegi.org.mx/mdmCache/service/wms?",
                  layers = "MapaBaseTopograficov61_sinsombreado",
                  group = "INEGI") %>% 
      addWMSTiles(
        baseUrl = "http://gaia.inegi.org.mx/NLB/tunnel/wms/wms61?",
        layers = "c100",
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          version = "1.3.0",
          crs = "EPSG:4326"
        ),
        attribution = "INEGI WMS",
        group = "INEGI"
      ) %>% 
      addWMSTiles(
        baseUrl = "http://gaia.inegi.org.mx/NLB/tunnel/wms/wms61?",
        layers = "c200",
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          version = "1.3.0",
          crs = "EPSG:4326"
        ),
        attribution = "INEGI WMS",
        group = "INEGI"
      ) %>%
      addWMSTiles(
        baseUrl = "http://gaia.inegi.org.mx/NLB/tunnel/wms/wms61?",
        layers = "t112",
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          version = "1.3.0",
          crs = "EPSG:4326"
        ),
        attribution = "INEGI WMS",
        group = "INEGI"
      ) %>% 
      addWMSTiles(
        baseUrl = "http://gaia.inegi.org.mx/NLB/tunnel/wms/wms61?",
        layers = "c500",
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          version = "1.3.0",
          crs = "EPSG:4326"
        ),
        attribution = "INEGI WMS",
        group = "INEGI"
      ) %>%  
      
      # OSM (solo el provider)
      addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
      addWMSTiles(
        baseUrl = "http://gaia.inegi.org.mx/NLB/tunnel/wms/wms61?",
        layers = "c100",
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          version = "1.3.0",
          crs = "EPSG:4326"
        ),
        attribution = "INEGI WMS",
        group = "OSM"
      ) %>% 
      addWMSTiles(
        baseUrl = "http://gaia.inegi.org.mx/NLB/tunnel/wms/wms61?",
        layers = "c200",
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          version = "1.3.0",
          crs = "EPSG:4326"
        ),
        attribution = "INEGI WMS",
        group = "OSM"
      ) %>%
      addWMSTiles(
        baseUrl = "http://gaia.inegi.org.mx/NLB/tunnel/wms/wms61?",
        layers = "t112",
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          version = "1.3.0",
          crs = "EPSG:4326"
        ),
        attribution = "INEGI WMS",
        group = "OSM"
      ) %>% 
      addWMSTiles(
        baseUrl = "http://gaia.inegi.org.mx/NLB/tunnel/wms/wms61?",
        layers = "c500",
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          version = "1.3.0",
          crs = "EPSG:4326"
        ),
        attribution = "INEGI WMS",
        group = "OSM"
      ) %>%
      # Grupo Carto con layers WMS de INEGI
      addProviderTiles(providers$CartoDB, group = "Carto") %>% 
      addWMSTiles(
        baseUrl = "http://gaia.inegi.org.mx/NLB/tunnel/wms/wms61?",
        layers = "c100",
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          version = "1.3.0",
          crs = "EPSG:4326"
        ),
        attribution = "INEGI WMS",
        group = "Carto"
      ) %>% 
      addWMSTiles(
        baseUrl = "http://gaia.inegi.org.mx/NLB/tunnel/wms/wms61?",
        layers = "c200",
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          version = "1.3.0",
          crs = "EPSG:4326"
        ),
        attribution = "INEGI WMS",
        group = "Carto"
      ) %>%
      addWMSTiles(
        baseUrl = "http://gaia.inegi.org.mx/NLB/tunnel/wms/wms61?",
        layers = "t112",
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          version = "1.3.0",
          crs = "EPSG:4326"
        ),
        attribution = "INEGI WMS",
        group = "Carto"
      ) %>% 
      addWMSTiles(
        baseUrl = "http://gaia.inegi.org.mx/NLB/tunnel/wms/wms61?",
        layers = "c500",
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          version = "1.3.0",
          crs = "EPSG:4326"
        ),
        attribution = "INEGI WMS",
        group = "Carto"
      ) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>% 
      addWMSTiles(
        baseUrl = "http://gaia.inegi.org.mx/NLB/tunnel/wms/wms61?",
        layers = "c100",
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          version = "1.3.0",
          crs = "EPSG:4326"
        ),
        attribution = "INEGI WMS",
        group = "Esri.WorldImagery"
      ) %>% 
      addWMSTiles(
        baseUrl = "http://gaia.inegi.org.mx/NLB/tunnel/wms/wms61?",
        layers = "c200",
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          version = "1.3.0",
          crs = "EPSG:4326"
        ),
        attribution = "INEGI WMS",
        group = "Esri.WorldImagery"
      ) %>%
      addWMSTiles(
        baseUrl = "http://gaia.inegi.org.mx/NLB/tunnel/wms/wms61?",
        layers = "t112",
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          version = "1.3.0",
          crs = "EPSG:4326"
        ),
        attribution = "INEGI WMS",
        group = "Esri.WorldImagery"
      ) %>% 
      addWMSTiles(
        baseUrl = "http://gaia.inegi.org.mx/NLB/tunnel/wms/wms61?",
        layers = "c500",
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          version = "1.3.0",
          crs = "EPSG:4326"
        ),
        attribution = "INEGI WMS",
        group = "Esri.WorldImagery"
      ) %>%
      
      addLayersControl(baseGroups = c("INEGI","Carto","Esri.WorldImagery","OSM"),
                       options = layersControlOptions(collapsed=FALSE)) %>% 
      setView( lng = -102.1289
               , lat = 23.85341
               , zoom = 6) %>%
      setMaxBounds( lng1 = -117.64160
                    , lat1 = 13.87652 
                    , lng2 = -86.61621
                    , lat2 = 32.80434) %>% 
      addMiniMap()
  })
  
  
  rv = reactiveValues(
    clicks = data.frame(),
    buffers = list(),
    current_click = NULL,
    data_denue=data.frame()
  )
  
  observeEvent(input$mapa_parkour_click, {
    
    rv$current_click = input$mapa_parkour_click
    
    
    click = input$mapa_parkour_click
    
    leafletProxy("mapa_parkour") %>%
      clearGroup("current_click") %>%
      clearGroup("marcador") %>%  
      addMarkers(lng = click$lng, lat = click$lat,
                 group = "current_click")
  })
  
  observeEvent(input$id_buscar, {
    
    req(rv$current_click)
    
    showNotification("Obteniendo los negocios", type = "message", duration = 5)
    
    point <- st_point(
      c(
        as.numeric(rv$current_click$lng), 
        as.numeric(rv$current_click$lat)  
      )
    )
    
    point_sf <- st_sfc(point, crs = 4326) %>% 
      sf::st_transform(6372) %>% 
      sf::st_buffer(dist = input$id_radio) %>% 
      sf::st_transform(crs = 4326)
    
    leafletProxy("mapa_parkour") %>%
      clearShapes() %>%  
      clearMarkers() %>% 
      clearGroup("Denue")       
    
    # Manejo de errores para la consulta a la API
    consulta_inegi <- tryCatch({
      jsonlite::fromJSON(
        paste0("https://www.inegi.org.mx/app/api/denue/v1/consulta/Buscar/todos/",
               round(rv$current_click$lat,6), ",", 
               round(rv$current_click$lng,6), "/", 
               input$id_radio, "/", 
               "fbc6c57d-182c-40ea-965b-32c3809ce908"), 
        flatten = TRUE
      ) %>% 
        mutate(Longitud = as.numeric(Longitud), Latitud = as.numeric(Latitud)) %>% 
        janitor::clean_names() %>% 
        mutate(clase=substring(clee,6,11)) %>% 
        mutate(subsector=substring(clase,1,2)) %>% 
        left_join(scian,by=c("subsector"="codigo"))
    }, error = function(e) {
      return(NULL)
    })
    
    limite_1 <- sf::st_bbox(point_sf)
    
    # Verificar si hay error o no hay datos
    if (is.null(consulta_inegi) || nrow(consulta_inegi) == 0) {
      showNotification("No hay negocios en el área seleccionada", type = "warning", duration = 5)
      
      # Solo mostrar el polígono y hacer zoom al área
      leafletProxy("mapa_parkour") %>%
        addPolygons(data = point_sf) %>% 
        flyToBounds(
          lng1 = limite_1[[1]], 
          lat1 = limite_1[[2]], 
          lng2 = limite_1[[3]], 
          lat2 = limite_1[[4]]
        )
      
      return()
    }
    
    rv$data_denue=consulta_inegi
    
    conteo_cat = rv$data_denue %>%
      count(titulo,
            sort = TRUE)
    
    paleton = colorFactor(palette = "viridis", domain = conteo_cat$titulo)
    
    leafletProxy("mapa_parkour") %>%
      addPolygons(data = point_sf,weight = 1,opacity = 1,color = "black",fill=NA) %>% 
      addCircles(
        data = rv$data_denue,
        lng = ~longitud,
        lat = ~latitud,
        weight = 1,
        opacity = 1,
        fillOpacity = .8,
        color = ~paleton(titulo),
        popup = ~paste("<b>", nombre, "</b><br>",
                       "Sector:", titulo, "<br>"),
        group = "Denue"
      ) %>% 
      flyToBounds(
        lng1 = limite_1[[1]], 
        lat1 = limite_1[[2]], 
        lng2 = limite_1[[3]], 
        lat2 = limite_1[[4]]
      )
  })
  
  observeEvent(input$id_buscar, {
    
    if(nrow(rv$data_denue) > 0) {
      # Si hay datos, mostrar la tabla con datos
      output$plot_scian = gt::render_gt({
        rv$data_denue %>% 
          as_tibble() %>% 
          count(titulo, sort = TRUE) %>%
          arrange(desc(n)) %>% 
          mutate(prc = scales::percent(n/sum(n), accuracy = .01)) %>%
          gt(caption = paste0("Negocios por Sector (SCIAN), Total: ", scales::comma(nrow(rv$data_denue)))) %>%
          cols_nanoplot(
            plot_type = "bar",
            columns = n,
            autohide = FALSE,
            new_col_name = "nanoplots",
            new_col_label = md("*%*")
          ) %>%
          cols_align(align = "right", columns = nanoplots)
      })
    } else {
      # Si no hay datos, mostrar una tabla vacía
      output$plot_scian = gt::render_gt({
        data.frame(
          titulo = character(0),
          n = integer(0),
          prc = character(0),
          nanoplots = character(0)
        ) %>%
          gt(caption = "No se encontraron resultados") %>%
          cols_hide(columns = nanoplots)
      })
    }
  })
  
}

shinyApp(ui = ui, server = server)