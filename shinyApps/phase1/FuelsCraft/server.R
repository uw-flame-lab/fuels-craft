#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  # reactive values
  rv <- reactiveValues()
  rv$polygon <- NULL
  rv$ff_polygon <- NULL
  rv$custom_polygon <- NULL
  rv$ff_tree_inventory <- NULL
  rv$custom_tree_inventory <- NULL
  rv$ff_data <- NULL
  rv$custom_data <- NULL

    # output$distPlot <- renderPlot({
    # 
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # 
    # })

  drawEmptyMap <- function() {
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>% 
      setView(-120.15, 48.42, zoom = 12) %>%
      addDrawToolbar(
        targetGroup = "drawn",
        polylineOptions = FALSE,
        polygonOptions = drawPolygonOptions(showArea = FALSE),
        #polygonOptions = drawPolygonOptions(showArea = TRUE),  # this doesn't work (labels do not appear)
        circleOptions = FALSE,
        rectangleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE
        # editOptions = editToolbarOptions()
      )
  }
  
      
    output$map <- renderLeaflet({drawEmptyMap()})
    
    observeEvent(input$map_draw_new_feature, {
      feature <- input$map_draw_new_feature
      coords <- feature$geometry$coordinates[[1]]
      # Convert the coordinates to a GeoJSON string
      geojson_list <- list(
        type = "FeatureCollection",
        features = list(
          list(
            type = "Feature",
            properties = setNames(list(), character(0)),
            geometry = list(
              type = "Polygon",
              coordinates = list(coords)
            )
          )
        )
      )
      geojson_string <- toJSON(geojson_list, auto_unbox = TRUE)
      rv$polygon <- geojson_string
      
      # Convert the GeoJSON string to an sf object
      sf_object <- geojsonsf::geojson_sf(geojson_string)
      
      # Calculate the area of the polygon
      area <- st_area(sf_object)      
      # change area to km^2
      area <- as.numeric(area)/1000000
      units(area) <- 'km^2'
            
      if (as.numeric(area) < 16) {
        outputString <- paste0("area: ",area, " (ok, less than 16 km^2)")
        output$area <- renderPrint({outputString})
        updateActionButton(session, "createDomain", disabled = FALSE)
      } else {
        outputString <- paste0("area: ", area, " (too large, must be < 16 km^2)")
        output$area <- renderPrint({outputString})
        updateActionButton(session, "createDomain", disabled = TRUE)
      }
    })
    
    # handle clearMap button click
    observeEvent(input$clearMap, {
      rv$polygon <- NULL
      rv$ff_polygon <- NULL
      rv$custom_polygon <- NULL
      rv$ff_data <- NULL
      rv$custom_data <- NULL
      rv$ff_tree_inventory <- NULL
      rv$custom_tree_inventory <- NULL
      output$area <- renderPrint("")
      updateActionButton(session, "createDomain", disabled = TRUE)
      updateActionButton(session, "createRoadFeature", disabled = TRUE)
      updateActionButton(session, "createWaterFeature", disabled = TRUE)
      updateActionButton(session, "createTreeInventory", disabled = TRUE)
      updateActionButton(session, "exportTreeInventory", disabled = TRUE)
      updateActionButton(session, "createTreeGrid", disabled = TRUE)
      updateActionButton(session, "createSurfaceGrid", disabled = TRUE)
      updateActionButton(session, "createTopographyGrid", disabled = TRUE)
      updateActionButton(session, "getInputFiles", disabled = TRUE)
      output$map <- renderLeaflet({drawEmptyMap()})
    })
    
    # handle cropCustomPolygon button click
    observeEvent(input$cropCustomPolygon, {
      
      # convert rv$polygon to sf object, update rv$custom_polygon
      sf_object <- geojsonsf::geojson_sf(rv$polygon)
      rv$custom_polygon <- sf_object
      
      # get only the trees that are in rv$polygon and save to rv$custom_tree_inventory
      # Convert custom_data to sf object
      custom_data_sf <- st_as_sf(rv$custom_data, coords = c("Longitude", "Latitude"), crs = 4326)
      
      # Check if points are within the polygon
      within_polygon <- sapply(st_within(custom_data_sf, rv$custom_polygon), length) > 0
      
      # Subset the dataframe using the logical vector
      rv$custom_data <- rv$custom_data[within_polygon, ]      
      
      # save the custom tree inventory to a file with a timestamp
      timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
      write.csv(rv$custom_data, paste0("tree_inventory_custom_", timestamp, ".csv"), row.names = FALSE)

      # update the map
      loadTreeInventory()
    })
    
    observeEvent(input$removeFFTrees, {
      sf_object <- geojsonsf::geojson_sf(rv$polygon)
      
      # remove trees that are in rv$polygon from rv$ff_data
      # Convert ff_data to sf object
      ff_data_sf <- st_as_sf(rv$ff_data, coords = c("Longitude", "Latitude"), crs = 4326)
      
      # Check if points are within the polygon
      within_polygon <- sapply(st_within(ff_data_sf, sf_object), length) > 0
      
      # Subset the dataframe using the logical vector
      rv$ff_data <- rv$ff_data[!within_polygon, ]
      
      # save the ff tree inventory to a file with a timestamp
      timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
      write.csv(rv$ff_data, paste0("tree_inventory_ff_", timestamp, ".csv"), row.names = FALSE)
      
      loadTreeInventory()
    })

    # handle createDomain button click
    observeEvent(input$createDomain, {
      apiKey = input$api_key
      headers <- add_headers(
        accept = "application/json",
        `api-key` = apiKey,
        `Content-Type` = "application/json"
      )
      url <- "https://api.fastfuels.silvxlabs.com/v1/domains"
      
      # create geojson from rv$polygon
      geojson <- rv$polygon
      response <- POST(url, headers, body = geojson)
      contentResponse = content(response, "parsed")
      domain_id = contentResponse$id
      print(domain_id)
      
      if (domain_id != "") {
        updateTextInput(session, "domainId", value = domain_id)
        updateActionButton(session, "createRoadFeature", disabled = FALSE)
      }
    })
    
    # handle createRoadFeature button click
    observeEvent(input$createRoadFeature, {
      headers <- add_headers(
        accept = "application/json",
        `api-key` = input$api_key,
        `Content-Type` = "application/json"
      )
      url = paste0("https://api.fastfuels.silvxlabs.com/v1/domains/", input$domainId, "/features/road")
      response <- POST(url, headers, body = '{"sources": ["OSM"]}')
      contentResponse = content(response, "parsed")
      showPageSpinner()
      while (contentResponse$status != "completed") {
        Sys.sleep(5)
        response <- GET(url, headers)
        contentResponse = content(response, "parsed")
        print(contentResponse$status)
        # todo: check for error
      }
      hidePageSpinner()
      output$roadFeature <- renderPrint("completed")
      updateActionButton(session, "createWaterFeature", disabled = FALSE)
    })

    # handle createWaterFeature button click
    observeEvent(input$createWaterFeature, {
      headers <- add_headers(
        accept = "application/json",
        `api-key` = input$api_key,
        `Content-Type` = "application/json"
      )
      url = paste0("https://api.fastfuels.silvxlabs.com/v1/domains/", input$domainId, "/features/water")
      response <- POST(url, headers, body = '{"sources": ["OSM"]}')
      contentResponse = content(response, "parsed")
      showPageSpinner()
      while (contentResponse$status != "completed") {
        Sys.sleep(5)
        response <- GET(url, headers)
        contentResponse = content(response, "parsed")
        print(contentResponse$status)
      }
      hidePageSpinner()
      output$waterFeature <- renderPrint("completed")
      updateActionButton(session, "createTreeInventory", disabled = FALSE)
    })
    
    # handle createTreeInventory button click
    observeEvent(input$createTreeInventory, {
      headers <- add_headers(
        accept = "application/json",
        `api-key` = input$api_key,
        `Content-Type` = "application/json"
      )
      url = paste0("https://api.fastfuels.silvxlabs.com/v1/domains/", input$domainId, "/inventories/tree")
      response <- POST(url, headers, body = '{"sources": ["TreeMap"], "featureMasks": ["road", "water"]}')
      contentResponse = content(response, "parsed")
      showPageSpinner()
      while (contentResponse$status != "completed") {
        Sys.sleep(5)
        response <- GET(url, headers)
        contentResponse = content(response, "parsed")
        print(contentResponse$status)
      }
      hidePageSpinner()
      output$treeInventory <- renderPrint("completed")
      updateActionButton(session, "exportTreeInventory", disabled = FALSE)
    })
    
    # handle exportTreeInventory button click
    observeEvent(input$exportTreeInventory, {
      headers <- add_headers(
        accept = "application/json",
        `api-key` = input$api_key,
        `Content-Type` = "application/json"
      )
      url = paste0("https://api.fastfuels.silvxlabs.com/v1/domains/", input$domainId, "/inventories/tree/exports/csv")
      response <- POST(url, headers)
      contentResponse = content(response, "parsed")
      showPageSpinner()
      while (contentResponse$status != "completed") {
        Sys.sleep(5)
        response <- GET(url, headers)
        contentResponse = content(response, "parsed")
        print(contentResponse$status)
      }
      hidePageSpinner()
      output$exportTreeInventory <- renderPrint(contentResponse$signedUrl)
      rv$ff_tree_inventory <- contentResponse$signedUrl
      loadTreeInventory()
      updateActionButton(session, "loadCustomTreeInventory", disabled = FALSE)
      updateActionButton(session, "createTreeGrid", disabled = FALSE)
    })
    
    # handle loadFFTreeInventory button click
    observeEvent(input$loadFFTreeInventory, {
      rv$ff_tree_inventory <- "tree_inventory_test1.csv"
      loadTreeInventory()
      updateActionButton(session, "loadCustomTreeInventory", disabled = FALSE)
      updateActionButton(session, "createTreeGrid", disabled = FALSE)
    })
    
    # handle loadCustomTreeInventory button click
    observeEvent(input$loadCustomTreeInventory, {
      rv$custom_tree_inventory <- input$customTreeInventoryFile
      loadTreeInventory()
      # set rv$polygon based on rv$custom_polygon
      if (!is.null(rv$custom_polygon)) {
        # get coordinates from rv$custom_polygon which was calculated from: 
        # rv$custom_polygon <- st_as_sf(convex_hull)
        
#        rv$polygon <- geojson_from_coords(rv$custom_polygon$x[[1]][1])
        rv$polygon <- geojson_from_coords(rv$custom_polygon$x[[1]][[1]])
        
        
        
        
        
        
        
#        rv$polygon <- geojson_from_coords(rv$custom_polygon$geometry$coordinates[[1]])
#        rv$polygon <- geojsonsf::sf_geojson(rv$custom_polygon)
      }
      
      
      
      updateActionButton(session, "createDomain", disabled = FALSE)
    })
    
    observe({
      proxy <- leafletProxy("map")
      if (input$showFFTrees) {
        proxy %>% showGroup("ff_circles")
      } else {
        proxy %>% hideGroup("ff_circles")
      }
      if (input$showCustomTrees) {
        proxy %>% showGroup("custom_circles")
      } else {
        proxy %>% hideGroup("custom_circles")
      }
    })
    
    # handle createTreeGrid button click
    observeEvent(input$createTreeGrid, {
      headers <- add_headers(
        accept = "application/json",
        `api-key` = input$api_key,
        `Content-Type` = "application/json"
      )
      url = paste0("https://api.fastfuels.silvxlabs.com/v1/domains/", input$domainId, "/grids/tree")
      response <- POST(url, headers, body = '{"attributes": ["bulkDensity", "fuelMoisture"],
                                              "fuelMoisture": {"source": "uniform", "value": 100}
                                              }'
                       )
      contentResponse = content(response, "parsed")
      showPageSpinner()
      while (contentResponse$status != "completed") {
        Sys.sleep(5)
        response <- GET(url, headers)
        contentResponse = content(response, "parsed")
        print(contentResponse$status)
      }
      hidePageSpinner()
      output$treeGrid <- renderPrint("completed")
      updateActionButton(session, "createSurfaceGrid", disabled = FALSE)
    })
    
    # handle createSurfaceGrid button click
    observeEvent(input$createSurfaceGrid, {
      headers <- add_headers(
        accept = "application/json",
        `api-key` = input$api_key,
        `Content-Type` = "application/json"
      )
      url = paste0("https://api.fastfuels.silvxlabs.com/v1/domains/", input$domainId, "/grids/surface")
      response <- POST(url, headers, body = '{"attributes": ["fuelLoad", "fuelDepth", "fuelMoisture"]}')
      contentResponse = content(response, "parsed")
      showPageSpinner()
      while (contentResponse$status != "completed") {
        Sys.sleep(5)
        response <- GET(url, headers)
        contentResponse = content(response, "parsed")
        print(contentResponse$status)
      }
      hidePageSpinner()
      output$surfaceGrid <- renderPrint("completed")
      updateActionButton(session, "createTopographyGrid", disabled = FALSE)
    })
    
    # handle createTopographyGrid button click
    observeEvent(input$createTopographyGrid, {
      headers <- add_headers(
        accept = "application/json",
        `api-key` = input$api_key,
        `Content-Type` = "application/json"
      )
      url = paste0("https://api.fastfuels.silvxlabs.com/v1/domains/", input$domainId, "/grids/topography")
      response <- POST(url, headers, body = '{"attributes": ["elevation"]}')
      contentResponse = content(response, "parsed")
      showPageSpinner()
      while (contentResponse$status != "completed") {
        Sys.sleep(5)
        response <- GET(url, headers)
        contentResponse = content(response, "parsed")
        print(contentResponse$status)
      }
      hidePageSpinner()
      output$topographyGrid <- renderPrint("completed")
      updateActionButton(session, "getInputFiles", disabled = FALSE)
    })
    
    # handle getInputFiles button click
    observeEvent(input$getInputFiles, {
      headers <- add_headers(
        accept = "application/json",
        `api-key` = input$api_key,
        `Content-Type` = "application/json"
      )
      url = paste0("https://api.fastfuels.silvxlabs.com/v1/domains/", input$domainId, "/grids/exports/QUIC-Fire")
      response <- POST(url, headers)
      contentResponse = content(response, "parsed")
      showPageSpinner()
      while (contentResponse$status != "completed") {
        Sys.sleep(5)
        response <- GET(url, headers)
        contentResponse = content(response, "parsed")
        print(contentResponse$status)
      }
      hidePageSpinner()
      output$inputFiles <- renderPrint(contentResponse$signedUrl)
    })
    
    # handle change to customTreeAttributeChange slider
    observeEvent(input$customTreeAttributeChange, {
      if (!is.null(rv$custom_data)) {
        rv$custom_data[[input$customTreeAttribute]] <- rv$custom_data[[input$customTreeAttribute]] * (1 + input$customTreeAttributeChange/100)
        # update the map
        proxy <- leafletProxy("map")
        proxy %>% clearGroup("custom_circles")
        proxy %>% addCircles(
          data = rv$custom_data,
          ~Longitude, ~Latitude,
          color = "limegreen", radius = ~DIA/10.0, opacity = .7,
          fillColor = "limegreen", fillOpacity = .5,
          group="custom_circles"
        )
        
        # update the histogram
        output$attributeHist <- renderPlot({
          hist(rv$custom_data[[input$customTreeAttribute]], col = "limegreen", border = "white",
               xlab = input$customTreeAttribute, main = paste("Histogram of", input$customTreeAttribute))
        })
      }
    })
    
    
    ####################### HELPER FUNCTIONS #######################
    
    getData <- function(source) {
      csv_data <- read.csv(source)
      sf_data <- st_as_sf(csv_data, coords = c("X", "Y"), crs = 32610)  # UTM Zone 10N

      # Transform to WGS 84 (latitude and longitude)
      sf_data_wgs84 <- st_transform(sf_data, crs = 4326)

      # Extract latitude and longitude
      csv_data$Longitude <- st_coordinates(sf_data_wgs84)[, 1]
      csv_data$Latitude <- st_coordinates(sf_data_wgs84)[, 2]

      # get bounding polygon of all the Lat/Long points
      convex_hull <- st_convex_hull(st_union(sf_data_wgs84))
      bounding_polygon <- st_as_sf(convex_hull)
      
      return(list(csv_df = csv_data, polygon = bounding_polygon))
    }
    
    # get bounding rectangle of the bounding polygon
    getBoundingRect <- function(bounding_polygon) {
      bounding_rect <- st_bbox(bounding_polygon)
      rect_coords <- matrix(c(bounding_rect["xmin"], bounding_rect["ymin"],
                              bounding_rect["xmin"], bounding_rect["ymax"],
                              bounding_rect["xmax"], bounding_rect["ymax"],
                              bounding_rect["xmax"], bounding_rect["ymin"],
                              bounding_rect["xmin"], bounding_rect["ymin"]), 
                            ncol = 2, byrow = TRUE)
      bounding_rect_polygon <- st_polygon(list(rect_coords))
      return(bounding_rect_polygon)
    }

    loadTreeInventory <- function() {
      if(!is.null(rv$ff_tree_inventory) && is.null(rv$custom_tree_inventory)) {
        # we only have FastFuels (ff) tree inventory
        # show map with ff tree inventory and bounding box
        if (is.null(rv$ff_data)) {
          res <- getData(rv$ff_tree_inventory)
          rv$ff_data = res$csv_df
          rv$ff_polygon = res$polygon
        }
        bounding_polygon = rv$ff_polygon
        bounding_rect_polygon_ff = getBoundingRect(bounding_polygon)
        map <- leaflet() %>%
          addProviderTiles("Esri.WorldImagery") %>%
#          setView(-120.15, 48.42, zoom = 12) %>%
          addDrawToolbar(
            targetGroup = "drawn",
            polylineOptions = FALSE,
            polygonOptions = drawPolygonOptions(showArea = FALSE),
            #polygonOptions = drawPolygonOptions(showArea = TRUE),  # this doesn't work (labels do not appear)
            circleOptions = FALSE,
            rectangleOptions = FALSE,
            markerOptions = FALSE,
            circleMarkerOptions = FALSE
            # editOptions = editToolbarOptions()
          ) %>%
          addCircles(
            data = rv$ff_data,
            ~Longitude, ~Latitude,
            color = "blue", radius = ~DIA/10.0, opacity = .7,
            fillColor = "blue", fillOpacity = .5,
            group="ff_circles"
          ) %>%
          addPolygons(data = bounding_rect_polygon_ff, group="outline_ff", fill=FALSE, color="blue", weight=2)
      }
      else if(is.null(rv$ff_tree_inventory) && !is.null(rv$custom_tree_inventory)) {
        # we only have custom tree inventory
        # show map with custom tree inventory
        if (is.null(rv$custom_data)) {
          res <- getData(rv$custom_tree_inventory)
          rv$custom_data = res$csv_df
          rv$custom_polygon = res$polygon
          # populate the custom tree attribute dropdown with the column names
          updateSelectInput(session, "customTreeAttribute", choices = colnames(rv$custom_data))
        }
        bounding_polygon = rv$custom_polygon
        
        map <- leaflet() %>%
          addProviderTiles("Esri.WorldImagery") %>%
          setView(-120.15, 48.42, zoom = 12) %>%
          addDrawToolbar(
            targetGroup = "drawn",
            polylineOptions = FALSE,
            polygonOptions = drawPolygonOptions(showArea = FALSE),
            #polygonOptions = drawPolygonOptions(showArea = TRUE),  # this doesn't work (labels do not appear)
            circleOptions = FALSE,
            rectangleOptions = FALSE,
            markerOptions = FALSE,
            circleMarkerOptions = FALSE
            # editOptions = editToolbarOptions()
          ) %>%
          addCircles(
            data = rv$custom_data,
            ~Longitude, ~Latitude,
            color = "limegreen", radius = ~DIA/10.0, opacity = .7,
            fillColor = "limegreen", fillOpacity = .5,
            group="custom_circles"
          ) %>%
          addPolygons(data = bounding_polygon, group="outline_custom", fill=FALSE, color="limegreen", weight=2)
        }
      else {
        # we have both ff and custom tree inventories
        # show map with both tree inventories and bounding box
        if (is.null(rv$ff_data)) {
          res <- getData(rv$ff_tree_inventory)
          rv$ff_data = res$csv_df
          rv$ff_polygon = res$polygon
        }
        if (is.null(rv$custom_data)) {
          res <- getData(rv$custom_tree_inventory)
          rv$custom_data = res$csv_df
          rv$custom_polygon = res$polygon
        }
        bounding_polygon = rv$ff_polygon
        bounding_rect_polygon_ff = getBoundingRect(bounding_polygon)

        bounding_polygon_custom = rv$custom_polygon
        map <- leaflet() %>%
          addProviderTiles("Esri.WorldImagery") %>%
          setView(-120.15, 48.42, zoom = 12) %>%
          addDrawToolbar(
            targetGroup = "drawn",
            polylineOptions = FALSE,
            polygonOptions = drawPolygonOptions(showArea = FALSE),
            #polygonOptions = drawPolygonOptions(showArea = TRUE),  # this doesn't work (labels do not appear)
            circleOptions = FALSE,
            rectangleOptions = FALSE,
            markerOptions = FALSE,
            circleMarkerOptions = FALSE
            # editOptions = editToolbarOptions()
          ) %>%
          addCircles(
            data = rv$ff_data,
            ~Longitude, ~Latitude,
            color = "blue", radius = ~DIA/10.0, opacity = .7,
            fillColor = "blue", fillOpacity = .5,
            group="ff_circles"
          ) %>%
          addPolygons(data = bounding_rect_polygon_ff, group="outline_ff", fill=FALSE, color="blue", weight=2) %>%
          addCircles(
            data = rv$custom_data,
            ~Longitude, ~Latitude,
            color = "limegreen", radius = ~DIA/10.0, opacity = .7,
            fillColor = "limegreen", fillOpacity = .5,
            group="custom_circles"
          ) %>%
          addPolygons(data = bounding_polygon_custom, group="outline_custom", fill=FALSE, color="cyan", weight=2)
      }
      # Display the map
      output$map <- renderLeaflet({map})
    }
    
    
    geojson_from_coords <- function(coords) {
      geojson_list <- list(
        type = "FeatureCollection",
        features = list(
          list(
            type = "Feature",
            properties = setNames(list(), character(0)),
            geometry = list(
              type = "Polygon",
              coordinates = list(coords)
            )
          )
        )
      )
      return(toJSON(geojson_list, auto_unbox = TRUE))
    }
    # loadCustomTreeInventory <- function() {
    #   custom_data <- read.csv(rv$custom_tree_inventory)
    #   sf_data <- st_as_sf(custom_data, coords = c("X", "Y"), crs = 32610)  # UTM Zone 10N
    #   sf_data_wgs84 <- st_transform(sf_data, crs = 4326)
    #   custom_data$Longitude <- st_coordinates(sf_data_wgs84)[, 1]
    #   custom_data$Latitude <- st_coordinates(sf_data_wgs84)[, 2]
    #   
    #   map <- leaflet() %>%
    #     addProviderTiles("Esri.WorldImagery") %>%
    #     setView(-120.15, 48.42, zoom = 12) %>%
    #     addDrawToolbar(
    #       targetGroup = "drawn",
    #       polylineOptions = FALSE,
    #       polygonOptions = drawPolygonOptions(showArea = FALSE),
    #       circleOptions = FALSE,
    #       rectangleOptions = FALSE,
    #       markerOptions = FALSE,
    #       circleMarkerOptions = FALSE
    #     ) %>%
    #     addCircles(
    #       data = custom_data,
    #       ~Longitude, ~Latitude,
    #       color = "green", radius = ~DIA/10.0, opacity = .7,
    #       fillColor = "green", fillOpacity = .5,
    #       group="custom_circles"
    #     )
    # 
    #   # Display the map
    #   output$map <- renderLeaflet({map})
    # }
}
    

