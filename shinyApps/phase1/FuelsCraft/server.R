#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#



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
    rv$ff_data2 <- NULL # after add/remove trees by polygon to ff_data
    rv$ff_data_temp <- NULL   # after slider filters applied to ff_data2
    rv$custom_data <- NULL
    rv$custom_data2 <- NULL # after add/remove trees by polygon to custom_data
    rv$custom_data_temp <- NULL
    rv$ff_max_HT <- NULL
    rv$custom_max_HT <- NULL
    rv$ff_hist_ylim <- NULL
    rv$ff_breaks <- NULL
    rv$custom_hist_ylim <- NULL
    rv$custom_breaks <- NULL
  
    drawEmptyMap <- function() {
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
          circleMarkerOptions = FALSE,
          editOptions = editToolbarOptions()
        )
  #    return(map)
      renderLeaflet(map)
    }
    
    output$map <- drawEmptyMap()
#  output$map <- renderLeaflet({drawEmptyMap()})
  
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
      rv$ff_data2 <- NULL
      rv$ff_data_temp <- NULL
      rv$custom_data <- NULL
      rv$custom_data2 <- NULL
      rv$custom_data_temp <- NULL
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
      output$map <- drawEmptyMap()
    })
    
    # handle cropCustomPolygon button click
    observeEvent(input$cropCustomPolygon, {
      if(is.null(rv$polygon)) {
        return(NULL)
      }
      
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
      rv$custom_data2 <- rv$custom_data
      
      # update the map
      loadTreeInventory()
    })
    
    observeEvent(input$removeFFTreesByPolygon, {
      if(is.null(rv$polygon)) {
        return(NULL)
      }
      sf_object <- geojsonsf::geojson_sf(rv$polygon)
      
      # remove trees that are in rv$polygon from rv$ff_data
      # Convert ff_data to sf object
      ff_data_sf <- st_as_sf(rv$ff_data2, coords = c("Longitude", "Latitude"), crs = 4326)
      
      # Check if points are within the polygon
      within_polygon <- sapply(st_within(ff_data_sf, sf_object), length) > 0
      
      # Subset the dataframe using the logical vector
      rv$ff_data2 <- rv$ff_data2[!within_polygon, ]

      updateMapFF()
    })
    
    observeEvent(input$removeCustomTreesByPolygon, {
      if(is.null(rv$polygon)) {
        return(NULL)
      }
      sf_object <- geojsonsf::geojson_sf(rv$polygon)
      
      # remove trees that are in rv$polygon from rv$custom_data
      # Convert custom_data to sf object
      custom_data_sf <- st_as_sf(rv$custom_data2, coords = c("Longitude", "Latitude"), crs = 4326)
      
      # Check if points are within the polygon
      within_polygon <- sapply(st_within(custom_data_sf, sf_object), length) > 0
      
      # Subset the dataframe using the logical vector
      rv$custom_data2 <- rv$custom_data2[!within_polygon, ]
      
      updateMapCustom()
    })
    
    observeEvent(input$addFFTreesByPolygon, {
      if(is.null(rv$polygon)) {
        return(NULL)
      }
      # get a sample trees from existing rv$ff_data2
      new_trees <- rv$ff_data2[sample(nrow(rv$ff_data2), input$ffTreeCountToAdd), ]
      
      # Generate random points within the polygon
      sf_object <- geojsonsf::geojson_sf(rv$polygon)
      sample_points <- st_sample(sf_object, size = as.numeric(input$ffTreeCountToAdd), type = "random")
      
      # Convert sample points to a data frame with Longitude and Latitude
      coords <- st_coordinates(sample_points)
#      new_trees <- data.frame(Longitude = coords[, 1], Latitude = coords[, 2], new_trees)
      new_trees$Longitude <- coords[, 1]
      new_trees$Latitude <- coords[, 2]

      # change the Longitude and Latitude of the new trees to be within the polygon
      # Convert the polygon to a bounding box
      # sf_object <- geojsonsf::geojson_sf(rv$polygon)
      # bounding_box <- st_bbox(sf_object)
      # new_trees$Longitude <- runif(nrow(new_trees), bounding_box["xmin"], bounding_box["xmax"])
      # new_trees$Latitude <- runif(nrow(new_trees), bounding_box["ymin"], bounding_box["ymax"])

      # Add new trees to rv$ff_data2
      rv$ff_data2 <- rbind(rv$ff_data2, new_trees)
      
      updateMapFF()
    })

    observeEvent(input$addCustomTreesByPolygon, {
      if(is.null(rv$polygon)) {
        return(NULL)
      }
      # get a sample trees from existing rv$ff_data2
      new_trees <- rv$custom_data2[sample(nrow(rv$custom_data2), input$customTreeCountToAdd), ]
      
      # Generate random points within the polygon
      sf_object <- geojsonsf::geojson_sf(rv$polygon)
      sample_points <- st_sample(sf_object, size = as.numeric(input$customTreeCountToAdd), type = "random")
      
      # Convert sample points to a data frame with Longitude and Latitude
      coords <- st_coordinates(sample_points)
      new_trees$Longitude <- coords[, 1]
      new_trees$Latitude <- coords[, 2]
      
      # Add new trees to rv$custom_data2
      rv$custom_data2 <- rbind(rv$custom_data2, new_trees)
      
      updateMapCustom()
    })
    
    observeEvent(input$mergeInventories, {
      # merge rv$ff_data2 and rv$custom_data2
      merged_data <- rbind(rv$ff_data_temp, rv$custom_data_temp)
      
      # merge or replace trees inside custom polygon? 
      # I guess merge is ok since user can remove trees from ff_data. 
      
      rv$ff_data <- merged_data
      rv$ff_data2 <- merged_data
      rv$ff_data_temp <- merged_data
      rv$custom_tree_inventory <- NULL
      rv$custom_data <- NULL
      rv$custom_data2 <- NULL
      rv$custom_data_temp <- NULL
      
      # update the map
      loadTreeInventory()
    })
    
    observeEvent(input$uploadInventory, {
      if(is.null(rv$ff_data_temp)) {
        return(NULL)
      }
      
      # save the merged tree inventory to a file with a timestamp
      timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
      fn = paste0("tree_inventory_uploaded_", timestamp, ".csv")
      write.csv(rv$ff_data_temp, fn, row.names = FALSE)
      
      headers <- add_headers(
        accept = "application/json",
        `api-key` = input$api_key
      )
      url = paste0("https://api.fastfuels.silvxlabs.com/v1/domains/", input$domainId, "/inventories/tree/upload")
      
      files = list()
      files <- list(uploadFile = upload_file(fn, type = "text/csv"))
      response <- POST(
        url, headers,
        body = list(uploadFile = upload_file(fn)),
        encode = "multipart"
      )
      print(response)
      print(content(response, "text"))
    })
    
    observeEvent(input$saveInventory, {
      if(is.null(rv$ff_data_temp)) {
        return(NULL)
      }
      
      # save the merged tree inventory to a file with a timestamp
      timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
      write.csv(rv$ff_data_temp, paste0("tree_inventory_", input$domainId, "_", timestamp, ".csv"), row.names = FALSE)
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
      
      # see if contentResponse$detail exists, assume error... "API key has expired"
      if("detail" %in% names(contentResponse))
      {
        updateTextInput(session, "domainId", value = contentResponse$detail)
        return(NULL)
      }
      
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
      
      bodyContent = paste0('{"sources": ["TreeMap"], "TreeMap": {"version": "', input$treeMapVersion, '"}, "featureMasks": ["road", "water"]}')
      if(input$treeMapStyle == "raw") {
        # do nothing
      } else if (input$treeMapStyle == "fusion") {
        bodyContent = paste0('{"sources": ["TreeMap"], "TreeMap": {
                                              "version": "', input$treeMapVersion, '", "seed":123456789, 
                                              "canopyHeightMapConfiguration": {"source":"Meta2024"}}, 
                                              "featureMasks": ["road", "water"]
                                              }')
      }
      
#      response <- POST(url, headers, body = '{"sources": ["TreeMap"], "featureMasks": ["road", "water"]}')
      # response <- POST(url, headers, body = '{"sources": ["TreeMap"], 
      #                                         "TreeMap": {
      #                                         "version":"2014", 
      #                                         "seed":123456789, 
      #                                         "canopyHeightMapConfiguration": {"source":"Meta2024"}
      #                                         } 
      #                                         }')
      
      response <- POST(url, headers, body = bodyContent)
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
        
        rv$polygon <- geojson_from_coords(rv$custom_polygon$x[[1]][[1]])
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
      # default to zip (QUIC-Fire) input format
      url = paste0("https://api.fastfuels.silvxlabs.com/v1/domains/", input$domainId, "/grids/exports/QUIC-Fire")
      if (input$qfInputFormat == "zarr") {
        url = paste0("https://api.fastfuels.silvxlabs.com/v1/domains/", input$domainId, "/grids/exports/zarr")
      }
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
    
    # handle change to ffTreeHeight slider
    observeEvent(input$ffTreeHeight, {
        updateMapFF()
    })
    
    # handle change to customTreeHeight slider
    observeEvent(input$customTreeHeight, {
        updateMapCustom()
    })
    
    # resetFFTreeInventory
    observeEvent(input$resetFFTreeInventory, {
      rv$ff_data2 <- rv$ff_data
      rv$ff_data_temp <- rv$ff_data2
      if(!is.null(rv$ff_max_HT))
      {
        updateSliderInput(session, "ffTreeHeight", value = c(0, ceiling(rv$ff_max_HT)))
      }
    })
    
    # resetCustomTreeInventory
    observeEvent(input$resetCustomTreeInventory, {
      rv$custom_data2 <- rv$custom_data
      rv$custom_data_temp <- rv$custom_data
      if(!is.null(rv$custom_max_HT))
      {
        updateSliderInput(session, "customTreeHeight", value = c(0, ceiling(rv$custom_max_HT)))
      }
    })
    
    observeEvent(input$create3DVoxelPlot, {
      # download zarr.zip file from input$inputFiles url
      # unzip it to a temp directory
      # read the zarr file and create a 3D voxel plot
      # create a temp directory
      temp_dir <- tempdir()
      # download the xxx.zarr.zip file
#      download.file(input$inputFiles, file.path(temp_dir, "zarr.zip"))
      # unzip the file
      unzip("/Users/briandrye/repos/uw/fuels-craft/fuels-craft/shinyApps/phase1/FuelsCraft/grids.zarr.zip", exdir = temp_dir)
#      unzip(file.path(temp_dir, "....zarr.zip"), exdir = temp_dir)
      # read the zarr file
      zarr_file <- list.files(temp_dir, pattern = "\\.zarr$", full.names = TRUE)
      # read the zarr file using stars
      

      
      # Load the data
      file_name <- "/Users/briandrye/repos/uw/fuels-craft/fuels-craft/shinyApps/phase1/FuelsCraft/grids.zarr.zip"
      # check if file exists
      if (!file.exists(file_name)) {
        stop("File does not exist")
      }
      
      unzip("/Users/briandrye/repos/uw/fuels-craft/fuels-craft/shinyApps/phase1/FuelsCraft/grids.zarr.zip", exdir = "/Users/briandrye/repos/uw/fuels-craft/fuels-craft/shinyApps/phase1/FuelsCraft/")
      
      
      # Define the path to the unzipped Zarr directory
      unzipped_dir <- "/Users/briandrye/repos/uw/fuels-craft/fuels-craft/shinyApps/phase1/FuelsCraft/tree"
      
      # Read the Zarr data
      zarr_data <- read_stars(unzipped_dir, sub = "bulkDensity")
      
      # Convert the stars object to an array
      array_data <- as.array(zarr_data)
      
      # Create a grid
      dimensions <- dim(array_data) + 1  # Add 1 to dimensions
      origin <- c(0, 0, 0)
      spacing <- c(2, 2, 2)
      
      # Flatten the array in Fortran order (column-major)
      array_data_flat <- as.vector(aperm(array_data, c(3, 2, 1)))
      
      # Create a data frame for visualization
      df <- expand.grid(x = 1:dimensions[1], y = 1:dimensions[2], z = 1:dimensions[3])
      df$bulk_density <- array_data_flat
      
      # Threshold the data to focus on areas with significant biomass
      threshold <- 0.01  # Adjust this value based on your data
      df <- df[df$bulk_density > threshold, ]
      
      # Plot the data
      # p <- plot3d(df$x, df$y, df$z, col = heat.colors(length(df$bulk_density))[rank(df$bulk_density)], size = 3)
      # axes3d()
      # title3d("X", "Y", "Z", "Bulk Density (kg/m3)")

      
            
      # Create a 3D plot using plotly
      # p <- plot_ly(data = df, x = ~x, y = ~y, z = ~z, color = ~bulk_density, colors = "Viridis") %>%
      #   add_markers() %>%
      #   layout(scene = list(xaxis = list(title = "X"),
      #                       yaxis = list(title = "Y"),
      #                       zaxis = list(title = "Z")),
      #          title = "3D Voxel Plot of Bulk Density")
      
      output$voxel3dPlot <- renderPlot({
        plot3d(df$x, df$y, df$z, col = heat.colors(length(df$bulk_density))[rank(df$bulk_density)], size = 3)
        axes3d()
        title3d("X", "Y", "Z", "Bulk Density (kg/m3)")
      })
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
    
    setFFTreeInventory <- function() {
      res <- getData(rv$ff_tree_inventory)
      rv$ff_data = res$csv_df
      # remove rows that have NA in the HT column
      rv$ff_data <- rv$ff_data[!is.na(rv$ff_data$HT), ]
      rv$ff_data2 <- rv$ff_data
      # print how many rows were removed
      print(paste0("FF Inventory: removed ", nrow(res$csv_df) - nrow(rv$ff_data), " rows with NA in HT column"))
      rv$ff_data_temp <- rv$ff_data
      rv$ff_polygon = res$polygon
      rv$ff_max_HT = max(rv$ff_data$HT, na.rm = TRUE)
    }
    setCustomTreeInventory <- function() {
      res <- getData(rv$custom_tree_inventory)
      rv$custom_data = res$csv_df
      # remove rows that have NA in the HT column
      rv$custom_data <- rv$custom_data[!is.na(rv$custom_data$HT), ]
      rv$custom_data2 <- rv$custom_data
      # print how many rows were removed
      print(paste0("Custom Inventory: removed ", nrow(res$csv_df) - nrow(rv$custom_data), " rows with NA in HT column"))
      rv$custom_data_temp <- rv$custom_data
      rv$custom_polygon = res$polygon
      rv$custom_max_HT = max(rv$custom_data$HT, na.rm = TRUE)
    }

    clearMapFF <- function() {
      proxy <- leafletProxy("map")
      proxy %>% clearGroup("ff_circles")
      proxy %>% clearGroup("outline_ff")
    }

    clearMapCustom <- function() {
      proxy <- leafletProxy("map")
      proxy %>% clearGroup("custom_circles")
      proxy %>% clearGroup("outline_custom")
    } 
    
    loadTreeInventory <- function() {
      clearMapFF()
      clearMapCustom()
      
      if(!is.null(rv$ff_tree_inventory)) {
        initializeFFHeightSlider = FALSE
        if (is.null(rv$ff_data)) {
          setFFTreeInventory()
          initializeFFHeightSlider = TRUE
        }
        bounding_polygon = rv$ff_polygon
#        bounding_rect_polygon_ff = getBoundingRect(bounding_polygon)
        proxy <- leafletProxy("map")
#        proxy %>% addPolygons(data = bounding_rect_polygon_ff, group="outline_ff", fill=FALSE, color="blue", weight=2)
        proxy %>% addPolygons(data = bounding_polygon, group="outline_ff", fill=FALSE, color="blue", weight=2)
        updateMapFF()
        if(initializeFFHeightSlider) {
          updateSliderInput(session, "ffTreeHeight", min = 0, value = c(0, ceiling(rv$ff_max_HT)), max=ceiling(rv$ff_max_HT))
        }
      }
      if(!is.null(rv$custom_tree_inventory)) {
        initializeCustomHeightSlider = FALSE
        if (is.null(rv$custom_data)) {
          setCustomTreeInventory()
          initializeCustomHeightSlider = TRUE
          # populate the custom tree attribute dropdown with the column names
#          updateSelectInput(session, "customTreeAttribute", choices = colnames(rv$custom_data))
        }
        bounding_polygon = rv$custom_polygon
        proxy <- leafletProxy("map")
        proxy %>% addPolygons(data = bounding_polygon, group="outline_custom", fill=FALSE, color="limegreen", weight=2)
        updateMapCustom()
        if(initializeCustomHeightSlider) {
          updateSliderInput(session, "customTreeHeight", min = 0, value = c(0, ceiling(rv$custom_max_HT)), max=ceiling(rv$custom_max_HT))
        }
      }
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
    
    updateMapFF <- function() {
      # use rv$ff_data2... 
      # apply HT slider filter to it
      if(!is.null(input$ffTreeHeight))
      {
        rv$ff_data_temp <- rv$ff_data2[rv$ff_data2$HT >= input$ffTreeHeight[1] & rv$ff_data2$HT <= input$ffTreeHeight[2], ]
      }  
      if (!is.null(rv$ff_data_temp)) {
        currentColor = brewer.pal(9, "Blues")
        if(!identical(rv$ff_data_temp, rv$ff_data)) {
          currentColor = brewer.pal(9, "Greens")
        }
        
        palette <- colorNumeric(
          palette = currentColor,
          domain = c(0, ceiling(rv$ff_max_HT))
        )

        # update the map
        proxy <- leafletProxy("map")
#        proxy %>% clearGroup("drawn")    # doesn't seem to work
        proxy %>% clearGroup("ff_circles")
        proxy %>% addCircles(
          data = rv$ff_data_temp,
          ~Longitude, ~Latitude,
          color = ~palette(rv$ff_data_temp$HT), 
          radius = 3, opacity = .7,
          group="ff_circles"
        )
        
        # update the histogram
        output$ffHeightHist <- renderPlot({
          if(is.null(rv$ff_data_temp$HT)){
            return(NULL)
          }
          hist_obj <- hist(rv$ff_data_temp$HT, breaks=9, plot = FALSE)
          if(is.null(rv$ff_hist_ylim)){
            rv$ff_hist_ylim <- c(0, max(hist_obj$counts) * 1.1) 
            rv$ff_breaks <-hist_obj$breaks
          }
          hist(rv$ff_data_temp$HT, col = currentColor, border = "grey",
               breaks = rv$ff_breaks, ylim = rv$ff_hist_ylim, xlim = c(0, ceiling(rv$ff_max_HT)),
               xlab = "Tree Heights (HT)", main = paste0("Tree Count: ", length(rv$ff_data_temp$HT)))
        })
      }
    }
    
    updateMapCustom <- function() {
      # use rv$custom_data2... 
      # apply HT slider filter to it
      if(!is.null(input$customTreeHeight))
      {
        rv$custom_data_temp <- rv$custom_data2[rv$custom_data2$HT >= input$customTreeHeight[1] & rv$custom_data2$HT <= input$customTreeHeight[2], ]
      }

      if (!is.null(rv$custom_data_temp)) {
        currentColor = brewer.pal(9, "Reds")
        if(!identical(rv$custom_data_temp, rv$custom_data)) {
          currentColor = brewer.pal(9, "Oranges")
        }
        
        palette <- colorNumeric(
          palette = currentColor,
          domain = c(0, ceiling(rv$custom_max_HT))
        )
        
        # update the map
        proxy <- leafletProxy("map")
#        proxy %>% clearGroup("drawn")
        proxy %>% clearGroup("custom_circles")
        proxy %>% addCircles(
          data = rv$custom_data_temp,
          ~Longitude, ~Latitude,
          color = ~palette(rv$custom_data_temp$HT), 
          radius = 3, opacity = .7,
          group="custom_circles"
        )
        
        # update the histogram
        output$customHeightHist <- renderPlot({
          if(is.null(rv$custom_data_temp$HT)){
            return(NULL)
          }
          hist_obj <- hist(rv$custom_data_temp$HT, breaks=9, plot = FALSE)
          if(is.null(rv$custom_hist_ylim)){
            rv$custom_hist_ylim <- c(0, max(hist_obj$counts) * 1.1) 
            rv$custom_breaks <-hist_obj$breaks
          }
          hist(rv$custom_data_temp$HT, col = currentColor, border = "grey",
               breaks = rv$custom_breaks, ylim = rv$custom_hist_ylim, xlim = c(0, ceiling(rv$custom_max_HT)),
               xlab = "Tree Heights (HT)", main = paste0("Tree Count: ", length(rv$custom_data_temp$HT)))
        })
      }
    }

}
    

