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
    rv$enclosing_raster <- NULL

    mainPolygonColor = "#33ffff"

    # Create an empty sf table
    make_understory_sf <- function() {
      st_sf(data.frame(
        strata = character(0),
        fuelbedTab = character(0),
        fuelbed = integer(0),
        poly_id = character(0),
        stringsAsFactors = FALSE
      ), geometry = st_sfc(), crs = 4326)
    }

    rv$understory_sf <- make_understory_sf()

    # Add polygon row
    add_polygon_row <- function(tbl, strata, fuelbedTab, fuelbed = 1, polygon_sf, poly_id = NULL) {
      stopifnot(inherits(polygon_sf, "sf") || inherits(polygon_sf, "sfc"))
      if (inherits(polygon_sf, "sf")) geom <- st_geometry(polygon_sf)[[1]] else geom <- polygon_sf[[1]]
      if (is.null(poly_id)) poly_id <- paste0(strata, "_fb", fuelbed, "_", as.integer(Sys.time()))
      newrow <- st_sf(data.frame(strata = strata, fuelbedTab=fuelbedTab, fuelbed = as.integer(fuelbed), poly_id = as.character(poly_id),
                                 stringsAsFactors = FALSE),
                      geometry = st_sfc(geom), crs = st_crs(polygon_sf))
      bind_rows(tbl, newrow)
    }

    # Query helper: get all polygons for a strata and fuelbed
    get_fuelbed_sf <- function(tbl, strata, fuelbed = 1) {
      filter(tbl, strata == !!strata, fuelbed == !!as.integer(fuelbed))
    }


    drawEmptyMap <- function(editButton = FALSE, removeButton = FALSE) {
      map <- leaflet() %>%
        addProviderTiles("Esri.WorldImagery") %>%
        setView(-120.15, 48.42, zoom = 12) %>%   # Winthrop
#        setView(-81.73, 32.06, zoom = 12) %>%    # Fort Stewart
#        setView(-81.708, 31.896, zoom = 12) %>%    # Fort Stewart
        # attach JS that finds map2 and syncs both ways
        htmlwidgets::onRender("
        function(el, x) {
          var map1 = this;
          // find the other map by its output id
          var map2Widget = HTMLWidgets.find('#subMap');
          if(!map2Widget) {
            // maybe map2 not ready yet; try again shortly
            setTimeout(function(){
              var m2w = HTMLWidgets.find('#subMap');
              if(m2w) { map1.sync(m2w.getMap()); m2w.getMap().sync(map1); }
            }, 250);
          } else {
            var map2 = map2Widget.getMap();
            map1.sync(map2);
            map2.sync(map1);
          }
        }
      ") %>%
        addDrawToolbar(
#          targetGroup = "drawn",
          polylineOptions = FALSE,
#          polygonOptions = drawPolygonOptions(showArea = FALSE),
          polygonOptions = drawPolygonOptions(
            shapeOptions = drawShapeOptions(
              color = mainPolygonColor,    # stroke
              weight = 2,
              fill = FALSE,         # disable fill
              fillOpacity = 0       # extra safety
            ),
          ),
          #polygonOptions = drawPolygonOptions(showArea = TRUE),  # this doesn't work (labels do not appear)
          circleOptions = FALSE,
          rectangleOptions = FALSE,
          markerOptions = FALSE,
          circleMarkerOptions = FALSE,
          editOptions = editToolbarOptions(edit=editButton, remove=removeButton)
        )
      renderLeaflet(map)
    }

    output$map <- drawEmptyMap(editButton=TRUE, removeButton=TRUE)
    output$subMap <- drawEmptyMap(editButton=FALSE, removeButton=FALSE)

    observeEvent(input$subMap_draw_new_feature, {
      feature <- input$subMap_draw_new_feature

      leaflet_id <- feature$properties$`_leaflet_id`

      # Convert to sf
      drawn_sf <- st_read(
        jsonlite::toJSON(feature, auto_unbox = TRUE),
        quiet = TRUE
      )

      # Send message to remove
      session$sendCustomMessage("removeDrawLayer", leaflet_id)

      # Small delay before adding the managed version
      Sys.sleep(0.15)

      leafletProxy("subMap") %>%
        addPolygons(
          data = drawn_sf,
          group = "understory_polygons",
          layerId = paste0("managed_", leaflet_id),
          color = "green",
          weight = 2,
          fill = FALSE,
          fillOpacity = 0,
          highlightOptions = highlightOptions(
            weight = 4,
            color = "red",
            bringToFront = TRUE
          )
        )

      coords <- feature$geometry$coordinates[[1]]

      polygonId = feature$properties$`_leaflet_id`

      geom_json <- jsonlite::toJSON(feature$geometry, auto_unbox = TRUE)
      sfc <- st_as_sfc(as.character(geom_json), GeoJSON = TRUE)   # returns an sfc object
      sfc <- st_set_crs(sfc, 4326)                  # GeoJSON coordinates are lon/lat
      sf_row <- st_sf(geometry = sfc)

      # add to rv$understory_sf. Get strata from input$understory selectInput
      strata <- input$understory
      # get the selected fuelbed tab panel, then get the fuelbedList selectInput value
      fuelbedSelectId <- input[[paste0(strata, "_tabs")]]
      fuelbed <- input[[fuelbedSelectId]]

      rv$understory_sf <- add_polygon_row(rv$understory_sf, strata, fuelbedSelectId, fuelbed, sf_row, polygonId)
    })

    # Observe polygon clicks
    observeEvent(input$subMap_shape_click, {
      clicked <- input$subMap_shape_click

      if (!is.null(clicked) && grepl("^managed_", clicked$id)) {
        # Extract original leaflet_id
        leaflet_id <- as.numeric(gsub("^managed_", "", clicked$id))

        # Show confirmation modal
        showModal(modalDialog(
          title = "Delete Polygon? ",
          "Are you sure you want to delete this polygon?",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_delete", "Delete", class = "btn-danger")
          )
        ))

        # Store the ID to delete
        rv$polygon_to_delete <- leaflet_id
      }
    })

    observeEvent(input$confirm_delete, {
      leaflet_id <- rv$polygon_to_delete

      # Remove from map
      leafletProxy("subMap") %>%
        removeShape(layerId = paste0("managed_", leaflet_id))

      # Remove from data
      rv$understory_sf <- rv$understory_sf[rv$understory_sf$poly_id != leaflet_id, ]

      removeModal()
      rv$polygon_to_delete <- NULL
    })

    observeEvent(input$subMap_draw_deleted_features, {
      deleted <- input$subMap_draw_deleted_features

      if (!is.null(deleted) && length(deleted$features) > 0) {
        for (feature in deleted$features) {
          leaflet_id <- feature$properties$`_leaflet_id`

          # Remove the managed polygon from the map
          leafletProxy("subMap") %>%
            removeShape(layerId = paste0("managed_", leaflet_id))

          # Remove from rv$understory_sf
          rv$understory_sf <- rv$understory_sf[rv$understory_sf$poly_id != leaflet_id, ]
        }
      }
      print("After deletion")
    })

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
        # get list of suggested fuelbeds from rlandfire and populate fuelbedList
        #      suggested_fuelbeds <- getSuggestedFuelbeds(sf_object)
        suggested_fuelbeds <- getSuggestedFuelbeds2(sf_object)

        # create a named list for updateSelectInput
        namedChoices <- setNames(suggested_fuelbeds$Value, paste(suggested_fuelbeds$Value, suggested_fuelbeds$FUELBED))

        updateSelectInput(session, "shrub1_1fuelbedList", choices = namedChoices)
        updateSelectInput(session, "shrub1_2fuelbedList", choices = namedChoices)
        updateSelectInput(session, "shrub2_1fuelbedList", choices = namedChoices)
        updateSelectInput(session, "shrub2_2fuelbedList", choices = namedChoices)
        updateSelectInput(session, "herb1_1fuelbedList", choices = namedChoices)
        updateSelectInput(session, "herb1_2fuelbedList", choices = namedChoices)
      } else {
        outputString <- paste0("area: ", area, " (too large, must be < 16 km^2)")
        output$area <- renderPrint({outputString})
        updateActionButton(session, "createDomain", disabled = TRUE)
      }
    })

    observeEvent(input$Shrubs1_tabs, {
      message("User selected tab: ", input$Shrubs1_tabs)

      # remove existing polgons from subMap
      proxy <- leafletProxy("subMap")
      proxy %>% clearGroup("understory_polygons")

      # get polygons from rv$understory that have fuelbedTab == "shrub1_1fuelbedList"
      fb_polygons <- rv$understory_sf %>% filter(fuelbedTab == input$Shrubs1_tabs)
      if(nrow(fb_polygons) == 0) {
        return(NULL)
      }

      # add polygons to subMap
      for(i in 1:nrow(fb_polygons)) {
        proxy %>% addPolygons(data = fb_polygons[i, ],
                              color = "blue",
                              weight = 2,
                              fill = FALSE,
                              layerId = paste0("managed_", fb_polygons[i,]$poly_id),
                              group = "understory_polygons",
                              highlightOptions = highlightOptions(
                                weight = 4,
                                color = "red",
                                bringToFront = TRUE
                              )
        )
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

    observeEvent(input$zone_crs, {
      if(is.null(rv$polygon)) {
        return(NULL)
      }
      if(!is.null(rv$ff_tree_inventory))
      {
        setFFTreeInventory()
      }
      if(!is.null(rv$custom_tree_inventory)) {
        setCustomTreeInventory()
      }
      loadTreeInventory()
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

      # check if ffApplyHeightFilter is checked
      if(input$ffApplyHeightFilter) {
        new_trees <- rv$ff_data_temp[sample(nrow(rv$ff_data_temp), input$ffTreeCountToAdd), ]
      }

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

      # check if customApplyHeightFilter is checked
      if(input$customApplyHeightFilter) {
        new_trees <- rv$custom_data_temp[sample(nrow(rv$custom_data_temp), input$customTreeCountToAdd), ]
      }

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

      # get signed url for uploading the csv file
      headers <- add_headers(
        accept = "application/json",
        `api-key` = input$api_key,
        `Content-Type` = "application/json"
      )
      url = paste0("https://api.fastfuels.silvxlabs.com/v1/domains/", input$domainId, "/inventories/tree")
      response <- POST(url, headers, body = '{"sources": ["file"]}')
      signed_url_response = content(response, "parsed")

      file_path <- fn
      signed_url <- signed_url_response$file$url

      headers <- as.character(unlist(signed_url_response$file$headers))
      names(headers) <- names(signed_url_response$file$headers)

      # Read the file as binary
      file_data <- upload_file(file_path, type = "application/octet-stream")

      # Perform the PUT request
      response <- PUT(
        url = signed_url,
        body = file_data,
        add_headers(.headers = headers)
      )

      # Check the response
      print(status_code(response))
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
      output$exportTreeInventoryUrl <- renderPrint(contentResponse$signedUrl)
      rv$ff_tree_inventory <- contentResponse$signedUrl
      loadTreeInventory()
      updateActionButton(session, "loadCustomFile", disabled = FALSE)
      updateActionButton(session, "createTreeGrid", disabled = FALSE)
    })

    # handle loadFFTreeInventory button click
    observeEvent(input$loadFFTreeInventory, {
      rv$ff_tree_inventory <- "tree_inventory_test1Winthrop.csv"
      loadTreeInventory()
      updateActionButton(session, "loadCustomFile", disabled = FALSE)
      updateActionButton(session, "createTreeGrid", disabled = FALSE)
    })

    # handle loadCustomFile button click
    observeEvent(input$loadCustomFile, {

      rv$custom_tree_inventory <- input$customTreeInventoryFile

      # if CHM(.tif or .asc), create a tree inventory from the CHM
      if (grepl("\\.tif|.asc$", input$customTreeInventoryFile)) {
        chm <- raster::raster(input$customTreeInventoryFile)
        ttops <- lidR::locate_trees(chm, lmf(3))
        #load raster into memory
        chm <- chm * 1

        crowns <- silva2016(chm, ttops)()

        crown_metrics_maxZ <- terra::zonal(chm, crowns, fun = max)
        crowns$Z <- crown_metrics_maxZ["value"]

        # get Diameter and Crown Ratio, todo: figure out how to get these
#        crowns$DIA <- crown_metrics_maxZ$DIA
#        crowns$CR <- crown_metrics_maxZ$CR

        uniqueVals =  unique(values(crowns$Eglin_plot1))
        cleanVals = uniqueVals[!is.na(uniqueVals)]

        tree_inventory <- data.frame(
          TREE_ID = seq_len(length(cleanVals)),
          SPCD = 919,  # Species code
          STATUSCD = 1,  # Status code
          DIA = 24,  # Diameter
          HT = crown_metrics_maxZ[, "value"],  # Height
          CR = .3,  # Crown ratio
          X = st_coordinates(ttops)[, 1],  # X coordinate
          Y = st_coordinates(ttops)[, 2]   # Y coordinate
        )
        write.csv(tree_inventory, "chm_tree_inventory.csv", row.names = FALSE)
#        input$customTreeInventoryFile <- "chm_tree_inventory.csv"
        rv$custom_tree_inventory <- "chm_tree_inventory.csv"
      }

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


    observeEvent(input$addUnderstory, {
      print("addUnderstory clicked")
      # look at rv$understory_sf
      print(rv$understory_sf)
    })

    ####################### HELPER FUNCTIONS #######################

    getData <- function(source) {
      csv_data <- read.csv(source)

      sf_data <- st_as_sf(csv_data, coords = c("X", "Y"), crs = as.integer(input$zone_crs))

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

    getSuggestedFuelbeds <- function() {
      # get the bounding box of rv$polygon
      sf_object <- geojsonsf::geojson_sf(rv$polygon)
      bbox <- st_bbox(sf_object)
      # call the rlandfire api to get suggested fuelbeds
      url <- paste0("https://landfire.gov/arcgis/rest/services/IFTDSS/IFTDSS_Suggested_Fuelbeds/MapServer/identify?f=json&geometry={\"xmin\":", bbox["xmin"], ",\"ymin\":", bbox["ymin"], ",\"xmax\":", bbox["xmax"], ",\"ymax\":", bbox["ymax"], ",\"spatialReference\":{\"wkid\":4326}}&geometryType=esriGeometryEnvelope&sr=4326&tolerance=3&mapExtent={\"xmin\":", bbox["xmin"], ",\"ymin\":", bbox["ymin"], ",\"xmax\":", bbox["xmax"], ",\"ymax\":", bbox["ymax"], ",\"spatialReference\":{\"wkid\":4326}}&imageDisplay=800,600,96&returnGeometry=false&layerIds=&layerOption=all&time=&dynamicLayers=&gdbVersion=&returnZ=false&returnM=false&returnUnformattedValues=false&returnFieldName=false")
      response <- GET(url)
      contentResponse = content(response, "parsed")
      # extract the fuelbed names from contentResponse
      fuelbed_names <- c()
      for (feature in contentResponse$results) {
        fuelbed_name <- feature$attributes$FUELBED_NAME
        fuelbed_names <- c(fuelbed_names, fuelbed_name)
      }
      # return unique fuelbed names
      return(unique(fuelbed_names))
    }


    getSuggestedFuelbeds2_AI <- function(sf_object) {
      # Convert AOI to bounding box
      myAOI <- sf::st_bbox(sf_object) %>% as.numeric()

      save_file <- tempfile(fileext = ".zip")

      # Correct product name for FCCS fuelbeds
      resp <- landfireAPIv2(
        products = "FCCS2020",
        aoi = myAOI,
        email = "bdrye@uw.edu",
        path = save_file
      )

      lf_dir <- file.path(tempdir(), "lf")
      utils::unzip(save_file, exdir = lf_dir)

      print(list.files(lf_dir, full.names = TRUE, recursive = TRUE))

      lf <- terra::rast(list.files(lf_dir, pattern = ".tif$",
                                   full.names = TRUE,
                                   recursive = TRUE))

      print(lf)
    }




    getSuggestedFuelbeds2 <- function(sf_object) {

      #return(c("Fuelbed 52", "Fuelbed 53"))
      # landfire API is down for maintenance as of Dec 8, 2025

      myAOI <- getAOI(sf_object)
      save_file <- tempfile(fileext = ".zip")
      # Request FCCS fuelbeds for the polygon
      resp <- landfireAPIv2(products=c("240FCCS"), aoi=myAOI, email="bdrye@uw.edu", path=save_file)

      # create a new temp directory inside tempdir()

#      lf_dir <- tempfile("lf_")

      # make a unique folder name to use inside of tempdir()
      lf_dir <- file.path(tempdir(), paste0("lf_", as.integer(Sys.time())))

      utils::unzip(save_file, exdir = lf_dir)

      print(list.files(lf_dir, full.names = TRUE, recursive = TRUE))

      tif_files <- list.files(lf_dir, pattern = ".tif$",
                                   full.names = TRUE,
                                   recursive = TRUE)

      # read rasters into list
      r_list <- lapply(tif_files, rast) # looks like we only get one raster back...

      rv$enclosing_raster <- NULL
      # for each raster, check if it fully contains the polygon
      # try to find smallest enclosing raster

      for(r in r_list)
      {
        r_ext <- terra::ext(r)
        print(r_ext)
        print(crs(r))
        poly_bb <- st_transform(st_bbox(sf_object), crs = crs(r))
        encloses <- (poly_bb$xmin >= r_ext[1] && poly_bb$xmax <= r_ext[2] &&
                                poly_bb$ymin >= r_ext[3] && poly_bb$ymax <= r_ext[4])
        print(poly_bb)
        print(encloses)

        # if encloses and raster r$FUELBED exists, consider it

        # get vat.dbf from .tif.
        tiffile <- sources(r)[1]
        dbf_file <- paste0(tiffile, ".vat.dbf")   # typical name: file.tif.vat.dbf
        print(tiffile)

        if (!file.exists(dbf_file))
        {
          print("VAT DBF not found, skipping this raster")
          next
        }

        # read the DBF
        vat <- read.dbf(dbf_file, as.is = TRUE)

        if(encloses)
        {
          if("FUELBED" %in% names(vat))
          {
            print("FUELBED attribute found in VAT")
          }
          else
          {
            print("FUELBED attribute NOT found in .tif.vat.dbf file. Can't use this raster")
            plot(r)
            next
          }

          # if r is smaller than current enclosing raster, set it as the enclosing raster
          if(!is.null(rv$enclosing_raster))
          {
            curr_ext <- terra::ext(rv$enclosing_raster)
            curr_area <- (curr_ext[2] - curr_ext[1]) * (curr_ext[4] - curr_ext[3])
            new_area <- (r_ext[2] - r_ext[1]) * (r_ext[4] - r_ext[3])
            if(new_area >= curr_area)
            {
              print("Current enclosing raster is smaller than new raster")
              next
            }
            else
            {
              print("Found smaller enclosing raster")
            }
          }
          else
          {
            print("Found first enclosing raster")
          }
          rv$enclosing_raster <- r
          plot(rv$enclosing_raster)

          # plot sf_object on top
          # temp_poly <- st_transform(sf_object, crs = crs(r))
          # plot(st_geometry(temp_poly), add = TRUE, border = "red", lwd = 2)
        }
      }

      if(is.null(rv$enclosing_raster))
      {
        print("No enclosing raster found that contains the polygon?")
        # try anyway
        rv$enclosing_raster <- r_list[[1]]
        # return(NULL)
          }

      # get unique fuelbeds that are in the sf_object area of the enclosing raster
      masked_raster <- terra::mask(rv$enclosing_raster, vect(st_transform(sf_object, crs = crs(rv$enclosing_raster))))
      unique_fuelbeds <- unique(terra::values(masked_raster))
      # remove NA values
      unique_fuelbeds <- unique_fuelbeds[!is.na(unique_fuelbeds)]
      # print(unique_fuelbeds)
      # plot(masked_raster)

      # create temp_df that contains only the fuelbeds in the masked raster
      temp_df <- data.frame(levels(masked_raster$FUELBED))
      temp_df <- temp_df[temp_df$Value %in% unique_fuelbeds, ]

      # output$fuelbedPlot <- renderPlot({
      #   if(is.null(masked_raster)){
      #     return(NULL)
      #   }
      #   plot(masked_raster)
      #   temp_poly <- st_transform(sf_object, crs = crs(masked_raster))
      #   plot(st_geometry(temp_poly), add = TRUE, border = "blue", lwd = 2)
      # })

      # 1) inspect
      print(crs(masked_raster))
      print(ext(masked_raster))

      # 2) reproject (use "near" for categorical like FCCS/FUELBED)
      out_file <- tempfile(fileext = ".tif")
      masked_ll <- project(masked_raster,
                           "EPSG:4326",
                           method = "near",         # or "bilinear" for continuous data
                           filename = out_file,
                           overwrite = TRUE)

      # masked_ll is written to out_file; read as raster::RasterLayer for leaflet
      r_raster <- raster(out_file)

      # create palette from vat. It has fuelbed Value, R,G,B
      vat <- read.dbf(paste0(sources(rv$enclosing_raster)[1], ".vat.dbf"), as.is = TRUE)
      vals <- raster::getValues(r_raster)
      vals <- vals[!is.na(vals)]
      unique_vals <- sort(unique(vals))
      # create color palette
      rgb_matrix <- vat[match(unique_vals, vat$Value), c("R", "G", "B")]
      hex_colors <- rgb(rgb_matrix$R, rgb_matrix$G, rgb_matrix$B, maxColorValue = 255)
      names(hex_colors) <- unique_vals
      print(hex_colors)

      pal <- colorFactor(hex_colors, domain = unique_vals, na.color = "transparent")

      # 3) create a palette (categorical example)
      # vals <- raster::getValues(r_raster)
      # vals <- vals[!is.na(vals)]
      # unique_vals <- sort(unique(vals))
      # pal <- colorFactor(viridis(length(unique_vals)), domain = unique_vals, na.color = "transparent")

      # 4) add to leaflet and zoom to extent
      e <- terra::ext(masked_ll)   # xmin, xmax, ymin, ymax (terra)
      proxy <- leafletProxy("subMap")
      proxy %>%
#        addTiles() %>%
        removeControl(layerId="fuelbedLegend") %>%
#        addRasterImage(r_raster, colors = pal, opacity = 0.9) %>%   # use this to show masked raster (just polygon area)
        addRasterImage(rv$enclosing_raster, colors = pal, opacity = 0.9) %>%
        addPolygons(data = sf_object, fill = FALSE, color = mainPolygonColor, weight = 3, opacity = 0.9) %>%
        addLegend("bottomright", pal = pal, values = unique_vals, opacity="1", title = "Category", layerId="fuelbedLegend") %>%
        fitBounds(e[1], e[3], e[2], e[4])

      # temp_df contains all the fuelbeds in the enclosing raster
      # temp_df <- data.frame(levels(rv$enclosing_raster$FUELBED))
      # this has Value, FUELBED columns

      return(temp_df)
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

    # # Initialize reactive values with sample data
    rv$species_df = data.frame(
        id = 1:3,
        species = c("Douglas Fir", "Ponderosa Pine", "Lodgepole Pine"),
        relative_cover = c(0.35, 0.40, 0.25),
        bulk_density = c(12.5, 10.2, 11.8),
        sa_volume_ratio = c(2500, 2200, 2400),
        stringsAsFactors = FALSE
      )
    rv$next_id = 4  # To keep track of the next ID for new species

    # Render the table with action buttons
    output$species_table <- renderDT({
      df <- rv$species_df

      # Add action buttons column
      df$actionEdit <- paste0(
        '<button class="btn btn-primary btn-sm edit-btn" data-id="', df$id, '">',
        '<i class="fa fa-edit"></i> Edit</button> '
      )
      df$actionRemove <- paste0(
        '<button class="btn btn-danger btn-sm delete-btn" data-id="', df$id, '">',
        '<i class="fa fa-trash"></i> Remove</button>'
      )

      # Remove id column for display
      df_display <- df[, c("species", "relative_cover", "bulk_density",
                           "sa_volume_ratio", "actionEdit", "actionRemove")]

      datatable(
        df_display,
        colnames = c("Species", "Relative Cover", "Bulk Density (lb/ft³)",
                     "SA/Volume Ratio (ft⁻¹)", "", ""),
        escape = FALSE,  # Allow HTML in actions column
        selection = 'none',
        rownames = FALSE,
        options = list(
          dom = 't',  # Just the table, no search/pagination
          ordering = FALSE,
          columnDefs = list(
            list(className = 'dt-center', targets = 1:4),
            list(width = '200px', targets = 4)
          )
        )
      )
    })

    # Handle Edit button clicks
    observeEvent(input$species_table_cell_clicked, {
      info <- input$species_table_cell_clicked

      # Only process if actions column was clicked
      if (! is.null(info$value) && grepl("edit-btn", info$value)) {
        # Extract the ID from the button
        id <- as.numeric(gsub('.*data-id="([0-9]+)".*', '\\1', info$value))

        # Get the row data
        row_data <- rv$species_df[rv$species_df$id == id, ]

        showModal(modalDialog(
          title = "Edit Species",
          textInput("edit_species", "Species Name", value = row_data$species),
          numericInput("edit_relative_cover", "Relative Cover (0-1)",
                       value = row_data$relative_cover,
                       min = 0, max = 1, step = 0.01),
          numericInput("edit_bulk_density", "Bulk Density (lb/ft³)",
                       value = row_data$bulk_density,
                       min = 0, step = 0.1),
          numericInput("edit_sa_volume_ratio", "Surface Area to Volume Ratio (ft⁻¹)",
                       value = row_data$sa_volume_ratio,
                       min = 0, step = 10),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("save_edit", "Save Changes", class = "btn-primary")
          ),
          easyClose = TRUE
        ))

        # Store the ID being edited
        rv$editing_id <- id
      }

      # Handle Delete button clicks
      if (!is.null(info$value) && grepl("delete-btn", info$value)) {
        id <- as.numeric(gsub('.*data-id="([0-9]+)".*', '\\1', info$value))

        showModal(modalDialog(
          title = "Confirm Deletion",
          paste("Are you sure you want to delete",
                rv$species_df[rv$species_df$id == id, "species"], "?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_delete", "Delete", class = "btn-danger")
          )
        ))

        rv$deleting_id <- id
      }
    })

    # Save edited species
    observeEvent(input$save_edit, {
      id <- rv$editing_id
      row_idx <- which(rv$species_df$id == id)

      rv$species_df[row_idx, "species"] <- input$edit_species
      rv$species_df[row_idx, "relative_cover"] <- input$edit_relative_cover
      rv$species_df[row_idx, "bulk_density"] <- input$edit_bulk_density
      rv$species_df[row_idx, "sa_volume_ratio"] <- input$edit_sa_volume_ratio

      removeModal()
    })

    # Confirm deletion
    observeEvent(input$confirm_delete, {
      rv$species_df <- rv$species_df[rv$species_df$id != rv$deleting_id, ]
      removeModal()
    })

    # Add new species
    observeEvent(input$add_species, {
      showModal(modalDialog(
        title = "Add New Species",
        textInput("new_species", "Species Name", value = ""),
        numericInput("new_relative_cover", "Relative Cover (0-1)",
                     value = 0, min = 0, max = 1, step = 0.01),
        numericInput("new_bulk_density", "Bulk Density (lb/ft³)",
                     value = 0, min = 0, step = 0.1),
        numericInput("new_sa_volume_ratio", "Surface Area to Volume Ratio (ft⁻¹)",
                     value = 0, min = 0, step = 10),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_new", "Add Species", class = "btn-success")
        ),
        easyClose = TRUE
      ))
    })

    # Save new species
    observeEvent(input$save_new, {
      req(input$new_species != "")

      new_row <- data.frame(
        id = rv$next_id,
        species = input$new_species,
        relative_cover = input$new_relative_cover,
        bulk_density = input$new_bulk_density,
        sa_volume_ratio = input$new_sa_volume_ratio,
        stringsAsFactors = FALSE
      )

      rv$species_df <- rbind(rv$species_df, new_row)
      rv$next_id <- rv$next_id + 1

      removeModal()
    })



}


