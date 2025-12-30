#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(httr)
library(jsonlite)
library(geojsonsf)
library(shinycssloaders)
library(bslib)
library(RColorBrewer)
library(rlandfire)

library(stars)
library(rgl)
#library(reshape2)
library(lidR)
library(terra)
library(foreign)
library(raster)
library(viridis)
library(htmlwidgets)
library(ggplot2)
library(DT)


# Define UI for application that draws a histogram
fluidPage(
  tags$head(
    # Leaflet.Sync plugin
    tags$script(src = "https://rawcdn.githack.com/jieter/Leaflet.Sync/master/L.Map.Sync.js"),
    tags$style(HTML("
      /* only affect splitLayouts inside .no-overflow wrapper */
      .no-overflow .shiny-split-layout > div {
        overflow: visible !important;
      }
      /* optional: only allow when a control inside is focused (prevents accidental overflow) */
      .no-overflow .shiny-split-layout > div:focus-within {
        overflow: visible !important;
      }
    "))
  ),
  tags$head(tags$script(HTML("
    Shiny.addCustomMessageHandler('removeDrawLayer', function(layerId) {
      setTimeout(function() {
        var mapWidget = HTMLWidgets.find('#subMap');
        if (mapWidget) {
          var map = mapWidget.getMap();
          map.eachLayer(function(layer) {
            if (layer._leaflet_id === layerId) {
              map.removeLayer(layer);
            }
          });
        }
      }, 100);  // 100ms delay
    });
  "))),

  # Application title
    titlePanel("FuelsCraft Fuel Customization Tool"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        verbatimTextOutput("area"),
        actionButton("clearMap", "Clear Map", disabled=FALSE),
        selectInput(
                 inputId = "zone_crs",
                 label = "Select UTM Zone",
                 choices = list(
                   "10N Western coastal U.S." = 26910,
                   "11N CA, NV, UT, AZ" = 26911,
                   "12N CO, NM, WY" = 26912,
                   "13N Central Rockies" = 26913,
                   "14N Great Plains" = 26914,
                   "15N Midwest" = 26915,
                   "16N IL, KY, TN" = 26916,
                   "17N GA, FL, Carolinas" = 26917,
                   "18N Mid-Atlantic" = 26918,
                   "19N New England" = 26919
                 )
        ),
        hr(),

        textInput("customTreeInventoryFile", "CHM or Inventory File", value="tree_inventory_test1_customWinthrop.csv"),
        # textInput("customTreeInventoryFile", "CHM or Inventory File", value="tree_inventory_test1_custom.csv"),
        # textInput("customTreeInventoryFile", "CHM or Inventory File", value="Eglin_plot1.asc"),
        actionButton("loadCustomFile", "Load", disabled=FALSE),
        actionButton("cropCustomPolygon", "Crop", disabled=FALSE),
        h4("FastFuels API"),
        textInput("api_key", "API Key", value = "6e25655e09a14eb7a85489da28c038d0"),   # key created 5/20/2025 BrianTest
        actionButton("createDomain", "Create Domain", disabled=TRUE),
        textInput("domainId", "Domain ID", value=""),
        actionButton("createRoadFeature", "Create Road Feature", disabled=TRUE),
        verbatimTextOutput("roadFeature"),
        actionButton("createWaterFeature", "Create Water Feature", disabled=TRUE),
        verbatimTextOutput("waterFeature"),
        radioButtons("treeMapVersion", "TreeMap Version", choices=c("2014", "2016"), selected="2016", inline=TRUE),
        radioButtons("treeMapStyle", "Method", choices=c("raw", "fusion"), selected="fusion", inline=TRUE),

        actionButton("createTreeInventory", "Create Tree Inventory", disabled=TRUE),
        verbatimTextOutput("treeInventory"),
        actionButton("exportTreeInventory", "Export Tree Inventory", disabled=TRUE),
        verbatimTextOutput("exportTreeInventoryUrl"),
        h6("Optional: edit/merge/upload Tree Inventories using the tools to the right"),
        actionButton("createTreeGrid", "Create Tree Grid", disabled=TRUE),
        verbatimTextOutput("treeGrid"),
        actionButton("createSurfaceGrid", "Create Surface Grid", disabled=TRUE),
        verbatimTextOutput("surfaceGrid"),
        actionButton("createTopographyGrid", "Create Topography Grid", disabled=TRUE),
        verbatimTextOutput("topographyGrid"),

        radioButtons("qfInputFormat", "Method", choices=c("zip", "zarr"), selected="zip", inline=TRUE),
        actionButton("getInputFiles", "Get (QUICFire) Input Files", disabled=TRUE),
        verbatimTextOutput("inputFiles"),
        hr(),
        p("testing tools"),
        actionButton("loadFFTreeInventory", "Load FF Tree Inv.(shortcut, Winthrop)", disabled=FALSE)
      ),
      mainPanel(
        fluidRow(
#          leafletOutput("map", height = 500)
          column(6, leafletOutput("map"), height = "500px"),
          column(6, leafletOutput("subMap"), height="500px")
        ),
        # put the checkboxes on the same row
        # fluidRow(
        #   column(6, checkboxInput("showFFTrees", "Show FastFuels Trees", value = TRUE),
        #          plotOutput("ffHeightHist", width="250px", height="200px")
        #   ),
        #   column(6, checkboxInput("showCustomTrees", "Show Custom Trees", value = TRUE),
        #          plotOutput("customHeightHist", width="250px", height="200px"))
        # ),
        # create two sub-panels, one for ff, one for custom
        tabsetPanel(
          tabPanel("Inventory from FastFuels",
                     checkboxInput("showFFTrees", "Show FastFuels Trees", value = TRUE),
                     plotOutput("ffHeightHist", width="250px", height="200px"),
                     textInput("ffTreeCountToAdd", "", value="100", width = 80),
                     actionButton("addFFTreesByPolygon", "Add", disabled=FALSE),
                     actionButton("removeFFTreesByPolygon", "Remove", disabled=FALSE),
                     checkboxInput("ffApplyHeightFilter", "apply height filter when adding trees", value = TRUE),
                     sliderInput("ffTreeHeight", "Filter by Tree Height", min = 0, max = 60, value = c(0,60)),
                     actionButton("resetFFTreeInventory", "Reset", disabled=FALSE)
                   # fluidRow(
                   #   column(6, sliderInput("ffTreeHeight", "Filter by Tree Height", min = 0, max = 60, value = c(0,60)),
                   #                 actionButton("resetFFTreeInventory", "Reset", disabled=FALSE)),
                   #   column(6, plotOutput("ffHeightHist", width="250px", height="200px"))
                   # ),
          ),
          tabPanel("Optional: Your Custom Inventory",
                     checkboxInput("showCustomTrees", "Show Custom Trees", value = TRUE),
                     plotOutput("customHeightHist", width="250px", height="200px"),
                     textInput("customTreeCountToAdd", "", value="100", width = 80),
                     actionButton("addCustomTreesByPolygon", "Add", disabled=FALSE),
                     actionButton("removeCustomTreesByPolygon", "Remove", disabled=FALSE),
                     checkboxInput("customApplyHeightFilter", "apply height filter when adding trees", value = TRUE),
                     sliderInput("customTreeHeight", "Filter by Tree Height", min = 0, max = 60, value = c(0,60)),
                     actionButton("resetCustomTreeInventory", "Reset", disabled=FALSE),
                   actionButton("mergeInventories", "Merge Custom into FF", disabled=FALSE),
                   actionButton("uploadInventory", "Upload Modified FF Inventory", disabled=FALSE),
                   actionButton("saveInventory", "Save Inventory CSV", disabled=FALSE),
          ),
          tabPanel("Understory",
            fluidRow(
                   selectInput("understory", label=NULL, choices=c('Shrubs1', 'Shrubs2', 'Herbs1', 'Herbs2', 'Downed Wood: fine wood', 'Downed Wood: coarse wood', 'Litter, Lichen, Moss', 'Ground Fuels')),

                   # create UI for each understory option
                   mainPanel(
                     conditionalPanel(
                       condition = "input.understory == 'Shrubs1'",
                         tabsetPanel(
                           id="Shrubs1_tabs",
                           tabPanel("Fuelbed 1",
                             value="shrub1_1fuelbedList",
                             wellPanel(
                               id="shrub1_1",
                               selectInput("shrub1_1fuelbedList", label=NULL, choices=NULL),
                               sliderInput("shrub1_1fuelbedPct", "Percent Cover",  min = 0, max = 100, value=50),
                               radioButtons("shrub1_1spatialPattern", "Spatial pattern:",
                                        choices = c("Uniform", "Clumpy, random", "Clumpy, outside trees", "Clumpy, near trees"),
                                        selected = "Uniform",
                                        inline = TRUE),
                               sliderInput("shrub1_1percentLive", "Percent live (%):", min = 0, max = 100, value = 70),
                               sliderInput("shrub1_1height", "Height (m):", min = 0, max = 10, value = 2),
                               sliderInput("shrub1_1loading", "Loading (kg/m2):", min = 0, max = 20, value = 0),
                               radioButtons("shrub1_1needleDrape", "Needle Drape:",
                                        choices = c("Low", "Moderate", "High"),
                                        selected = "Moderate",
                                        inline = TRUE),
                               tags$div(id = "shrub1_1_species_div")
                             )
                           ),
                           tabPanel("Fuelbed 2",
                              value="shrub1_2fuelbedList",
                              wellPanel(
                                id="shrub1_2",
                                selectInput("shrub1_2fuelbedList", label=NULL, choices=NULL),
                                sliderInput("shrub1_2fuelbedPct", "Percent Cover",  min = 0, max = 100, value=50),
                                radioButtons("shrub1_2spatialPattern", "Spatial pattern:",
                                             choices = c("Uniform", "Clumpy, random", "Clumpy, outside trees", "Clumpy, near trees"),
                                             selected = "Uniform",
                                             inline = TRUE),
                                sliderInput("shrub1_2percentLive", "Percent live (%):", min = 0, max = 100, value = 70),
                                sliderInput("shrub1_2height", "Height (m):", min = 0, max = 10, value = 2),
                                sliderInput("shrub1_2loading", "Loading (kg/m2):", min = 0, max = 20, value = 0),
                                radioButtons("shrub1_2needleDrape", "Needle Drape:",
                                             choices = c("Low", "Moderate", "High"),
                                             selected = "Moderate",
                                             inline = TRUE),
                                tags$div(id = "shrub1_2_species_div")
                              )
                           ),
                        ),
                     ),
                     conditionalPanel(
                       condition = "input.understory == 'Shrubs2'",
                       tabsetPanel(
                         id="Shrubs2_tabs",
                         tabPanel("Fuelbed 1",
                                  value="shrub2_1fuelbedList",
                                  wellPanel(
                                    id="shrub2_1",
                                    selectInput("shrub2_1fuelbedList", label=NULL, choices=NULL),
                                    sliderInput("shrub2_1fuelbedPct", "Percent Cover",  min = 0, max = 100, value=50),
                                    radioButtons("shrub2_1spatialPattern", "Spatial pattern:",
                                                 choices = c("Uniform", "Clumpy, random", "Clumpy, outside trees", "Clumpy, near trees"),
                                                 selected = "Uniform",
                                                 inline = TRUE),
                                    sliderInput("shrub2_1percentLive", "Percent live (%):", min = 0, max = 100, value = 70),
                                    sliderInput("shrub2_1height", "Height (m):", min = 0, max = 10, value = 2),
                                    sliderInput("shrub2_1loading", "Loading (kg/m2):", min = 0, max = 20, value = 0),
                                    tags$div(id = "shrub2_1_species_div")
                                  )
                         ),
                         tabPanel("Fuelbed 2",
                                  value="shrub2_2fuelbedList",
                                  wellPanel(
                                    id="shrub2_2",
                                    selectInput("shrub2_2fuelbedList", label=NULL, choices=NULL),
                                    sliderInput("shrub2_2fuelbedPct", "Percent Cover",  min = 0, max = 100, value=50),
                                    radioButtons("shrub2_2spatialPattern", "Spatial pattern:",
                                                 choices = c("Uniform", "Clumpy, random", "Clumpy, outside trees", "Clumpy, near trees"),
                                                 selected = "Uniform",
                                                 inline = TRUE),
                                    sliderInput("shrub2_2percentLive", "Percent live (%):", min = 0, max = 100, value = 70),
                                    sliderInput("shrub2_2height", "Height (m):", min = 0, max = 10, value = 2),
                                    sliderInput("shrub2_2loading", "Loading (kg/m2):", min = 0, max = 20, value = 0),
                                    tags$div(id = "shrub2_2_species_div")
                                  )
                         ),
                       ),
                     ),
                     conditionalPanel(
                       condition = "input.understory == 'Herbs1'",
                       tabsetPanel(
                         id="Herbs1_tabs",
                         tabPanel("Fuelbed 1",
                                  value="herb1_1fuelbedList",
                                  wellPanel(
                                    id="herb1_1",
                                    selectInput("herb1_1fuelbedList", label=NULL, choices=NULL),
                                    sliderInput("herb1_1fuelbedPct", "Percent Cover",  min = 0, max = 100, value=50),
                                    radioButtons("herb1_1spatialPattern", "Spatial pattern:",
                                                 choices = c("Outside of tree crowns", "Evenly distributed", "Near trees"),
                                                 selected = "Outside of tree crowns",
                                                 inline = TRUE),
                                    sliderInput("herb1_1percentLive", "Percent live (%):", min = 0, max = 100, value = 70),
                                    sliderInput("herb1_1height", "Height (m):", min = 0, max = 10, value = 2),
                                    sliderInput("herb1_1loading", "Loading (kg/m2):", min = 0, max = 20, value = 0),
                                    tags$div(id = "herb1_1_species_div")
                                  )
                         ),
                         tabPanel("Fuelbed 2",
                                  value="herb1_2fuelbedList",
                                  wellPanel(
                                    id="herb1_2",
                                    selectInput("herb1_2fuelbedList", label=NULL, choices=NULL),
                                    sliderInput("herb1_2fuelbedPct", "Percent Cover",  min = 0, max = 100, value=50),
                                    radioButtons("herb1_2spatialPattern", "Spatial pattern:",
                                                 choices = c("Outside of tree crowns", "Evenly distributed", "Near trees"),
                                                 selected = "Outside of tree crowns",
                                                 inline = TRUE),
                                    sliderInput("herb1_2percentLive", "Percent live (%):", min = 0, max = 100, value = 70),
                                    sliderInput("herb1_2height", "Height (m):", min = 0, max = 10, value = 2),
                                    sliderInput("herb1_2loading", "Loading (kg/m2):", min = 0, max = 20, value = 0),
                                    tags$div(id = "herb1_2_species_div")
                                  )
                         ),
                       ),
                     ),
                     conditionalPanel(
                       condition = "input.understory == 'Herbs2'",
                       tabsetPanel(
                         id="Herbs2_tabs",
                         tabPanel("Fuelbed 1",
                                  value="herb2_1fuelbedList",
                                  wellPanel(
                                    id="herb2_1",
                                    selectInput("herb2_1fuelbedList", label=NULL, choices=NULL),
                                    sliderInput("herb2_1fuelbedPct", "Percent Cover",  min = 0, max = 100, value=50),
                                    radioButtons("herb2_1spatialPattern", "Spatial pattern:",
                                                 choices = c("Uniform", "Clumpy, random", "Clumpy, outside trees", "Clumpy, near trees"),
                                                 selected = "Uniform",
                                                 inline = TRUE),
                                    sliderInput("herb2_1percentLive", "Percent live (%):", min = 0, max = 100, value = 70),
                                    sliderInput("herb2_1height", "Height (m):", min = 0, max = 10, value = 2),
                                    sliderInput("herb2_1loading", "Loading (kg/m2):", min = 0, max = 20, value = 0),
                                    tags$div(id = "herb2_1_species_div")
                                  )
                         ),
                         tabPanel("Fuelbed 2",
                                  value="herb2_2fuelbedList",
                                  wellPanel(
                                    id="herb2_2",
                                    selectInput("herb2_2fuelbedList", label=NULL, choices=NULL),
                                    sliderInput("herb2_2fuelbedPct", "Percent Cover",  min = 0, max = 100, value=50),
                                    radioButtons("herb2_2spatialPattern", "Spatial pattern:",
                                                 choices = c("Uniform", "Clumpy, random", "Clumpy, outside trees", "Clumpy, near trees"),
                                                 selected = "Uniform",
                                                 inline = TRUE),
                                    sliderInput("herb2_2percentLive", "Percent live (%):", min = 0, max = 100, value = 70),
                                    sliderInput("herb2_2height", "Height (m):", min = 0, max = 10, value = 2),
                                    sliderInput("herb2_2loading", "Loading (kg/m2):", min = 0, max = 20, value = 0),
                                    tags$div(id = "herb2_2_species_div")
                                  )
                         ),
                       ),
                     ),
                     conditionalPanel(
                       condition = "input.understory == 'Downed Wood: fine wood'",
                       tabsetPanel(
                         id="DownedFine_tabs",
                         tabPanel("Fuelbed 1",
                                  value="downedFine_1fuelbedList",
                                  wellPanel(
                                    id="downedFine_1",
                                    selectInput("downedFine_1fuelbedList", label=NULL, choices=NULL),
                                    radioButtons("downedFine_1spatialPattern", "Spatial pattern:",
                                                 choices = c("Beneath tree crowns", "Evenly distributed"),
                                                 selected = "Beneath tree crowns",
                                                 inline = TRUE),
                                    sliderInput("downedFine_1fuelbedPct", "Percent Cover",  min = 0, max = 200, value=0),
                                    sliderInput("downedFine_1fuelbedDepth", "Depth",  min = 0, max = 10, value=0),
                                    sliderInput("downedFine_1fuelbed1hrLoad", "1hr load",  min = 0, max = 10, value=0),
                                    sliderInput("downedFine_1fuelbed10hrLoad", "10hr load",  min = 0, max = 10, value=0),
                                    sliderInput("downedFine_1fuelbed100hrLoad", "100hr load",  min = 0, max = 10, value=0),
                                  )
                         ),
                         tabPanel("Fuelbed 2",
                                  value="downedFine_2fuelbedList",
                                  wellPanel(
                                    id="downedFine_2",
                                    selectInput("downedFine_2fuelbedList", label=NULL, choices=NULL),
                                    radioButtons("downedFine_1spatialPattern", "Spatial pattern:",
                                                 choices = c("Beneath tree crowns", "Evenly distributed"),
                                                 selected = "Beneath tree crowns",
                                                 inline = TRUE)                                    ),
                                    sliderInput("downedFine_2fuelbedPct", "Percent Cover",  min = 0, max = 200, value=0),
                                    sliderInput("downedFine_2fuelbedDepth", "Depth",  min = 0, max = 10, value=0),
                                    sliderInput("downedFine_2fuelbed1hrLoad", "1hr load",  min = 0, max = 10, value=0),
                                    sliderInput("downedFine_2fuelbed10hrLoad", "10hr load",  min = 0, max = 10, value=0),
                                    sliderInput("downedFine_2fuelbed100hrLoad", "100hr load",  min = 0, max = 10, value=0),
                                  )
                       ),
                     ),
                   )
            ),
            fluidRow(
              actionButton("addUnderstory", "Add Understory", disabled=FALSE)
            )
          )
        )
#        actionButton("create3DVoxelPlot", "Visualize Voxels", disabled=FALSE),
#        plotOutput("voxel3dPlot", width="100%", height="300px") %>% withSpinner(color="#0dc5c1"),

          #plotOutput("distPlot"),
#           verbatimTextOutput("polygon_coords"),
#        leafletOutput("mapWithTrees"),
        # add a dropdown of tree attributes, will fill in options from tree list data once loaded.
        # selectInput("customTreeAttribute", "Tree Attribute", choices=NULL),
        # sliderInput("customTreeAttributeChange", "Change Tree Attribute by %", min = 0, max = 200, value = 100),
        # add hist

    )
  )
)
