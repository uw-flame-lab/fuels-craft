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

library(stars)
library(rgl)
#library(reshape2)


# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("FuelsCraft Tree Inventory Tool"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        textInput("customTreeInventoryFile", "Custom Inventory File", value="tree_inventory_test1_custom.csv"),
        actionButton("loadCustomTreeInventory", "Load", disabled=FALSE),
        actionButton("cropCustomPolygon", "Crop", disabled=FALSE),
        h4("FastFuels API"),
        textInput("api_key", "API Key", value = ""),
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
        verbatimTextOutput("exportTreeInventory"),
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
        actionButton("loadFFTreeInventory", "Load FF Tree Inv.(shortcut)", disabled=FALSE)
      ),
      mainPanel(
        fluidRow(
          column(6, actionButton("clearMap", "Clear Map", disabled=FALSE)),
          column(6,
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
            )
          ),
        ),

        leafletOutput("map"),
        # put the checkboxes on the same row
        fluidRow(
          column(6, checkboxInput("showFFTrees", "Show FastFuels Trees", value = TRUE),
                 plotOutput("ffHeightHist", width="250px", height="200px")
          ),
          column(6, checkboxInput("showCustomTrees", "Show Custom Trees", value = TRUE),
                 plotOutput("customHeightHist", width="250px", height="200px"))
        ),
        # create two sub-panels, one for ff, one for custom
        tabsetPanel(
          tabPanel("Inventory from FastFuels",
                     textInput("ffTreeCountToAdd", "", value="1000", width = 80),
                     actionButton("addFFTreesByPolygon", "Add", disabled=FALSE),
                     actionButton("removeFFTreesByPolygon", "Remove", disabled=FALSE),
                     sliderInput("ffTreeHeight", "Filter by Tree Height", min = 0, max = 60, value = c(0,60)),
                     actionButton("resetFFTreeInventory", "Reset", disabled=FALSE)
                   # fluidRow(
                   #   column(6, sliderInput("ffTreeHeight", "Filter by Tree Height", min = 0, max = 60, value = c(0,60)),
                   #                 actionButton("resetFFTreeInventory", "Reset", disabled=FALSE)),
                   #   column(6, plotOutput("ffHeightHist", width="250px", height="200px"))
                   # ),
          ),
          tabPanel("Optional: Your Custom Inventory",
                     textInput("customTreeCountToAdd", "", value="1000", width = 80),
                     actionButton("addCustomTreesByPolygon", "Add", disabled=FALSE),
                     actionButton("removeCustomTreesByPolygon", "Remove", disabled=FALSE),
                     sliderInput("customTreeHeight", "Filter by Tree Height", min = 0, max = 60, value = c(0,60)),
                     actionButton("resetCustomTreeInventory", "Reset", disabled=FALSE)
          )
        ),
        actionButton("mergeInventories", "Merge Custom into FF", disabled=FALSE),
        actionButton("uploadInventory", "Upload Modified FF Inventory", disabled=FALSE),
        actionButton("saveInventory", "Save Inventory CSV", disabled=FALSE),
        verbatimTextOutput("area"),
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
