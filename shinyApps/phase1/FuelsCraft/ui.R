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

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("FuelsCraft Tree Inventory Tool"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        textInput("customTreeInventoryFile", "Custom Tree Inventory", value="tree_inventory_test1_custom.csv"),
        actionButton("loadCustomTreeInventory", "Load Custom Tree Inventory", disabled=FALSE),
        hr(),
        h4("FastFuels API"),
        textInput("api_key", "API Key", value = "b6a21fbe22c54a1890ce0d2252d2f584"),
        actionButton("createDomain", "Create Domain", disabled=TRUE), 
        textInput("domainId", "Domain ID", value=""),
        actionButton("createRoadFeature", "Create Road Feature", disabled=TRUE),
        verbatimTextOutput("roadFeature"),
        actionButton("createWaterFeature", "Create Water Feature", disabled=TRUE),
        verbatimTextOutput("waterFeature"),
        actionButton("createTreeInventory", "Create Tree Inventory", disabled=TRUE),
        verbatimTextOutput("treeInventory"),
        actionButton("exportTreeInventory", "Export Tree Inventory", disabled=TRUE),
        verbatimTextOutput("exportTreeInventory"),
        
        actionButton("createTreeGrid", "Create Tree Grid", disabled=TRUE),
        verbatimTextOutput("treeGrid"),
        actionButton("createSurfaceGrid", "Create Surface Grid", disabled=TRUE),
        verbatimTextOutput("surfaceGrid"),
        actionButton("createTopographyGrid", "Create Topography Grid", disabled=TRUE),
        verbatimTextOutput("topographyGrid"),
        actionButton("getInputFiles", "Get (QUICFire) Input Files", disabled=TRUE),
        verbatimTextOutput("inputFiles"),
        hr(),
        p("testing tools"),
        actionButton("loadFFTreeInventory", "Load FF Tree Inv.(shortcut)", disabled=FALSE)
      ),
      mainPanel(
          #plotOutput("distPlot"), 
        leafletOutput("map"),
#           verbatimTextOutput("polygon_coords"),
        verbatimTextOutput("area"),
#        leafletOutput("mapWithTrees"),
        checkboxInput("showFFTrees", "Show FastFuels Trees", value = TRUE),
        checkboxInput("showCustomTrees", "Show Custom Trees", value = TRUE),
        actionButton("clearMap", "Clear Map", disabled=FALSE), 
        actionButton("cropCustomPolygon", "Crop Custom", disabled=FALSE),
        actionButton("removeFFTrees", "Remove FF Trees", disabled=FALSE),
        # add a dropdown of tree attributes, will fill in options from tree list data once loaded. 
        selectInput("customTreeAttribute", "Tree Attribute", choices=NULL),
        sliderInput("customTreeAttributeChange", "Change Tree Attribute by %", min = 0, max = 200, value = 100),
        # add hist
        plotOutput("attributeHist")
      )
    )
)
