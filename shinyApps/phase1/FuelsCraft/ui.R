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
        h4("FastFuels API"),
        textInput("api_key", "API Key", value = "b6a21fbe22c54a1890ce0d2252d2f584"),
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
        fluidRow(
          actionButton("clearMap", "Clear Map", disabled=FALSE), 
        ), 
        leafletOutput("map"),
        # put the checkboxes on the same row
        fluidRow(
          column(6, checkboxInput("showFFTrees", "Show FastFuels Trees", value = TRUE)),
          column(6, checkboxInput("showCustomTrees", "Show Custom Trees", value = TRUE))
        ),
        # create two sub-panels, one for ff, one for custom
        tabsetPanel(
          tabPanel("Inventory from FastFuels", 
                   br(),
                   actionButton("removeFFTreesByPolygon", "Remove Trees in polygon", disabled=FALSE),
                   actionButton("addFFTreesByPolygon", "Add Trees in polygon", disabled=FALSE),
                   fluidRow(
                     column(6, sliderInput("ffTreeHeight", "Tree Height", min = 0, max = 60, value = c(0,60)), 
                                   actionButton("resetFFTreeInventory", "Reset", disabled=FALSE)),
                     column(6, plotOutput("ffHeightHist", width="250px", height="200px"))
                   ),
          ),
          tabPanel("Optional: Your Custom Inventory", 
                   br(),
                   textInput("customTreeInventoryFile", "Custom Inventory File", value="tree_inventory_test1_custom.csv"),
                   actionButton("loadCustomTreeInventory", "Load", disabled=FALSE),
                   actionButton("cropCustomPolygon", "Crop", disabled=FALSE),

                   actionButton("removeCustomTreesByPolygon", "Remove Trees in polygon", disabled=FALSE),
                   fluidRow(
                     column(6, sliderInput("customTreeHeight", "Tree Height", min = 0, max = 60, value = c(0,60)), 
                               actionButton("resetCustomTreeInventory", "Reset", disabled=FALSE)),
                     column(6, plotOutput("customHeightHist", width="250px", height="200px"))
                   ),
          )
        ),
        actionButton("mergeInventories", "Merge Custom into FF", disabled=FALSE),
        actionButton("uploadInventory", "Upload Modified FF Inventory", disabled=FALSE),
        verbatimTextOutput("area"),

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
