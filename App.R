
library(tidyverse)
library(bigrquery)
library(shiny)
library(shinycssloaders)
library(shinydashboard)

ui <- fluidPage(
  # css
  tags$head(tags$style(
    HTML(
      '
      #sidebar {
      background-color: transparent;
      border-color: transparent
      }
      
      body, label, input, button, select {
      font-family: "Arial";
      }'
    )
  )),
  
  # Application title
  titlePanel("pepperQuery 1.0 Alpha"),
  h4("Tony Liu, Jan 2020"),
  
  # Sidebar with options add/remove tabs
  sidebarLayout(
    sidebarPanel(
      id = "sidebar",
      width = 5,
      
      div(style="display: inline-block;vertical-align:top; padding-top: 0px;",
          textInput("addTabName", "Tab to add: ", "", width = "180px")), 
      div(style="display: inline-block;vertical-align:top; padding-top: 25px;",
          actionButton("addTabButton", "Add tab")),
      # br(),
      div(style="display: inline-block;vertical-align:top; padding-top: 25px;",
          br()),
      div(style="display: inline-block;vertical-align:top; padding-top: 0px;",
          textInput("removeTabName", "Tab to remove: ", "", width = "180px")), 
      div(style="display: inline-block;vertical-align:top; padding-top: 25px;",
          actionButton("removeTabButton", "Remove tab")),
      
      br(), 
      textAreaInput("inputSQL", "SQL to run:", "", width = "550px", height = "400px"),
      # verbatimTextOutput("value")
      actionButton("SubmitSQLButton", "Submit Query")
      ),
    
    mainPanel(
      id = "mainPanel", 
      width = 5, 
      textOutput("inputTabsString"),
      tabsetPanel(id = "tabs",
                  tabPanel("Home", 
                           "This is the home tab")
      )
    )
  )
)

server <- function(input, output, session) {
  
  reactiveValueList <- reactiveValues(tabList = "")
  
  observeEvent(input$addTabButton, {
    
    if (input$addTabName == "") {
      showNotification("Tab name cannot be empty")
    } else if (input$addTabName %in% reactiveValueList$tabList) {
      showNotification("Tab already added, tab name must be unique.")
    } else {
      reactiveValueList$tabList <- c(reactiveValueList$tabList, input$addTabName)
      insertTab(inputId = "tabs",
                tabPanel(input$addTabName), 
                target = "Home", position = "before")
    }
  })
  observeEvent(input$removeTabButton, {
    reactiveValueList$tabList <- reactiveValueList$tabList[
      reactiveValueList$tabList != input$removeTabName]
    removeTab(inputId = "tabs", target = input$removeTabName)
  })
  
  output$inputTabsString <- renderText({
    reactiveValueList$tabList
  })
}

shinyApp(ui, server)







