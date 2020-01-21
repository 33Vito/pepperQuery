
# ------------loading packages------------
library(tidyverse)
library(bigrquery)
library(googleAuthR)
library(shiny)
library(shinycssloaders)
library(shinydashboard)

# ------------google authenticate------------
gar_gce_auth()

# ------------utility functions------------
withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "output")
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}

# ------------builing UI------------
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
  fluidRow(
    column(
      id = "InputPanel",
      width = 5,

      br(), 
      tabsetPanel(
        tabPanel("SQL", 
                 textAreaInput("inputSQL", "SQL to run:", 
                               "SELECT * FROM `bigquery-public-data.chicago_crime.crime` LIMIT 1000", 
                               width = "600", height = "400px"),
                 actionButton("SubmitSQLButton", "Submit SQL code")), 
        tabPanel("R", 
                 textAreaInput("inputR", "R to run:", 
                               "tbl %>% count()", 
                               width = "600px", height = "400px"),
                 actionButton("SubmitRButton", "Submit R code")
                 )
      ),
      hr(), 
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
      div(style="display: inline-block;vertical-align:top; padding-top: 0px;",
          selectizeInput("downloadDataFormat", "Format", c("csv", "xlsx"), width = "80px")),
      div(style="display: inline-block;vertical-align:top; padding-top: 25px;",
          downloadButton("downloadData", "Download data from active tab")), 
      
      # tagAppendAttributes(
      #   textOutput("inputTabsString"), 
      #   style="white-space:pre-wrap;")
      ),
    
    column(7, 
           tabsetPanel(
             tabPanel("R console", 
                      hr(),
                      verbatimTextOutput("RConsoleOutput")
                      ),
             tabPanel("R table", 
                      hr(),
                      DT::dataTableOutput("RTableOutput")
                      ),
             tabPanel("R plot", 
                      hr(),
                      plotOutput("RPlotOutput", height = "550px")
                      ), 
             tabPanel("R billing", 
                      hr(),
                      verbatimTextOutput("RBillingOutput")
             )
           )),
    
    column(
      id = "mainPanel", 
      width = 12, 
      tabsetPanel(id = "activeTab",
                  tabPanel("Home", 
                           br(), 
                           DT::dataTableOutput("Home_tbl"), 
                           hr(), 
                           tagAppendAttributes(textOutput("Home_sql"), style="white-space:pre-wrap;"))
      )
    )
  )
)

# ------------building server------------
server <- function(input, output, session) {
  
  reactiveValueList <- reactiveValues(tabList = "Home")
  
  observeEvent(input$addTabButton, {
    
    if (input$addTabName == "") {
      showNotification("Tab name cannot be empty")
    } else if (input$addTabName %in% reactiveValueList$tabList) {
      showNotification("Tab name already added, tab name must be unique.")
    } else if (str_detect(input$addTabName, "^[0-9]")) {
      showNotification("Tab name must start with letter.")
    } else if (str_detect(input$addTabName, " ")) {
      showNotification("Tab name cannot contain space.")
    } else {
      reactiveValueList$tabList <- c(reactiveValueList$tabList, input$addTabName)
      insertTab(inputId = "activeTab",
                tabPanel(input$addTabName, 
                         br(), 
                         tagAppendAttributes(textOutput(paste0(input$addTabName, "_sql")), 
                                             style="white-space:pre-wrap;"), 
                         hr(), 
                         DT::dataTableOutput(paste0(input$addTabName, "_tbl"))), 
                target = "Home", position = "before")
      }
    })
  
  observeEvent(input$removeTabButton, {
    reactiveValueList$tabList <- reactiveValueList$tabList[
      reactiveValueList$tabList != input$removeTabName]
    removeTab(inputId = "activeTab", target = input$removeTabName)
    reactiveValueList[[paste0(input$activeTab, "_sql")]] <- NULL
    reactiveValueList[[paste0(input$activeTab, "_tbl")]] <- NULL
    })
  
  output$inputTabsString <- renderText({
    paste0("All tabs: ", paste(reactiveValueList$tabList, collapse=" "), ";\n", 
           "Active tab: ", input$activeTab, ";\n"
           )
    })
  
  observeEvent(input$SubmitSQLButton, {
    reactiveValueList[[paste0(input$activeTab, "_sql")]] <-
      input$inputSQL
    
    withCallingHandlers(
    sql_sent <- tryCatch(
        bq_project_query("scg-dai-sci-dev", input$inputSQL), 
        error = function(e) e)
    , message = function(m) output$RBillingOutput <- renderPrint(m$message))
    
    if ("error" %in% class(sql_sent)) {
      showNotification(sql_sent$message)
    } else {
      reactiveValueList[[paste0(input$activeTab, "_tbl")]] <-
        bq_table_download(sql_sent, page_size = 1000)
      
      output[[paste0(input$activeTab, "_sql")]] <- renderText({
        reactiveValueList[[paste0(input$activeTab, "_sql")]]
      })
      
      output[[paste0(input$activeTab, "_tbl")]] <-
        DT::renderDataTable({
          reactiveValueList[[paste0(input$activeTab, "_tbl")]] %>%
            DT::datatable(
              filter = "top", 
              extensions = 'Buttons',
              options = list(
                dom = 'Blfrtip',
                buttons = c('copy', 'csv', 'pdf'),
                pageLength = 10,
                lengthMenu = c(10, 50, 100, 200)
              )
            )
        }, server = T)
    }
  })

  # output$Home_sql <- renderText({
  #   reactiveValueList$Home_sql
  # })

  # output$Home_tbl <- DT::renderDataTable({
  #   reactiveValueList$Home_tbl %>%
  #     DT::datatable(extensions = 'Buttons', 
  #                   options = list(dom = 'Bfrtip',
  #                                  buttons = c('copy', 'csv', 'pdf')))
  # })
  
  RCodeListener <- eventReactive(input$SubmitRButton, {input$inputR})
  
  output$RPlotOutput <- renderPlot({
    RCodeListener() %>% 
      paste0("reactiveValueList$", input$activeTab, "_", .) %>% 
      parse(text=.) %>% eval()
  })

  output$RConsoleOutput <- renderPrint({
    RCodeListener() %>%
      paste0("reactiveValueList$", input$activeTab, "_", .) %>%
      parse(text=.) %>% eval()
  })
  
  output$RTableOutput <- DT::renderDataTable({
    RCodeListener() %>%
      paste0("reactiveValueList$", input$activeTab, "_", .) %>%
      parse(text=.) %>% eval() %>% 
      DT::datatable(extensions = 'Buttons', 
                    filter = "top", 
                    options = list(dom = 'Blfrtip',
                                   buttons = c('copy', 'csv', 'pdf'), 
                                   pageLength = 10,
                                   lengthMenu = c(10, 50, 100, 200)))
  }, server = FALSE)
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename <- function() {
      paste0(input$activeTab, ".", input$downloadDataFormat)
    },
    content <- function(file) {
      switch(input$downloadDataFormat,
      "csv" = write.csv(reactiveValueList[[paste0(input$activeTab, "_tbl")]], 
                        file, row.names = FALSE,  na = ""), 
      "xlsx" = openxlsx::write.xlsx(reactiveValueList[[paste0(input$activeTab, "_tbl")]], 
                                    file)
      )
    }
  )
  
  session$allowReconnect("force") # to allow reconnection to restore state
  onStop(fun = function() {
    # str(isolate(session$clientData))
    # reactiveValueList <- reactiveValueList()
  })
}

shinyApp(ui, server)

## Demo code

# tbl %>% str()
# 
# tbl %>% 
#   count(primary_type, arrest) %>% 
#   spread(arrest, n, sep="_")
# 
# tbl %>% 
#   count(primary_type, arrest) %>% 
#   ggplot(aes(x=reorder(primary_type,n,sum), y=n, fill=arrest)) + 
#   geom_col() + 
#   coord_flip() + 
#   xlab("Crime type") + 
#   ylab("Count") + 
#   theme_gray(base_size = 15)





