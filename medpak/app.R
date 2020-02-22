library(shiny)
library(DT)
library(readxl)
library(data.table)
library(tidyverse)
readRDS("data.rds")


# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Reactivity"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            fileInput("file1", "Zgjidh skedarin Excel", buttonLabel = "Zgjidh...",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv",".
                  xls", ".xlsx")
            ),
            tags$hr(),
            
            # Input: Selector for choosing dataset ----
            selectInput(inputId = "dataset", selected = NULL, 
                        label = "Zgjidhni indikatorin:",
                        choices = c("", "Diagnoza", "Njesite administrative",
                                    "Ka nje kujdestar me pagese",
                                    "Nevoja per karrige me rrota", "Nevoja per paterica")),
            
            # Output: Verbatim text for data summary ----
            tableOutput("summary")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(

            DT::dataTableOutput("table")
            
        )
    )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
    
    datasetInput <- reactive({
        inFile <- input$file1
        if (!is.null(inFile)){
            data <- read_excel(inFile$datapath)
            data <- data[, -c(1:2)]        }
        else {
            data <- data[, -c(1:2)]
            data
        }    
        diagnose <- as.data.frame(table(data$`Diagnoza mjeksore`)) %>% 
        set_names('Diagnoza mjeksore', 'Numri')
        unit <-as.data.frame(table(data$Adresa)) %>% 
            set_names('Adresa', 'Numri')
        survey <- as.data.frame(sum(!is.na(data$`Prindi/Kujdestari`)))%>% 
            set_names('Numri')
        chair <- as.data.frame(table(data$`Nevoja per karrike me rrota`)) %>% 
            set_names('Nevoja per karrike me rrota', 'Numri')
        paterica <- as.data.frame(table(data$`Per paterica`)) %>% 
            set_names('Nevoja per paterica', 'Numri')
        switch(input$dataset,
               "Diagnoza" = diagnose,
               "Njesite administrative" = unit,
               "Ka nje kujdestar me pagese" = survey,
               "Nevoja per karrige me rrota" = chair,
               "Nevoja per paterica" = paterica)
        
        
    })
    

    
    # Generate a summary of the dataset ----
    # The output$summary depends on the datasetInput reactive
    # expression, so will be re-executed whenever datasetInput is
    # invalidated, i.e. whenever the input$dataset changes
    output$summary <- renderTable({
        dataset <- datasetInput()
        dataset
    })
    
     
    # Filter data based on selections
    output$table <-  DT::renderDataTable({
    inFile <- input$file1 
        if (!is.null(inFile)) {
                data <- read_excel(inFile$datapath)
                DT::datatable(data, 
                              extensions = c('Buttons','Scroller', 'SearchPanes'), 
                              options = list(
                                  dom = 'Bfrtip',
                                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                  lengthMenu = c(5, 30, 50), pageLength = 15,
                                  language = list(
                                      search = 'Kërko',
                                      paginate = list(previous = 'Pas', `next` = 'Para'),
                                      info = 'Po, _TOTAL_ rekorde nga _START_ deri _END_'
                                  ),  
                                  initComplete = JS(
                                      "function(settings, json) {",
                                      "$(this.api().table().header()).css({'background-color': '#ad1d28', 'color': '#fff'});",
                                      "}"),
                                  searchHighlight = TRUE,
                                  autoWidth = TRUE))
                
        }
        else {
            DT::datatable(data, 
            extensions = c('Buttons','Scroller', 'SearchPanes'), 
            options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                lengthMenu = c(5, 30, 50), pageLength = 15,
                language = list(
                    search = 'Kërko',
                    paginate = list(previous = 'Pas', `next` = 'Para'),
                    info = 'Po, _TOTAL_ rekorde nga _START_ deri _END_'
                ),  
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#ad1d28', 'color': '#fff'});",
                    "}"),
                searchHighlight = TRUE,
                autoWidth = TRUE))
        }
    
    
    })
}

# Create Shiny app ----
shinyApp(ui, server)