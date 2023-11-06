library(shiny)

# Define UI for data download app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Download any Dataset"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Choose dataset ----
      selectInput("dataset", "Choose a dataset:",
                  choices = c("penguins", "cats", "dogs")),
      
      # Button
      downloadButton("downloadData", "Download")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      h1("Let's explore datasets!"),
      h4("Select your favourite animal and click on the download button to explore the respective datasets!"),
      
      p("Each dataset contains basic information about the", em("different characteristics"), "of each animal."),
      
      conditionalPanel(
        condition = "input.dataset === 'penguins'",
        img(src = "penguins.avif", height = 140, width = 300)
      ),
      
      conditionalPanel(
        condition = "input.dataset === 'cats'",
        img(src = "cats.jpeg", height = 140, width = 300)
      ),
      
      conditionalPanel(
        condition = "input.dataset === 'dogs'",
        img(src = "dogs.jpeg", height = 140, width = 300)
      ),
      
      tableOutput("table")
      
    )
    
  )
)

# Define server logic to display and download selected file ----
server <- function(input, output) {
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "penguins" = "penguins.csv",
           "cats" = "cat-lovers.csv",
           "dogs" = "dogs.csv")
  })
  
  # Table of selected dataset ----
  output$table <- renderTable({
    read.csv(datasetInput())
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(read.csv(datasetInput()), file, row.names = FALSE)
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)
