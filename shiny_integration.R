library(shiny)
ui <- fluidPage(    
  titlePanel(""),
  sidebarLayout(      
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      textInput("dem_name","Democratic Candidate"),
      textInput("rep_name","Republican Candidate"),
      selectInput("elec_geog", "Geography:", 
                  choices=c("County", "Media Market")),
      selectInput("result_present", "Result Presentation:", 
                  choices=c("Two-Way", "Three-Way")),
      sliderInput("width", "Counties Displayed:", min = 5, max = 20, value = 10),
      hr(),
      helpText("Data from AT&T (1961) The World's Telephones.")
    ),
    mainPanel()
  )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)