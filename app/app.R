library(shiny)
library(tidyverse)

# Read in cleaned data
bank <- readRDS("bank_clean.rds")

# Define UI for application
ui <- fluidPage(
  # Title
  titlePanel("Bank Marketing Explorer"),

  # Side Bar
  sidebarLayout(
    sidebarPanel(
      h3("Subset the Data"),
      p("(INSERT Subsetting widgets and button)")
    ),
    
    # Main Panel
    mainPanel(
      tabsetPanel(
        ### ABOUT TAB ###
        tabPanel("About",
                 h3("Purpose of the App"),
                 p("(PURPOSE)"),
                 
                 h3("The Data"),
                 p("(DESCRIBE DATA)"),
                 p("Source:",
                   a("Banking Dataset on Kaggle",
                     href = "https://www.kaggle.com/datasets/prakharrathi25/banking-dataset-marketing-targets"),
                   ),
                 p("(NOTES?)"),
                 
                 # Image?

        ),
        
        
        ### DATA DOWNLOAD TAB ###
        tabPanel("Data Download",
                 p("(INSERT Data table and download button)")
        ),
        
        
        ### DATA EXPLORATION TAB ###
        tabPanel("Data Exploration",
                 tabsetPanel(
                   tabPanel("Categorical Summaries",
                            p("(INSERT Contingency tables and bar charts)")
                   ),
                   tabPanel("Numeric Summaries",
                            p("(INSERT Summary statistics and plots)")
                   )
                 )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Placeholder
}


# Run the application 
shinyApp(ui = ui, server = server)
