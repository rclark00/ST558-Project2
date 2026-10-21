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
      # Job group button
      radioButtons("job_sub",
                   "Job Group",
                   choiceNames = c("All", "White-collar", "Blue-collar/Service",
                                   "Not working", "Unknown"),
                   choiceValues = c("all", "White-collar", "Blue-collar/Service",
                                    "Not working", "Unknown")),
      # Education button
      radioButtons("edu_sub",
                   "Education Level",
                   choiceNames = c("All", "Primary", "Secondary", "Tertiary", "Unknown"),
                   choiceValues = c("all", "primary", "secondary", "tertiary", "unknown")),
      # Previously contacted button
      radioButtons("prev_sub",
                   "Contacted in a Previous Campaign?",
                   choiceNames = c("All", "No", "Yes"),
                   choiceValues = c("all", "No", "Yes")),

      actionButton("subset_button", "Sample the data")    
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
