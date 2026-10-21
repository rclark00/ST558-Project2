library(shiny)
library(tidyverse)

# Read in cleaned data
bank <- readRDS("bank_clean.rds")

# Define numeric variables ("label for user" = "column")
numeric_vars <- c("Age (years)" = "age",
                  "Balance (euros)" = "balance",
                  "Last Call Duration (seconds)" = "duration",
                  "Contacts This Campaign" = "campaign",
                  "Contacts Before This Campaign" = "previous")

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
      
      # User select first numeric variable and slider
      selectizeInput("num1_var",
                     "Numeric Variable 1",
                     choices = numeric_vars,
                     selected = "age"),
      uiOutput("num1_slider"),
      
      # User select second numeric variable and slider
      selectizeInput("num2_var",
                     "Numeric Variable 2",
                     choices = numeric_vars,
                     selected = "campaign"),
      uiOutput("num2_slider"),
      
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
                 # Show download button once data has been selected
                 conditionalPanel("input.subset_button",
                                  downloadButton("download_data", "Download Data (.csv)")),
                 br(),
                 DT::dataTableOutput("data_table")
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
  # This object holds data subset
  subset_vals <- reactiveValues(data = NULL)  # NULL default
  
  # Dynamic sliders
  output$num1_slider <- renderUI({
    var_values <- bank |> pull(input$num1_var)
    sliderInput("num1_range",
                "Range:",
                min = min(var_values),
                max = max(var_values),
                value = c(min(var_values), max(var_values)))
  })
  
  output$num2_slider <- renderUI({
    var_values <- bank |> pull(input$num2_var)
    sliderInput("num2_range",
                "Range:",
                min = min(var_values),
                max = max(var_values),
                value = c(min(var_values), max(var_values)))
  })
  
  # Prevent dropdowns from choosing same variable
  observeEvent(input$num1_var, {
    num1 <- input$num1_var
    num2 <- input$num2_var
    choices <- numeric_vars
    if (num1 != num2){
      choices <- choices[-which(choices == num1)]
      updateSelectizeInput(session,
                           "num2_var",
                           choices = choices,
                           selected = num2)
    }
  })
  observeEvent(input$num2_var, {
    num1 <- input$num1_var
    num2 <- input$num2_var
    choices <- numeric_vars
    if (num1 != num2){
      choices <- choices[-which(choices == num2)]
      updateSelectizeInput(session,
                           "num1_var",
                           choices = choices,
                           selected = num1)
    }
  })
  
  # observeEvent() to look for action button
  observeEvent(input$subset_button, {
    if(input$job_sub == "all"){
      job_vals <- levels(bank$job_group)
    } else {
      job_vals <- input$job_sub
    }
    
    if(input$edu_sub == "all"){
      edu_vals <- levels(bank$education)
    } else {
      edu_vals <- input$edu_sub
    }
    
    if(input$prev_sub == "all"){
      prev_vals <- levels(bank$prev_contacted)
    } else {
      prev_vals <- input$prev_sub
    }
    
    subset_vals$data <- bank |>
      filter(job_group %in% job_vals,
             education %in% edu_vals,
             prev_contacted %in% prev_vals,
             get(input$num1_var) >= input$num1_range[1],
             get(input$num1_var) <= input$num1_range[2],
             get(input$num2_var) >= input$num2_range[1],
             get(input$num2_var) <= input$num2_range[2])
  })
  # Data table of the subsetted data
  output$data_table <- DT::renderDataTable({
    # Default message
    validate(
      need(!is.null(subset_vals$data), "Please choose your subset on the sidebar and click the 'Sample the data' button.")
    )
    DT::datatable(data = subset_vals$data,
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  
  # Download the subsetted data
  output$download_data <- downloadHandler(
    filename = function() {
      "bank_subset.csv"
    },
    content = function(file) {
      write_csv(subset_vals$data, file)
    }
  )
}


# Run the application 
shinyApp(ui = ui, server = server)
