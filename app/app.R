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

# Define categorical variables ("label for user" = "column")
cat_vars <- c("Job Group" = "job_group",
              "Job" = "job",
              "Marital Status" = "marital",
              "Education" = "education",
              "Credit in Default" = "default",
              "Housing Loan" = "housing",
              "Personal Loan" = "loan",
              "Contact Type" = "contact",
              "Month of Last Contact" = "month",
              "Previous Campaign Outcome" = "poutcome",
              "Previously Contacted" = "prev_contacted",
              "Subscribed" = "y")

# Define UI for application
ui <- fluidPage(
  # Title
  titlePanel("Bank Marketing Explorer"),

  # Side Bar
  sidebarLayout(
    sidebarPanel(
      h3("Subset the data"),
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
      
      actionButton("subset_button", "Subset the data")    
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
                            fluidRow(
                              column(6,
                                     selectizeInput("cat_var1",
                                                    "Variable 1",
                                                    choices = cat_vars,
                                                    selected = "job_group")),
                              column(6,
                                     selectizeInput("cat_var2",
                                                    "Variable 2 (optional)",
                                                    choices = c("None" = "none", cat_vars),
                                                    selected = "y"))
                            ),
                            h4("Contingency Table"),
                            tableOutput("cat_table"),
                            h4("Bar Chart"),
                            # Show bar type when there's a fill variable
                            conditionalPanel("input.cat_var2 != 'none'",
                                             radioButtons("bar_position",
                                                          "Bar Type",
                                                          choiceNames = c("Stacked", "Side-by-side", "Filled (proportions)"),
                                                          choiceValues = c("stack", "dodge", "fill"),
                                                          inline = TRUE)),
                            plotOutput("cat_plot")
                   ),
                   tabPanel("Numeric Summaries",
                            fluidRow(
                              column(6,
                                     selectizeInput("num_var",
                                                    "Numeric Variable",
                                                    choices = numeric_vars,
                                                    selected = "age")),
                              column(6,
                                     selectizeInput("num_group",
                                                    "Summarize Across",
                                                    choices = c("None" = "none", cat_vars),
                                                    selected = "y"))
                            ),
                            h4("Summary Statistics"),
                            tableOutput("num_table")
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
      need(!is.null(subset_vals$data), "Please choose your subset on the sidebar and click the 'Subset the data' button.")
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
  # One-way or two-way contingency table
  output$cat_table <- renderTable({
    # Message prior to subsetting
    validate(
      need(!is.null(subset_vals$data), "Please choose your subset on the sidebar and click the 'Subset the data' button.")
    )
    # Case: Same variable chosen twice
    validate(
      need(input$cat_var1 != input$cat_var2, "Please choose two different variables.")
    )
    
    if(input$cat_var2 == "none"){
      # One-way table
      subset_vals$data |>
        group_by(!!sym(input$cat_var1)) |>
        summarize(count = n())
    } else {
      # Two-way table
      two_way <- subset_vals$data |>
        group_by(!!sym(input$cat_var1), !!sym(input$cat_var2)) |>
        summarize(count = n(), .groups = "drop") |>
        pivot_wider(names_from = !!sym(input$cat_var2), values_from = count, values_fill = 0)
      
      # Case: Variable 2 = Subscribed --> Add %
      if(input$cat_var2 == "y"){
        two_way <- two_way |>
          mutate(pct_yes = round(100 * yes / (no + yes), 1))
      }
      two_way
    }
  }, digits = 1)
  
  # Bar chart
  output$cat_plot <- renderPlot({
    # Message prior to subsetting
    validate(
      need(!is.null(subset_vals$data), "Please choose your subset on the sidebar and click the 'Subset the data' button.")
    )
    # Case: Same variable chosen twice
    validate(
      need(input$cat_var1 != input$cat_var2, "Please choose two different variables.")
    )
    
    # Variable 1 labels
    x_label <- names(cat_vars)[cat_vars == input$cat_var1]
    
    if(input$cat_var2 == "none"){
      # Single variable: bar chart of counts
      g <- ggplot(subset_vals$data, aes_string(x = input$cat_var1)) +
        geom_bar() +
        labs(x = x_label,
             y = "Number of Clients",
             title = paste("Clients by", x_label))
    } else {
      # Two variables: Variable 2 fills the bars
      fill_label <- names(cat_vars)[cat_vars == input$cat_var2]
      if(input$bar_position == "fill"){
        y_label <- "Proportion of Clients"
      } else {
        y_label <- "Number of Clients"
      }
      g <- ggplot(subset_vals$data, aes_string(x = input$cat_var1, fill = input$cat_var2)) +
        geom_bar(position = input$bar_position) +
        labs(x = x_label,
             y = y_label,
             title = paste(fill_label, "by", x_label)) +
        scale_fill_discrete(fill_label)
    }
    
    # Tilt x-axis labels
    g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Summary statistics (numeric with possible categorical)
  output$num_table <- renderTable({
    # Message prior to subsetting
    validate(
      need(!is.null(subset_vals$data), "Please choose your subset on the sidebar and click the 'Subset the data' button.")
    )
    
    num_data <- subset_vals$data
    
    # Case: categorical variable ("Summarized Across") --> group
    if(input$num_group != "none"){
      num_data <- num_data |>
        group_by(!!sym(input$num_group))
    }
    
    num_data |>
      summarize(n = n(),
                mean = mean(!!sym(input$num_var)),
                median = median(!!sym(input$num_var)),
                sd = sd(!!sym(input$num_var)))
  }, digits = 1)
}


# Run the application 
shinyApp(ui = ui, server = server)
