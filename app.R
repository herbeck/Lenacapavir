# Install required packages if not already installed
# Uncomment the following lines to install the packages
# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("shinythemes")

library(shiny)
library(ggplot2)
library(shinythemes)

# Define UI for the dashboard
ui <- fluidPage(
  
  # Apply a clean theme
  theme = shinytheme("cosmo"),
  
  # Application title
  titlePanel("Lenacapavir PrEP Cost-Effectiveness Dashboard"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      width = 3,  # Adjust the width as needed
      
      # Input: $/LEN
      sliderInput("price_len",
                  "$/LEN (USD):",
                  min = 20,
                  max = 200,
                  value = 80,
                  step = 20,
                  post = " USD"),
      
      # Input: HIV Incidence
      sliderInput("incidence",
                  "HIV Incidence (0.01 = 1%; 1 infection per 100 person years):",
                  min = 0.001,    # Set minimum to 0.001 to avoid division by zero
                  max = 0.05,     # Set maximum to 0.05 to align with plot's x-axis
                  value = 0.01,
                  step = 0.005),
      
      # Input: Lifetime Treatment Cost
      sliderInput("cost_treatment",
                  "Lifetime Treatment Cost (USD):",
                  value = 10000,
                  min = 2000,     # Updated minimum to match plot's x-axis
                  max = 20000,
                  step = 500),
      
      # Input: DALYs per HIV Infection
      sliderInput("dalys",
                  "DALYs per HIV Infection:",
                  value = 20,
                  min = 0,
                  max = 50,
                  step = 5),
      
      hr(),
      
      # Calculated Outputs
      h4("Calculated Output"),
      
      # Output: $/DALY
      verbatimTextOutput("dalysOutput")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      width = 9,
      
      # Tabs for different sections
      tabsetPanel(
        
        # Tab 1: $/DALY Calculations
        tabPanel("$/DALY Averted Calculations",
                 
                 # Calculation Breakdown
                 fluidRow(
                   column(12,
                          wellPanel(
                            h4("Calculation Breakdown"),
                            verbatimTextOutput("calculationBreakdown")
                          )
                   )
                 ),
                 
                 # Plots
                 fluidRow(
                   column(6,
                          wellPanel(
                            h4("$/DALY Averted vs HIV Incidence"),
                            plotOutput("plot_incidence")
                          )
                   ),
                   column(6,
                          wellPanel(
                            h4("$/DALY Averted vs Lifetime Treatment Cost"),
                            plotOutput("plot_cost_treatment")
                          )
                   )
                 )
        ),
        
        # Tab 2: Documentation
        tabPanel("Documentation",
                 fluidRow(
                   column(12,
                          wellPanel(
                            h4("Dashboard Documentation"),
                            fluidPage(
                              h4("Overview"),
                              p("This dashboard calculates and visualizes the Cost per Disability-Adjusted Life Year (\\$/DALY) for HIV prevention strategies using Lenacapavir (LEN). Adjust the input parameters to explore how different factors influence the cost-effectiveness of interventions."),
                              
                              h4("Inputs"),
                              tags$ul(
                                tags$li(strong("$/LEN (USD):"), " The cost of Lenacapavir (LEN) prevention."),
                                tags$li(strong("HIV Incidence:"), " The rate of new HIV infections in a population."),
                                tags$li(strong("Lifetime Treatment Cost (USD):"), " The total cost associated with lifetime ART (Antiretroviral Therapy) treatment."),
                                tags$li(strong("DALYs per HIV Infection:"), " The number of Disability-Adjusted Life Years associated with one HIV infection.")
                              ),
                              
                              h4("Outputs"),
                              tags$ul(
                                tags$li(strong("$/DALY averted:"), " The calculated cost per Disability-Adjusted Life Year averted based on the input parameters.")
                              ),
                              
                              h4("Equations"),
                              withMathJax(
                                helpText(
                                  "\\[ \\text{\\$/DALY averted} = \\frac{\\left(\\frac{1}{\\text{Incidence}} \\times \\text{Cost LEN}\\right) - \\text{Cost Treatment}}{\\text{DALYs per HIV Infection}} \\]"
                                ),
                                helpText(
                                  "Where:",
                                  "\\[ \\text{Cost LEN} = \\text{Price of LEN} + 20 \\]"
                                )
                              ),
                              
                              h4("Example Calculation"),
                              p("Given the following parameters:"),
                              tags$ul(
                                tags$li("HIV Incidence = 1% (0.01)"),
                                tags$li("$/LEN = $100"),
                                tags$li("Cost Treatment = $10,000"),
                                tags$li("DALYs per HIV Infection = 20")
                              ),
                              p("First, calculate Cost LEN:"),
                              withMathJax(
                                helpText("\\[ \\text{Cost LEN} = 100 + 20 = 120 \\]")
                              ),
                              p("Then, calculate $/DALY averted:"),
                              withMathJax(
                                helpText("\\[ \\text{\\$/DALY averted} = \\frac{\\left(\\frac{1}{0.01} \\times 120\\right) - 10000}{20} = \\frac{(100 \\times 120) - 10000}{20} = 100 \\]")
                              )
                            )
                          )
                   )
                 )
        )
      )
    )
  )
)

# Define server logic required to calculate $/DALY and generate plots
server <- function(input, output) {
  
  # Reactive expression to calculate $/DALY
  calculate_daly <- reactive({
    # Extract input values
    price_len <- input$price_len
    incidence <- input$incidence         # Already in proportion (0.001 to 0.05)
    cost_treatment <- input$cost_treatment
    dalys <- input$dalys
    
    # Calculate Cost LEN
    cost_len <- price_len + 20
    
    # Prevent division by zero or negative DALYs
    if (incidence <= 0 || dalys <= 0) {
      return(NA)
    }
    
    # Calculate $/DALY using the provided formula
    daly_value <- ( ( (1 / incidence) * cost_len)  - cost_treatment) / dalys
    
    return(daly_value)
  })
  
  # Render $/DALY output
  output$dalysOutput <- renderText({
    daly <- calculate_daly()
    
    if (is.na(daly)) {
      "$/DALY: Undefined (Check input values)"
    } else {
      paste0("$/DALY averted: $", format(round(daly, 2), big.mark = ","))
    }
  })
  
  # Render Calculation Breakdown
  output$calculationBreakdown <- renderText({
    price_len <- input$price_len
    incidence <- input$incidence
    cost_treatment <- input$cost_treatment
    dalys <- input$dalys
    cost_len <- price_len + 20
    
    if (incidence <= 0 || dalys <= 0) {
      return("$/DALY averted: Undefined (Incidence and DALYs must be greater than 0)")
    }
    
    # Calculate $/DALY averted
    daly_value <- (((1 / incidence) * cost_len) - cost_treatment) / dalys
    
    paste0("$/DALY averted = ((1 / ", incidence, ") * ", cost_len, " - ", cost_treatment, ") / ", dalys, " = $", round(daly_value, 2))
  })
  
  # Plot $/DALY averted vs HIV Incidence
  output$plot_incidence <- renderPlot({
    incidence_seq <- seq(0.001, 0.05, by = 0.001)  
    price_len <- input$price_len
    cost_treatment <- input$cost_treatment
    dalys <- input$dalys
    
    cost_len <- price_len + 20
    daly_values <- ((1 / incidence_seq) * cost_len - cost_treatment) / dalys
    
    # Remove any non-finite values to prevent mismatched row counts
    valid_indices <- is.finite(daly_values)
    incidence_seq <- incidence_seq[valid_indices]
    daly_values <- daly_values[valid_indices]
    
    data <- data.frame(HIV_Incidence = incidence_seq, daly_cost = daly_values)
    
    ggplot(data, aes(x = HIV_Incidence, y = daly_cost)) +
      geom_line(color = "darkgreen") +
      geom_point(color = "darkgreen") +
      labs(title = "$/DALY averted vs HIV Incidence",
           x = "HIV Incidence",
           y = "$ per DALY averted") +
      scale_x_continuous(limits = c(0, 0.05)) +
      scale_y_continuous(limits = c(-500, 2000)) +
      theme_minimal()
  })
  
  # Plot $/DALY vs Lifetime Treatment Cost
  output$plot_cost_treatment <- renderPlot({
    cost_treatment_seq <- seq(2000, 20000, by = 500)  # 2000 to 20000 by 500: 37 points
    price_len <- input$price_len
    incidence <- input$incidence
    dalys <- input$dalys
    
    cost_len <- price_len + 20
    daly_values <- ((1 / incidence) * cost_len - cost_treatment_seq) / dalys
    
    # Remove any non-finite values to prevent mismatched row counts
    valid_indices <- is.finite(daly_values)
    cost_treatment_seq <- cost_treatment_seq[valid_indices]
    daly_values <- daly_values[valid_indices]
    
    data <- data.frame(Lifetime_Treatment_Cost = cost_treatment_seq, daly_cost = daly_values)
    
    ggplot(data, aes(x = Lifetime_Treatment_Cost, y = daly_cost)) +
      geom_line(color = "purple") +
      geom_point(color = "purple") +
      labs(title = "$/DALY averted vs Lifetime Treatment Cost",
           x = "Lifetime Treatment Cost (USD)",
           y = "$ per DALY averted") +
      scale_x_continuous(limits = c(2000, 20000)) +
      scale_y_continuous(limits = c(-500, 2000)) +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
