library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

# Load data
admissions <- read_csv("Data/first_time_clients_by_county.csv") %>%
  mutate(County = if_else(County == "Total", "Statewide Total", County))

rates <- read_csv("Data/admission_rates_by_county_and_drug.csv")
percent_within <- read_csv("Data/admissions_by_primary_drug_within_county.csv")
unmet_demand <- read_csv("Data/met_unmet_treatment_demand.csv")
age_gender <- read_csv("Data/admissions_by_age_gender_drug.csv")

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = span("NJ Substance Use", style = "color: white;")),
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
        .skin-purple .main-header .logo {
          background-color: #693699;
          color: white;
        }
        .skin-purple .main-header .logo:hover {
          background-color: #5e2f8e;
        }
        .skin-purple .main-header .navbar {
          background-color: #111;
        }
        .skin-purple .main-sidebar {
          background-color: #111;
        }
        .skin-purple .main-sidebar .sidebar .sidebar-menu .active a {
          background-color: #693699;
          color: white;
        }
        .skin-purple .main-sidebar .sidebar .sidebar-menu a {
          color: white;
        }
        .skin-purple .content-wrapper, .skin-purple .right-side {
          background-color: #f8f8f8;
        }
        .box {
          border-top: 3px solid #693699;
        }
        .value-box .icon {
          background: #693699 !important;
        }
      "))
    ),
    sidebarMenu(
      menuItem("Unmet Demand", tabName = "demand", icon = icon("exclamation-circle")),
      menuItem("Age & Gender", tabName = "agegender", icon = icon("venus-mars")),
      menuItem("Admission Patterns", tabName = "patterns", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "demand",
              fluidRow(
                valueBoxOutput("total_unmet"),
                valueBoxOutput("avg_unmet_pct"),
                valueBoxOutput("total_demand")
              ),
              fluidRow(
                box(plotOutput("unmet_bar_plot"), width = 6),
                box(DTOutput("demand_table"), width = 6)
              )
      ),
      
      tabItem(tabName = "agegender",
              fluidRow(
                box(selectInput("drug_select", "Select Drug",
                                choices = unique(age_gender$Drug),
                                selected = "Alcohol"), width = 4),
                box(plotOutput("age_gender_plot"), width = 8)
              )
      ),
      
      tabItem(tabName = "patterns",
              fluidRow(
                box(selectizeInput("county_select", "Select County",
                                   choices = unique(admissions$County),
                                   selected = "Atlantic", multiple = TRUE),
                    width = 4),
                box(plotOutput("county_plot"), width = 8)
              ),
              fluidRow(
                box(title = "Rate by County & Drug", collapsible = TRUE, collapsed = FALSE, width = 12,
                    plotOutput("rate_plot"),
                    p("Note: The “Drugs Rate” includes admissions for substances such as heroin, cocaine, marijuana, methamphetamine, other opiates, and similar illicit or prescription drugs. This rate reflects the combined admissions for all non-alcohol substances reported by treatment providers."))
              )
      )
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    admissions %>%
      filter(County %in% input$county_select)
  })
  
  output$county_plot <- renderPlot({
    filtered_data() %>%
      select(-Other_N) %>%
      pivot_longer(cols = ends_with("_N"),
                   names_to = "Drug", values_to = "Count") %>%
      mutate(Drug = str_remove(Drug, "_N")) %>%
      ggplot(aes(x = County, y = Count, fill = Drug)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = "First-Time Admissions by Drug Type", y = "Number of Clients", x = "County") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16, face = "bold")
      )
  })
  
  output$rate_plot <- renderPlot({
    rates %>%
      select(-Unknown_Rate, -Total_Rate) %>%
      pivot_longer(cols = ends_with("Rate"), names_to = "Drug", values_to = "Rate") %>%
      ggplot(aes(x = Rate, y = reorder(County, Rate), fill = Drug)) +
      geom_col(position = "dodge") +
      theme_minimal() +
      labs(title = "Admission Rates by County & Drug", x = "Rate per 100,000", y = "County") +
      theme(
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16, face = "bold")
      )
  })
  
  output$total_unmet <- renderValueBox({
    valueBox(
      format(sum(unmet_demand$Unmet_Demand, na.rm = TRUE), big.mark = ","),
      "Total Unmet Demand",
      icon = icon("exclamation-circle"),
      color = "red"
    )
  })
  
  output$avg_unmet_pct <- renderValueBox({
    valueBox(
      paste0(round(mean(unmet_demand$Unmet_Demand_Percent, na.rm = TRUE), 1), "%"),
      "Avg. Unmet Demand % (All Counties)",
      icon = icon("percent"),
      color = "orange"
    )
  })
  
  output$total_demand <- renderValueBox({
    valueBox(
      format(sum(unmet_demand$Total_Demand, na.rm = TRUE), big.mark = ","),
      "Total Demand (Met + Unmet)",
      icon = icon("chart-line"),
      color = "purple"
    )
  })
  
  output$unmet_bar_plot <- renderPlot({
    unmet_demand %>%
      ggplot(aes(x = reorder(County, Unmet_Demand), y = Unmet_Demand)) +
      geom_col(fill = "#d9534f") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Unmet Demand by County", x = "County", y = "Clients with Unmet Demand") +
      theme(
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold")
      )
  })
  
  output$demand_table <- renderDT({
    unmet_demand %>%
      select(County, Met_Demand, Unmet_Demand, Unmet_Demand_Percent) %>%
      datatable(
        colnames = c("County", "Met Demand", "Unmet Demand", "Unmet % of Total"),
        options = list(
          pageLength = 10,
          lengthMenu = list(c(5, 10, 15, 22), c('5', '10', '15', 'All'))
        )
      )
  })
  
  output$age_gender_plot <- renderPlot({
    age_gender %>%
      filter(Drug == input$drug_select, !str_detect(Age_Group, "Total")) %>%
      pivot_longer(cols = c(Female_N, Male_N), names_to = "Gender", values_to = "Count") %>%
      mutate(Gender = if_else(Gender == "Female_N", "Female", "Male")) %>%
      ggplot(aes(x = Age_Group, y = Count, fill = Gender)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = paste("Admissions by Age and Gender for", input$drug_select),
           x = "Age Group", y = "Number of Admissions") +
      scale_fill_manual(values = c("#0072B2", "#D55E00")) +
      theme(
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16, face = "bold")
      )
  })
}

shinyApp(ui, server)






 