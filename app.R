# =========================
# CPS FUNDING DASHBOARD APP
# =========================

# ---- Libraries ----
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(readr)
library(scales)

# ---- Load & Prepare Data ----
df <- read.csv("Data/BudgetBook.csv", encoding = "latin1")
colnames(df) <- make.names(colnames(df))

school_budget <- df %>%
  rename(`School Name` = Unit.Name) %>%
  group_by(`School Name`) %>%
  summarise(
    FY23_Budget = sum(FY23.Adopted.Budget, na.rm = TRUE),
    FY24_Budget = sum(FY24.Proposed.Budget, na.rm = TRUE)
  ) %>%
  mutate(
    Difference = FY24_Budget - FY23_Budget
  )

# ======================
# UI
# ======================
ui <- dashboardPage(
  
  dashboardHeader(title = "CPS Funding Insights"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Funding Overview", tabName = "overview", icon = icon("chart-bar")),
      menuItem("Radar Comparison", tabName = "radar", icon = icon("bullseye")),
      
      hr(),
      
      conditionalPanel(
        condition = "input.tabs == 'overview'",
        selectizeInput(
          "school",
          "Search for a School:",
          choices = NULL,
          options = list(placeholder = "Type a school name...")
        )
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'radar'",
        selectizeInput("radar_school1", "First School:", choices = NULL),
        selectizeInput("radar_school2", "Second School:", choices = NULL)
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ---- INTRODUCTION ----
      tabItem(
        tabName = "intro",
        box(
          width = 8,
          title = "About Chicago Public Schools (CPS)",
          status = "primary",
          solidHeader = TRUE,
          
          p("Chicago Public Schools (CPS) is the third-largest school district in the United States,
            serving hundreds of thousands of students across the city."),
          
          p("School funding plays a critical role in shaping educational opportunities.
            Changes in annual budgets reflect enrollment shifts, staffing needs, and
            district priorities."),
          
          p("This dashboard allows users to explore how school-level funding changed
            between the 2023 and 2024 school years using publicly available CPS budget data."),
          
          h4("What You Can Explore"),
          tags$ul(
            tags$li("School-level funding changes"),
            tags$li("Year-over-year budget differences"),
            tags$li("Comparisons between two schools")
          ),
          
          p(strong("Use the menu on the left to begin exploring."))
        )
      ),
      
      # ---- FUNDING OVERVIEW ----
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            width = 8,
            title = "Funding Change (FY23 → FY24)",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("budgetPlot", height = 350)
          ),
          
          box(
            width = 4,
            title = "Summary Table",
            status = "info",
            solidHeader = TRUE,
            tableOutput("summaryTable")
          )
        )
      ),
      
      # ---- RADAR COMPARISON ----
      tabItem(
        tabName = "radar",
        fluidRow(
          box(
            width = 8,
            title = "School Funding Profile Comparison",
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("fundingRadar", height = 400)
          )
        )
      )
    )
  )
)

# ======================
# SERVER
# ======================
server <- function(input, output, session) {
  
  school_choices <- sort(unique(school_budget$`School Name`))
  
  updateSelectizeInput(session, "school", choices = school_choices, server = TRUE)
  updateSelectizeInput(session, "radar_school1", choices = school_choices, server = TRUE)
  updateSelectizeInput(session, "radar_school2", choices = school_choices, server = TRUE)
  
  selected_school <- reactive({
    req(input$school)
    school_budget %>% filter(`School Name` == input$school)
  })
  
  output$budgetPlot <- renderPlotly({
    df_plot <- selected_school()
    
    plot_ly(
      x = c("FY23 Budget", "FY24 Budget"),
      y = c(df_plot$FY23_Budget, df_plot$FY24_Budget),
      type = "bar",
      text = dollar(c(df_plot$FY23_Budget, df_plot$FY24_Budget)),
      hoverinfo = "text",
      marker = list(color = c("#1f77b4", "#2ca02c"))
    ) %>%
      layout(yaxis = list(title = "Total Budget ($)"))
  })
  
  output$summaryTable <- renderTable({
    selected_school() %>%
      mutate(
        FY23_Budget = dollar(FY23_Budget),
        FY24_Budget = dollar(FY24_Budget),
        Difference = dollar(Difference)
      )
  })
  
  output$fundingRadar <- renderPlotly({
    req(input$radar_school1, input$radar_school2)
    
    s1 <- school_budget %>% filter(`School Name` == input$radar_school1)
    s2 <- school_budget %>% filter(`School Name` == input$radar_school2)
    
    r1 <- c(s1$FY23_Budget, s1$FY24_Budget, s1$Difference)
    r2 <- c(s2$FY23_Budget, s2$FY24_Budget, s2$Difference)
    
    metrics <- c("FY23 Budget", "FY24 Budget", "Change")
    max_val <- max(c(r1, r2))
    
    plot_ly(type = "scatterpolar", mode = "lines") %>%
      add_trace(r = r1, theta = metrics, fill = "toself", name = input$radar_school1) %>%
      add_trace(r = r2, theta = metrics, fill = "toself", name = input$radar_school2) %>%
      layout(
        polar = list(radialaxis = list(visible = TRUE, range = c(0, max_val)))
      )
  })
}

# ---- Run App ----
shinyApp(ui, server)