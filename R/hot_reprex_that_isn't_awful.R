# Load libraries ----
library(shiny)
library(bslib)
library(rhandsontable)

# Define UI ----
ui <- fluidPage(
  fluidRow(column(12, rHandsontableOutput("hot_table")))
)

# Define server ----
server <- function(input, output, session) {
  # Create sample data with 200 rows ----
  set.seed(123) # For reproducible random data

  first_names <- c(
    "Alice",
    "Bob",
    "Charlie",
    "Diana",
    "Eve",
    "Frank",
    "Grace",
    "Henry",
    "Iris",
    "Jack",
    "Kate",
    "Liam",
    "Maya",
    "Noah",
    "Olivia",
    "Paul",
    "Quinn",
    "Rachel",
    "Sam",
    "Tara"
  )
  last_names <- c(
    "Johnson",
    "Smith",
    "Brown",
    "Prince",
    "Adams",
    "Wilson",
    "Davis",
    "Miller",
    "Taylor",
    "Anderson",
    "Thomas",
    "Jackson",
    "White",
    "Harris",
    "Martin",
    "Thompson",
    "Garcia",
    "Martinez",
    "Robinson",
    "Clark"
  )

  departments <- c(
    "Sales & Marketing",
    "Information Technology",
    "Human Resources",
    "Finance & Accounting",
    "Research & Development",
    "Customer Service"
  )

  statuses <- c(
    "Active Full-Time",
    "Active Part-Time",
    "Inactive",
    "On Leave",
    "Contractor",
    "Pending"
  )

  priorities <- c(
    "High Priority",
    "Medium Priority",
    "Low Priority",
    "Critical Priority"
  )

  categories <- c(
    "Category Alpha",
    "Category Beta",
    "Category Gamma",
    "Category Delta",
    "Category Epsilon",
    "Category Zeta",
    "Category Eta",
    "Category Theta"
  )

  regions <- c(
    "North America",
    "South America",
    "Eastern Europe",
    "Western Europe",
    "Asia Pacific",
    "Middle East",
    "Africa",
    "Central America"
  )

  sample_data <- data.frame(
    UniqueIdentifier = 1:200,
    FullEmployeeName = paste(
      sample(first_names, 200, replace = TRUE),
      sample(last_names, 200, replace = TRUE)
    ),
    DepartmentAffiliation = sample(departments, 200, replace = TRUE),
    CurrentEmploymentStatus = sample(statuses, 200, replace = TRUE),
    ProjectPriorityLevel = sample(priorities, 200, replace = TRUE),
    BusinessUnitCategory = sample(categories, 200, replace = TRUE),
    GeographicalRegion = sample(regions, 200, replace = TRUE),
    PerformanceScore = round(runif(200, min = 60, max = 100), 1),
    stringsAsFactors = FALSE
  )

  # Render handsontable ----
  output$hot_table <- renderRHandsontable({
    rhandsontable(sample_data, stretchH = "all", overflow = "visible") %>%
      # Add dropdown for DepartmentAffiliation
      hot_col(
        "DepartmentAffiliation",
        type = "dropdown",
        source = c(
          "Sales & Marketing",
          "Information Technology",
          "Human Resources",
          "Finance & Accounting",
          "Research & Development",
          "Customer Service"
        )
      )
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
