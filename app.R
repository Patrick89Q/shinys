library(shiny)
library(bslib)
library(rhino)
my_theme <- bs_theme(
  version = 5,
  preset = "shiny",
  primary = "firebrick",  # Custom blue color
  "navbar-bg" = "orange"
)






library(echarts4r)
library(tidyverse)
# Create a data frame
df <- data.frame(category = c("A", "B", "C", "D"),
                 value = c(10, 20, 30, 40))

# Create a bar chart
df %>%
  e_charts(category) %>%
  e_bar(value)


# Load necessary libraries
library(dplyr)
library(echarts4r)

# Set seed for reproducibility
set.seed(123)

# Define the time range
dates <- seq.Date(
  from = as.Date("2020-01-01"),
  to = as.Date("2024-12-01"),
  by = "month"
)

# Simulate data for Facility A and Facility B
simulated_data <- data.frame(
  date = rep(dates, 2),
  facility = rep(c("Facility A", "Facility B"), each = length(dates)),
  births = c(
    rnorm(length(dates), mean = 100, sd = 20),
    # Facility A
    rnorm(length(dates), mean = 150, sd = 25)
  ) # Facility B
)

# Ensure births are positive integers
simulated_data$births <- round(abs(simulated_data$births))

# View the first few rows of the data
head(simulated_data)

# Plot the data using echarts4r
echarts_sim <- simulated_data %>%
  group_by(facility) %>%
  e_charts(date) %>%
  e_line(births, smooth = TRUE) %>%
  e_title("Monthly Live Births (2020-2024)") %>%
  e_x_axis(name = "Date") %>%
  e_y_axis(name = "Number of Births") %>%
  e_tooltip(trigger = "axis") %>%
  e_legend(right = 0) %>%
  e_theme("vintage")  # Apply a theme

# Plot the data using echarts4r with custom line colors
simulated_data %>%
  group_by(facility) %>%
  e_charts(date) %>%
  e_line(births, smooth = TRUE) %>%
  e_title("Monthly Live Births (2020-2024)") %>%
  e_x_axis(name = "Date") %>%
  e_y_axis(name = "Number of Births") %>%
  e_tooltip(trigger = "axis") %>%
  e_legend(right = 0) %>%
  e_theme("vintage") %>%
  e_color(c("midnightblue", "firebrick")) %>%
  e_legend(position = "bottom")# Custom colors for Facility A and Facility B



# Plot the data using echarts4r with custom tooltip and legend
echrtas <- simulated_data %>%
  group_by(facility) %>%
  e_charts(date) %>%
  e_line(births, smooth = FALSE) %>%
  e_title("Monthly Live Births (2020-2024)", textStyle = list(color = "black")) %>%
  e_x_axis(name = "Date") %>%
  e_y_axis(name = "Number of Births", textStyle = list(color = "black")) %>%
  e_tooltip(
    trigger = "axis",
    backgroundColor = "white",
    # Set tooltip background color to white
    borderColor = "orange",
    # Add a light gray border
    textStyle = list(color = "black"),
    # Set text color to black
    borderRadius = 8            # Add rounded corners
  ) %>%
  e_legend(bottom = 0, # Move legend to the bottom
           textStyle = list(color = "black")) %>% # Set legend text color) 
  e_theme("vintage") %>%
  e_color(c("firebrick", "midnightblue"))  # Custom colors for Facility A and Facility B



# UI
ui <- page_navbar(
  title = "Healthcare Dashboard",
  theme = my_theme,
  nav_spacer(),
  # Home page
  nav_panel(
    title = "Home",
    p("Welcome to the Healthcare Analytics Dashboard. This platform provides insights into Maternity and ANC services."),
    layout_columns(
      fill = TRUE,
      value_box(
        title = "Welcome",
        value = "Healthcare Analytics",
        showcase = bsicons::bs_icon("hospital"),
        theme_color = "primary"
      ),
      value_box(
        title = "Total Patients",
        value = "1,234",
        showcase = bsicons::bs_icon("people"),
        theme_color = "info"
      ),
      value_box(
        title = "Total Patients",
        value = "1,234",
        showcase = bsicons::bs_icon("people"),
        theme_color = "info"
      ),
      value_box(
        title = "Total Patients",
        value = "1,234",
        showcase = bsicons::bs_icon("people"),
        theme_color = "info"
      ),
      value_box(
        title = "Active Cases",
        value = "456",
        showcase = bsicons::bs_icon("activity"),
        theme_color = "success"
      )
    ),
    layout_columns(
      card(
        card_header("Overview"),
        
        echrtas
      ),
      card(
        card_header("Overview"),
        
        echrtas
      )
      
    )
    
  ),
  
  # Maternity page
  nav_panel(
    title = "Maternity",
    page_sidebar(
      sidebar = sidebar(
        dateRangeInput("date_range_mat", "Select Date Range",
                       start = Sys.Date() - 30,
                       end = Sys.Date()),
        selectInput("facility_mat", "Select Facility",
                    choices = c("Facility A", "Facility B", "Facility C"))
      ),
      card(
        card_header("Maternity Statistics"),
        echrtas
      ),
      card(
        card_header("Key Metrics"),
        layout_columns(
          value_box(
            title = "Deliveries",
            value = textOutput("deliveries"),
            showcase = bsicons::bs_icon("heart"),
            theme_color = "primary"
          ),
          value_box(
            title = "C-Sections",
            value = textOutput("csections"),
            showcase = bsicons::bs_icon("clipboard2-pulse"),
            theme_color = "warning"
          ),
          value_box(
            title = "C-Sections",
            value = textOutput("csectionss"),
            showcase = bsicons::bs_icon("clipboard2-pulse"),
            theme_color = "warning"
          )
        )
      )
    )
  ),
  
  # ANC page
  nav_panel(
    title = "ANC",
    page_sidebar(
      sidebar = sidebar(
        dateRangeInput("date_range_anc", "Select Date Range",
                       start = Sys.Date() - 30,
                       end = Sys.Date()),
        selectInput("facility_anc", "Select Facility",
                    choices = c("Facility A", "Facility B", "Facility C")),
        checkboxGroupInput("visits", "Visit Type",
                           choices = c("First Visit", "Second Visit",
                                       "Third Visit", "Fourth Visit"))
      ),
      card(
        card_header("ANC Visits Trend"),
        plotOutput("anc_plot")
      ),
      card(
        card_header("ANC Summary"),
        layout_columns(
          value_box(
            title = "Total Visits",
            value = textOutput("total_visits"),
            showcase = bsicons::bs_icon("calendar-check"),
            theme_color = "success"
          ),
          value_box(
            title = "Total Patients",
            value = "1,234",
            showcase = bsicons::bs_icon("people"),
            theme_color = "info"
          ),
          value_box(
            title = "Total Patients",
            value = "1,234",
            showcase = bsicons::bs_icon("people"),
            theme_color = "info"
          ),
          value_box(
            title = "Total Patients",
            value = "1,234",
            showcase = bsicons::bs_icon("people"),
            theme_color = "info"
          ),
          value_box(
            title = "Total Patients",
            value = "1,234",
            showcase = bsicons::bs_icon("people"),
            theme_color = "info"
          ),
          value_box(
            title = "First Visits",
            value = textOutput("first_visits"),
            showcase = bsicons::bs_icon("1-circle"),
            theme_color = "info"
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Dummy data for demonstration
  output$maternity_plot <- renderPlot({
    plot(1:10, main = "Maternity Trends")
  })
  
  output$anc_plot <- renderPlot({
    plot(10:1, main = "ANC Visit Trends")
  })
  
  output$deliveries <- renderText({ "125" })
  output$csections <- renderText({ "45" })
  output$csectionss <- renderText({ "450" })
  output$total_visits <- renderText({ "789" })
  output$first_visits <- renderText({ "234" })
}

# Run the app
shinyApp(ui = ui, server = server)
