# Shiny Dashboard -------------------------------------------------------------

# Load necessary libraries 
library(shiny)          # shiny for building interactive web applications
library(ggplot2)        # ggplot2 for creating graphs
library(dplyr)          # dplyr for data manipulation
library(shinydashboard) # shinydashboard for creating dashboard layouts in Shiny
library(shinyjs)        # shinyjs for enhancing Shiny apps with JavaScript
library(tidyr)          # tidyr for tidying data
library(maps)           # maps for mapping functions
library(plotly)         # plotly for interactive plots
library(purrr)          # purrr for functional programming

# Load Data from GitHub -------------------------------------------------------
# Read census population estimates data from GitHub
CensusEstMN <- read.csv('https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/2020%20to%202022%20Pop.%20Estimates/cc-est2022-agesex.csv')

# List of URLs for CHD (Coronary Heart Disease) data files for multiple years
CHD_files <- list(
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2018.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2019.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2020.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2021.csv'
)

# Load CHD data from the URLs
CHD_data <- lapply(CHD_files, read.csv)

# Load raw data for Community Health Board (CHB) and MN Region
chb_raw <- read.csv('https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/Community%20Health%20Board%20as%20of%201_17_2024.csv')
mn_region_raw <- read.csv('https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/State%20Community%20Health%20Services%20Advisory%20Committee%20as%20of%201_17_2024.csv')

# Data Wrangling --------------------------------------------------------------
# Clean and prepare CHD data
CHD_data <- lapply(CHD_data, function(df) {
  # Add LocationID column if Latitude is present in the dataset
  df$LocationID <- ifelse("Latitude" %in% colnames(df), as.character(df$Latitude), NA)
  # Correct any misspelled column names
  colnames(df)[colnames(df) == 'Geolocatioin'] <- 'Geolocation'
  df
})

# Combine all CHD data into one data frame
CHD_Final <- bind_rows(CHD_data)

# Standardize County Names
standardize_county_names <- function(x) {
  tools::toTitleCase(tolower(gsub("St. Louis", "St Louis", x)))
}

CHD_Final$LocationName <- standardize_county_names(CHD_Final$LocationName)
mn_region_raw$County <- standardize_county_names(mn_region_raw$County)
chb_raw$County <- standardize_county_names(chb_raw$County)
CensusEstMN$CTYNAME <- standardize_county_names(gsub(" County", "", CensusEstMN$CTYNAME))

# Select relevant data and merge with region and CHB data
Selected_Locations <- CHD_Final |>
  filter(Year == 2021, StateAbbr == "MN") |>
  left_join(mn_region_raw, by = c("LocationName" = "County")) |>
  left_join(chb_raw, by = c("LocationName" = "County"))

# Filter and select specific columns for population estimates and CHD data
PopEst_CHDMN <- CensusEstMN |>
  filter(YEAR == 3) |>
  inner_join(Selected_Locations, by = c("CTYNAME" = "LocationName")) |>
  select(CTYNAME, Data_Value_Type, AGE18PLUS_TOT, Measure, Data_Value, High_Confidence_Limit, Low_Confidence_Limit, Region, CHB)

# Function to calculate aggregate values---------------------------------------
# Define a function to calculate aggregate values for a given dataframe, user input, and filter criterion
aggregate_values <- function(df, userInput, filterBy) {
  df |>
    filter(!!sym(filterBy) == userInput) |>
    mutate(
      Aggregate_Data_Value = Data_Value * AGE18PLUS_TOT / 100,
      Aggregate_Low_Confidence_Limit = Low_Confidence_Limit * AGE18PLUS_TOT / 100,
      Aggregate_High_Confidence_Limit = High_Confidence_Limit * AGE18PLUS_TOT / 100
    ) |>
    group_by(across(all_of(filterBy)), Data_Value_Type) |>
    summarise(
      Aggregate_Data_Value = sum(Aggregate_Data_Value) / sum(AGE18PLUS_TOT) * 100,
      Aggregate_Low_Confidence_Limit = sum(Aggregate_Low_Confidence_Limit) / sum(AGE18PLUS_TOT) * 100,
      Aggregate_High_Confidence_Limit = sum(Aggregate_High_Confidence_Limit) / sum(AGE18PLUS_TOT) * 100,
      .groups = 'drop'
    )
}

# Pre-calculate Minnesota total------------------------------------------------
# Calculate aggregate values for Minnesota
mn_total <- PopEst_CHDMN |>
  mutate(StateAbbr = "MN") |>
  aggregate_values("MN", 'StateAbbr') |>
  mutate(across(everything(), ~tidyr::replace_na(., 0)))

# Function to compute y-axis limits -------------------------------------------
# Define a function to compute y-axis limits for given data list
compute_y_axis_limits <- function(data_list) {
  min_value <- min(sapply(data_list, function(df) min(df$`Low Confidence Limit`, na.rm = TRUE)))
  max_value <- max(sapply(data_list, function(df) max(df$`High Confidence Limit`, na.rm = TRUE)))
  c(min_value, max_value)
}

# Function to create ggplot graph ---------------------------------------------
# Define a function to create ggplot graph with given data and y-axis limits
chd_plot <- function(data, y_limits) {
  ggplot(data, aes(x = `Data Type`, y = `Point Estimate`, color = `Data Type`)) +
    geom_errorbar(aes(ymin = `Low Confidence Limit`, ymax = `High Confidence Limit`), width = 0.2) +
    geom_point() +
    ylim(y_limits) +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.line = element_blank(),
      legend.text = element_text(size = 12)
    ) +
    scale_color_manual(values = c("Age-adjusted prevalence" = "#78BE21", "Crude prevalence" = "#003865"))
}

# Function to generate narrative text -----------------------------------------
# Define a function to generate narrative text based on comparison data and highlighted elements
generate_narrative <- function(county_data, comparison_data, comparison_name, highlighted_year, highlighted_county, data_type) {
  narrative <- paste0(
    "In ", highlighted_year, ", <b>adults aged ≥18 years</b> in ", highlighted_county, " had a <b>coronary heart disease</b> ",
    data_type, " of <b>", round(county_data$`Point Estimate`, 2), "% (95% CI: ", round(county_data$`Low Confidence Limit`, 2), "-",
    round(county_data$`High Confidence Limit`, 2), ")</b>, compared to the ", comparison_name, "'s <b>",
    round(comparison_data$`Point Estimate`, 2), "% (95% CI: ", round(comparison_data$`Low Confidence Limit`, 2), "-",
    round(comparison_data$`High Confidence Limit`, 2), ")</b>."
  )
  
  if (county_data$`Low Confidence Limit` > comparison_data$`High Confidence Limit` || county_data$`High Confidence Limit` < comparison_data$`Low Confidence Limit`) {
    if (county_data$`Point Estimate` < comparison_data$`Low Confidence Limit`) {
      narrative <- paste0(narrative, " The confidence limits (low & high) values is <b>lower</b> than the ", comparison_name, ".")
    } else {
      narrative <- paste0(narrative, " The confidence limits values <b>higher</b> than the ", comparison_name, ".")
    }
  } else {
    narrative <- paste0(narrative, " The difference in the CI values is <b>not statistically significant</b>.")
  }
  
  narrative
}

# Function to prepare map data ------------------------------------------------
prepare_map_data <- function(map_data, merge_data, merge_by, highlight_criteria = NULL) {
  map_data <- map_data |>
    left_join(merge_data, by = merge_by) |>
    mutate(across(where(is.factor), as.character))  # Convert factors to character
  
  if (!is.null(highlight_criteria)) {
    map_data <- map_data |>
      filter(!!sym(highlight_criteria) %in% merge_data[[highlight_criteria]])
  }
  
  map_data
}

# Function to create maps with hover info with county name -----------
# This function creates an interactive plotly map for a given dataset and highlight criteria
create_plotly_map <- function(map_data, highlight_data) {
  plot <- ggplot(map_data, aes(x = long, y = lat, group = group, text = subregion)) +
    geom_polygon(aes(fill = "unhighlighted"), color = "white") +
    geom_polygon(data = highlight_data, aes(fill = "highlighted"), color = "white") +
    scale_fill_manual(values = c(unhighlighted = "#78BE21", highlighted = "#003865"), guide = "none") +
    coord_fixed(1.3) +
    theme_void()
  
  # Call ggplotly on the plot object and remove legend from the Minnesota Region Map, Minnesota CHB Map, and Minnesota County Map
  ggplotly(plot, tooltip = "text") |>
    layout(showlegend = FALSE)
}

# Define UI -------------------------------------------------------------------
# Define the user interface (UI) for the Shiny dashboard
ui <- function(request) {
  dashboardPage(
    dashboardHeader(title = "CDC Places to MN Regions", titleWidth = 400), # Create dashboard header with title
    dashboardSidebar(
      width = 350,
      selectInput("parGlobal_region", label = "Select SCHSAC Region of Interest", choices = sort(unique(mn_region_raw$Region)), selected = "Northwest (NW)", width = 350), # Dropdown for selecting SCHSAC region
      selectInput("parGlobal_chb", label = "Select Community Health Board", choices = sort(unique(chb_raw$CHB)), selected = "(NW) Quin County", width = 350), # Dropdown for selecting Community Health Board
      selectInput("parGlobal_county", label = "Select County of Interest", choices = sort(unique(mn_region_raw$County)), selected = NULL, width = 350), # Dropdown for selecting county
      selectInput("parGlobal_chdYear", label = "Select Year", choices = sort(unique(Selected_Locations$Year), decreasing = TRUE), selected = max(unique(Selected_Locations$Year)), width = 350), # Dropdown for selecting year
      selectInput("parGlobal_chdStateRegionChb", label = "Select Comparison", choices = c("State", "Region", "CHB"), selected = "State", multiple = FALSE, width = 350), # Dropdown for selecting Comparison (State, Region, & CHB)
      sidebarMenu(
        menuItem("Home", tabName = "tn_homePage"), # Menu item for Home page
        menuItem("Coronary Heart Disease", tabName = "tn_coronaryHeartDisease")
      )
    ),
    dashboardBody(
      shinyjs::useShinyjs(), # Javascript
      tabItems(
        tabItem(
          tabName = "tn_homePage",
          tabsetPanel(
            tabPanel(
              "Home Page",
              fluidRow(
                column(
                  width = 12,
                  h1("Welcome to the CDC PLACES MN Region Dashboard"), # Display welcome message
                  h4(tags$b("Objective:"), "Use CDC PLACES methodology to create MN Regions ShinyLive dashboard for health indicators.", tags$a(href="https://www.cdc.gov/places/faqs/using-data/index.html", "Link to CDC PLACES methodology to calculate the aggregate values.", target="_blank")), # Link to external site
                  tags$h4(tags$b("Why this project?"), "Before the CDC Places project, the CDC Behavioral Risk Factor Surveillance System BRFSS, allowed for state projected healthcare indicators. This process was not able to be applied to the county level. Now, with CDC Places counties can view some projected healthcare indicators. However, currently the CDC Places project does not show in an easy format aggregate county regions. By doing this project, I am not only going to help Quin County CHS, but other county regions in the state of Minnesota or even the United States."), # Description of the project
                  tags$h3("Those involved with this project are:"), # Project participants
                  tags$h4(tags$b("Emmanuel Fle Chea"), ", MPH, Public Health Data Science, University of Minnesota School of Public Health"), # Participant 1
                  tags$h4(tags$b("Mr. Patrick Olson"), " (Preceptor), Quin County Community Health Board, Community Resource Liaison/Associate/Researcher"), # Participant 2
                  tags$h4(tags$b("Lesson Learned:"), style = "font-size: 16px;"),
                  tags$ul(
                    tags$li("How to use CDC PLACES methodology to calculate the aggregate values for each health measures in excel spreadsheet. Used this idea and calculated the aggregate values in ShinyLive application and also learned how to put the calculation into a function. I also learned how to use unitesting on each step of the code to make sure that my calculations were correct before building the ShinyLive application.", style = "font-size: 16px;"),
                    tags$li("How to bookmark specify part of the ShinyLive dashboard using the bookmark function.", style = "font-size: 16px;"),
                    tags$li("How to navigate challenges of integrating multiple data sources and also the use of collapsible on the plots and maps to allow user to minimize plots and maps.", style = "font-size: 16px;")
                  ),
                  tags$h4(tags$b("Limitations:"), style = "font-size: 16px;"),
                  tags$ul(
                    tags$li("Fewer Packages: ShinyLive application is in the experimental and developmental stage, not all packages are available in the WebAssembly environment used by ShinyLive.", style = "font-size: 16px;"),
                    tags$li("Security Concerns: Running code in the browser introduces security risks. Although ShinyLive tries to mitigate these risks, it’s essential to be cautious when handling sensitive data or executing untrusted code.", style = "font-size: 16px;"),
                    tags$li("Large Download Size: The payload size for the downloaded assets can be significantly larger compared to traditional Shiny deployments. It takes longer time to load a ShinyLive app and get it to run than the original Shiny app.", style = "font-size: 16px;")
                  ),
                  tags$h4(tags$b("Struggles:"), style = "font-size: 16px;"),
                  tags$ul(
                    tags$li("Continuous troubleshooting of the application.", style = "font-size: 16px;"),
                    tags$li("My preceptor Mr. Olson had a little experience using functions so I had to self learned functions as I work on the ShinyLive application.", style = "font-size: 16px;"),
                    tags$li("Technical challenges in deploying the dashboard.", style = "font-size: 16px;")
                  )
                )
              )
            ),
            tabPanel(
              "Minnesota, SCHSAC Region, CHB",
              fluidRow(
                column(
                  width = 11,
                  h3(HTML("Updating the Select State Community Health Services Advisory Committee (SCHSAC) Region of Interest filter will highlight all of the counties in the selected SCHSAC region in <font color=blue>blue</font> on the <b>Minnesota Region Map.</b>")), # Explanation of functionality
                  h3(HTML("Updating the Select Community Health Board (CHB) filter will highlight all of the counties in the selected CHB in <font color=blue>blue</font> on the <b>Minnesota CHB Map.</b>")), # Explanation of functionality
                  h3(HTML("Updating the Select County of Interest filter will highlight the selected county in <font color=blue>blue</font> on the <b>Minnesota County Map.</b>")) # Explanation of functionality
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  box(
                    title = "Minnesota Region Map",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    plotlyOutput("mn_region_map", height = "400px")  # Use plotlyOutput for interactive map
                  )
                ),
                column(
                  width = 4,
                  box(
                    title = "Minnesota CHB Map",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    plotlyOutput("mn_chb_map", height = "400px")
                  )
                ),
                column(
                  width = 4,
                  box(
                    title = "Minnesota County Map",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    plotlyOutput("mn_county_map", height = "400px")  # Changed to plotlyOutput
                  )
                )
              )
            )
          )
        ),
        tabItem(
          tabName = "tn_coronaryHeartDisease",
          tabsetPanel(
            id = "tpId_coronaryHeartDisease",
            tabPanel(
              "Adults>=18 CHD Exposure",
              fluidRow(
                column(
                  width = 12,
                  uiOutput("narrative_text", style = "font-size: 16px;")
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  box(
                    title = uiOutput("selected_state_title"),
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    plotOutput("plot_state", height = "200px"),
                    tableOutput("table_state")
                  )
                ),
                column(
                  width = 6,
                  box(
                    title = uiOutput("selected_region_title"),
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    plotOutput("plot_chbRegion", height = "200px"),
                    tableOutput("table_region")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  box(
                    title = uiOutput("selected_chb_title"),
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    plotOutput("plot_chdCHB", height = "200px"),
                    tableOutput("table_chb")
                  )
                ),
                column(
                  width = 6,
                  box(
                    title = uiOutput("selected_county_title"),
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    plotOutput("plot_county", height = "200px"),
                    tableOutput("table_county")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  selectInput("parLocal_prevalence", label = "Select Prevalence", choices = c("Age-adjusted prevalence", "Crude prevalence"), selected = "Age-adjusted prevalence", width = 350)
                ),
                column(
                  width = 8,
                  box(
                    title = "Minnesota CHD Exposure Map",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    plotlyOutput("mn_adults_chd_exposure_map", height = "400px", width = "700px")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

# Server Logic ----------------------------------------------------------------
# Define the server logic for the Shiny dashboard
server <- function(input, output, session) {
  
  # Reactive data for the region map based on user selection
  reactive_region_map_data <- reactive({
    selected_region <- input$parGlobal_region
    
    # Prepare map data
    mn_map_data <- map_data("county", region = "minnesota")
    mn_map_data$subregion <- standardize_county_names(mn_map_data$subregion)
    
    # Prepare data to highlight the selected region
    mn_map_data <- prepare_map_data(mn_map_data, mn_region_raw, c("subregion" = "County"))
    
    counties_in_region <- mn_region_raw |>
      filter(Region == selected_region) |>
      pull(County)
    counties_in_region <- map_chr(counties_in_region, standardize_county_names)
    
    list(map_data = mn_map_data, highlight_data = mn_map_data |> filter(subregion %in% counties_in_region))
  })
  
  # Reactive data for the CHB map based on user selection
  reactive_chb_map_data <- reactive({
    selected_chb <- input$parGlobal_chb
    
    # Prepare map data
    mn_map_data <- map_data("county", region = "minnesota")
    mn_map_data$subregion <- standardize_county_names(mn_map_data$subregion)
    
    # Prepare data to highlight the selected CHB
    mn_map_data <- prepare_map_data(mn_map_data, chb_raw, c("subregion" = "County"))
    
    counties_in_chb <- chb_raw |>
      filter(CHB == selected_chb) |>
      pull(County)
    counties_in_chb <- map_chr(counties_in_chb, standardize_county_names)
    
    list(map_data = mn_map_data, highlight_data = mn_map_data |> filter(subregion %in% counties_in_chb))
  })
  
  # Reactive data for the county map based on user selection
  reactive_county_map_data <- reactive({
    selected_county <- input$parGlobal_county
    
    # Prepare map data
    mn_map_data <- map_data("county", region = "minnesota")
    mn_map_data$subregion <- standardize_county_names(mn_map_data$subregion)
    
    selected_county_data <- mn_map_data |>
      filter(subregion == standardize_county_names(selected_county))
    
    list(map_data = mn_map_data, highlight_data = selected_county_data)
  })
  
  # Render the Minnesota Region Map with hover info
  output$mn_region_map <- renderPlotly({
    map_data <- reactive_region_map_data()
    create_plotly_map(map_data$map_data, map_data$highlight_data)
  })
  
  # Render the Minnesota CHB Map with hover info
  output$mn_chb_map <- renderPlotly({
    map_data <- reactive_chb_map_data()
    create_plotly_map(map_data$map_data, map_data$highlight_data)
  })
  
  # Render the Minnesota County Map with hover info
  output$mn_county_map <- renderPlotly({
    map_data <- reactive_county_map_data()
    create_plotly_map(map_data$map_data, map_data$highlight_data)
  })
  
  # Observe the selected region and update county choices accordingly
  observe({
    region <- input$parGlobal_region
    if (!is.null(region) && region != "") {
      counties_in_region <- mn_region_raw |>
        filter(Region == region) |>
        pull(County)
      updateSelectInput(session, "parGlobal_county", choices = sort(unique(counties_in_region)))
      
      # Update CHB choices based on selected region
      chbs_in_region <- chb_raw |>
        filter(County %in% counties_in_region) |>
        distinct(CHB)
      updateSelectInput(session, "parGlobal_chb", choices = sort(unique(chbs_in_region$CHB)))
    } else {
      updateSelectInput(session, "parGlobal_county", choices = sort(unique(mn_region_raw$County)))
      updateSelectInput(session, "parGlobal_chb", choices = sort(unique(chb_raw$CHB)))
    }
  })
  
  # Observe the selected CHB and update only the CHB map
  observeEvent(input$parGlobal_chb, {
    chb <- input$parGlobal_chb
    if (!is.null(chb) && chb != "") {
      counties_in_chb <- chb_raw |>
        filter(CHB == chb) |>
        pull(County)
      # No update to county map or select input to keep them unchanged
    }
  })
  
  # Render narrative for the selected region
  output$region_narrative <- renderUI({
    filtered_region <- mn_region_raw |> filter(County == input$parGlobal_county)
    region_vector <- unique(filtered_region$Region)
    HTML(paste0("<b>", region_vector, " Region</b> is made up of the following counties: ", paste(unique(filtered_region$County), collapse = ", "), "."))
  })
  
  # Render narrative for the selected CHB
  output$chb_narrative_01 <- renderUI({
    filtered_chb <- chb_raw |> filter(County == input$parGlobal_county)
    chb_name_vector <- unique(filtered_chb$CHBName)
    HTML(paste0("<b>", chb_name_vector, " Community Health Board</b> includes: ", paste(unique(filtered_chb$County), collapse = ", "), "."))
  })
  
  # Highlight text in narrative
  highlight_text <- function(text, keyword) {
    gsub(keyword, paste0("<font color='red'>", keyword, "</font>"), text)
  }
  
  # Render counties in the selected region with highlighting
  output$region_counties <- renderUI({
    selected_county <- input$parGlobal_county
    regions <- mn_region_raw |>
      group_by(Region) |>
      summarise(Counties = paste(County, collapse = ", "))
    
    regions_text <- regions |>
      mutate(Text = paste0("<b>", Region, " Region::</b> ", Counties)) |>
      pull(Text)
    
    regions_text <- map_chr(regions_text, ~ highlight_text(.x, selected_county))
    HTML(paste(regions_text, collapse = "<br>"))
  })
  
  # Render counties in the selected CHB with highlighting
  output$chb_counties <- renderUI({
    selected_county <- input$parGlobal_county
    chbs <- chb_raw |>
      group_by(CHB) |>
      summarise(Counties = paste(County, collapse = ", "))
    
    chb_text <- chbs |>
      mutate(Text = paste0("<b>", CHB, "::</b> ", Counties)) |>
      pull(Text)
    
    chb_text <- map_chr(chb_text, ~ highlight_text(.x, selected_county))
    HTML(paste(chb_text, collapse = "<br>"))
  })
  
  # Render titles for selected county, region, state, and CHB
  output$selected_county_title <- renderText({
    HTML(paste("Coronary Heart Disease Exposure", "<br/>", input$parGlobal_county, "County"))
  })
  
  output$selected_region_title <- renderText({
    county_region <- mn_region_raw |> filter(County == input$parGlobal_county) |> pull(Region) |> unique()
    HTML(paste("Coronary Heart Disease Exposure", "<br/>", county_region, "Region"))
  })
  
  output$selected_state_title <- renderText({
    HTML(paste("Coronary Heart Disease Exposure", "<br/>Minnesota"))
  })
  
  output$selected_chb_title <- renderText({
    county_chb <- chb_raw |> filter(County == input$parGlobal_county) |> pull(CHB) |> unique()
    HTML(paste("Coronary Heart Disease Exposure", "<br/>", county_chb, "CHB"))
  })
  
  # Reactive Data for plotting ------------------------------------------------
  # Define reactive data for the selected county
  reactive_county_data <- reactive({
    PopEst_CHDMN |>
      filter(CTYNAME == input$parGlobal_county) |>
      aggregate_values(input$parGlobal_county, "CTYNAME") |>
      select(-CTYNAME) |>
      rename(
        `Data Type` = Data_Value_Type,
        `Point Estimate` = Aggregate_Data_Value,
        `Low Confidence Limit` = Aggregate_Low_Confidence_Limit,
        `High Confidence Limit` = Aggregate_High_Confidence_Limit
      ) |>
      select(`Data Type`, `Low Confidence Limit`, `Point Estimate`, `High Confidence Limit`)
  })
  
  # Define reactive data for the selected region
  reactive_region_data <- reactive({
    county_region <- mn_region_raw |> filter(County == input$parGlobal_county) |> pull(Region) |> unique()
    PopEst_CHDMN |>
      filter(Region %in% county_region) |>
      aggregate_values(county_region, 'Region') |>
      select(-Region) |>
      rename(
        `Data Type` = Data_Value_Type,
        `Point Estimate` = Aggregate_Data_Value,
        `Low Confidence Limit` = Aggregate_Low_Confidence_Limit,
        `High Confidence Limit` = Aggregate_High_Confidence_Limit
      ) |>
      select(`Data Type`, `Low Confidence Limit`, `Point Estimate`, `High Confidence Limit`)
  })
  
  # Define reactive data for the selected CHB
  reactive_chb_data <- reactive({
    county_chb <- chb_raw |> filter(County == input$parGlobal_county) |> pull(CHB) |> unique()
    PopEst_CHDMN |>
      filter(CHB %in% county_chb) |>
      aggregate_values(county_chb, 'CHB') |>
      select(-CHB) |>
      rename(
        `Data Type` = Data_Value_Type,
        `Point Estimate` = Aggregate_Data_Value,
        `Low Confidence Limit` = Aggregate_Low_Confidence_Limit,
        `High Confidence Limit` = Aggregate_High_Confidence_Limit
      ) |>
      select(`Data Type`, `Low Confidence Limit`, `Point Estimate`, `High Confidence Limit`)
  })
  
  # Compute y-axis limits based on reactive data
  y_axis_limits <- reactive({
    data_list <- list(
      reactive_county_data(),
      reactive_region_data(),
      reactive_chb_data(),
      mn_total |>
        rename(
          `Data Type` = Data_Value_Type,
          `Point Estimate` = Aggregate_Data_Value,
          `Low Confidence Limit` = Aggregate_Low_Confidence_Limit,
          `High Confidence Limit` = Aggregate_High_Confidence_Limit
        )
    )
    compute_y_axis_limits(data_list)
  })
  
  # Render plot for the selected county
  output$plot_county <- renderPlot({
    chd_plot(reactive_county_data(), y_axis_limits())
  })
  
  # Render plot for the selected region
  output$plot_chbRegion <- renderPlot({
    chd_plot(reactive_region_data(), y_axis_limits())
  })
  
  # Render plot for the selected CHB
  output$plot_chdCHB <- renderPlot({
    chd_plot(reactive_chb_data(), y_axis_limits())
  })
  
  # Render plot for the state of Minnesota
  output$plot_state <- renderPlot({
    data <- mn_total |>
      rename(
        `Data Type` = Data_Value_Type,
        `Point Estimate` = Aggregate_Data_Value,
        `Low Confidence Limit` = Aggregate_Low_Confidence_Limit,
        `High Confidence Limit` = Aggregate_High_Confidence_Limit
      ) |>
      select(`Data Type`, `Low Confidence Limit`, `Point Estimate`, `High Confidence Limit`)
    chd_plot(data, y_axis_limits())
  })
  
  # Summary Tables -----------------------------------------------------------
  # Render summary table for the selected county
  output$table_county <- renderTable({
    reactive_county_data()
  })
  
  # Render summary table for the selected region
  output$table_region <- renderTable({
    reactive_region_data()
  })
  
  # Render summary table for the selected CHB
  output$table_chb <- renderTable({
    reactive_chb_data()
  })
  
  # Render summary table for the state of Minnesota
  output$table_state <- renderTable({
    mn_total |>
      rename(
        `Data Type` = Data_Value_Type,
        `Point Estimate` = Aggregate_Data_Value,
        `Low Confidence Limit` = Aggregate_Low_Confidence_Limit,
        `High Confidence Limit` = Aggregate_High_Confidence_Limit
      ) |>
      select(`Data Type`, `Low Confidence Limit`, `Point Estimate`, `High Confidence Limit`)
  })
  
  # Narrative ----------------------------------------------------------------
  # Render narrative text based on selected inputs
  output$narrative_text <- renderUI({
    county_data <- reactive_county_data()
    state_data <- mn_total |>
      rename(
        `Data Type` = Data_Value_Type,
        `Point Estimate` = Aggregate_Data_Value,
        `Low Confidence Limit` = Aggregate_Low_Confidence_Limit,
        `High Confidence Limit` = Aggregate_High_Confidence_Limit
      )
    
    region_data <- reactive_region_data()
    chb_data <- reactive_chb_data()
    
    county <- input$parGlobal_county
    comparison <- input$parGlobal_chdStateRegionChb
    year <- "2021"
    highlighted_year <- highlight_text(year, year)
    highlighted_county <- highlight_text(county, county)
    highlighted_age_adjusted_prevalence <- highlight_text("age-adjusted prevalence", "age-adjusted prevalence")
    highlighted_crude_prevalence <- highlight_text("crude prevalence", "crude prevalence")
    
    age_adjusted_narrative <- NULL
    crude_prevalence_narrative <- NULL
    
    if (comparison == "State") {
      age_adjusted_narrative <- generate_narrative(
        county_data[county_data$`Data Type` == "Age-adjusted prevalence",],
        state_data[state_data$`Data Type` == "Age-adjusted prevalence",],
        "state", highlighted_year, highlighted_county, highlighted_age_adjusted_prevalence
      )
      crude_prevalence_narrative <- generate_narrative(
        county_data[county_data$`Data Type` == "Crude prevalence",],
        state_data[state_data$`Data Type` == "Crude prevalence",],
        "state", highlighted_year, highlighted_county, highlighted_crude_prevalence
      )
    } else if (comparison == "Region") {
      age_adjusted_narrative <- generate_narrative(
        county_data[county_data$`Data Type` == "Age-adjusted prevalence",],
        region_data[region_data$`Data Type` == "Age-adjusted prevalence",],
        "region", highlighted_year, highlighted_county, highlighted_age_adjusted_prevalence
      )
      crude_prevalence_narrative <- generate_narrative(
        county_data[county_data$`Data Type` == "Crude prevalence",],
        region_data[region_data$`Data Type` == "Crude prevalence",],
        "region", highlighted_year, highlighted_county, highlighted_crude_prevalence
      )
    } else if (comparison == "CHB") {
      age_adjusted_narrative <- generate_narrative(
        county_data[county_data$`Data Type` == "Age-adjusted prevalence",],
        chb_data[chb_data$`Data Type` == "Age-adjusted prevalence",],
        "CHB", highlighted_year, highlighted_county, highlighted_age_adjusted_prevalence
      )
      crude_prevalence_narrative <- generate_narrative(
        county_data[county_data$`Data Type` == "Crude prevalence",],
        chb_data[chb_data$`Data Type` == "Crude prevalence",],
        "CHB", highlighted_year, highlighted_county, highlighted_crude_prevalence
      )
    }
    
    HTML(paste(age_adjusted_narrative, "<br><br>", crude_prevalence_narrative))
  })
  
  # Render CHD Exposure Map ---------------------------------------------------------
  output$mn_adults_chd_exposure_map <- renderPlotly({
    selected_prevalence <- input$parLocal_prevalence
    
    exposure_data <- Selected_Locations |>
      filter(Data_Value_Type == selected_prevalence) |>
      mutate(LocationName = ifelse(LocationName == "St. Louis", "St Louis", LocationName)) |>
      select(LocationName, Data_Value, Region, CHB, Low_Confidence_Limit, High_Confidence_Limit) |>
      rename(County = LocationName,
             `Point Estimate` = Data_Value,
             `Low Confidence Limit` = Low_Confidence_Limit,
             `High Confidence Limit` = High_Confidence_Limit) |>
      mutate(County = standardize_county_names(County))
    
    mn_map_data <- map_data("county", region = "minnesota")
    mn_map_data$subregion <- standardize_county_names(mn_map_data$subregion)
    
    map_data <- merge(mn_map_data, exposure_data, by.x = "subregion", by.y = "County", all.x = TRUE) |>
      arrange(order)
    
    plot <- ggplot(map_data, aes(x = long, y = lat, group = group, fill = `Point Estimate`, text = paste(
      "<br>Region:", Region,
      "<br>CHB:", CHB,
      "<br>County:", subregion,
      "<br>", selected_prevalence, ":", round(`Point Estimate`, 2), "%",
      "<br>95% CI:", round(`Low Confidence Limit`, 2), "-", round(`High Confidence Limit`, 2)
    ))) +
      geom_polygon(color = "black") +
      scale_fill_gradient(low = "#78BE21", high = "#003865", na.value = "grey50") +
      theme_void() +
      theme(legend.position = "right") +
      labs(fill = paste(selected_prevalence, "(%)"))
    
    ggplotly(plot, tooltip = "text") |>
      layout(hoverlabel = list(bgcolor = "white", bordercolor = "black", font = list(color = "black")))
  })
  
  # Bookmarking -----------------------------------------------------------------
  # the next 8 lines are from https://mastering-shiny.org/action-bookmark.html as well as enableBookmarking in the shinyApp function
  # Automatically bookmark every time an input changes
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  # Update the query string
  onBookmarked(updateQueryString)
}

# Create Shiny app
shinyApp(ui = ui, server = server, enableBookmarking = "url") # Run the Shiny application