# Shiny Dashboard -------------------------------------------------------------

# Load necessary libraries for the application
library(shiny)            # shiny package for building interactive web applications
library(ggplot2)          # ggplot2 package for creating graphics
library(dplyr)            # dplyr package for data manipulation
library(shinydashboard)   # shinydashboard package for creating dashboards
library(shinyjs)          # shinyjs package for adding JavaScript functionality
library(tidyr)            # tidyr package for tidying and handling missing values

# Load Data from GitHub -------------------------------------------------------
# Read census estimate data for Minnesota from a CSV file hosted on GitHub
CensusEstMN <- read.csv('https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/2020%20to%202022%20Pop.%20Estimates/cc-est2022-agesex.csv')

# List of URLs from GitHub for Coronary Heart Disease (CHD) data CSV files from 2018 to 2021
CHD_files <- list(
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2018.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2019.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2020.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2021.csv'
)

# Reading each CHD data file into a list of data frames
CHD_data <- lapply(CHD_files, read.csv)

# Read Community Health Board (CHB) data from a CSV file from GitHub
chb_raw <- read.csv('https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/Community%20Health%20Board%20as%20of%201_17_2024.csv')

# Read Minnesota region data from a CSV file from GitHub
mn_region_raw <- read.csv('https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/State%20Community%20Health%20Services%20Advisory%20Committee%20as%20of%201_17_2024.csv')

# Data Wrangling --------------------------------------------------------------
# Clean and merge CHD data
CHD_data <- lapply(CHD_data, function(df) {
  df$LocationID <- ifelse("Latitude" %in% colnames(df), as.character(df$Latitude), NA) # Add LocationID if the Latitude column exists
  colnames(df)[colnames(df) == 'Geolocatioin'] <- 'Geolocation' # Correct the spelling of Geolocation column if needed
  df # Return the modified data frame
})

# Combine all cleaned CHD data frames into one data frame
CHD_Final <- bind_rows(CHD_data) # Combine all cleaned CHD data frames into one data frame using bind_rows

# Filter and select specific locations and data for the year 2021 in MN
Selected_Locations <- CHD_Final |>
  filter(Year == 2021, StateAbbr == "MN") |> # Filter CHD data for the year 2021 and state of Minnesota
  left_join(mn_region_raw, by = c("LocationName" = "County")) |> # Join with region data
  left_join(chb_raw, by = c("LocationName" = "County")) # Join with CHB data

# Remove "County" from county names in census data
CensusEstMN$CTYNAME <- gsub(" County", "", CensusEstMN$CTYNAME) # Remove "County" from county names in the census data

# Population estimates for CHD in MN
PopEst_CHDMN <- CensusEstMN |>
  filter(YEAR == 3) |> # Filter census data for the year 2021
  inner_join(Selected_Locations, by = c("CTYNAME" = "LocationName")) |> # Join with selected CHD locations
  select(CTYNAME, Data_Value_Type, AGE18PLUS_TOT, Measure, Data_Value, High_Confidence_Limit, Low_Confidence_Limit, Region, CHB) # Select relevant columns

# Function to calculate aggregate values---------------------------------------
# CDC PLACES methodology aggregate calculation here: https://www.cdc.gov/places/faqs/using-data/index.html
aggregate_values <- function(df, userInput, filterBy) {
  df |>
    filter(!!sym(filterBy) == userInput) |> # Filter by user input
    mutate(
      Aggregate_Data_Value = Data_Value * AGE18PLUS_TOT / 100, # Calculate aggregate data value
      Aggregate_Low_Confidence_Limit = Low_Confidence_Limit * AGE18PLUS_TOT / 100, # Calculate aggregate low confidence limit
      Aggregate_High_Confidence_Limit = High_Confidence_Limit * AGE18PLUS_TOT / 100 # Calculate aggregate high confidence limit
    ) |>
    group_by(across(all_of(filterBy)), Data_Value_Type) |> # Group by filter criteria and data value type
    summarise(
      Aggregate_Data_Value = sum(Aggregate_Data_Value) / sum(AGE18PLUS_TOT) * 100, # Summarise aggregate data value
      Aggregate_Low_Confidence_Limit = sum(Aggregate_Low_Confidence_Limit) / sum(AGE18PLUS_TOT) * 100, # Summarise aggregate low confidence limit
      Aggregate_High_Confidence_Limit = sum(Aggregate_High_Confidence_Limit) / sum(AGE18PLUS_TOT) * 100, # Summarise aggregate high confidence limit
      .groups = 'drop' # Drop grouping
    )
}

# Pre-calculate Minnesota total------------------------------------------------
mn_total <- PopEst_CHDMN |>
  mutate(StateAbbr = "MN") |> # Add state abbreviation
  aggregate_values("MN", 'StateAbbr') |> # Calculate the aggregate CHD data for the state of Minnesota
  mutate(across(everything(), ~tidyr::replace_na(., 0))) # Handle NA values

# Function to compute y-axis limits -------------------------------------------
compute_y_axis_limits <- function(data_list) {
  min_value <- min(sapply(data_list, function(df) min(df$`Low Confidence Limit`, na.rm = TRUE))) # Find the minimum value across all data frames
  max_value <- max(sapply(data_list, function(df) max(df$`High Confidence Limit`, na.rm = TRUE))) # Find the maximum value across all data frames
  c(min_value, max_value) # Return the range of y-axis limits
}

# Function to create ggplot graph ---------------------------------------------
chd_plot <- function(data, y_limits) {
  ggplot(data, aes(x = `Data Type`, y = `Point Estimate`, color = `Data Type`)) +
    geom_errorbar(aes(ymin = `Low Confidence Limit`, ymax = `High Confidence Limit`), width = 0.2) + # Add error bars
    geom_point() + # Add points
    ylim(y_limits) + # Set y-axis limits
    theme_minimal() + # Use minimal theme
    theme(
      axis.title.y = element_blank(), # Remove y-axis title
      axis.text.x = element_blank(),  # Remove x-axis text
      axis.title.x = element_blank(), # Remove x-axis title
      legend.text = element_text(size = 12) # Set legend text size
    )
}

# Function to generate narrative text -----------------------------------------
generate_narrative <- function(county_data, comparison_data, comparison_name, highlighted_year, highlighted_county, data_type) {
  paste0(
    "In ", highlighted_year, ", <b>adults aged ≥18 years</b> in ", highlighted_county, " had a <b>coronary heart disease</b> ",
    data_type, " of <b>", round(county_data$`Point Estimate`, 2), "% (95% CI: ", round(county_data$`Low Confidence Limit`, 2), "-",
    round(county_data$`High Confidence Limit`, 2), ")</b>, compared to the ", comparison_name, "'s <b>",
    round(comparison_data$`Point Estimate`, 2), "% (95% CI: ", round(comparison_data$`Low Confidence Limit`, 2), "-",
    round(comparison_data$`High Confidence Limit`, 2), ")</b>."
  )
}

# Bookmarking Functionality ---------------------------------------------------
enableBookmarking(store = "url") # Enable bookmarking with URL storage

# Define UI -------------------------------------------------------------------
ui <- function(request) {
  dashboardPage(
    dashboardHeader(title = "CDC Places to MN Regions", titleWidth = 400), # Create dashboard header with title
    dashboardSidebar(
      width = 350,
      div(style = "margin-bottom: 10px;", bookmarkButton(label = "Bookmark")), # Add bookmark button
      selectInput("parGlobal_region", label = "Select SCHSAC Region of Interest", choices = sort(unique(mn_region_raw$Region)), selected = NULL, width = 350), # Create dropdown for selecting SCHSAC region
      selectInput("parGlobal_county", label = "Select County of Interest", choices = sort(unique(mn_region_raw$County)), selected = NULL, width = 350), # Create dropdown for selecting county
      selectInput("parLocal_chdYear", label = "Select Year", choices = sort(unique(Selected_Locations$Year), decreasing = TRUE), selected = max(unique(Selected_Locations$Year)), width = 350), # Create dropdown for selecting year
      selectInput("par_chdStateRegionChb", label = "Select Comparison", choices = c("All", "State", "Region", "CHB"), selected = "All", multiple = FALSE, width = 350), # Create dropdown for selecting comparison type
      sidebarMenu(
        menuItem("Home", tabName = "tn_homePage"), # Create menu item for Home page
        menuItem("Region & CHB Definition", tabName = "tn_regionChbDefinitions"), # Create menu item for Region & CHB Definition
        menuItem("Coronary Heart Disease", tabName = "tn_coronaryHeartDisease") # Create menu item for Coronary Heart Disease
      )
    ),
    dashboardBody(
      shinyjs::useShinyjs(), # Enable shinyjs functionality
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
                  h4("This Shiny application replicates the work represented ", tags$a(href="https://data.web.health.state.mn.us/web/mndata/", "here!", target= "_blank")), # Link to external site
                  tags$h4("Before the CDC Places project, the CDC Behavioral Risk Factor Surveillance System BRFSS, allowed for state projected healthcare indicators. This process was not able to be applied to the county level. Now, with CDC Places counties can view some projected healthcare indicators. However, currently the CDC Places project does not show in an easy format aggregate county regions. By doing this project, I am not only going to help Quin County CHS, but other county regions in the state of MN or even the US."), # Description of the project
                  tags$h3("Those involved with this project are:"), # Project participants
                  tags$h4(tags$b("Emmanuel Fle Chea"), ", MPH, Public Health Data Science, University of Minnesota School of Public Health"), # Participant 1
                  tags$h4(tags$b("Mr. Patrick Olson"), " (Preceptor), Quin County Community Health Board, Community Resource Liaison/Associate/Researcher") # Participant 2
                )
              )
            )
          )
        ),
        tabItem(
          tabName = "tn_regionChbDefinitions",
          fluidRow(
            column(
              width = 12,
              tabsetPanel(
                tabPanel(
                  "Region/CHB",
                  fluidRow(
                    column(
                      width = 12,
                      h3(HTML("Updating the Select County of Interest filter will highlight the county in <font color=red>red</font> while the Regions and Community Health Boards will remain in <b>bold</b>.")), # Explanation of functionality
                      h3("For this tab, the Select SCHSAC Region and Select Community Health Board filters are greyed out because they do not execute any function on this tab."), # Note on disabled filters
                      h3("The purpose for this tab is to provide a quick reference for what counties fall under which region and Community Health Board."), # Purpose of the tab
                      tags$hr(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;") # Horizontal rule for separation
                    )
                  ),
                  fluidRow(
                    column(6, uiOutput("region_narrative", style = "font-size: 20px;")), # Region narrative output
                    column(6, uiOutput("chb_narrative_01", style = "font-size: 20px;")) # CHB narrative output
                  ),
                  fluidRow(
                    column(
                      width = 12,
                      h3("Regions and Counties"), # Regions and Counties heading
                      uiOutput("region_counties") # UI output for regions and counties list
                    ),
                    column(
                      width = 12,
                      h3("Community Health Boards"), # Community Health Boards heading
                      uiOutput("chb_counties") # UI output for CHB and counties list
                    )
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
                  uiOutput("narrative_text", style = "font-size: 16px;") # Display narrative text above the graphs
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  box(
                    title = uiOutput("selected_state_title"), # State title output
                    status = "primary", # Box status
                    solidHeader = TRUE, # Solid header
                    collapsible = TRUE, # Collapsible box
                    width = NULL, # Full width
                    plotOutput("plot_state", height = "200px"), # State plot output
                    tableOutput("table_state") # State table output
                  )
                ),
                column(
                  width = 6,
                  box(
                    title = uiOutput("selected_region_title"), # Region title output
                    status = "primary", # Box status. "primary": Blue (sometimes dark blue); "success": Green; "info": Blue; "warning": Orange; "danger": Red; NULL: no background color
                    solidHeader = TRUE, # Solid header
                    collapsible = TRUE, # Collapsible box
                    width = NULL, # Full width
                    plotOutput("plot_chbRegion", height = "200px"), # Region plot output
                    tableOutput("table_region") # Region table output
                  )
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  box(
                    title = uiOutput("selected_chb_title"), # CHB title output
                    status = "primary", # Box status
                    solidHeader = TRUE, # Solid header
                    collapsible = TRUE, # Collapsible box
                    width = NULL, # Full width
                    plotOutput("plot_chdCHB", height = "200px"), # CHB plot output
                    tableOutput("table_chb") # CHB table output
                  )
                ),
                column(
                  width = 6,
                  box(
                    title = uiOutput("selected_county_title"), # County title output
                    status = "primary", # Box status
                    solidHeader = TRUE, # Solid header
                    collapsible = TRUE, # Collapsible box
                    width = NULL, # Full width
                    plotOutput("plot_county", height = "200px"), # County plot output
                    tableOutput("table_county") # County table output
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
server <- function(input, output, session) {
  # Update the Select County of Interest dropdown based on the selected SCHSAC Region
  observe({
    region <- input$parGlobal_region
    if (!is.null(region) && region != "") {
      # Filter counties based on the selected SCHSAC Region
      counties_in_region <- mn_region_raw |>
        filter(Region == region) |>
        pull(County)
      updateSelectInput(session, "parGlobal_county", choices = sort(unique(counties_in_region)))
    } else {
      updateSelectInput(session, "parGlobal_county", choices = sort(unique(mn_region_raw$County)))
    }
  })
  
  observe({
    updateSelectInput(session, "parGlobal_chb", choices = unique(chb_raw$CHBName)) # Update CHB input choices based on unique CHB names in the data
  })
  
  output$region_narrative <- renderUI({
    filtered_region <- mn_region_raw |> filter(County == input$parGlobal_county) # Filter region data based on the selected county
    HTML(paste0("<b>", unique(filtered_region$RegionName), " Region</b> is made up of the following counties: ", paste(unique(filtered_region$County), collapse = ", "), ".")) # Generate region narrative text
  })
  
  output$chb_narrative_01 <- renderUI({
    filtered_chb <- chb_raw |> filter(County == input$parGlobal_county) # Filter CHB data based on the selected county
    HTML(paste0("<b>", unique(filtered_chb$CHBName), " Community Health Board</b> includes: ", paste(unique(filtered_chb$County), collapse = ", "), ".")) # Generate CHB narrative text
  })
  
  highlight_text <- function(text, keyword) {
    gsub(keyword, paste0("<font color='red'>", keyword, "</font>"), text) # Highlight the selected county in red
  }
  
  output$region_counties <- renderUI({
    selected_county <- input$parGlobal_county
    regions <- mn_region_raw |>
      group_by(Region) |>
      summarise(Counties = paste(County, collapse = ", ")) # Group and summarize counties by region
    
    regions_text <- regions |>
      mutate(Text = paste0("<b>", Region, " Region::</b> ", Counties)) |>
      pull(Text) # Create region text
    
    regions_text <- sapply(regions_text, highlight_text, keyword = selected_county) # Highlight selected county
    HTML(paste(regions_text, collapse = "<br>")) # Render HTML for regions and counties list
  })
  
  output$chb_counties <- renderUI({
    selected_county <- input$parGlobal_county
    chbs <- chb_raw |>
      group_by(CHB) |>
      summarise(Counties = paste(County, collapse = ", ")) # Group and summarize counties by CHB
    
    chb_text <- chbs |>
      mutate(Text = paste0("<b>", CHB, "::</b> ", Counties)) |>
      pull(Text) # Create CHB text
    
    chb_text <- sapply(chb_text, highlight_text, keyword = selected_county) # Highlight selected county
    HTML(paste(chb_text, collapse = "<br>")) # Render HTML for CHBs and counties list
  })
  
  output$selected_county_title <- renderText({
    HTML(paste("Coronary Heart Disease Exposure", "<br/>", input$parGlobal_county, "County")) # Create the title for the selected county
  })
  
  output$selected_region_title <- renderText({
    county_region <- mn_region_raw |> filter(County == input$parGlobal_county) |> pull(Region) |> unique() # Get region for selected county
    HTML(paste("Coronary Heart Disease Exposure", "<br/>", county_region, "Region")) # Create the title for the selected region
  })
  
  output$selected_state_title <- renderText({
    HTML(paste("Coronary Heart Disease Exposure", "<br/>Minnesota")) # Create the title for the state
  })
  
  output$selected_chb_title <- renderText({
    county_chb <- chb_raw |> filter(County == input$parGlobal_county) |> pull(CHB) |> unique() # Get CHB for selected county
    HTML(paste("Coronary Heart Disease Exposure", "<br/>", county_chb, "CHB")) # Create the title for the selected CHB
  })
  
  # Reactive Data for plotting ------------------------------------------------
  reactive_county_data <- reactive({
    PopEst_CHDMN |>
      filter(CTYNAME == input$parGlobal_county) |> # Filter data for selected county
      aggregate_values(input$parGlobal_county, "CTYNAME") |> # Aggregate data for selected county
      select(-CTYNAME) |>
      rename(
        `Data Type` = Data_Value_Type,
        `Point Estimate` = Aggregate_Data_Value,
        `Low Confidence Limit` = Aggregate_Low_Confidence_Limit,
        `High Confidence Limit` = Aggregate_High_Confidence_Limit
      ) |>
      select(`Data Type`, `Low Confidence Limit`, `Point Estimate`, `High Confidence Limit`)
  })
  
  reactive_region_data <- reactive({
    county_region <- mn_region_raw |> filter(County == input$parGlobal_county) |> pull(Region) |> unique() # Get region for selected county
    PopEst_CHDMN |>
      filter(Region == county_region) |> # Filter data for selected region
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
  
  reactive_chb_data <- reactive({
    county_chb <- chb_raw |> filter(County == input$parGlobal_county) |> pull(CHB) |> unique() # Get CHB for selected county
    PopEst_CHDMN |>
      filter(CHB == county_chb) |> # Filter data for selected CHB
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
  
  # Compute y-axis limits
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
    compute_y_axis_limits(data_list) # Compute y-axis limits for all plots, handling NA values
  })
  
  output$plot_county <- renderPlot({
    chd_plot(reactive_county_data(), y_axis_limits()) # Render plot for the selected county with customized y-axis limits
  })
  
  output$plot_chbRegion <- renderPlot({
    chd_plot(reactive_region_data(), y_axis_limits()) # Render plot for the selected region with customized y-axis limits
  })
  
  output$plot_chdCHB <- renderPlot({
    chd_plot(reactive_chb_data(), y_axis_limits()) # Render plot for the selected CHB with customized y-axis limits
  })
  
  output$plot_state <- renderPlot({
    data <- mn_total |>
      rename(
        `Data Type` = Data_Value_Type,
        `Point Estimate` = Aggregate_Data_Value,
        `Low Confidence Limit` = Aggregate_Low_Confidence_Limit,
        `High Confidence Limit` = Aggregate_High_Confidence_Limit
      ) |>
      select(`Data Type`, `Low Confidence Limit`, `Point Estimate`, `High Confidence Limit`)
    chd_plot(data, y_axis_limits()) # Render plot for the state with customized y-axis limits
  })
  
  # Summary Tables -----------------------------------------------------------
  output$table_county <- renderTable({
    reactive_county_data() # Render summary table for the selected county
  })
  
  output$table_region <- renderTable({
    reactive_region_data() # Render summary table for the selected region
  })
  
  output$table_chb <- renderTable({
    reactive_chb_data() # Render summary table for the selected CHB
  })
  
  output$table_state <- renderTable({
    mn_total |>
      rename(
        `Data Type` = Data_Value_Type,
        `Point Estimate` = Aggregate_Data_Value,
        `Low Confidence Limit` = Aggregate_Low_Confidence_Limit,
        `High Confidence Limit` = Aggregate_High_Confidence_Limit
      ) |>
      select(`Data Type`, `Low Confidence Limit`, `Point Estimate`, `High Confidence Limit`) # Render summary table for the state with renamed columns
  })
  
  # Narrative ----------------------------------------------------------------
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
    comparison <- input$par_chdStateRegionChb
    year <- "2021"
    highlighted_year <- highlight_text(year, year)
    highlighted_county <- highlight_text(county, county)
    highlighted_age_adjusted_prevalence <- highlight_text("age-adjusted prevalence", "age-adjusted prevalence")
    highlighted_crude_prevalence <- highlight_text("crude prevalence", "crude prevalence")
    
    age_adjusted_narrative <- NULL
    crude_prevalence_narrative <- NULL
    
    if (comparison == "All") {
      age_adjusted_narrative <- generate_narrative(
        county_data[county_data$`Data Type` == "Age-adjusted prevalence",],
        state_data[state_data$`Data Type` == "Age-adjusted prevalence",],
        "state", highlighted_year, highlighted_county, highlighted_age_adjusted_prevalence
      )
      age_adjusted_narrative <- paste0(age_adjusted_narrative, " compared to the region's <b>",
                                       round(region_data$`Point Estimate`[region_data$`Data Type` == "Age-adjusted prevalence"], 2), "% (95% CI: ", round(region_data$`Low Confidence Limit`[region_data$`Data Type` == "Age-adjusted prevalence"], 2), "-", round(region_data$`High Confidence Limit`[region_data$`Data Type` == "Age-adjusted prevalence"], 2), ")</b>, the CHB's <b>",
                                       round(chb_data$`Point Estimate`[chb_data$`Data Type` == "Age-adjusted prevalence"], 2), "% (95% CI: ", round(chb_data$`Low Confidence Limit`[chb_data$`Data Type` == "Age-adjusted prevalence"], 2), "-", round(chb_data$`High Confidence Limit`[chb_data$`Data Type` == "Age-adjusted prevalence"], 2), ")</b>."
      )
      
      crude_prevalence_narrative <- generate_narrative(
        county_data[county_data$`Data Type` == "Crude prevalence",],
        state_data[state_data$`Data Type` == "Crude prevalence",],
        "state", highlighted_year, highlighted_county, highlighted_crude_prevalence
      )
      crude_prevalence_narrative <- paste0(crude_prevalence_narrative, " compared to the region's <b>",
                                           round(region_data$`Point Estimate`[region_data$`Data Type` == "Crude prevalence"], 2), "% (95% CI: ", round(region_data$`Low Confidence Limit`[region_data$`Data Type` == "Crude prevalence"], 2), "-", round(region_data$`High Confidence Limit`[region_data$`Data Type` == "Crude prevalence"], 2), ")</b>, the CHB's <b>",
                                           round(chb_data$`Point Estimate`[chb_data$`Data Type` == "Crude prevalence"], 2), "% (95% CI: ", round(chb_data$`Low Confidence Limit`[chb_data$`Data Type` == "Crude prevalence"], 2), "-", round(chb_data$`High Confidence Limit`[chb_data$`Data Type` == "Crude prevalence"], 2), ")</b>."
      )
    } else if (comparison == "State") {
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
    
    HTML(paste(age_adjusted_narrative, "<br><br>", crude_prevalence_narrative)) # Render the narrative text
  })
}

# Run the app -----------------------------------------------------------------
shinyApp(ui = ui, server = server) # Run the Shiny application 