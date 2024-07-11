# Shiny Dashboard

# Load Packages ---------------------------------------------------------------
library(shiny)            # shiny package for building interactive web applications
library(ggplot2)          # ggplot2 package for creating graphics
library(dplyr)            # dplyr package for data manipulation
library(shinydashboard)   # shinydashboard package for creating dashboards
library(shinyjs)          # shinyjs package for adding JavaScript functionality
library(tidyr)            # tidyr package for handling missing values

# Load census estimate data for Minnesota from GitHub -------------------------
CensusEstMN <- read.csv(
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/2020%20to%202022%20Pop.%20Estimates/cc-est2022-agesex.csv'
) # Reading census estimate data for Minnesota from a CSV file hosted on GitHub

# List of URLs from GitHub for Coronary Heart Disease (CHD) data CSV files from 2018 to 2021
CHD_files <- list(
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2018.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2019.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2020.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2021.csv'
) # Creating a list of URLs for CHD data CSV files from 2018 to 2021 hosted on GitHub

# Read each CHD data file into a list of data frames
CHD_data <- lapply(CHD_files, read.csv) # Reading each CHD data file into a list of data frames using lapply

# Read Community Health Board (CHB) data from a CSV file from GitHub
chb_raw <- read.csv(
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/Community%20Health%20Board%20as%20of%201_17_2024.csv'
) # Reading CHB data from a CSV file hosted on GitHub

# Read Minnesota region data from a CSV file from GitHub
mn_region_raw <- read.csv(
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/State%20Community%20Health%20Services%20Advisory%20Committee%20as%20of%201_17_2024.csv'
) # Reading Minnesota region data from a CSV file hosted on GitHub

# Data Wrangling --------------------------------------------------------------
# Clean and merge CHD data
CHD_data <- lapply(CHD_data, function(df) {
  df$LocationID <- ifelse("Latitude" %in% colnames(df), as.character(df$Latitude), NA) # Adding LocationID if the Latitude column exists
  colnames(df)[colnames(df) == 'Geolocatioin'] <- 'Geolocation' # Correcting the spelling of Geolocation column if needed
  df
}) # Cleaning each CHD data frame by adding LocationID and correcting the Geolocation column name

# Combine all cleaned CHD data frames into one data frame
CHD_Final <- bind_rows(CHD_data) # Combining all cleaned CHD data frames into one data frame using bind_rows

# Filter and select specific locations and data for the year 2021 in MN
Selected_Locations <- CHD_Final |>
  filter(Year == 2021, StateAbbr == "MN") |>
  left_join(mn_region_raw, by = c("LocationName" = "County")) |>
  left_join(chb_raw, by = c("LocationName" = "County"))
# Filtering CHD data for the year 2021 and state of Minnesota, then joining with region and CHB data

# Remove "County" from county names in census data
CensusEstMN$CTYNAME <- gsub(" County", "", CensusEstMN$CTYNAME) # Removing "County" from county names in the census data

# Population estimates for CHD in MN
PopEst_CHDMN <- CensusEstMN |>
  filter(YEAR == 3) |> # Filtering census data for the year 2021
  inner_join(Selected_Locations, by = c("CTYNAME" = "LocationName")) |> # Joining with selected CHD locations
  select(CTYNAME, Data_Value_Type, AGE18PLUS_TOT, Measure, Data_Value, High_Confidence_Limit, Low_Confidence_Limit, Region, CHB)
# Selecting relevant columns

# Function to calculate aggregate values
aggregate_values <- function(df, userInput, filterBy) {
  df |>
    filter(!!sym(filterBy) == userInput) |>
    mutate(Aggregate_Data_Value = Data_Value * AGE18PLUS_TOT / 100,
           Aggregate_Low_Confidence_Limit = Low_Confidence_Limit * AGE18PLUS_TOT / 100,
           Aggregate_High_Confidence_Limit = High_Confidence_Limit * AGE18PLUS_TOT / 100) |>
    group_by(across(all_of(filterBy)), Data_Value_Type) |>
    summarise(Aggregate_Data_Value = sum(Aggregate_Data_Value) / sum(AGE18PLUS_TOT) * 100,
              Aggregate_Low_Confidence_Limit = sum(Aggregate_Low_Confidence_Limit) / sum(AGE18PLUS_TOT) * 100,
              Aggregate_High_Confidence_Limit = sum(Aggregate_High_Confidence_Limit) / sum(AGE18PLUS_TOT) * 100, .groups = 'drop')
} # Defining a function to calculate aggregate values for CHD data

# Pre-calculate Minnesota total
mn_total <- PopEst_CHDMN |>
  mutate(StateAbbr = "MN") |> # Adding StateAbbr column with value "MN"
  aggregate_values("MN", 'StateAbbr') |> # Calculating the aggregate CHD data for the state of Minnesota
  mutate(across(everything(), ~tidyr::replace_na(., 0))) # Handling NA values

# Function to compute y-axis limits -------------------------------------------
compute_y_axis_limits <- function(data_list) {
  min_value <- min(sapply(data_list, function(df) min(df$`Low Confidence Limit`, na.rm = TRUE))) # Finding the minimum value across all data frames
  max_value <- max(sapply(data_list, function(df) max(df$`High Confidence Limit`, na.rm = TRUE))) # Finding the maximum value across all data frames
  c(min_value, max_value) # Returning the range of y-axis limits
} # This helps fixing the y-axis to particular values because at first the y-axis of the four graphs had different values and it was changing.

# Function to create ggplot graph ---------------------------------------------
chd_plot <- function(data, y_limits) {
  ggplot(data, aes(x = `Data Type`, y = `Point Estimate`, color = `Data Type`)) +
    geom_errorbar(aes(ymin = `Low Confidence Limit`, ymax = `High Confidence Limit`), width = 0.2) +
    geom_point() +
    ylim(y_limits) +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(), # Removing y-axis title
      axis.text.x = element_blank(), # Removing x-axis text
      axis.title.x = element_blank(), # Removing x-axis title
      legend.text = element_text(size = 12) # Setting legend text size
    )
} # Defining a function to create ggplot graphs with consistent formatting

# Define UI -------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "CDC Places to MN Regions", titleWidth = 400), # Creating dashboard header with title and width
  dashboardSidebar(
    width = 350, # Setting sidebar width
    selectInput("parGlobal_county", label = "Select County of Interest", choices = sort(unique(mn_region_raw$County)), selected = "Kittson", width = 350), # County selection input
    selectInput("parLocal_chdYear", label = "Select Year", choices = sort(unique(Selected_Locations$Year), decreasing = TRUE), selected = max(unique(Selected_Locations$Year)), width = 350), # Year selection input
    selectInput("par_chdStateRegionChb", label = "Select Comparison", choices = c("All", "State", "Region", "CHB"), selected = "All", multiple = FALSE, width = 350), # Comparison selection input
    sidebarMenu(
      menuItem("Home", tabName = "tn_homePage"), # Home tab
      menuItem("Region & CHB Definition", tabName = "tn_regionChbDefinitions"), # Region & CHB Definition tab
      menuItem("Coronary Heart Disease", tabName = "tn_coronaryHeartDisease") # Coronary Heart Disease tab
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(), # Enabling shinyjs for JavaScript functionality
    tabItems(
      tabItem(
        tabName = "tn_homePage",
        tabsetPanel(
          tabPanel(
            "Home Page",
            fluidRow(
              column(
                width = 12,
                h1("Welcome to the CDC PLACES MN Region Dashboard"), # Welcome message
                h4("This Shiny application replicates the work represented ", tags$a(href="https://data.web.health.state.mn.us/web/mndata/", "here!", target= "_blank")), # Link to external site
                tags$h4("Before the CDC Places project, the CDC Behavioral Risk Factor Surveillance System BRFSS, allowed for state projected healthcare indicators. This process was not able to be applied to the county level. Now, with CDC Places counties can view some projected healthcare indicators. However, currently the CDC Places project does not show in an easy format aggregate county regions. By doing this project, I am not only going to help Quin County CHS, but other county regions in the state of MN or even the US."), # Project description
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
                  status = "primary", # Box status
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

# Server Logic ----------------------------------------------------------------
server <- function(input, output, session) {
  observe({
    updateSelectInput(
      session,
      "parGlobal_region",
      choices = unique(mn_region_raw$RegionName)
    ) # Updating region input choices based on unique region names in the data
  })
  
  observe({
    updateSelectInput(
      session,
      "parGlobal_chb",
      choices = unique(chb_raw$CHBName)
    ) # Updating CHB input choices based on unique CHB names in the data
  })
  
  output$region_narrative <- renderUI({
    filtered_region <- mn_region_raw |>
      filter(County == input$parGlobal_county)
    HTML(
      paste0(
        "<b>", unique(filtered_region$RegionName), " Region</b> is made up of the following counties: ",
        paste(unique(filtered_region$County), collapse = ", "), "."
      )
    ) # Creating HTML content for displaying the region narrative based on the selected county
  })
  
  output$chb_narrative_01 <- renderUI({
    filtered_chb <- chb_raw |>
      filter(County == input$parGlobal_county)
    HTML(
      paste0(
        "<b>", unique(filtered_chb$CHBName), " Community Health Board</b> includes: ",
        paste(unique(filtered_chb$County), collapse = ", "), "."
      )
    ) # Creating HTML content for displaying the CHB narrative based on the selected county
  })
  
  # Helper function to highlight selected county in text
  highlight_county <- function(text, selected_county) {
    gsub(selected_county, paste0("<font color='red'>", selected_county, "</font>"), text)
  } # Defining a function to highlight the selected county in red
  
  output$region_counties <- renderUI({
    selected_county <- input$parGlobal_county
    regions <- mn_region_raw |>
      group_by(Region) |>
      summarise(Counties = paste(County, collapse = ", "))
    
    regions_text <- regions |>
      mutate(Text = paste0("<b>", Region, " Region::</b> ", Counties)) |>
      pull(Text)
    
    regions_text <- sapply(regions_text, highlight_county, selected_county = selected_county)
    HTML(paste(regions_text, collapse = "<br>"))
  }) # Rendering HTML for regions and counties list, highlighting the selected county
  
  output$chb_counties <- renderUI({
    selected_county <- input$parGlobal_county
    chbs <- chb_raw |>
      group_by(CHB) |>
      summarise(Counties = paste(County, collapse = ", "))
    
    chb_text <- chbs |>
      mutate(Text = paste0("<b>", CHB, "::</b> ", Counties)) |>
      pull(Text)
    
    chb_text <- sapply(chb_text, highlight_county, selected_county = selected_county)
    HTML(paste(chb_text, collapse = "<br>"))
  }) # Rendering HTML for CHBs and counties list, highlighting the selected county
  
  output$selected_county_title <- renderText({
    HTML(paste("Coronary Heart Disease Exposure", "<br/>", input$parGlobal_county, "County"))
  }) # Creating the title for the selected county
  
  output$selected_region_title <- renderText({
    county_region <- mn_region_raw |>
      filter(County == input$parGlobal_county) |>
      pull(Region) |>
      unique()
    HTML(paste("Coronary Heart Disease Exposure", "<br/>", county_region, "Region"))
  }) # Creating the title for the selected region
  
  output$selected_state_title <- renderText({
    HTML(paste("Coronary Heart Disease Exposure", "<br/>Minnesota"))
  }) # Creating the title for the state
  
  output$selected_chb_title <- renderText({
    county_chb <- chb_raw |>
      filter(County == input$parGlobal_county) |>
      pull(CHB) |>
      unique()
    HTML(paste("Coronary Heart Disease Exposure", "<br/>", county_chb, "CHB"))
  }) # Creating the title for the selected CHB
  
  # Reactive Data for plotting--------------
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
  }) # Filtering and aggregating CHD data for the selected county and renaming columns for the summary table
  
  reactive_region_data <- reactive({
    county_region <- mn_region_raw |>
      filter(County == input$parGlobal_county) |>
      pull(Region) |>
      unique()
    PopEst_CHDMN |>
      filter(Region == county_region) |>
      aggregate_values(county_region, 'Region') |>
      select(-Region) |>
      rename(
        `Data Type` = Data_Value_Type,
        `Point Estimate` = Aggregate_Data_Value,
        `Low Confidence Limit` = Aggregate_Low_Confidence_Limit,
        `High Confidence Limit` = Aggregate_High_Confidence_Limit
      ) |>
      select(`Data Type`, `Low Confidence Limit`, `Point Estimate`, `High Confidence Limit`)
  }) # Filtering and aggregating CHD data for the selected region and renaming columns for the summary table
  
  reactive_chb_data <- reactive({
    county_chb <- chb_raw |>
      filter(County == input$parGlobal_county) |>
      pull(CHB) |>
      unique()
    PopEst_CHDMN |>
      filter(CHB == county_chb) |>
      aggregate_values(county_chb, 'CHB') |>
      select(-CHB) |>
      rename(
        `Data Type` = Data_Value_Type,
        `Point Estimate` = Aggregate_Data_Value,
        `Low Confidence Limit` = Aggregate_Low_Confidence_Limit,
        `High Confidence Limit` = Aggregate_High_Confidence_Limit
      ) |>
      select(`Data Type`, `Low Confidence Limit`, `Point Estimate`, `High Confidence Limit`)
  }) # Filtering and aggregating CHD data for the selected CHB and renaming columns for the summary table
  
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
    compute_y_axis_limits(data_list) # Computing y-axis limits for all plots, handling NA values
  })
  
  output$plot_county <- renderPlot({
    chd_plot(reactive_county_data(), y_axis_limits()) # Rendering plot for the selected county with customized y-axis limits
  })
  
  output$plot_chbRegion <- renderPlot({
    chd_plot(reactive_region_data(), y_axis_limits()) # Rendering plot for the selected region with customized y-axis limits
  })
  
  output$plot_chdCHB <- renderPlot({
    chd_plot(reactive_chb_data(), y_axis_limits()) # Rendering plot for the selected CHB with customized y-axis limits
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
    chd_plot(data, y_axis_limits()) # Rendering plot for the state with customized y-axis limits
  })
  
  # Summary Tables -----------------------------------------------------------
  output$table_county <- renderTable({
    reactive_county_data() # Rendering summary table for the selected county
  })
  
  output$table_region <- renderTable({
    reactive_region_data() # Rendering summary table for the selected region
  })
  
  output$table_chb <- renderTable({
    reactive_chb_data() # Rendering summary table for the selected CHB
  })
  
  output$table_state <- renderTable({
    mn_total |>
      rename(
        `Data Type` = Data_Value_Type,
        `Point Estimate` = Aggregate_Data_Value,
        `Low Confidence Limit` = Aggregate_Low_Confidence_Limit,
        `High Confidence Limit` = Aggregate_High_Confidence_Limit
      ) |>
      select(`Data Type`, `Low Confidence Limit`, `Point Estimate`, `High Confidence Limit`)
  }) # Rendering summary table for the state with renamed columns
}

# Run the app -----------------------------------------------------------------
shinyApp(ui = ui, server = server) # Running the Shiny application 