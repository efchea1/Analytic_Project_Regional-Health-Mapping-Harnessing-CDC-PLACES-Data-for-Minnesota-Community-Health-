# Load Packages ---------------------------------------------------------------
library(shiny)            # shiny package for building interactive web applications
library(ggplot2)          # ggplot2 package for creating graphics
library(dplyr)            # dplyr package for data manipulation
library(shinydashboard)   # shinydashboard package for creating dashboards
library(shinyjs)          # shinyjs package for adding JavaScript functionality

# Load Data -------------------------------------------------------------------
# Read census estimate data for Minnesota from a CSV file
CensusEstMN <- read.csv(
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/2020%20to%202022%20Pop.%20Estimates/cc-est2022-agesex.csv'
)

# List of URLs for Coronary Heart Disease (CHD) data files from 2018 to 2021
CHD_files <- list(
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2018.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2019.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2020.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2021.csv'
)

# Read each CHD data file into a list of data frames
CHD_data <- lapply(CHD_files, read.csv)

# Read Community Health Board (CHB) data from a CSV file
chb_raw <- read.csv(
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/Community%20Health%20Board%20as%20of%201_17_2024.csv'
)

# Read Minnesota region data from a CSV file
mn_region_raw <- read.csv(
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/State%20Community%20Health%20Services%20Advisory%20Committee%20as%20of%201_17_2024.csv'
)

# Data Wrangling --------------------------------------------------------------
# Clean and merge CHD data
CHD_data <- lapply(CHD_data, function(df) {
  df$LocationID <- ifelse("Latitude" %in% colnames(df), as.character(df$Latitude), NA) # Add LocationID if Latitude column exists
  colnames(df)[colnames(df) == 'Geolocatioin'] <- 'Geolocation' # Correct spelling of Geolocation column if needed
  df
})

# Combine all cleaned CHD data frames into one data frame
CHD_Final <- bind_rows(CHD_data)

# Filter and select specific locations and data for the year 2021 in MN
Selected_Locations <- CHD_Final |>
  filter(Year == 2021, StateAbbr == "MN") |>
  left_join(mn_region_raw, by = c("LocationName" = "County")) |> # Join with mn_region_raw by County
  left_join(chb_raw, by = c("LocationName" = "County")) # Join with chb_raw by County

# Remove "County" from county names in census data
CensusEstMN$CTYNAME <- gsub(" County", "", CensusEstMN$CTYNAME)

# Population estimates for CHD in MN
PopEst_CHDMN <- CensusEstMN |>
  filter(YEAR == 3) |> # Filter data for the year 2021 (YEAR == 3)
  inner_join(Selected_Locations, by = c("CTYNAME" = "LocationName")) |> # Join census data with selected CHD locations
  select(CTYNAME, Data_Value_Type, AGE18PLUS_TOT, Measure, Data_Value, High_Confidence_Limit, Low_Confidence_Limit, Region, CHB) # Select relevant columns

# Function to calculate aggregate values
aggregate_values <- function(df, userInput, filterBy) {
  df |>
    filter(!!sym(filterBy) == userInput) |> # Filter by user input
    mutate(Aggregate_Data_Value = Data_Value * AGE18PLUS_TOT / 100, # Calculate aggregate data value
           Aggregate_Low_Confidence_Limit = Low_Confidence_Limit * AGE18PLUS_TOT / 100, # Calculate aggregate low confidence limit
           Aggregate_High_Confidence_Limit = High_Confidence_Limit * AGE18PLUS_TOT / 100) |> # Calculate aggregate high confidence limit
    group_by(across(all_of(filterBy)), Data_Value_Type) |> # Group by specified columns and Data_Value_Type
    summarise(Aggregate_Data_Value = sum(Aggregate_Data_Value) / sum(AGE18PLUS_TOT) * 100, # Summarize aggregate data value
              Aggregate_Low_Confidence_Limit = sum(Aggregate_Low_Confidence_Limit) / sum(AGE18PLUS_TOT) * 100, # Summarize aggregate low confidence limit
              Aggregate_High_Confidence_Limit = sum(Aggregate_High_Confidence_Limit) / sum(AGE18PLUS_TOT) * 100, .groups = 'drop') # Summarize aggregate high confidence limit
}

# Pre-calculate Minnesota total ------------------------------------------------
mn_total <- PopEst_CHDMN |>
  mutate(StateAbbr = "MN") |>  # Add StateAbbr column for aggregation
  aggregate_values("MN", 'StateAbbr') # Aggregate CHD data for the state

# Define UI -------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "CDC Places to MN Regions", titleWidth = 400), # Dashboard header with title
  dashboardSidebar(
    width = 350, # Sidebar width
    selectInput("parGlobal_county", label = "Select County of Interest", choices = sort(unique(mn_region_raw$County)), selected = "Kittson", width = 350), # County selection input
    selectInput("parLocal_chdYear", label = "Select Year", choices = sort(unique(Selected_Locations$Year), decreasing = TRUE), selected = max(unique(Selected_Locations$Year)), width = 350), # Year selection input
    selectInput("par_chdStateRegionChb", label = "Select Comparison", choices = c("All", "State", "Region", "Community Health Board"), selected = "All", multiple = FALSE, width = 350), # Comparison selection input
    sidebarMenu(
      menuItem("Home", tabName = "tn_homePage"), # Home tab
      menuItem("Region & CHB Definition", tabName = "tn_regionChbDefinitions"), # Region & CHB Definition tab
      menuItem("Coronary Heart Disease", tabName = "tn_coronaryHeartDisease") # Coronary Heart Disease tab
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(), # Enable shinyjs for JavaScript functionality
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
                  plotOutput("plot_state", height = "300px"), # State plot output
                  tableOutput("table_state") # State table output
                )
              ),
              column(
                width = 6,
                box(
                  title = uiOutput("selected_region_title"), # Region title output
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("plot_chbRegion", height = "300px"), # Region plot output
                  tableOutput("table_region") # Region table output
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                box(
                  title = uiOutput("selected_chb_title"), # CHB title output
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("plot_chdCHB", height = "300px"), # CHB plot output
                  tableOutput("table_chb") # CHB table output
                )
              ),
              column(
                width = 6,
                box(
                  title = uiOutput("selected_county_title"), # County title output
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("plot_confidenceInterval", height = "300px"), # County plot output
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
      choices = unique(mn_region_raw$RegionName) # Update region input choices based on unique region names in the data
    )
  })
  
  observe({
    updateSelectInput(
      session,
      "parGlobal_chb",
      choices = unique(chb_raw$CHBName) # Update CHB input choices based on unique CHB names in the data
    )
  })
  
  output$region_narrative <- renderUI({
    filtered_region <- mn_region_raw |>
      filter(County == input$parGlobal_county) # Filter region data for the selected county
    HTML(
      paste0(
        "<b>", unique(filtered_region$RegionName), " Region</b> is made up of the following counties: ",
        paste(unique(filtered_region$County), collapse = ", "), "." # Create HTML content for displaying the region narrative
      )
    )
  })
  
  output$chb_narrative_01 <- renderUI({
    filtered_chb <- chb_raw |>
      filter(County == input$parGlobal_county) # Filter CHB data for the selected county
    HTML(
      paste0(
        "<b>", unique(filtered_chb$CHBName), " Community Health Board</b> includes: ",
        paste(unique(filtered_chb$County), collapse = ", "), "." # Create HTML content for displaying the CHB narrative
      )
    )
  })
  
  # Helper function to highlight selected county in text
  highlight_county <- function(text, selected_county) {
    gsub(selected_county, paste0("<font color='red'>", selected_county, "</font>"), text) # Highlight the selected county in red
  }
  
  output$region_counties <- renderUI({
    selected_county <- input$parGlobal_county
    regions <- mn_region_raw |>
      group_by(Region) |>
      summarise(Counties = paste(County, collapse = ", ")) # Group data by region and concatenate county names
    
    regions_text <- regions |>
      mutate(Text = paste0("<b>", Region, " Region::</b> ", Counties)) |>
      pull(Text) # Create HTML text with bold region names
    
    regions_text <- sapply(regions_text, highlight_county, selected_county = selected_county) # Highlight selected county in the regions text
    HTML(paste(regions_text, collapse = "<br>")) # Render HTML for regions and counties
  })
  
  output$chb_counties <- renderUI({
    selected_county <- input$parGlobal_county
    chbs <- chb_raw |>
      group_by(CHB) |>
      summarise(Counties = paste(County, collapse = ", ")) # Group data by CHB and concatenate county names
    
    chb_text <- chbs |>
      mutate(Text = paste0("<b>", CHB, "::</b> ", Counties)) |>
      pull(Text) # Create HTML text with bold CHB names
    
    chb_text <- sapply(chb_text, highlight_county, selected_county = selected_county) # Highlight selected county in the CHB text
    HTML(paste(chb_text, collapse = "<br>")) # Render HTML for CHBs and counties
  })
  
  output$selected_county_title <- renderText({
    HTML(paste("Coronary Heart Disease Exposure", "<br/>", input$parGlobal_county, "County")) # Create the name of the selected county above the graph
  })
  
  output$selected_region_title <- renderText({
    county_region <- mn_region_raw |>
      filter(County == input$parGlobal_county) |>
      pull(Region) |>
      unique() # Get the region of the selected county
    HTML(paste("Coronary Heart Disease Exposure", "<br/>", county_region, "Region")) # Create the name of the selected region above the graph
  })
  
  output$selected_state_title <- renderText({
    HTML(paste("Coronary Heart Disease Exposure", "<br/>Minnesota")) # Create the name of the state above the graph
  })
  
  output$selected_chb_title <- renderText({
    county_chb <- chb_raw |>
      filter(County == input$parGlobal_county) |>
      pull(CHB) |>
      unique() # Get the CHB of the selected county
    HTML(paste("Coronary Heart Disease Exposure", "<br/>", county_chb, "CHB")) # Create the name of the selected CHB above the graph
  })
  
  # Reactive Data for plotting--------------
  reactive_CHD_data <- reactive({
    PopEst_CHDMN |>
      filter(CTYNAME == input$parGlobal_county) |>
      aggregate_values(input$parGlobal_county, "CTYNAME") # Filter and aggregate CHD data for the selected county
  })
  
  reactive_region_data <- reactive({
    county_region <- mn_region_raw |>
      filter(County == input$parGlobal_county) |>
      pull(Region) |>
      unique() # Get the region of the selected county
    PopEst_CHDMN |>
      filter(Region == county_region) |>
      aggregate_values(county_region, 'Region') # Filter and aggregate CHD data for the selected region
  })
  
  reactive_chb_data <- reactive({
    county_chb <- chb_raw |>
      filter(County == input$parGlobal_county) |>
      pull(CHB) |>
      unique() # Get the CHB of the selected county
    PopEst_CHDMN |>
      filter(CHB == county_chb) |>
      aggregate_values(county_chb, 'CHB') # Filter and aggregate CHD data for the selected CHB
  })
  
  # Reactive expression to compute y-axis limits
  y_axis_limits <- reactive({
    data_list <- list(
      reactive_CHD_data(),
      reactive_region_data(),
      reactive_chb_data(),
      mn_total
    )
    
    min_value <- min(sapply(data_list, function(df) min(df$Aggregate_Low_Confidence_Limit))) # Compute minimum value across all plots
    max_value <- max(sapply(data_list, function(df) max(df$Aggregate_High_Confidence_Limit))) # Compute maximum value across all plots
    
    c(min_value, max_value) # Return y-axis limits
  })
  
  output$plot_confidenceInterval <- renderPlot({
    data <- reactive_CHD_data() # Get the filtered CHD data
    ggplot(data, aes(x = Data_Value_Type, y = Aggregate_Data_Value, color = Data_Value_Type)) +
      geom_errorbar(aes(ymin = Aggregate_Low_Confidence_Limit, ymax = Aggregate_High_Confidence_Limit), width = 0.2) +
      geom_point() +
      ylim(y_axis_limits()) + # Set y-axis limits
      theme_minimal() +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) # Remove x-axis
  })
  
  output$plot_chbRegion <- renderPlot({
    data <- reactive_region_data() # Get the aggregated CHD data for the region
    ggplot(data, aes(x = Data_Value_Type, y = Aggregate_Data_Value, color = Data_Value_Type)) +
      geom_errorbar(aes(ymin = Aggregate_Low_Confidence_Limit, ymax = Aggregate_High_Confidence_Limit), width = 0.2) +
      geom_point() +
      ylim(y_axis_limits()) + # Set y-axis limits
      theme_minimal() +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) # Remove x-axis
  })
  
  output$plot_chdCHB <- renderPlot({
    data <- reactive_chb_data() # Get the aggregated CHD data for the CHB
    ggplot(data, aes(x = Data_Value_Type, y = Aggregate_Data_Value, color = Data_Value_Type)) +
      geom_errorbar(aes(ymin = Aggregate_Low_Confidence_Limit, ymax = Aggregate_High_Confidence_Limit), width = 0.2) +
      geom_point() +
      ylim(y_axis_limits()) + # Set y-axis limits
      theme_minimal() +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) # Remove x-axis
  })
  
  output$plot_state <- renderPlot({
    data <- mn_total # Get the pre-calculated CHD data for the state
    ggplot(data, aes(x = Data_Value_Type, y = Aggregate_Data_Value, color = Data_Value_Type)) +
      geom_errorbar(aes(ymin = Aggregate_Low_Confidence_Limit, ymax = Aggregate_High_Confidence_Limit), width = 0.2) +
      geom_point() +
      ylim(y_axis_limits()) + # Set y-axis limits
      theme_minimal() +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) # Remove x-axis
  })
  
  # Summary Tables -----------------------------------------------------------
  output$table_county <- renderTable({
    reactive_CHD_data()
  })
  
  output$table_region <- renderTable({
    reactive_region_data()
  })
  
  output$table_chb <- renderTable({
    reactive_chb_data()
  })
  
  output$table_state <- renderTable({
    mn_total
  })
}

# Run the app -----------------------------------------------------------------
shinyApp(ui = ui, server = server) # Run the Shiny application 














# # Load Packages ---------------------------------------------------------------
# library(shiny)            # shiny package for building interactive web applications
# library(ggplot2)          # ggplot2 package for creating graphics
# library(dplyr)            # dplyr package for data manipulation
# library(shinydashboard)   # shinydashboard package for creating dashboards
# library(shinyjs)          # shinyjs package for adding JavaScript functionality
# 
# # Load Data -------------------------------------------------------------------
# # Read census estimate data for Minnesota from a CSV file
# CensusEstMN <- read.csv(
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/2020%20to%202022%20Pop.%20Estimates/cc-est2022-agesex.csv'
# )
# 
# # List of URLs for Coronary Heart Disease (CHD) data files from 2018 to 2021
# CHD_files <- list(
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2018.csv',
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2019.csv',
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2020.csv',
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2021.csv'
# )
# 
# # Read each CHD data file into a list of data frames
# CHD_data <- lapply(CHD_files, read.csv)
# 
# # Read Community Health Board (CHB) data from a CSV file
# chb_raw <- read.csv(
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/Community%20Health%20Board%20as%20of%201_17_2024.csv'
# )
# 
# # Read Minnesota region data from a CSV file
# mn_region_raw <- read.csv(
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/State%20Community%20Health%20Services%20Advisory%20Committee%20as%20of%201_17_2024.csv'
# )
# 
# # Data Wrangling --------------------------------------------------------------
# # Clean and merge CHD data
# CHD_data <- lapply(CHD_data, function(df) {
#   df$LocationID <- ifelse("Latitude" %in% colnames(df), as.character(df$Latitude), NA) # Add LocationID if Latitude column exists
#   colnames(df)[colnames(df) == 'Geolocatioin'] <- 'Geolocation' # Correct spelling of Geolocation column if needed
#   df
# })
# 
# # Combine all cleaned CHD data frames into one data frame
# CHD_Final <- bind_rows(CHD_data)
# 
# # Filter and select specific locations and data for the year 2021 in MN
# Selected_Locations <- CHD_Final |>
#   filter(Year == 2021, StateAbbr == "MN") |>
#   left_join(mn_region_raw, by = c("LocationName" = "County")) |> # Join with mn_region_raw by County
#   left_join(chb_raw, by = c("LocationName" = "County")) # Join with chb_raw by County
# 
# # Remove "County" from county names in census data
# CensusEstMN$CTYNAME <- gsub(" County", "", CensusEstMN$CTYNAME)
# 
# # Population estimates for CHD in MN
# PopEst_CHDMN <- CensusEstMN |>
#   filter(YEAR == 3) |> # Filter data for the year 2021 (YEAR == 3)
#   inner_join(Selected_Locations, by = c("CTYNAME" = "LocationName")) |> # Join census data with selected CHD locations
#   select(CTYNAME, Data_Value_Type, AGE18PLUS_TOT, Measure, Data_Value, High_Confidence_Limit, Low_Confidence_Limit, Region, CHB) # Select relevant columns
# 
# # Function to calculate aggregate values
# aggregate_values <- function(df, userInput, filterBy) {
#   df |>
#     filter(!!sym(filterBy) == userInput) |> # Filter by user input
#     mutate(Aggregate_Data_Value = Data_Value * AGE18PLUS_TOT / 100, # Calculate aggregate data value
#            Aggregate_Low_Confidence_Limit = Low_Confidence_Limit * AGE18PLUS_TOT / 100, # Calculate aggregate low confidence limit
#            Aggregate_High_Confidence_Limit = High_Confidence_Limit * AGE18PLUS_TOT / 100) |> # Calculate aggregate high confidence limit
#     group_by(across(all_of(filterBy)), Data_Value_Type) |> # Group by specified columns and Data_Value_Type
#     summarise(Aggregate_Data_Value = sum(Aggregate_Data_Value) / sum(AGE18PLUS_TOT) * 100, # Summarize aggregate data value
#               Aggregate_Low_Confidence_Limit = sum(Aggregate_Low_Confidence_Limit) / sum(AGE18PLUS_TOT) * 100, # Summarize aggregate low confidence limit
#               Aggregate_High_Confidence_Limit = sum(Aggregate_High_Confidence_Limit) / sum(AGE18PLUS_TOT) * 100, .groups = 'drop') # Summarize aggregate high confidence limit
# }
# 
# # Pre-calculate Minnesota total ------------------------------------------------
# mn_total <- PopEst_CHDMN |>
#   mutate(StateAbbr = "MN") |>  # Add StateAbbr column for aggregation
#   aggregate_values("MN", 'StateAbbr') # Aggregate CHD data for the state
# 
# # Define UI -------------------------------------------------------------------
# ui <- dashboardPage(
#   dashboardHeader(title = "CDC Places to MN Regions", titleWidth = 400), # Dashboard header with title
#   dashboardSidebar(
#     width = 350, # Sidebar width
#     selectInput("parGlobal_county", label = "Select County of Interest", choices = sort(unique(mn_region_raw$County)), selected = "Kittson", width = 350), # County selection input
#     selectInput("parLocal_chdYear", label = "Select Year", choices = sort(unique(Selected_Locations$Year), decreasing = TRUE), selected = max(unique(Selected_Locations$Year)), width = 350), # Year selection input
#     selectInput("par_chdStateRegionChb", label = "Select Comparison", choices = c("All", "State", "Region", "Community Health Board"), selected = "All", multiple = FALSE, width = 350), # Comparison selection input
#     sidebarMenu(
#       menuItem("Home", tabName = "tn_homePage"), # Home tab
#       menuItem("Region & CHB Definition", tabName = "tn_regionChbDefinitions"), # Region & CHB Definition tab
#       menuItem("Coronary Heart Disease", tabName = "tn_coronaryHeartDisease") # Coronary Heart Disease tab
#     )
#   ),
#   dashboardBody(
#     shinyjs::useShinyjs(), # Enable shinyjs for JavaScript functionality
#     tabItems(
#       tabItem(
#         tabName = "tn_homePage",
#         tabsetPanel(
#           tabPanel(
#             "Home Page",
#             fluidRow(
#               column(
#                 width = 12,
#                 h1("Welcome to the CDC PLACES MN Region Dashboard"), # Welcome message
#                 h4("This Shiny application replicates the work represented ", tags$a(href="https://data.web.health.state.mn.us/web/mndata/", "here!", target= "_blank")), # Link to external site
#                 tags$h4("Before the CDC Places project, the CDC Behavioral Risk Factor Surveillance System BRFSS, allowed for state projected healthcare indicators. This process was not able to be applied to the county level. Now, with CDC Places counties can view some projected healthcare indicators. However, currently the CDC Places project does not show in an easy format aggregate county regions. By doing this project, I am not only going to help Quin County CHS, but other county regions in the state of MN or even the US."), # Project description
#                 tags$h3("Those involved with this project are:"), # Project participants
#                 tags$h4(tags$b("Emmanuel Fle Chea"), ", MPH, Public Health Data Science, University of Minnesota School of Public Health"), # Participant 1
#                 tags$h4(tags$b("Mr. Patrick Olson"), " (Preceptor), Quin County Community Health Board, Community Resource Liaison/Associate/Researcher") # Participant 2
#               )
#             )
#           )
#         )
#       ),
#       tabItem(
#         tabName = "tn_regionChbDefinitions",
#         fluidRow(
#           column(
#             width = 12,
#             tabsetPanel(
#               tabPanel(
#                 "Region/CHB",
#                 fluidRow(
#                   column(
#                     width = 12,
#                     h3(HTML("Updating the Select County of Interest filter will highlight the county in <font color=red>red</font> while the Regions and Community Health Boards will remain in <b>bold</b>.")), # Explanation of functionality
#                     h3("For this tab, the Select SCHSAC Region and Select Community Health Board filters are greyed out because they do not execute any function on this tab."), # Note on disabled filters
#                     h3("The purpose for this tab is to provide a quick reference for what counties fall under which region and Community Health Board."), # Purpose of the tab
#                     tags$hr(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;") # Horizontal rule for separation
#                   )
#                 ),
#                 fluidRow(
#                   column(6, uiOutput("region_narrative", style = "font-size: 20px;")), # Region narrative output
#                   column(6, uiOutput("chb_narrative_01", style = "font-size: 20px;")) # CHB narrative output
#                 ),
#                 fluidRow(
#                   column(
#                     width = 12,
#                     h3("Regions and Counties"), # Regions and Counties heading
#                     uiOutput("region_counties") # UI output for regions and counties list
#                   ),
#                   column(
#                     width = 12,
#                     h3("Community Health Boards"), # Community Health Boards heading
#                     uiOutput("chb_counties") # UI output for CHB and counties list
#                   )
#                 )
#               )
#             )
#           )
#         )
#       ),
#       tabItem(
#         tabName = "tn_coronaryHeartDisease",
#         tabsetPanel(
#           id = "tpId_coronaryHeartDisease",
#           tabPanel(
#             "Adults>=18 CHD Exposure",
#             fluidRow(
#               column(
#                 width = 6,
#                 box(
#                   title = uiOutput("selected_state_title"), # State title output
#                   status = "primary", # Box status
#                   solidHeader = TRUE, # Solid header
#                   collapsible = TRUE, # Collapsible box
#                   plotOutput("plot_state") # State plot output
#                 )
#               ),
#               column(
#                 width = 6,
#                 box(
#                   title = uiOutput("selected_region_title"), # Region title output
#                   status = "primary",
#                   solidHeader = TRUE,
#                   collapsible = TRUE,
#                   plotOutput("plot_chbRegion") # Region plot output
#                 )
#               )
#             ),
#             fluidRow(
#               column(
#                 width = 6,
#                 box(
#                   title = uiOutput("selected_chb_title"), # CHB title output
#                   status = "primary",
#                   solidHeader = TRUE,
#                   collapsible = TRUE,
#                   plotOutput("plot_chdCHB") # CHB plot output
#                 )
#               ),
#               column(
#                 width = 6,
#                 box(
#                   title = uiOutput("selected_county_title"), # County title output
#                   status = "primary",
#                   solidHeader = TRUE,
#                   collapsible = TRUE,
#                   plotOutput("plot_confidenceInterval") # County plot output
#                 )
#               )
#             )
#           )
#         )
#       )
#     )
#   )
# )
# 
# # Server Logic ----------------------------------------------------------------
# server <- function(input, output, session) {
#   observe({
#     updateSelectInput(
#       session,
#       "parGlobal_region",
#       choices = unique(mn_region_raw$RegionName) # Update region input choices based on unique region names in the data
#     )
#   })
#   
#   observe({
#     updateSelectInput(
#       session,
#       "parGlobal_chb",
#       choices = unique(chb_raw$CHBName) # Update CHB input choices based on unique CHB names in the data
#     )
#   })
#   
#   output$region_narrative <- renderUI({
#     filtered_region <- mn_region_raw |>
#       filter(County == input$parGlobal_county) # Filter region data for the selected county
#     HTML(
#       paste0(
#         "<b>", unique(filtered_region$RegionName), " Region</b> is made up of the following counties: ",
#         paste(unique(filtered_region$County), collapse = ", "), "." # Create HTML content for displaying the region narrative
#       )
#     )
#   })
#   
#   output$chb_narrative_01 <- renderUI({
#     filtered_chb <- chb_raw |>
#       filter(County == input$parGlobal_county) # Filter CHB data for the selected county
#     HTML(
#       paste0(
#         "<b>", unique(filtered_chb$CHBName), " Community Health Board</b> includes: ",
#         paste(unique(filtered_chb$County), collapse = ", "), "." # Create HTML content for displaying the CHB narrative
#       )
#     )
#   })
#   
#   # Helper function to highlight selected county in text
#   highlight_county <- function(text, selected_county) {
#     gsub(selected_county, paste0("<font color='red'>", selected_county, "</font>"), text) # Highlight the selected county in red
#   }
#   
#   output$region_counties <- renderUI({
#     selected_county <- input$parGlobal_county
#     regions <- mn_region_raw |>
#       group_by(Region) |>
#       summarise(Counties = paste(County, collapse = ", ")) # Group data by region and concatenate county names
#     
#     regions_text <- regions |>
#       mutate(Text = paste0("<b>", Region, " Region::</b> ", Counties)) |>
#       pull(Text) # Create HTML text with bold region names
#     
#     regions_text <- sapply(regions_text, highlight_county, selected_county = selected_county) # Highlight selected county in the regions text
#     HTML(paste(regions_text, collapse = "<br>")) # Render HTML for regions and counties
#   })
#   
#   output$chb_counties <- renderUI({
#     selected_county <- input$parGlobal_county
#     chbs <- chb_raw |>
#       group_by(CHB) |>
#       summarise(Counties = paste(County, collapse = ", ")) # Group data by CHB and concatenate county names
#     
#     chb_text <- chbs |>
#       mutate(Text = paste0("<b>", CHB, "::</b> ", Counties)) |>
#       pull(Text) # Create HTML text with bold CHB names
#     
#     chb_text <- sapply(chb_text, highlight_county, selected_county = selected_county) # Highlight selected county in the CHB text
#     HTML(paste(chb_text, collapse = "<br>")) # Render HTML for CHBs and counties
#   })
#   
#   output$selected_county_title <- renderText({
#     HTML(paste("Coronary Heart Disease Exposure", "<br/>", input$parGlobal_county, "County")) # Create the name of the selected county above the graph
#   })
#   
#   output$selected_region_title <- renderText({
#     county_region <- mn_region_raw |>
#       filter(County == input$parGlobal_county) |>
#       pull(Region) |>
#       unique() # Get the region of the selected county
#     HTML(paste("Coronary Heart Disease Exposure", "<br/>", county_region, "Region")) # Create the name of the selected region above the graph
#   })
#   
#   output$selected_state_title <- renderText({
#     HTML(paste("Coronary Heart Disease Exposure", "<br/>Minnesota")) # Create the name of the state above the graph
#   })
#   
#   output$selected_chb_title <- renderText({
#     county_chb <- chb_raw |>
#       filter(County == input$parGlobal_county) |>
#       pull(CHB) |>
#       unique() # Get the CHB of the selected county
#     HTML(paste("Coronary Heart Disease Exposure", "<br/>", county_chb, "CHB")) # Create the name of the selected CHB above the graph
#   })
#   
#   # Reactive Data for plotting--------------
#   reactive_CHD_data <- reactive({
#     PopEst_CHDMN |>
#       filter(CTYNAME == input$parGlobal_county) |>
#       aggregate_values(input$parGlobal_county, "CTYNAME") # Filter and aggregate CHD data for the selected county
#   })
#   
#   reactive_region_data <- reactive({
#     county_region <- mn_region_raw |>
#       filter(County == input$parGlobal_county) |>
#       pull(Region) |>
#       unique() # Get the region of the selected county
#     PopEst_CHDMN |>
#       filter(Region == county_region) |>
#       aggregate_values(county_region, 'Region') # Filter and aggregate CHD data for the selected region
#   })
#   
#   reactive_chb_data <- reactive({
#     county_chb <- chb_raw |>
#       filter(County == input$parGlobal_county) |>
#       pull(CHB) |>
#       unique() # Get the CHB of the selected county
#     PopEst_CHDMN |>
#       filter(CHB == county_chb) |>
#       aggregate_values(county_chb, 'CHB') # Filter and aggregate CHD data for the selected CHB
#   })
#   
#   # Reactive expression to compute y-axis limits
#   y_axis_limits <- reactive({
#     data_list <- list(
#       reactive_CHD_data(),
#       reactive_region_data(),
#       reactive_chb_data(),
#       mn_total
#     )
#     
#     min_value <- min(sapply(data_list, function(df) min(df$Aggregate_Low_Confidence_Limit))) # Compute minimum value across all plots
#     max_value <- max(sapply(data_list, function(df) max(df$Aggregate_High_Confidence_Limit))) # Compute maximum value across all plots
#     
#     c(min_value, max_value) # Return y-axis limits
#   })
#   
#   output$plot_confidenceInterval <- renderPlot({
#     data <- reactive_CHD_data() # Get the filtered CHD data
#     ggplot(data, aes(x = Data_Value_Type, y = Aggregate_Data_Value, color = Data_Value_Type)) +
#       geom_errorbar(aes(ymin = Aggregate_Low_Confidence_Limit, ymax = Aggregate_High_Confidence_Limit), width = 0.2) +
#       geom_point() +
#       labs(x = "Data Value Type", y = "Estimate (Rate)") +
#       ylim(y_axis_limits()) + # Set y-axis limits
#       theme_minimal()
#   })
#   
#   output$plot_chbRegion <- renderPlot({
#     data <- reactive_region_data() # Get the aggregated CHD data for the region
#     ggplot(data, aes(x = Data_Value_Type, y = Aggregate_Data_Value, color = Data_Value_Type)) +
#       geom_errorbar(aes(ymin = Aggregate_Low_Confidence_Limit, ymax = Aggregate_High_Confidence_Limit), width = 0.2) +
#       geom_point() +
#       labs(x = "Data Value Type", y = "Estimate (Rate)") +
#       ylim(y_axis_limits()) + # Set y-axis limits
#       theme_minimal()
#   })
#   
#   output$plot_chdCHB <- renderPlot({
#     data <- reactive_chb_data() # Get the aggregated CHD data for the CHB
#     ggplot(data, aes(x = Data_Value_Type, y = Aggregate_Data_Value, color = Data_Value_Type)) +
#       geom_errorbar(aes(ymin = Aggregate_Low_Confidence_Limit, ymax = Aggregate_High_Confidence_Limit), width = 0.2) +
#       geom_point() +
#       labs(x = "Data Value Type", y = "Estimate (Rate)") +
#       ylim(y_axis_limits()) + # Set y-axis limits
#       theme_minimal()
#   })
#   
#   output$plot_state <- renderPlot({
#     data <- mn_total # Get the pre-calculated CHD data for the state
#     ggplot(data, aes(x = Data_Value_Type, y = Aggregate_Data_Value, color = Data_Value_Type)) +
#       geom_errorbar(aes(ymin = Aggregate_Low_Confidence_Limit, ymax = Aggregate_High_Confidence_Limit), width = 0.2) +
#       geom_point() +
#       labs(x = "Data Value Type", y = "Estimate (Rate)") +
#       ylim(y_axis_limits()) + # Set y-axis limits
#       theme_minimal()
#   })
# }
# 
# # Run the app -----------------------------------------------------------------
# shinyApp(ui = ui, server = server) # Run the Shiny application  


















# ## Shiny Dashboard
# 
# # Load Packages ---------------------------------------------------------------
# library(shiny)            # shiny package for building interactive web applications
# library(ggplot2)          # ggplot2 package for creating graphics
# library(dplyr)            # dplyr package for data manipulation
# library(shinydashboard)   # shinydashboard package for creating dashboards
# library(shinyjs)          # shinyjs package for adding JavaScript functionality
# 
# # # Load Data -------------------------------------------------------------------
# # # Read census estimate data for Minnesota from a CSV file
# CensusEstMN <- read.csv(
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/2020%20to%202022%20Pop.%20Estimates/cc-est2022-agesex.csv'
# )
# 
# # # List of URLs for Coronary Heart Disease (CHD) data files from 2018 to 2021
# CHD_files <- list(
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2018.csv',
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2019.csv',
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2020.csv',
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2021.csv'
# )
# 
# # # Read each CHD data file into a list of data frames
# CHD_data <- lapply(CHD_files, read.csv)
# 
# # Read Community Health Board (CHB) data from a CSV file
# chb_raw <- read.csv(
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/Community%20Health%20Board%20as%20of%201_17_2024.csv'
# )
# 
# # # Read Minnesota region data from a CSV file
# mn_region_raw <- read.csv(
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/State%20Community%20Health%20Services%20Advisory%20Committee%20as%20of%201_17_2024.csv'
# )
# 
# # # Data Wrangling --------------------------------------------------------------
# # # Clean and merge CHD data
# CHD_data <- lapply(CHD_data, function(df) {
#   df$LocationID <- ifelse("Latitude" %in% colnames(df), as.character(df$Latitude), NA) # Add LocationID if Latitude column exists
#   colnames(df)[colnames(df) == 'Geolocatioin'] <- 'Geolocation' # Correct spelling of Geolocation column if needed
#   df
# })
# 
# # Combine all cleaned CHD data frames into one data frame
# CHD_Final <- bind_rows(CHD_data)
# 
# # Filter and select specific locations and data for the year 2021 in MN
# Selected_Locations <- CHD_Final |>
#   filter(Year == 2021, StateAbbr == "MN") |>
#   left_join(mn_region_raw, by = c("LocationName" = "County")) |> # Join with mn_region_raw by County
#   left_join(chb_raw, by = c("LocationName" = "County")) # Join with chb_raw by County
# 
# # Remove "County" from county names in census data
# CensusEstMN$CTYNAME <- gsub(" County", "", CensusEstMN$CTYNAME)
# 
# # Population estimates for CHD in MN
# PopEst_CHDMN <- CensusEstMN |>
#   filter(YEAR == 3) |> # Filter data for the year 2021 (YEAR == 3)
#   inner_join(Selected_Locations, by = c("CTYNAME" = "LocationName")) |> # Join census data with selected CHD locations
#   select(CTYNAME, Data_Value_Type, AGE18PLUS_TOT, Measure, Data_Value, High_Confidence_Limit, Low_Confidence_Limit, Region, CHB) # Select relevant columns
# 
# # Function to calculate aggregate values
# aggregate_values <- function(df, userInput, filterBy) {
#   df |>
#     filter(!!sym(filterBy) == userInput) |> # Filter by user input
#     mutate(Aggregate_Data_Value = Data_Value * AGE18PLUS_TOT / 100, # Calculate aggregate data value
#            Aggregate_Low_Confidence_Limit = Low_Confidence_Limit * AGE18PLUS_TOT / 100, # Calculate aggregate low confidence limit
#            Aggregate_High_Confidence_Limit = High_Confidence_Limit * AGE18PLUS_TOT / 100) |> # Calculate aggregate high confidence limit
#     group_by(across(all_of(filterBy)), Data_Value_Type) |> # Group by specified columns and Data_Value_Type
#     summarise(Aggregate_Data_Value = sum(Aggregate_Data_Value) / sum(AGE18PLUS_TOT) * 100, # Summarize aggregate data value
#               Aggregate_Low_Confidence_Limit = sum(Aggregate_Low_Confidence_Limit) / sum(AGE18PLUS_TOT) * 100, # Summarize aggregate low confidence limit
#               Aggregate_High_Confidence_Limit = sum(Aggregate_High_Confidence_Limit) / sum(AGE18PLUS_TOT) * 100, .groups = 'drop') # Summarize aggregate high confidence limit
# }
# 
# # Define UI -------------------------------------------------------------------
# ui <- dashboardPage(
#   dashboardHeader(title = "CDC Places to MN Regions", titleWidth = 400), # Dashboard header with title
#   dashboardSidebar(
#     width = 350, # Sidebar width
#     selectInput("parGlobal_county", label = "Select County of Interest", choices = sort(unique(mn_region_raw$County)), selected = "Kittson", width = 350), # County selection input
#     selectInput("parLocal_chdYear", label = "Select Year", choices = sort(unique(Selected_Locations$Year), decreasing = TRUE), selected = max(unique(Selected_Locations$Year)), width = 350), # Year selection input
#     selectInput("par_chdStateRegionChb", label = "Select Comparison", choices = c("All", "State", "Region", "Community Health Board"), selected = "All", multiple = FALSE, width = 350), # Comparison selection input
#     sidebarMenu(
#       menuItem("Home", tabName = "tn_homePage"), # Home tab
#       menuItem("Region & CHB Definition", tabName = "tn_regionChbDefinitions"), # Region & CHB Definition tab
#       menuItem("Coronary Heart Disease", tabName = "tn_coronaryHeartDisease") # Coronary Heart Disease tab
#     )
#   ),
#   dashboardBody(
#     shinyjs::useShinyjs(), # Enable shinyjs for JavaScript functionality
#     tabItems(
#       tabItem(
#         tabName = "tn_homePage",
#         tabsetPanel(
#           tabPanel(
#             "Home Page",
#             fluidRow(
#               column(
#                 width = 12,
#                 h1("Welcome to the CDC PLACES MN Region Dashboard"), # Welcome message
#                 h4("This Shiny application replicates the work represented ", tags$a(href="https://data.web.health.state.mn.us/web/mndata/", "here!", target= "_blank")), # Link to external site
#                 tags$h4("Before the CDC Places project, the CDC Behavioral Risk Factor Surveillance System BRFSS, allowed for state projected healthcare indicators. This process was not able to be applied to the county level. Now, with CDC Places counties can view some projected healthcare indicators. However, currently the CDC Places project does not show in an easy format aggregate county regions. By doing this project, I am not only going to help Quin County CHS, but other county regions in the state of MN or even the US."), # Project description
#                 tags$h3("Those involved with this project are:"), # Project participants
#                 tags$h4(tags$b("Emmanuel Fle Chea"), ", MPH, Public Health Data Science, University of Minnesota School of Public Health"), # Participant 1
#                 tags$h4(tags$b("Mr. Patrick Olson"), " (Preceptor), Quin County Community Health Board, Community Resource Liaison/Associate/Researcher") # Participant 2
#               )
#             )
#           )
#         )
#       ),
#       tabItem(
#         tabName = "tn_regionChbDefinitions",
#         fluidRow(
#           column(
#             width = 12,
#             tabsetPanel(
#               tabPanel(
#                 "Region/CHB",
#                 fluidRow(
#                   column(
#                     width = 12,
#                     h3(HTML("Updating the Select County of Interest filter will highlight the county in <font color=red>red</font> while the Regions and Community Health Boards will remain in <b>bold</b>.")), # Explanation of functionality
#                     h3("For this tab, the Select SCHSAC Region and Select Community Health Board filters are greyed out because they do not execute any function on this tab."), # Note on disabled filters
#                     h3("The purpose for this tab is to provide a quick reference for what counties fall under which region and Community Health Board."), # Purpose of the tab
#                     tags$hr(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;") # Horizontal rule for separation
#                   )
#                 ),
#                 fluidRow(
#                   column(6, uiOutput("region_narrative", style = "font-size: 20px;")), # Region narrative output
#                   column(6, uiOutput("chb_narrative_01", style = "font-size: 20px;")) # CHB narrative output
#                 ),
#                 fluidRow(
#                   column(
#                     width = 12,
#                     h3("Regions and Counties"), # Regions and Counties heading
#                     uiOutput("region_counties") # UI output for regions and counties list
#                   ),
#                   column(
#                     width = 12,
#                     h3("Community Health Boards"), # Community Health Boards heading
#                     uiOutput("chb_counties") # UI output for CHB and counties list
#                   )
#                 )
#               )
#             )
#           )
#         )
#       ),
#       tabItem(
#         tabName = "tn_coronaryHeartDisease",
#         tabsetPanel(
#           id = "tpId_coronaryHeartDisease",
#           tabPanel(
#             "Adults>=18 CHD Exposure",
#             fluidRow(
#               column(
#                 width = 6,
#                 box(
#                   title = uiOutput("selected_state_title"), # State title output
#                   status = "primary", # Box status
#                   solidHeader = TRUE, # Solid header
#                   collapsible = TRUE, # Collapsible box
#                   plotOutput("plot_state") # State plot output
#                 )
#               ),
#               column(
#                 width = 6,
#                 box(
#                   title = uiOutput("selected_region_title"), # Region title output
#                   status = "primary",
#                   solidHeader = TRUE,
#                   collapsible = TRUE,
#                   plotOutput("plot_chbRegion") # Region plot output
#                 )
#               )
#             ),
#             fluidRow(
#               column(
#                 width = 6,
#                 box(
#                   title = uiOutput("selected_chb_title"), # CHB title output
#                   status = "primary",
#                   solidHeader = TRUE,
#                   collapsible = TRUE,
#                   plotOutput("plot_chdCHB") # CHB plot output
#                 )
#               ),
#               column(
#                 width = 6,
#                 box(
#                   title = uiOutput("selected_county_title"), # County title output
#                   status = "primary",
#                   solidHeader = TRUE,
#                   collapsible = TRUE,
#                   plotOutput("plot_confidenceInterval") # County plot output
#                 )
#               )
#             )
#           )
#         )
#       )
#     )
#   )
# )
# 
# # Server Logic ----------------------------------------------------------------
# server <- function(input, output, session) {
#   observe({
#     updateSelectInput(
#       session,
#       "parGlobal_region",
#       choices = unique(mn_region_raw$RegionName) # Update region input choices based on unique region names in the data
#     )
#   })
# 
#   observe({
#     updateSelectInput(
#       session,
#       "parGlobal_chb",
#       choices = unique(chb_raw$CHBName) # Update CHB input choices based on unique CHB names in the data
#     )
#   })
# 
#   output$region_narrative <- renderUI({
#     filtered_region <- mn_region_raw |>
#       filter(County == input$parGlobal_county) # Filter region data for the selected county
#     HTML(
#       paste0(
#         "<b>", unique(filtered_region$RegionName), " Region</b> is made up of the following counties: ",
#         paste(unique(filtered_region$County), collapse = ", "), "." # Create HTML content for displaying the region narrative
#       )
#     )
#   })
# 
#   output$chb_narrative_01 <- renderUI({
#     filtered_chb <- chb_raw |>
#       filter(County == input$parGlobal_county) # Filter CHB data for the selected county
#     HTML(
#       paste0(
#         "<b>", unique(filtered_chb$CHBName), " Community Health Board</b> includes: ",
#         paste(unique(filtered_chb$County), collapse = ", "), "." # Create HTML content for displaying the CHB narrative
#       )
#     )
#   })
# 
#   # Helper function to highlight selected county in text
#   highlight_county <- function(text, selected_county) {
#     gsub(selected_county, paste0("<font color='red'>", selected_county, "</font>"), text) # Highlight the selected county in red
#   }
# 
#   output$region_counties <- renderUI({
#     selected_county <- input$parGlobal_county
#     regions <- mn_region_raw |>
#       group_by(Region) |>
#       summarise(Counties = paste(County, collapse = ", ")) # Group data by region and concatenate county names
# 
#     regions_text <- regions |>
#       mutate(Text = paste0("<b>", Region, " Region::</b> ", Counties)) |>
#       pull(Text) # Create HTML text with bold region names
# 
#     regions_text <- sapply(regions_text, highlight_county, selected_county = selected_county) # Highlight selected county in the regions text
#     HTML(paste(regions_text, collapse = "<br>")) # Render HTML for regions and counties
#   })
# 
#   output$chb_counties <- renderUI({
#     selected_county <- input$parGlobal_county
#     chbs <- chb_raw |>
#       group_by(CHB) |>
#       summarise(Counties = paste(County, collapse = ", ")) # Group data by CHB and concatenate county names
# 
#     chb_text <- chbs |>
#       mutate(Text = paste0("<b>", CHB, "::</b> ", Counties)) |>
#       pull(Text) # Create HTML text with bold CHB names
# 
#     chb_text <- sapply(chb_text, highlight_county, selected_county = selected_county) # Highlight selected county in the CHB text
#     HTML(paste(chb_text, collapse = "<br>")) # Render HTML for CHBs and counties
#   })
# 
#   output$selected_county_title <- renderText({
#     HTML(paste("Coronary Heart Disease Exposure", "<br/>", input$parGlobal_county, "County")) # Create the name of the selected county above the graph
#   })
# 
#   output$selected_region_title <- renderText({
#     county_region <- mn_region_raw |>
#       filter(County == input$parGlobal_county) |>
#       pull(Region) |>
#       unique() # Get the region of the selected county
#     HTML(paste("Coronary Heart Disease Exposure", "<br/>", county_region, "Region")) # Create the name of the selected region above the graph
#   })
# 
#   output$selected_state_title <- renderText({
#     HTML(paste("Coronary Heart Disease Exposure", "<br/>Minnesota")) # Create the name of the state above the graph
#   })
# 
#   output$selected_chb_title <- renderText({
#     county_chb <- chb_raw |>
#       filter(County == input$parGlobal_county) |>
#       pull(CHB) |>
#       unique() # Get the CHB of the selected county
#     HTML(paste("Coronary Heart Disease Exposure", "<br/>", county_chb, "CHB")) # Create the name of the selected CHB above the graph
#   })
# 
#   # Reactive Data for plotting--------------
#   reactive_CHD_data <- reactive({
#     PopEst_CHDMN |>
#       filter(CTYNAME == input$parGlobal_county) |>
#       aggregate_values(input$parGlobal_county, "CTYNAME") # Filter and aggregate CHD data for the selected county
#   })
# 
#   reactive_region_data <- reactive({
#     county_region <- mn_region_raw |>
#       filter(County == input$parGlobal_county) |>
#       pull(Region) |>
#       unique() # Get the region of the selected county
#     PopEst_CHDMN |>
#       filter(Region == county_region) |>
#       aggregate_values(county_region, 'Region') # Filter and aggregate CHD data for the selected region
#   })
# 
#   reactive_chb_data <- reactive({
#     county_chb <- chb_raw |>
#       filter(County == input$parGlobal_county) |>
#       pull(CHB) |>
#       unique() # Get the CHB of the selected county
#     PopEst_CHDMN |>
#       filter(CHB == county_chb) |>
#       aggregate_values(county_chb, 'CHB') # Filter and aggregate CHD data for the selected CHB
#   })
# 
#   reactive_state_data <- reactive({
#     PopEst_CHDMN |>
#       mutate(StateAbbr = "MN") |>  # Add StateAbbr column for aggregation
#       aggregate_values("MN", 'StateAbbr') # Aggregate CHD data for the state
#   })
# 
#   # Reactive expression to compute y-axis limits
#   y_axis_limits <- reactive({
#     data_list <- list(
#       reactive_CHD_data(),
#       reactive_region_data(),
#       reactive_chb_data(),
#       reactive_state_data()
#     )
# 
#     min_value <- min(sapply(data_list, function(df) min(df$Aggregate_Low_Confidence_Limit))) # Compute minimum value across all plots
#     max_value <- max(sapply(data_list, function(df) max(df$Aggregate_High_Confidence_Limit))) # Compute maximum value across all plots
# 
#     c(min_value, max_value) # Return y-axis limits
#   })
# 
#   output$plot_confidenceInterval <- renderPlot({
#     data <- reactive_CHD_data() # Get the filtered CHD data
#     ggplot(data, aes(x = Data_Value_Type, y = Aggregate_Data_Value, color = Data_Value_Type)) +
#       geom_errorbar(aes(ymin = Aggregate_Low_Confidence_Limit, ymax = Aggregate_High_Confidence_Limit), width = 0.2) +
#       geom_point() +
#       labs(x = "Data Value Type", y = "Estimate (Rate)") +
#       ylim(y_axis_limits()) + # Set y-axis limits
#       theme_minimal()
#   })
# 
#   output$plot_chbRegion <- renderPlot({
#     data <- reactive_region_data() # Get the aggregated CHD data for the region
#     ggplot(data, aes(x = Data_Value_Type, y = Aggregate_Data_Value, color = Data_Value_Type)) +
#       geom_errorbar(aes(ymin = Aggregate_Low_Confidence_Limit, ymax = Aggregate_High_Confidence_Limit), width = 0.2) +
#       geom_point() +
#       labs(x = "Data Value Type", y = "Estimate (Rate)") +
#       ylim(y_axis_limits()) + # Set y-axis limits
#       theme_minimal()
#   })
# 
#   output$plot_chdCHB <- renderPlot({
#     data <- reactive_chb_data() # Get the aggregated CHD data for the CHB
#     ggplot(data, aes(x = Data_Value_Type, y = Aggregate_Data_Value, color = Data_Value_Type)) +
#       geom_errorbar(aes(ymin = Aggregate_Low_Confidence_Limit, ymax = Aggregate_High_Confidence_Limit), width = 0.2) +
#       geom_point() +
#       labs(x = "Data Value Type", y = "Estimate (Rate)") +
#       ylim(y_axis_limits()) + # Set y-axis limits
#       theme_minimal()
#   })
# 
#   output$plot_state <- renderPlot({
#     data <- reactive_state_data() # Get the aggregated CHD data for the state
#     ggplot(data, aes(x = Data_Value_Type, y = Aggregate_Data_Value, color = Data_Value_Type)) +
#       geom_errorbar(aes(ymin = Aggregate_Low_Confidence_Limit, ymax = Aggregate_High_Confidence_Limit), width = 0.2) +
#       geom_point() +
#       labs(x = "Data Value Type", y = "Estimate (Rate)") +
#       ylim(y_axis_limits()) + # Set y-axis limits
#       theme_minimal()
#   })
# }
# 
# # Run the app -----------------------------------------------------------------
# shinyApp(ui = ui, server = server) # Run the Shiny application 