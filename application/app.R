## Shiny Dashboard
## Load Packages ---------------------------------------------------------------
library(shiny)            # shiny package for building interactive web applications
library(readr)            # readr package for reading data files
library(ggplot2)          # ggplot2 package for creating graphics
library(dplyr)            # dplyr package for data manipulation
library(shinydashboard)   # shinydashboard package for creating dashboards
library(shinyjs)          # shinyjs package for adding JavaScript functionality
library(plotly)           # plotly package for creating interactive plots

# Load Data -------------------------------------------------------------------
# Load Minnesota CDC Places Census Estimate data for 2020 to 2022 from a CSV file available online
CensusEstMN <- read.csv(
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/2020%20to%202022%20Pop.%20Estimates/cc-est2022-agesex.csv'
)

# List of URLs for CHD data files from 2018 to 2021
CHD_files <- list(
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2018.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2019.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2020.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2021.csv'
)

# Read each CHD data file into a list of data frames
CHD_data <- lapply(CHD_files, read.csv)

# Load Community Health Board (CHB) data from a CSV file
chb_raw <- read.csv(
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/Community%20Health%20Board%20as%20of%201_17_2024.csv'
)

# Load Minnesota region data from a CSV file
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
  filter(LocationName %in% c("Aitkin", "Anoka", "Becker", "Beltrami", "Benton", "Big Stone", "Blue Earth", "Brown", "Carlton", "Carver",
                             "Cass", "Chippewa", "Chisago", "Clay", "Clearwater", "Cook", "Cottonwood", "Crow Wing", "Dakota", "Dodge", "Douglas",
                             "Faribault", "Fillmore", "Freeborn", "Goodhue", "Grant", "Hennepin", "Houston", "Hubbard", "Isanti", "Itasca",
                             "Jackson", "Kanabec", "Kandiyohi", "Kittson", "Koochiching", "Lac qui Parle", "Lake", "Lake of the Woods", "Le Sueur",
                             "Lincoln", "Lyon", "Mahnomen", "Marshall", "Martin", "McLeod", "Meeker", "Mille Lacs", "Morrison", "Mower", "Murray",
                             "Nicollet", "Nobles", "Norman", "Olmsted", "Otter Tail", "Pennington", "Pine", "Pipestone", "Polk", "Pope", "Ramsey",
                             "Red Lake", "Redwood", "Renville", "Rice", "Rock", "Roseau", "Scott", "Sherburne", "Sibley", "St. Louis", "Stearns",
                             "Steele", "Stevens", "Swift", "Todd", "Traverse", "Wabasha", "Wadena", "Waseca", "Washington", "Watonwan",
                             "Wilkin", "Winona", "Wright", "Yellow Medicine"),
         Year == 2021, StateAbbr == "MN")

# Remove " County" from county names in census data
CensusEstMN$CTYNAME <- gsub(" County", "", CensusEstMN$CTYNAME)

# Population estimates for CHD in MN
PopEst_CHDMN <- CensusEstMN |> 
  filter(YEAR == 3) |> # Filter data for the year 2022 (YEAR == 3)
  inner_join(Selected_Locations, by = c("CTYNAME" = "LocationName")) |> # Join census data with selected CHD locations
  select(CTYNAME, Data_Value_Type, AGE18PLUS_TOT, Measure, Data_Value, High_Confidence_Limit, Low_Confidence_Limit) # Select relevant columns

# Function to calculate aggregate values
calc_aggregate_values <- function(df) {
  df |> 
    mutate(Aggregate_Data_Value = Data_Value * AGE18PLUS_TOT / 100, # Calculate aggregate data value
           Aggregate_Low_Confidence_Limit = Low_Confidence_Limit * AGE18PLUS_TOT / 100, # Calculate aggregate low confidence limit
           Aggregate_High_Confidence_Limit = High_Confidence_Limit * AGE18PLUS_TOT / 100) |> # Calculate aggregate high confidence limit
    select(CTYNAME, Data_Value_Type, AGE18PLUS_TOT, Aggregate_High_Confidence_Limit,
           Aggregate_Data_Value, Aggregate_Low_Confidence_Limit) # Select is used to Select relevant columns 
}

# Calculate crude values
CHD_Crude21MN <- calc_aggregate_values(PopEst_CHDMN |> filter(Data_Value_Type == 'Crude prevalence'))
# Calculate age-adjusted values
CHD_Adj21MN <- calc_aggregate_values(PopEst_CHDMN |> filter(Data_Value_Type == 'Age-adjusted prevalence'))
# Combine crude and age-adjusted values
CHD_MN21 <- bind_rows(CHD_Adj21MN, CHD_Crude21MN)

# App user interface --------------------------------
# Define UI -------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(
    title = "CDC Places to MN Regions",
    titleWidth = 400
  ), # Dashboard header with title
  dashboardSidebar(
    width = 350,
    selectInput(
      "parGlobal_county",
      label = "Select County of Interest",
      choices = sort(unique(mn_region_raw$County)),
      selected = "Kittson",
      width = 350
    ), # Sidebar with county selection input
    sidebarMenu(
      menuItem("Home", tabName = "tn_homePage"),
      menuItem("Region & CHB Definition", tabName = "tn_regionChbDefinitions"),
      menuItem("Coronary Heart Disease", tabName = "tn_coronaryHeartDisease")
    ) # Sidebar menu with navigation items
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
                h1("Welcome to the CDC PLACES MN Region Dashboard"),
                h4("This Shiny application replicates the work represented ",
                   tags$a(href="https://data.web.health.state.mn.us/web/mndata/", "here!", target= "_blank")),
                tags$h4("Before the CDC Places project, the CDC Behavioral Risk Factor Surveillance System BRFSS, allowed for state
                projected healthcare indicators. This process was not able to be applied to the county level. Now, with
                CDC Places counties can view some projected healthcare indicators. However, currently the CDC Places project
                does not show in an easy format aggregate county regions. By doing this project, I am not only going to help
                Quin County CHS, but other county regions in the state of MN or even the US."),
                tags$h3("Those involved with this project are:"),
                tags$h4(tags$b("Emmanuel Fle Chea"), ", MPH, Public Health Data Science, University of Minnesota School of Public Health"), # tags$b() bold the texts in the parentheses
                tags$h4(tags$b("Mr. Patrick Olson"), " (Preceptor), Quin County Community Health Board, Community Resource Liaison/Associate/Researcher") # tags$b() bold the texts in the parentheses
              ) # Home page content with welcome message and project details
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
                    # Narrative section explaining the purpose of the dashboard
                    h3(HTML("Updating the Select County of Interest filter will highlight the county in <font color=red>red</font> while the Regions and Community Health Boards will remain in <b>bold</b>.")),
                    h3("For this tab, the Select SCHSAC Region and Select Community Health Board filters are greyed out because they do not execute any function on this tab."),
                    h3("The purpose for this tab is to provide a quick reference for what counties fall under which region and Community Health Board."),
                    tags$hr(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;") # Instructions and purpose for Region/CHB tab
                  )
                ),
                fluidRow(
                  column(6, uiOutput("region_narrative", style = "font-size: 20px;")), # Region narrative output
                  column(6, uiOutput("chb_narrative_01", style = "font-size: 20px;")) # CHB narrative output
                ),
                fluidRow(
                  column(
                    width = 12,
                    h3("Regions and Counties"),
                    uiOutput("region_counties"), # UI output for regions and counties list
                  ),
                  column(
                    width = 12,
                    h3("Community Health Boards"),
                    uiOutput("chb_counties"), # UI output for CHB and counties list
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
                selectInput(
                  "parLocal_chdYear",
                  label = "Select Year",
                  choices = sort(unique(Selected_Locations$Year), decreasing = TRUE),
                  selected = max(unique(Selected_Locations$Year))
                ), # Year selection input for CHD data
                selectInput(
                  "par_leadStateRegionChb",
                  label = "Select Comparison",
                  choices = c("All", "State", "Region", "Community Health Board"),
                  selected = "All",
                  multiple = FALSE
                ), # Comparison selection input for CHD data
                fluidRow(
                  column(
                    width = 10,
                    box(
                      title = "Coronary Heart Disease Exposure Estimate",
                      status = "primary",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      plotOutput("plot_chdEstimate") # Plot output for CHD exposure estimate
                    ),
                    box(
                      title = "Confidence Interval for Coronary Heart Disease Estimate",
                      status = "primary",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      plotOutput("plot_confidenceInterval") # Plot output for CHD confidence interval
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
)

# Server Logic ----------------------------------------------------------------
server <- function(input, output, session) {
  observe({
    updateSelectInput(
      session,
      "parGlobal_region",
      choices = unique(mn_region_raw$RegionName)
    ) # Update region input choices based on unique region names in the data
  })
  
  observe({
    updateSelectInput(
      session,
      "parGlobal_chb",
      choices = unique(chb_raw$CHBName)
    ) # Update CHB input choices based on unique CHB names in the data
  })
  
  output$region_narrative <- renderUI({
    filtered_region <- mn_region_raw |>
      filter(County == input$parGlobal_county) # Filter region data for the selected county
    HTML(
      paste0(
        "<b>", unique(filtered_region$RegionName), " Region</b> is made up of the following counties: ",
        paste(unique(filtered_region$County), collapse = ", "), "."
      ) # Create HTML content for displaying the region narrative
    )
  })
  
  output$chb_narrative_01 <- renderUI({
    filtered_chb <- chb_raw |>
      filter(County == input$parGlobal_county) # Filter CHB data for the selected county
    HTML(
      paste0(
        "<b>", unique(filtered_chb$CHBName), " Community Health Board</b> includes: ",
        paste(unique(filtered_chb$County), collapse = ", "), "."
      ) # Create HTML content for displaying the CHB narrative
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
  
  # Reactive Data for plotting
  reactive_CHD_data <- reactive({
    CHD_MN21 |> filter(CTYNAME == input$parGlobal_county) # Filter CHD data for the selected county
  })
  
  output$plot_chdEstimate <- renderPlot({
    data <- reactive_CHD_data() # Get the filtered CHD data
    ggplot(data, aes(x = CTYNAME, y = Aggregate_Data_Value, fill = Data_Value_Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Coronary Heart Disease Exposure Estimate", x = "County", y = "Estimate") # Plot CHD exposure estimate
  })
  
  output$plot_confidenceInterval <- renderPlot({
    data <- reactive_CHD_data() # Get the filtered CHD data
    ggplot(data, aes(x = CTYNAME, y = Aggregate_Data_Value, color = Data_Value_Type)) +
      geom_errorbar(aes(ymin = Aggregate_Low_Confidence_Limit, ymax = Aggregate_High_Confidence_Limit), width = 0.2) +
      labs(title = "Confidence Interval for Coronary Heart Disease Estimate", x = "County", y = "Estimate") # Plot CHD confidence interval
  })
}

# Run the app -----------------------------------------------------------------
shinyApp(ui = ui, server = server) # Run the Shiny application 