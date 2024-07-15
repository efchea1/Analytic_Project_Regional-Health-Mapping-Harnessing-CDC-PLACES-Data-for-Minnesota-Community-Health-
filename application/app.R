# Shiny Dashboard

# Load Packages ---------------------------------------------------------------
library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(shinyjs)
library(tidyr)

# Load census estimate data for Minnesota from GitHub -------------------------
CensusEstMN <- read.csv('https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/2020%20to%202022%20Pop.%20Estimates/cc-est2022-agesex.csv')

# List of URLs from GitHub for Coronary Heart Disease (CHD) data CSV files from 2018 to 2021
CHD_files <- list(
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2018.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2019.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2020.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2021.csv'
)

# Read each CHD data file into a list of data frames
CHD_data <- lapply(CHD_files, read.csv)

# Read Community Health Board (CHB) data from a CSV file from GitHub
chb_raw <- read.csv('https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/Community%20Health%20Board%20as%20of%201_17_2024.csv')

# Read Minnesota region data from a CSV file from GitHub
mn_region_raw <- read.csv('https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/State%20Community%20Health%20Services%20Advisory%20Committee%20as%20of%201_17_2024.csv')

# Data Wrangling --------------------------------------------------------------
# Clean and merge CHD data
CHD_data <- lapply(CHD_data, function(df) {
  df$LocationID <- ifelse("Latitude" %in% colnames(df), as.character(df$Latitude), NA)
  colnames(df)[colnames(df) == 'Geolocatioin'] <- 'Geolocation'
  df
})

# Combine all cleaned CHD data frames into one data frame
CHD_Final <- bind_rows(CHD_data)

# Filter and select specific locations and data for the year 2021 in MN
Selected_Locations <- CHD_Final |>
  filter(Year == 2021, StateAbbr == "MN") |>
  left_join(mn_region_raw, by = c("LocationName" = "County")) |>
  left_join(chb_raw, by = c("LocationName" = "County"))

# Remove "County" from county names in census data
CensusEstMN$CTYNAME <- gsub(" County", "", CensusEstMN$CTYNAME)

# Population estimates for CHD in MN
PopEst_CHDMN <- CensusEstMN |>
  filter(YEAR == 3) |>
  inner_join(Selected_Locations, by = c("CTYNAME" = "LocationName")) |>
  select(CTYNAME, Data_Value_Type, AGE18PLUS_TOT, Measure, Data_Value, High_Confidence_Limit, Low_Confidence_Limit, Region, CHB)

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
}

# Pre-calculate Minnesota total
mn_total <- PopEst_CHDMN |>
  mutate(StateAbbr = "MN") |>
  aggregate_values("MN", 'StateAbbr') |>
  mutate(across(everything(), ~tidyr::replace_na(., 0)))

# Function to compute y-axis limits -------------------------------------------
compute_y_axis_limits <- function(data_list) {
  min_value <- min(sapply(data_list, function(df) min(df$`Low Confidence Limit`, na.rm = TRUE)))
  max_value <- max(sapply(data_list, function(df) max(df$`High Confidence Limit`, na.rm = TRUE)))
  c(min_value, max_value)
}

# Function to create ggplot graph ---------------------------------------------
chd_plot <- function(data, y_limits, title = NULL) {
  ggplot(data, aes(x = `Data Type`, y = `Point Estimate`, color = `Data Type`)) +
    geom_errorbar(aes(ymin = `Low Confidence Limit`, ymax = `High Confidence Limit`), width = 0.2) +
    geom_point() +
    ylim(y_limits) +
    labs(title = title) +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      legend.text = element_text(size = 12),
      plot.title = element_text(hjust = 0.5, size = 14)
    )
}

# Define UI -------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "CDC Places to MN Regions", titleWidth = 400),
  dashboardSidebar(
    width = 350,
    selectInput("parGlobal_county", label = "Select County of Interest", choices = sort(unique(mn_region_raw$County)), selected = "Kittson", width = 350),
    selectInput("parLocal_chdYear", label = "Select Year", choices = sort(unique(Selected_Locations$Year), decreasing = TRUE), selected = max(unique(Selected_Locations$Year)), width = 350),
    selectInput("par_chdStateRegionChb", label = "Select Comparison", choices = c("All", "State", "Region", "CHB"), selected = "All", multiple = FALSE, width = 350),
    sidebarMenu(
      menuItem("Home", tabName = "tn_homePage"),
      menuItem("Region & CHB Definition", tabName = "tn_regionChbDefinitions"),
      menuItem("Coronary Heart Disease", tabName = "tn_coronaryHeartDisease")
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
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
                h4("This Shiny application replicates the work represented ", tags$a(href="https://data.web.health.state.mn.us/web/mndata/", "here!", target= "_blank")),
                tags$h4("Before the CDC Places project, the CDC Behavioral Risk Factor Surveillance System BRFSS, allowed for state projected healthcare indicators. This process was not able to be applied to the county level. Now, with CDC Places counties can view some projected healthcare indicators. However, currently the CDC Places project does not show in an easy format aggregate county regions. By doing this project, I am not only going to help Quin County CHS, but other county regions in the state of MN or even the US."),
                tags$h3("Those involved with this project are:"),
                tags$h4(tags$b("Emmanuel Fle Chea"), ", MPH, Public Health Data Science, University of Minnesota School of Public Health"),
                tags$h4(tags$b("Mr. Patrick Olson"), " (Preceptor), Quin County Community Health Board, Community Resource Liaison/Associate/Researcher")
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
                    h3(HTML("Updating the Select County of Interest filter will highlight the county in <font color=red>red</font> while the Regions and Community Health Boards will remain in <b>bold</b>.")),
                    h3("For this tab, the Select SCHSAC Region and Select Community Health Board filters are greyed out because they do not execute any function on this tab."),
                    h3("The purpose for this tab is to provide a quick reference for what counties fall under which region and Community Health Board."),
                    tags$hr(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;")
                  )
                ),
                fluidRow(
                  column(6, uiOutput("region_narrative", style = "font-size: 20px;")),
                  column(6, uiOutput("chb_narrative_01", style = "font-size: 20px;"))
                ),
                fluidRow(
                  column(
                    width = 12,
                    h3("Regions and Counties"),
                    uiOutput("region_counties")
                  ),
                  column(
                    width = 12,
                    h3("Community Health Boards"),
                    uiOutput("chb_counties")
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
                uiOutput("narrative_text")
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
    )
  })
  
  observe({
    updateSelectInput(
      session,
      "parGlobal_chb",
      choices = unique(chb_raw$CHBName)
    )
  })
  
  output$region_narrative <- renderUI({
    filtered_region <- mn_region_raw |>
      filter(County == input$parGlobal_county)
    HTML(
      paste0(
        "<b>", unique(filtered_region$RegionName), " Region</b> is made up of the following counties: ",
        paste(unique(filtered_region$County), collapse = ", "), "."
      )
    )
  })
  
  output$chb_narrative_01 <- renderUI({
    filtered_chb <- chb_raw |>
      filter(County == input$parGlobal_county)
    HTML(
      paste0(
        "<b>", unique(filtered_chb$CHBName), " Community Health Board</b> includes: ",
        paste(unique(filtered_chb$County), collapse = ", "), "."
      )
    )
  })
  
  highlight_county <- function(text, selected_county) {
    gsub(selected_county, paste0("<font color='red'>", selected_county, "</font>"), text)
  }
  
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
  })
  
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
  })
  
  output$selected_county_title <- renderText({
    HTML(paste("Coronary Heart Disease Exposure", "<br/>", input$parGlobal_county, "County"))
  })
  
  output$selected_region_title <- renderText({
    county_region <- mn_region_raw |>
      filter(County == input$parGlobal_county) |>
      pull(Region) |>
      unique()
    HTML(paste("Coronary Heart Disease Exposure", "<br/>", county_region, "Region"))
  })
  
  output$selected_state_title <- renderText({
    HTML(paste("Coronary Heart Disease Exposure", "<br/>Minnesota"))
  })
  
  output$selected_chb_title <- renderText({
    county_chb <- chb_raw |>
      filter(County == input$parGlobal_county) |>
      pull(CHB) |>
      unique()
    HTML(paste("Coronary Heart Disease Exposure", "<br/>", county_chb, "CHB"))
  })
  
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
  })
  
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
  })
  
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
  })
  
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
  
  output$plot_county <- renderPlot({
    chd_plot(reactive_county_data(), y_axis_limits(), title = paste(input$parGlobal_county, "County"))
  })
  
  output$plot_chbRegion <- renderPlot({
    chd_plot(reactive_region_data(), y_axis_limits(), title = paste(unique(mn_region_raw |> filter(County == input$parGlobal_county) |> pull(Region)), "Region"))
  })
  
  output$plot_chdCHB <- renderPlot({
    chd_plot(reactive_chb_data(), y_axis_limits(), title = paste(unique(chb_raw |> filter(County == input$parGlobal_county) |> pull(CHB)), "CHB"))
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
    chd_plot(data, y_axis_limits(), title = "Minnesota")
  })
  
  output$table_county <- renderTable({
    reactive_county_data()
  })
  
  output$table_region <- renderTable({
    reactive_region_data()
  })
  
  output$table_chb <- renderTable({
    reactive_chb_data()
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
  })
  
  output$narrative_text <- renderUI({
    county_data <- reactive_county_data() |>
      filter(`Data Type` == "Crude prevalence")
    
    state_data <- mn_total |>
      rename(
        `Data Type` = Data_Value_Type,
        `Point Estimate` = Aggregate_Data_Value,
        `Low Confidence Limit` = Aggregate_Low_Confidence_Limit,
        `High Confidence Limit` = Aggregate_High_Confidence_Limit
      ) |>
      filter(`Data Type` == "Crude prevalence")
    
    region_data <- reactive_region_data() |>
      filter(`Data Type` == "Crude prevalence")
    
    chb_data <- reactive_chb_data() |>
      filter(`Data Type` == "Crude prevalence")
    
    county <- input$parGlobal_county
    
    comparison <- input$par_chdStateRegionChb
    
    narrative <- NULL
    
    if (comparison == "All") {
      narrative <- paste0(
        "In 2021, ", county, " had a crude prevalence of ",
        round(county_data$`Point Estimate`, 2), "% (95% CI: ", round(county_data$`Low Confidence Limit`, 2), "-", round(county_data$`High Confidence Limit`, 2),
        "%), which is less than the region's ", round(region_data$`Point Estimate`, 2), "% (95% CI: ", round(region_data$`Low Confidence Limit`, 2), "-", round(region_data$`High Confidence Limit`, 2),
        "%), less than the CHB's ", round(chb_data$`Point Estimate`, 2), "% (95% CI: ", round(chb_data$`Low Confidence Limit`, 2), "-", round(chb_data$`High Confidence Limit`, 2),
        "%), and less than the state's ", round(state_data$`Point Estimate`, 2), "% (95% CI: ", round(state_data$`Low Confidence Limit`, 2), "-", round(state_data$`High Confidence Limit`, 2), "%)."
      )
    } else if (comparison == "State") {
      narrative <- paste0(
        "In 2021, ", county, " had a crude prevalence of ",
        round(county_data$`Point Estimate`, 2), "% (95% CI: ", round(county_data$`Low Confidence Limit`, 2), "-", round(county_data$`High Confidence Limit`, 2),
        "%), compared to the state's ", round(state_data$`Point Estimate`, 2), "% (95% CI: ", round(state_data$`Low Confidence Limit`, 2), "-", round(state_data$`High Confidence Limit`, 2), "%)."
      )
    } else if (comparison == "Region") {
      narrative <- paste0(
        "In 2021, ", county, " had a crude prevalence of ",
        round(county_data$`Point Estimate`, 2), "% (95% CI: ", round(county_data$`Low Confidence Limit`, 2), "-", round(county_data$`High Confidence Limit`, 2),
        "%), compared to the region's ", round(region_data$`Point Estimate`, 2), "% (95% CI: ", round(region_data$`Low Confidence Limit`, 2), "-", round(region_data$`High Confidence Limit`, 2), "%)."
      )
    } else if (comparison == "CHB") {
      narrative <- paste0(
        "In 2021, ", county, " had a crude prevalence of ",
        round(county_data$`Point Estimate`, 2), "% (95% CI: ", round(county_data$`Low Confidence Limit`, 2), "-", round(county_data$`High Confidence Limit`, 2),
        "%), compared to the CHB's ", round(chb_data$`Point Estimate`, 2), "% (95% CI: ", round(chb_data$`Low Confidence Limit`, 2), "-", round(chb_data$`High Confidence Limit`, 2), "%)."
      )
    }
    
    HTML(narrative)
  })
}

# Run the app -----------------------------------------------------------------
shinyApp(ui = ui, server = server) 