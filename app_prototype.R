# ######### Shiny Dashboard
# ## Load Packages -------------------------------------------------------------
# library(shiny)
# library(readr)
# library(ggplot2)
# library(kableExtra)
# library(dplyr)
# library(tidyverse)
# library(shinydashboard)
# library(shinyjs)
# 
# 
# # Load Data ------------------------------------------------------------------
# # Minnesota CDC Places Census Estimate data for 2020 to 2022
# CensusEstMN <- read.csv(
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/2020%20to%202022%20Pop.%20Estimates/cc-est2022-agesex.csv',
# )
# 
# # Minnesota Coronary Heart Disease (CHD) data 2018 to 2021
# CHD_files <- list(
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2018.csv',
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2019.csv',
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2020.csv',
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2021.csv'
# )
# 
# CHD_data <- lapply(CHD_files, read.csv)
# 
# # CHB, County
# chb_raw <- read.csv(
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/Community%20Health%20Board%20as%20of%201_17_2024.csv',
# )
# 
# # MN Region
# mn_region_raw <- read.csv(
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/State%20Community%20Health%20Services%20Advisory%20Committee%20as%20of%201_17_2024.csv',
# )
# 
# 
# # Data Wrangling --------------------------------------------------------------
# # Clean and merge CHD data
# CHD_data[[1]]$Latitude <- "NA"
# colnames(CHD_data[[1]])[colnames(CHD_data[[1]]) == 'Latitude'] <- 'LocationID'
# colnames(CHD_data[[1]])[colnames(CHD_data[[1]]) == 'Geolocatioin'] <- 'Geolocation'
# CHD_data <- lapply(CHD_data, function(df) {
#   df$LocationID <- as.character(df$LocationID)
#   return(df)
# })
# CHD_Final <- bind_rows(CHD_data)
# 
# 
# # Filter and select locations
# Selected_Locations <- CHD_Final |>
#   filter(LocationName %in% c("Aitkin", "Anoka", "Becker", "Beltrami", "Benton", "Big Stone", "Blue Earth", "Brown", "Carlton", "Carver",
#                              "Cass", "Chippewa", "Chisago", "Clay", "Clearwater", "Cook", "Cottonwood", "Crow Wing", "Dakota", "Dodge", "Douglas",
#                              "Faribault", "Fillmore", "Freeborn", "Goodhue", "Grant", "Hennepin", "Houston", "Hubbard", "Isanti", "Itasca",
#                              "Jackson", "Kanabec", "Kandiyohi", "Kittson", "Koochiching", "Lac qui Parle", "Lake", "Lake of the Woods", "Le Sueur",
#                              "Lincoln", "Lyon", "Mahnomen", "Marshall", "Martin", "McLeod", "Meeker", "Mille Lacs", "Morrison", "Mower", "Murray",
#                              "Nicollet", "Nobles", "Norman", "Olmsted", "Otter Tail", "Pennington", "Pine", "Pipestone", "Polk", "Pope", "Ramsey",
#                              "Red Lake", "Redwood", "Renville", "Rice", "Rock", "Roseau", "Scott", "Sherburne", "Sibley", "St. Louis", "Stearns",
#                              "Steele", "Stevens", "Swift", "Todd", "Traverse", "Wabasha", "Wadena", "Waseca", "Washington", "Watonwan",
#                              "Wilkin", "Winona", "Wright", "Yellow Medicine"),
#          Year == 2021, StateAbbr == "MN")
# 
# 
# # Clean census data
# CensusEstMN$CTYNAME <- gsub(" County", "", CensusEstMN$CTYNAME)
# 
# 
# # Population estimates for CHD in MN
# PopEst_CHDMN <- CensusEstMN |>
#   filter(YEAR == 3) |>
#   inner_join(Selected_Locations, by = c("CTYNAME" = "LocationName")) |>
#   select(CTYNAME, Data_Value_Type, AGE18PLUS_TOT, Measure, Data_Value, High_Confidence_Limit, Low_Confidence_Limit)
# 
# 
# # Calculate crude and age-adjusted values
# calc_aggregate_values <- function(df) {
#   df |>
#     mutate(Aggregate_Data_Value = Data_Value * AGE18PLUS_TOT / 100,
#            Aggregate_Low_Confidence_Limit = Low_Confidence_Limit * AGE18PLUS_TOT / 100,
#            Aggregate_High_Confidence_Limit = High_Confidence_Limit * AGE18PLUS_TOT / 100) |>
#     select(CTYNAME, Data_Value_Type, AGE18PLUS_TOT, Aggregate_High_Confidence_Limit,
#            Aggregate_Data_Value, Aggregate_Low_Confidence_Limit)
# }
# 
# 
# CHD_Crude21MN <- calc_aggregate_values(PopEst_CHDMN |> filter(Data_Value_Type == 'Crude prevalence'))
# CHD_Adj21MN <- calc_aggregate_values(PopEst_CHDMN |> filter(Data_Value_Type == 'Age-adjusted prevalence'))
# CHD_MN21 <- bind_rows(CHD_Adj21MN, CHD_Crude21MN)
# 
# 
# # User Interface --------------------------------------------------------------
# ui <- dashboardPage(
#   dashboardHeader(
#     title = "CDC Places to MN Regions",
#     titleWidth = 400
#     #disable = TRUE #uncomment if the header should be hid
#   ),
# 
#   dashboardSidebar(
#     width = 350, #This makes the sidebar wider. However, the input boxes seem to have a set dimension resulting in long names still wrapping
# 
#     # Other input elements...
#     selectInput(
#       "parGlobal_county",
#       label = "Select County of Interest",
#       choices = sort(unique(mn_region_raw$County)),
#       selected = "Kittson", #"Kittson"
#       width = 350
#     ),
# 
#     # Sidebar is required to have sub menus because it requires the tabName to reference
#     # By having sidebarMenu id, you can reference it and hide other filters with shinyjs
#     # The CHB and Region Filters are greyed out when on the Region & CHB Definitions
#     sidebarMenu(
#       menuItem("Home", tabName = "tn_homePage"),
#       menuItem("Region & CHB Definitions", tabName = "tn_regionChbDefinations"),
#       menuItem("Coronary Heart Disease", tabName = "tn_coronaryHeartDisease")
#     )
#   ),
#   dashboardBody(
#     shinyjs::useShinyjs(), # Thank you Abby Stamm at MDH for suggesting to only call one function in a package rather then load entire package
#     tabItems(
#       tabItem(
#         tabName = "tn_homePage", # tabName is what ties the menuItem to the tabItem
#         tabsetPanel(
#           tabPanel(
#             "Home Page",
#             # Narrative section explaining the purpose of the dashboard
#             fluidRow(
#               column(
#                 width = 12,
#                 h1("Welcome to the CDC PLACES MN Region Dashboard"),
#                 h4("This Shiny application replicates the work represented ",
#                    tags$a(href="https://data.web.health.state.mn.us/web/mndata/", "here!", target= "_blank")),
#                 tags$h4("Before the CDC Places project, the CDC Behavioral Risk Factor Surveillance System BRFSS, allowed for state
#                     projected healthcare indicators. This process was not able to be applied to the county level. Now, with
#                     CDC Places counties can view some projected healthcare indicators. However, currently the CDC Places project
#                     does not show in an easy format aggregate county regions. By doing this project, I am not only going to help
#                     Quin County CHS, but other county regions in the state of MN or even the US."),
#                 tags$h3("Those involved with this project:"),
#                 tags$h4("Emmanuel Fle Chea, MPH, Public Health Data Science, University of Minnesota School of Public Health"),
#                 tags$h4("Mr. Patrick Olson (Preceptor), Quin County Community Health Board, Community Resource Liaison/Associate/Researcher"),
#               )
#             )
#           )
#         )
#       ),
#       tabItem(
#         tabName = "tn_regionChbDefinations",
#         fluidRow(
#           column(
#             width = 12,
#             tabsetPanel(
#               tabPanel(
#                 "Region",
#                 fluidRow(
#                   # Narrative section explaining the dashboard purpose
#                   column(
#                     width = 12,
#                     h3(HTML("Updating the Select County of Interest filter, wll highlight the county in <font color=red>red</font> while the Regions will remain in <b>bold</b>.")),
#                     h3("For this tab, the Select SCHSAC Region and Select Community Health Board filters are greyed out because they do not execute any function on this tab."),
#                     h3("The purpose for this tab is to provide a quick reference for what counties fall under which region."),
#                     #The next line inserts a line between the narrative and the data
#                     tags$hr(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;")
#                   )
#                 ),
#                 fluidRow(
#                   uiOutput("region_narrative", style = "font-size: 20px;")
#                 )
#               ),
#               tabPanel(
#                 "CHB",
#                 fluidRow(
#                   # Narrative section explaining the purpose of the dashboard
#                   column(
#                     width = 12,
#                     h3(HTML("Updating the Select County of Interest filter, wll highlight the county in <font color=red>red</font> while the Community Health Boards will remain in <b>bold</b>.")),
#                     h3("For this tab, the Select SCHSAC Region and Select Community Health Board filters are greyed out because they don't have any functions on this page."),
#                     h3("The purpose for this tab is to provide a quick reference for what counties fall under which Community Health Board."),
#                     #The next line inserts a line between the narrative and the data
#                     tags$hr(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;")
#                   )
#                 ),
#                 fluidRow(
#                   column(6, uiOutput("region_narrative", style = "font-size: 20px;")),
#                   column(6, uiOutput("chb_narrative_01", style = "font-size: 20px;"))
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
#                 width = 2,
#                 selectInput(
#                   "parLocal_leadYear",
#                   label = "Select Year",
#                   choices = sort(unique(Selected_Locations$Year), decreasing = TRUE),
#                   selected = max(unique(Selected_Locations$Year))
#                 ),
#                 selectInput(
#                   "par_leadStateRegionChb",
#                   label = "Select Comparison",
#                   choices = c("All", "State", "Region", "Community Health Board"),
#                   selected = "All",
#                   multiple = FALSE
#                 )
#               ),
#               column(
#                 width = 10,
#                 box(
#                   title = "Coronary Heart Disease Exposure Estimate",
#                   status = "primary",
#                   solidHeader = TRUE,
#                   collapsible = TRUE,
#                   plotOutput("plot_chdEstimate")
#                 ),
#                 box(
#                   title = "Confidence Interval for Coronary Heart Disease Estimate",
#                   status = "primary",
#                   solidHeader = TRUE,
#                   collapsible = TRUE,
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
# # Preload Data prior to Server -----------------------------
# #To improve performance load these once and don't have them run every time the server runs
# 
# 
# #* Region & County Definitions ----------------------------------------------
# # Create region data frame for global narrative reference
# schsac_raw <- mn_region_raw
# schsac_raw$Region <-  paste("<b>", schsac_raw$Region, "</b>") #Bold Regions so it is easier to understand narrative
# 
# # Create chb data frame for global narrative reference
# # CHB Narrative 01 (It is split in half so it displays in two nice columns on the UI
# chb_raw_01 <- chb_raw[1:33,]
# chb_raw_01$CHB <-  paste("<b>", chb_raw_01$CHB, "</b>") #Bold CHBs so it is easier to understand narrative
# 
# # Create chb data frame for global narrative reference
# # CHB Narrative 02 (It is split in half so it displays in two nice columns on the UI
# chb_raw_02 <- chb_raw[34:nrow(chb_raw),] # :nrow means it goes to the end of the data frame
# chb_raw_02$CHB <-  paste("<b>", chb_raw_02$CHB, "</b>") #Bold CHBs so it is easier to understand narrative
# 
# 
# # Server ----------------------------------------------------------------------
# 
# # Region & County ---------------------------------------------------------
# 
# output$region_narrative <- renderUI({
#   # Replace the values that are equal to input county by adding the font tag
#   schsac_raw$County[schsac_raw$County == input$parGlobal_county] <-  paste("<font color=red>", schsac_raw$County[schsac_raw$County == input$parGlobal_county], "</font>")
# 
#   # Split the data frame by the group column
#   schsac_split <- split(schsac_raw, schsac_raw$Region)
# 
#   # Apply a function to each subset to create a new column with comma-separated values
#   schsac_character <- sapply(schsac_split, function(x) paste(x$County, collapse = ", "))
# 
#   # Convert the result to a data frame
#   schsac_df <- as.data.frame(schsac_character)
# 
#   # Create an empty character vector to store the results
#   schsac_result <- character()
# 
#   # Loop through each row of schsac_df
#   for (i in 1:nrow(schsac_df)) {
#     # Get the row name and schsac_character_placeholder as strings
#     rowname <- as.character(rownames(schsac_df)[i])
#     schsac_character_placeholder <- as.character(schsac_df$schsac_character[i])
# 
#     # Concatenate them with a separator
#     concat <- paste0(rowname, ":: ", schsac_character_placeholder)
# 
#     # Append the result to the vector
#     schsac_result <- c(schsac_result, concat)
#   }
# 
#   # Paste the result vector and create a new line after each Region
#   HTML(paste(schsac_result, collapse=  "<br/>"))
# 
#   })
# 
# output$chb_narrative_01 <- renderUI({
#   # Replace the values that are equal to input county by adding the font tab
#   chb_raw_01$County[chb_raw_01$County == input$parGlobal_county] <-  paste("<font color=red>", chb_raw_01$County[chb_raw_01$County == input$parGlobal_county], "</font>")
# 
#   # Split the data frame by the chb
#   chb_split <- split(chb_raw_01 , chb_raw_01$CHB)
# 
#   # Apply a function to each subset to create a new column with comma-separated values
#   chb_character <- sapply(chb_split, function(x) paste(x$County, collapse = ", "))
# 
#   # Convert the result to a data frame
#   chb_df <- as.data.frame(chb_character)
# 
#   # Create an empty character vector to store the results
#   chb_result <- character()
# 
#   # Loop through each row of chb_df
#   for (i in 1:nrow(chb_df)) {
#     # Get the row name and chb_character_placeholder as strings
#     rowname <- as.character(rownames(chb_df)[i])
#     chb_character_placeholder <- as.character(chb_df$chb_character[i])
# 
#     # Concatenate them with a separator
#     concat <- paste0(rowname, ":: ", chb_character_placeholder)
# 
#     # Append the result to the vector
#     chb_result <- c(chb_result, concat)
#   }
# 
#   # Paste the result vector and create a new line after each CHB
#   HTML(paste(chb_result, collapse =  "<br/>"))
#   })
# 
# output$chb_narrative_02 <- renderUI({
#   # Replace the values that are equal to input county by adding the font tab
#   chb_raw_02$County[chb_raw_02$County == input$parGlobal_county] <-  paste("<font color=red>", chb_raw_02$County[chb_raw_02$County == input$parGlobal_county], "</font>")
# 
#   # Split the data frame by the chb
#   chb_split <- split(chb_raw_02 , chb_raw_02$CHB)
# 
#   # Apply a function to each subset to create a new column with comma-separated values
#   chb_character <- sapply(chb_split, function(x) paste(x$County, collapse = ", "))
# 
#   # Convert the result to a data frame
#   chb_df <- as.data.frame(chb_character)
# 
#   # Create an empty character vector to store the results
#   chb_result <- character()
# 
#   # Loop through each row of chb_df
#   for (i in 1:nrow(chb_df)) {
#     # Get the row name and chb_character_placeholder as strings
#     rowname <- as.character(rownames(chb_df)[i])
#     chb_character_placeholder <- as.character(chb_df$chb_character[i])
# 
#     # Concatenate them with a separator
#     concat <- paste0(rowname, ":: ", chb_character_placeholder)
# 
#     # Append the result to the vector
#     chb_result <- c(chb_result, concat)
#   }
# 
#   # Paste the result vector and create a new line after each CHB
#   HTML(paste(chb_result, collapse =  "<br/>"))
# })
# 
# # Update the region input based on the county input
# # In case data source doesn't have the same number of counties Null, Blank, Minnesota are examples.
# # Currently, the county source is from the region data source but if it ever changes this should help capture it
# observeEvent(input$parGlobal_county, {
#   if(input$parGlobal_county %in% mn_region_raw$County) {
#     updateSelectInput(
#       session, "parGlobal_region",
#       choices =  unique(mn_region_raw$Region[order(mn_region_raw$County == input$parGlobal_county, decreasing = TRUE)]),
#       selected = unique(mn_region_raw$Region[order(mn_region_raw$County == input$parGlobal_county, decreasing = TRUE)])[1] #Default to the first choice, which will automatically change the cascading results when the parameter is mapped to a ggplot
#     )
#   }
#   else {
#     updateSelectInput(
#       session, "parGlobal_region",
#       choices = "",
#       selected = ""
#     )
#   }
# }
# )
# 
# # Update the chb input based on the county input
# # In case data source doesn't have the same number of counties Null, Blank, Minnesota are examples.
# # Currently, the county source is from the chb data source but if it ever changes this should help capture it
# observeEvent(input$parGlobal_county, {
#   if(input$parGlobal_county %in% chb_raw$County) {
#     updateSelectInput(
#       session, "parGlobal_chb",
#       choices =  unique(chb_raw$CHB[order(chb_raw$County == input$parGlobal_county, decreasing = TRUE)]),
#       selected = unique(chb_raw$CHB[order(chb_raw$County == input$parGlobal_county, decreasing = TRUE)])[1] #Default to the first choice, which will automatically change the cascading results when the parameter is mapped to a ggplot
#     )
#   }
#   else {
#     updateSelectInput(
#       session, "parGlobal_chb",
#       choices = "",
#       selected = ""
#     )
#   }
# }
# )
# 
# # Observe what sidebar the user is on and only allow for read only access to global parameters
# observe({
#   if(input$smID %in% c("tn_homePage","tn_regionChbDefinations")) # it requires an ID of sidebarMenu (in this case)
#   {
#     # shinyjs::disable("parGlobal_region")
#     # shinyjs::disable("parGlobal_chb")
#     shinyjs::hide("parGlobal_region")
#     shinyjs::hide("parGlobal_chb")
#   }
#   else
#   {
#     shinyjs::enable("parGlobal_region")
#     shinyjs::enable("parGlobal_chb")
#   }
# })
# 
# server <- function(input, output, session) {
#   observe({
#     updateSelectInput(
#       session,
#       "parGlobal_region",
#       choices = unique(mn_region_raw$RegionName)
#     )
#   })
# 
#   observe({
#     updateSelectInput(
#       session,
#       "parGlobal_chb",
#       choices = unique(chb_raw$CHBName)
#     )
#   })
# 
#   output$region_narrative <- renderUI({
#     filtered_region <- mn_region_raw |> 
#       filter(County == input$parGlobal_county)
#     HTML(
#       paste0(
#         "<b>", unique(filtered_region$RegionName), " Region</b> is made up of the following counties: ",
#         paste(unique(filtered_region$County), collapse = ", "), "."
#       )
#     )
#   })
# 
#   output$chb_narrative_01 <- renderUI({
#     filtered_chb <- chb_raw |> 
#       filter(County == input$parGlobal_county)
#     HTML(
#       paste0(
#         "<b>", unique(filtered_chb$CHBName), " Community Health Board</b> includes: ",
#         paste(unique(filtered_chb$County), collapse = ", "), "."
#       )
#     )
#   })
# 
#   # Reactive Data
#   reactive_CHD_data <- reactive({
#     CHD_MN21 |> filter(CTYNAME == input$parGlobal_county)
#   })
# 
#   output$plot_chdEstimate <- renderPlot({
#     data <- reactive_CHD_data()
#     ggplot(data, aes(x = CTYNAME, y = Aggregate_Data_Value, fill = Data_Value_Type)) +
#       geom_bar(stat = "identity", position = "dodge") +
#       labs(title = "Coronary Heart Disease Exposure Estimate", x = "County", y = "Estimate")
#   })
# 
#   output$plot_confidenceInterval <- renderPlot({
#     data <- reactive_CHD_data()
#     ggplot(data, aes(x = CTYNAME, y = Aggregate_Data_Value, color = Data_Value_Type)) +
#       geom_errorbar(aes(ymin = Aggregate_Low_Confidence_Limit, ymax = Aggregate_High_Confidence_Limit), width = 0.2) +
#       labs(title = "Confidence Interval for Coronary Heart Disease Estimate", x = "County", y = "Estimate")
#   })
# }
# 
# # App Initialization ----------------------------------------------------------
# shinyApp(ui = ui, server = server) 


















































# ####### Shiny Dashboard
# ## Load Packages ---------------------------------------------------------------
# library(shiny)
# library(readr)
# library(ggplot2)
# library(kableExtra)
# library(dplyr)
# library(tidyverse)
# library(shinydashboard)
# library(shinyjs)
# 
# 
# # Load Data -------------------------------------------------------------------
# # Minnesota CDC Places Census Estimate data for 2020 to 2022
# CensusEstMN <- read.csv(
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/2020%20to%202022%20Pop.%20Estimates/cc-est2022-agesex.csv',
# )
# 
# # Minnesota Coronary Heart Disease (CHD) data 2018 to 2021
# CHD_files <- list(
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2018.csv',
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2019.csv',
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2020.csv',
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2021.csv'
# )
# 
# CHD_data <- lapply(CHD_files, read.csv)
# 
# # CHB, County
# chb_raw <- read.csv(
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/Community%20Health%20Board%20as%20of%201_17_2024.csv',
# )
# 
# # MN Region
# mn_region_raw <- read.csv(
#   'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/State%20Community%20Health%20Services%20Advisory%20Committee%20as%20of%201_17_2024.csv',
# )
# 
# 
# # Data Wrangling --------------------------------------------------------------
# # Clean and merge CHD data
# CHD_data[[1]]$Latitude <- "NA"
# colnames(CHD_data[[1]])[colnames(CHD_data[[1]]) == 'Latitude'] <- 'LocationID'
# colnames(CHD_data[[1]])[colnames(CHD_data[[1]]) == 'Geolocatioin'] <- 'Geolocation'
# CHD_data <- lapply(CHD_data, function(df) {
#   df$LocationID <- as.character(df$LocationID)
#   return(df)
# })
# CHD_Final <- bind_rows(CHD_data)
# 
# 
# # Filter and select locations
# Selected_Locations <- CHD_Final |>
#   filter(LocationName %in% c("Aitkin", "Anoka", "Becker", "Beltrami", "Benton", "Big Stone", "Blue Earth", "Brown", "Carlton", "Carver",
#                              "Cass", "Chippewa", "Chisago", "Clay", "Clearwater", "Cook", "Cottonwood", "Crow Wing", "Dakota", "Dodge", "Douglas",
#                              "Faribault", "Fillmore", "Freeborn", "Goodhue", "Grant", "Hennepin", "Houston", "Hubbard", "Isanti", "Itasca",
#                              "Jackson", "Kanabec", "Kandiyohi", "Kittson", "Koochiching", "Lac qui Parle", "Lake", "Lake of the Woods", "Le Sueur",
#                              "Lincoln", "Lyon", "Mahnomen", "Marshall", "Martin", "McLeod", "Meeker", "Mille Lacs", "Morrison", "Mower", "Murray",
#                              "Nicollet", "Nobles", "Norman", "Olmsted", "Otter Tail", "Pennington", "Pine", "Pipestone", "Polk", "Pope", "Ramsey",
#                              "Red Lake", "Redwood", "Renville", "Rice", "Rock", "Roseau", "Scott", "Sherburne", "Sibley", "St. Louis", "Stearns",
#                              "Steele", "Stevens", "Swift", "Todd", "Traverse", "Wabasha", "Wadena", "Waseca", "Washington", "Watonwan",
#                              "Wilkin", "Winona", "Wright", "Yellow Medicine"),
#          Year == 2021, StateAbbr == "MN")
# 
# 
# # Clean census data
# CensusEstMN$CTYNAME <- gsub(" County", "", CensusEstMN$CTYNAME)
# 
# 
# # Population estimates for CHD in MN
# PopEst_CHDMN <- CensusEstMN |>
#   filter(YEAR == 3) |>
#   inner_join(Selected_Locations, by = c("CTYNAME" = "LocationName")) |>
#   select(CTYNAME, Data_Value_Type, AGE18PLUS_TOT, Measure, Data_Value, High_Confidence_Limit, Low_Confidence_Limit)
# 
# 
# # Calculate crude and age-adjusted values
# calc_aggregate_values <- function(df) {
#   df |>
#     mutate(Aggregate_Data_Value = Data_Value * AGE18PLUS_TOT / 100,
#            Aggregate_Low_Confidence_Limit = Low_Confidence_Limit * AGE18PLUS_TOT / 100,
#            Aggregate_High_Confidence_Limit = High_Confidence_Limit * AGE18PLUS_TOT / 100) |>
#     select(CTYNAME, Data_Value_Type, AGE18PLUS_TOT, Aggregate_High_Confidence_Limit,
#            Aggregate_Data_Value, Aggregate_Low_Confidence_Limit)
# }
# 
# 
# CHD_Crude21MN <- calc_aggregate_values(PopEst_CHDMN |> filter(Data_Value_Type == 'Crude prevalence'))
# CHD_Adj21MN <- calc_aggregate_values(PopEst_CHDMN |> filter(Data_Value_Type == 'Age-adjusted prevalence'))
# CHD_MN21 <- bind_rows(CHD_Adj21MN, CHD_Crude21MN)
# 
# 
# # User Interface --------------------------------------------------------------
# ui <- dashboardPage(
#   dashboardHeader(
#     title = "CDC Places to MN Regions",
#     titleWidth = 400
#     #disable = TRUE #uncomment if the header should be hid
#   ),
# 
#   dashboardSidebar(
#     width = 350, #This makes the sidebar wider. However, the input boxes seem to have a set dimension resulting in long names still wrapping
# 
#     # Other input elements...
#     selectInput(
#       "parGlobal_county",
#       label = "Select County of Interest",
#       choices = sort(unique(mn_region_raw$County)),
#       selected = "Aitkin", #"Kittson"
#       width = 350
#     ),
# 
#     # Sidebar is required to have sub menus because it requires the tabName to reference
#     # By having sidebarMenu id, you can reference it and hide other filters with shinyjs
#     # The CHB and Region Filters are greyed out when on the Region & CHB Definitions
#     sidebarMenu(
#       menuItem("Home", tabName = "tn_homePage"),
#       menuItem("Region & CHB Definitions", tabName = "tn_regionChbDefinations"),
#       menuItem("Coronary Heart Disease", tabName = "tn_coronaryHeartDisease")
#     )
#   ),
#   dashboardBody(
#     shinyjs::useShinyjs(), # Thank you Abby Stamm at MDH for suggesting to only call one function in a package rather then load entire package
#     tabItems(
#       tabItem(
#         tabName = "tn_homePage", # tabName is what ties the menuItem to the tabItem
#         tabsetPanel(
#           tabPanel(
#             "Home Page",
#             # Narrative section explaining the purpose of the dashboard
#             fluidRow(
#               column(
#                 width = 12,
#                 h1("Welcome to the CDC PLACES MN Region Dashboard"),
#                 h4("This Shiny application replicates the work represented ",
#                    tags$a(href="https://data.web.health.state.mn.us/web/mndata/", "here!", target= "_blank")),
#                 tags$h4("Before the CDC Places project, the CDC Behavioral Risk Factor Surveillance System BRFSS, allowed for state
#                     projected healthcare indicators. This process was not able to be applied to the county level. Now, with
#                     CDC Places counties can view some projected healthcare indicators. However, currently the CDC Places project
#                     does not show in an easy format aggregate county regions. By doing this project, I am not only going to help
#                     Quin County CHS, but other county regions in the state of MN or even the US."),
#                 tags$h3("Those involved with this project:"),
#                 tags$h4("Emmanuel Fle Chea, MPH, Public Health Data Science, University of Minnesota School of Public Health"),
#                 tags$h4("Mr. Patrick Olson (Preceptor), Quin County Community Health Board, Community Resource Liaison/Associate/Researcher"),
#               )
#             )
#           )
#         )
#       ),
#       tabItem(
#         tabName = "tn_regionChbDefinations",
#         fluidRow(
#           column(
#             width = 12,
#             tabsetPanel(
#               tabPanel(
#                 "Region/CHB",
#                 fluidRow(
#                   column(
#                     width = 12,
#                     # Narrative section explaining the purpose of the dashboard
#                     h3(HTML("Updating the Select County of Interest filter will highlight the county in <font color=red>red</font> while the Regions and Community Health Boards will remain in <b>bold</b>.")),
#                     h3("For this tab, the Select SCHSAC Region and Select Community Health Board filters are greyed out because they do not execute any function on this tab."),
#                     h3("The purpose for this tab is to provide a quick reference for what counties fall under which region and Community Health Board."),
#                     tags$hr(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;")
#                   )
#                 ),
#                 fluidRow(
#                   column(6, uiOutput("region_narrative", style = "font-size: 20px;")),
#                   column(6, uiOutput("chb_narrative_01", style = "font-size: 20px;"))
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
#                 width = 2,
#                 selectInput(
#                   "parLocal_leadYear",
#                   label = "Select Year",
#                   choices = sort(unique(Selected_Locations$Year), decreasing = TRUE),
#                   selected = max(unique(Selected_Locations$Year))
#                 ),
#                 selectInput(
#                   "par_leadStateRegionChb",
#                   label = "Select Comparison",
#                   choices = c("All", "State", "Region", "Community Health Board"),
#                   selected = "All",
#                   multiple = FALSE
#                 )
#               ),
#               column(
#                 width = 10,
#                 box(
#                   title = "Coronary Heart Disease Exposure Estimate",
#                   status = "primary",
#                   solidHeader = TRUE,
#                   collapsible = TRUE,
#                   plotOutput("plot_chdEstimate")
#                 ),
#                 box(
#                   title = "Confidence Interval for Coronary Heart Disease Estimate",
#                   status = "primary",
#                   solidHeader = TRUE,
#                   collapsible = TRUE,
#                   plotOutput("plot_confidenceInterval")
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
# # Server ----------------------------------------------------------------------
# 
# # Region & County -----------------------
# 
# server <- function(input, output, session) {
#   observe({
#     updateSelectInput(
#       session,
#       "parGlobal_region",
#       choices = unique(mn_region_raw$RegionName)
#     )
#   })
# 
#   observe({
#     updateSelectInput(
#       session,
#       "parGlobal_chb",
#       choices = unique(chb_raw$CHBName)
#     )
#   })
# 
#   output$region_narrative <- renderUI({
#     filtered_region <- mn_region_raw %>%
#       filter(County == input$parGlobal_county)
#     HTML(
#       paste0(
#         "<b>", unique(filtered_region$RegionName), " Region</b> is made up of the following counties: ",
#         paste(unique(filtered_region$County), collapse = ", "), "."
#       )
#     )
#   })
# 
#   output$chb_narrative_01 <- renderUI({
#     filtered_chb <- chb_raw %>%
#       filter(County == input$parGlobal_county)
#     HTML(
#       paste0(
#         "<b>", unique(filtered_chb$CHBName), " Community Health Board</b> includes: ",
#         paste(unique(filtered_chb$County), collapse = ", "), "."
#       )
#     )
#   })
# 
#   # Reactive Data
#   reactive_CHD_data <- reactive({
#     CHD_MN21 %>% filter(CTYNAME == input$parGlobal_county)
#   })
# 
#   output$plot_chdEstimate <- renderPlot({
#     data <- reactive_CHD_data()
#     ggplot(data, aes(x = CTYNAME, y = Aggregate_Data_Value, fill = Data_Value_Type)) +
#       geom_bar(stat = "identity", position = "dodge") +
#       labs(title = "Coronary Heart Disease Exposure Estimate", x = "County", y = "Estimate")
#   })
# 
#   output$plot_confidenceInterval <- renderPlot({
#     data <- reactive_CHD_data()
#     ggplot(data, aes(x = CTYNAME, y = Aggregate_Data_Value, color = Data_Value_Type)) +
#       geom_errorbar(aes(ymin = Aggregate_Low_Confidence_Limit, ymax = Aggregate_High_Confidence_Limit), width = 0.2) +
#       labs(title = "Confidence Interval for Coronary Heart Disease Estimate", x = "County", y = "Estimate")
#   })
# }
# 
# # App Initialization ----------------------------------------------------------
# shinyApp(ui = ui, server = server)


















# # Shiny Dashboard
#
# ## Load Packages ---------------------------------------------------------------
library(shiny)
library(readr)
library(ggplot2)
library(kableExtra)
library(dplyr)
library(tidyverse)
library(shinydashboard)
library(shinyjs)
# ##
#
# # Load Data -------------------------------------------------------------------
# # Minnesota CDC Places Census Estimate data for 2020 to 2022
CensusEstMN <- read.csv(
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/2020%20to%202022%20Pop.%20Estimates/cc-est2022-agesex.csv'
)
#
# # Minnesota Coronary Heart Disease (CHD) data 2018 to 2021
CHD_files <- list(
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2018.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2019.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2020.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2021.csv'
)
#
CHD_data <- lapply(CHD_files, read.csv)
#
# # CHB, County
chb_raw <- read.csv(
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/Community%20Health%20Board%20as%20of%201_17_2024.csv'
)
#
# # MN Region
mn_region_raw <- read.csv(
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/State%20Community%20Health%20Services%20Advisory%20Committee%20as%20of%201_17_2024.csv'
)
#
# # Data Wrangling --------------------------------------------------------------
# # Clean and merge CHD data
CHD_data <- lapply(CHD_data, function(df) {
  colnames(df)[colnames(df) == 'Latitude'] <- 'LocationID'
  colnames(df)[colnames(df) == 'Geolocatioin'] <- 'Geolocation'
  df$LocationID <- as.character(df$LocationID)
  return(df)
})
CHD_Final <- bind_rows(CHD_data)
#
# # Filter and select locations
# Selected_Locations <- CHD_Final %>%
#   filter(LocationName %in% c("Aitkin", "Anoka", "Becker", "Beltrami", "Benton", "Big Stone", "Blue Earth", "Brown", "Carlton", "Carver",
#                              "Cass", "Chippewa", "Chisago", "Clay", "Clearwater", "Cook", "Cottonwood", "Crow Wing", "Dakota", "Dodge", "Douglas",
#                              "Faribault", "Fillmore", "Freeborn", "Goodhue", "Grant", "Hennepin", "Houston", "Hubbard", "Isanti", "Itasca",
#                              "Jackson", "Kanabec", "Kandiyohi", "Kittson", "Koochiching", "Lac qui Parle", "Lake", "Lake of the Woods", "Le Sueur",
#                              "Lincoln", "Lyon", "Mahnomen", "Marshall", "Martin", "McLeod", "Meeker", "Mille Lacs", "Morrison", "Mower", "Murray",
#                              "Nicollet", "Nobles", "Norman", "Olmsted", "Otter Tail", "Pennington", "Pine", "Pipestone", "Polk", "Pope", "Ramsey",
#                              "Red Lake", "Redwood", "Renville", "Rice", "Rock", "Roseau", "Scott", "Sherburne", "Sibley", "St. Louis", "Stearns",
#                              "Steele", "Stevens", "Swift", "Todd", "Traverse", "Wabasha", "Wadena", "Waseca", "Washington", "Watonwan",
#                              "Wilkin", "Winona", "Wright", "Yellow Medicine"),
#          Year == 2021, StateAbbr == "MN")
#
# # Clean census data
# CensusEstMN$CTYNAME <- gsub(" County", "", CensusEstMN$CTYNAME)
#
# # Population estimates for CHD in MN
# PopEst_CHDMN <- CensusEstMN %>%
#   filter(YEAR == 3) %>%
#   inner_join(Selected_Locations, by = c("CTYNAME" = "LocationName")) %>%
#   select(CTYNAME, Data_Value_Type, AGE18PLUS_TOT, Measure, Data_Value, High_Confidence_Limit, Low_Confidence_Limit)
#
# # Calculate crude and age-adjusted values
# calc_aggregate_values <- function(df) {
#   df %>%
#     mutate(Aggregate_Data_Value = Data_Value * AGE18PLUS_TOT / 100,
#            Aggregate_Low_Confidence_Limit = Low_Confidence_Limit * AGE18PLUS_TOT / 100,
#            Aggregate_High_Confidence_Limit = High_Confidence_Limit * AGE18PLUS_TOT / 100) %>%
#     select(CTYNAME, Data_Value_Type, AGE18PLUS_TOT, Aggregate_High_Confidence_Limit,
#            Aggregate_Data_Value, Aggregate_Low_Confidence_Limit)
# }
#
# CHD_Crude21MN <- calc_aggregate_values(PopEst_CHDMN %>% filter(Data_Value_Type == 'Crude prevalence'))
# CHD_Adj21MN <- calc_aggregate_values(PopEst_CHDMN %>% filter(Data_Value_Type == 'Age-adjusted prevalence'))
# CHD_MN21 <- bind_rows(CHD_Adj21MN, CHD_Crude21MN)
#
# # User Interface --------------------------------------------------------------
# ui <- dashboardPage(
#   dashboardHeader(
#     title = "CDC Places to MN Regions",
#     titleWidth = 400
#   ),
#   dashboardSidebar(
#     width = 350,
#     selectInput(
#       "parGlobal_county",
#       label = "Select County of Interest",
#       choices = sort(unique(mn_region_raw$County)),
#       selected = "Kittson",
#       width = 350
#     ),
#     sidebarMenu(
#       menuItem("Home", tabName = "tn_homePage"),
#       menuItem("Region & CHB Definitions", tabName = "tn_regionChbDefinations"),
#       menuItem("Coronary Heart Disease", tabName = "tn_coronaryHeartDisease")
#     )
#   ),
#   dashboardBody(
#     shinyjs::useShinyjs(),
#     tabItems(
#       tabItem(
#         tabName = "tn_homePage",
#         tabsetPanel(
#           tabPanel(
#             "Home Page",
#             fluidRow(
#               column(
#                 width = 12,
#                 h1("Welcome to the CDC PLACES MN Region Dashboard"),
#                 h4("This Shiny application replicates the work represented ",
#                    tags$a(href="https://data.web.health.state.mn.us/web/mndata/", "here!", target= "_blank")),
#                 tags$h4("Before the CDC Places project, the CDC Behavioral Risk Factor Surveillance System BRFSS, allowed for state
#                     projected healthcare indicators. This process was not able to be applied to the county level. Now, with
#                     CDC Places counties can view some projected healthcare indicators. However, currently the CDC Places project
#                     does not show in an easy format aggregate county regions. By doing this project, I am not only going to help
#                     Quin County CHS, but other county regions in the state of MN or even the US."),
#                 tags$h3("Those involved with this project:"),
#                 tags$h4("Emmanuel Fle Chea, MPH, Public Health Data Science, University of Minnesota School of Public Health"),
#                 tags$h4("Mr. Patrick Olson (Preceptor), Quin County Community Health Board, Community Resource Liaison/Associate/Researcher")
#               )
#             )
#           )
#         )
#       ),
#       tabItem(
#         tabName = "tn_regionChbDefinations",
#         fluidRow(
#           column(
#             width = 12,
#             tabItem(
#               tabName = "tn_regionChbDefinations",
#               tabsetPanel(
#                 tabPanel(
#                   "Region",
#                   fluidRow(
#                     # Narrative section explaining the dashboard purpose
#                     column(
#                       width = 12,
#                       h3(HTML("Updating the Select County of Interest filter, wll highlight the county in <font color=red>red</font> while the Regions will remain in <b>bold</b>.")),
#                       h3("For this tab, the Select SCHSAC Region and Select Community Health Board filters are greyed out because they do not execute any function on this tab."),
#                       h3("The purpose for this tab is to provide a quick reference for what counties fall under which region."),
#                       #The next line inserts a line between the narrative and the data
#                       tags$hr(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;")
#                     )
#                   ),
#                   fluidRow(
#                     uiOutput("region_narrative", style = "font-size: 20px;")
#                   )
#                 ),
#                 tabPanel(
#                   "CHB",
#                   fluidRow(
#                     # Narrative section explaining the purpose of the dashboard
#                     column(
#                       width = 12,
#                       h3(HTML("Updating the Select County of Interest filter, wll highlight the county in <font color=red>red</font> while the Community Health Boards will remain in <b>bold</b>.")),
#                       h3("For this tab, the Select SCHSAC Region and Select Community Health Board filters are greyed out because they don't have any functions on this page."),
#                       h3("The purpose for this tab is to provide a quick reference for what counties fall under which Community Health Board."),
#                       #The next line inserts a line between the narrative and the data
#                       tags$hr(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;")
#                     )
#                   ),
#                   fluidRow(
#                     column(6, uiOutput("chb_narrative_01", style = "font-size: 20px;")),
#                     column(6, uiOutput("chb_narrative_02", style = "font-size: 20px;"))
#                   )
#                 )
#               )
#             ),
#                 fluidRow(
#                   column(6, tableOutput("RegionDefinition")),
#                   column(6, tableOutput("CHBDefinition"))
#                 )
#               )
#             )
#           )
#         )
#       ),
#       tabItem(
#         tabName = "tn_coronaryHeartDisease",
#         tabsetPanel(
#           tabPanel(
#             "County Level",
#             fluidRow(
#               column(
#                 width = 12,
#                 tags$h1("Crude Prevalence for Coronary Heart Disease in Minnesota in 2021"),
#                 tags$h2("County Level Data"),
#                 tags$h4("This dataset provides county-level data for Minnesota counties in 2021.
#                         It includes the prevalence of coronary heart disease, confidence limits, and population estimates.")
#               )
#             ),
#             fluidRow(
#               column(
#                 width = 12,
#                 DT::dataTableOutput("county_table")
#               )
#             )
#           ),
#           tabPanel(
#             "Age-Adjusted Level",
#             fluidRow(
#               column(
#                 width = 12,
#                 tags$h1("Age-Adjusted Prevalence for Coronary Heart Disease in Minnesota in 2021"),
#                 tags$h2("County Level Data"),
#                 tags$h4("This dataset provides county-level data for Minnesota counties in 2021.
#                         It includes the age-adjusted prevalence of coronary heart disease, confidence limits, and population estimates.")
#               )
#             ),
#             fluidRow(
#               column(
#                 width = 12,
#                 DT::dataTableOutput("age_adjusted_table")
#               )
#             )
#           )
#         )
#       )
# )
#
#
#
# # Server Logic -----------------------------------------------------------------
# server <- function(input, output, session) {
#
#   output$county_table <- DT::renderDataTable({
#     CHD_Crude21MN
#   })
#
#   output$age_adjusted_table <- DT::renderDataTable({
#     CHD_Adj21MN
#   })
#
#   output$region_narrative <- renderUI({
#     selected_county <- input$parGlobal_county
#     chb <- chb_raw %>% filter(County == selected_county) %>% select(CHB)
#     chb <- chb$CHB
#     region <- mn_region_raw %>% filter(County == selected_county) %>% select(Region)
#     region <- region$Region
#     region_text <- paste0("County: <b><font color=red>", selected_county, "</font></b><br>Region: <b>", region, "</b>")
#     HTML(region_text)
#   })
#
#   output$chb_narrative <- renderUI({
#     selected_county <- input$parGlobal_county
#     chb <- chb_raw %>% filter(County == selected_county) %>% select(CHB)
#     chb <- chb$CHB
#     chb_text <- paste0("County: <b><font color=red>", selected_county, "</font></b><br>Community Health Board: <b>", chb, "</b>")
#     HTML(chb_text)
#   })
#
#   output$RegionDefinition <- renderTable({
#     mn_region_raw %>% mutate(County = ifelse(County == input$parGlobal_county, paste0("<font color=red>", County, "</font>"), County)) %>%
#       arrange(Region) %>%
#       kable("html", escape = FALSE, row.names = FALSE) %>%
#       kable_styling("striped", full_width = FALSE)
#   })
#
#   output$CHBDefinition <- renderTable({
#     chb_raw %>% mutate(County = ifelse(County == input$parGlobal_county, paste0("<font color=red>", County, "</font>"), County)) %>%
#       arrange(CHB) %>%
#       kable("html", escape = FALSE, row.names = FALSE) %>%
#       kable_styling("striped", full_width = FALSE)
#   })
# }
#
# # Create Shiny App -------------------------------------------------------------
# shinyApp(ui = ui, server = server)









######### Shiny Dashboard
## Load Packages ---------------------------------------------------------------
library(shiny)
library(readr)
library(ggplot2)
library(kableExtra)
library(dplyr)
library(tidyverse)
library(shinydashboard)
library(shinyjs)
##


# Load Data -------------------------------------------------------------------
# Minnesota CDC Places Census Estimate data for 2020 to 2022
CensusEstMN <- read.csv(
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/2020%20to%202022%20Pop.%20Estimates/cc-est2022-agesex.csv',
)

# Minnesota Coronary Heart Disease (CHD) data 2018 to 2021
CHD_files <- list(
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2018.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2019.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2020.csv',
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/CDC%20Places/Places%20CDC%20Estimates/CHD/CHD2021.csv'
)

CHD_data <- lapply(CHD_files, read.csv)

# CHB, County
chb_raw <- read.csv(
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/Community%20Health%20Board%20as%20of%201_17_2024.csv',
)

# MN Region
mn_region_raw <- read.csv(
  'https://raw.githubusercontent.com/quincountychsmn/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/State%20Community%20Health%20Services%20Advisory%20Committee%20as%20of%201_17_2024.csv',
)


# Data Wrangling --------------------------------------------------------------
# Clean and merge CHD data
CHD_data[[1]]$Latitude <- "NA"
colnames(CHD_data[[1]])[colnames(CHD_data[[1]]) == 'Latitude'] <- 'LocationID'
colnames(CHD_data[[1]])[colnames(CHD_data[[1]]) == 'Geolocatioin'] <- 'Geolocation'
CHD_data <- lapply(CHD_data, function(df) {
  df$LocationID <- as.character(df$LocationID)
  return(df)
})
CHD_Final <- bind_rows(CHD_data)


# Filter and select locations
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


# Clean census data
CensusEstMN$CTYNAME <- gsub(" County", "", CensusEstMN$CTYNAME)


# Population estimates for CHD in MN
PopEst_CHDMN <- CensusEstMN |>
  filter(YEAR == 3) |>
  inner_join(Selected_Locations, by = c("CTYNAME" = "LocationName")) |>
  select(CTYNAME, Data_Value_Type, AGE18PLUS_TOT, Measure, Data_Value, High_Confidence_Limit, Low_Confidence_Limit)


# Calculate crude and age-adjusted values
calc_aggregate_values <- function(df) {
  df |>
    mutate(Aggregate_Data_Value = Data_Value * AGE18PLUS_TOT / 100,
           Aggregate_Low_Confidence_Limit = Low_Confidence_Limit * AGE18PLUS_TOT / 100,
           Aggregate_High_Confidence_Limit = High_Confidence_Limit * AGE18PLUS_TOT / 100) |>
    select(CTYNAME, Data_Value_Type, AGE18PLUS_TOT, Aggregate_High_Confidence_Limit,
           Aggregate_Data_Value, Aggregate_Low_Confidence_Limit)
}


CHD_Crude21MN <- calc_aggregate_values(PopEst_CHDMN |> filter(Data_Value_Type == 'Crude prevalence'))
CHD_Adj21MN <- calc_aggregate_values(PopEst_CHDMN |> filter(Data_Value_Type == 'Age-adjusted prevalence'))
CHD_MN21 <- bind_rows(CHD_Adj21MN, CHD_Crude21MN)


# User Interface --------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(
    title = "CDC Places to MN Regions",
    titleWidth = 400
    #disable = TRUE #uncomment if the header should be hid
  ),

  dashboardSidebar(
    width = 350, #This makes the sidebar wider. However, the input boxes seem to have a set dimension resulting in long names still wrapping

    # Other input elements...
    selectInput(
      "parGlobal_county",
      label = "Select County of Interest",
      choices = sort(unique(mn_region_raw$County)),
      selected = "Kittson", #"Kittson"
      width = 350
    ),

    # Sidebar is required to have sub menus because it requires the tabName to reference
    # By having sidebarMenu id, you can reference it and hide other filters with shinyjs
    # The CHB and Region Filters are greyed out when on the Region & CHB Definitions
    sidebarMenu(
      menuItem("Home", tabName = "tn_homePage"),
      menuItem("Region & CHB Definitions", tabName = "tn_regionChbDefinations"),
      menuItem("Coronary Heart Disease", tabName = "tn_coronaryHeartDisease")
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(), # Thank you Abby Stamm at MDH for suggesting to only call one function in a package rather then load entire package
    tabItems(
      tabItem(
        tabName = "tn_homePage", # tabName is what ties the menuItem to the tabItem
        tabsetPanel(
          tabPanel(
            "Home Page",
            # Narrative section explaining the purpose of the dashboard
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
                tags$h3("Those involved with this project:"),
                tags$h4("Emmanuel Fle Chea, MPH, Public Health Data Science, University of Minnesota School of Public Health"),
                tags$h4("Mr. Patrick Olson (Preceptor), Quin County Community Health Board, Community Resource Liaison/Associate/Researcher"),
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "tn_regionChbDefinations",
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
                    #The next line inserts a line between the narrative and the data
                    tags$hr(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;")
                  )
                ),
                fluidRow(
                  column(6, uiOutput("region_narrative", style = "font-size: 20px;")),
                  column(6, uiOutput("chb_narrative_01", style = "font-size: 20px;"))
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
                width = 2,
                selectInput(
                  "parLocal_leadYear",
                  label = "Select Year",
                  choices = sort(unique(Selected_Locations$Year), decreasing = TRUE),
                  selected = max(unique(Selected_Locations$Year))
                ),
                selectInput(
                  "par_leadStateRegionChb",
                  label = "Select Comparison",
                  choices = c("All", "State", "Region", "Community Health Board"),
                  selected = "All",
                  multiple = FALSE
                )
              ),
              column(
                width = 10,
                box(
                  title = "Coronary Heart Disease Exposure Estimate",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("plot_chdEstimate")
                ),
                box(
                  title = "Confidence Interval for Coronary Heart Disease Estimate",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("plot_confidenceInterval")
                )
              )
            )
          )
        )
      )
    )
  )
)

# Preload Data prior to Server -----------------------------
#To improve performance load these once and don't have them run every time the server runs


#* Region & County Definitions ---------------------------------------------
# Create region data frame for global narrative reference
schsac_raw <- mn_region_raw
schsac_raw$Region <-  paste("<b>", schsac_raw$Region, "</b>") #Bold Regions so it is easier to understand narrative

# Create chb data frame for global narrative reference
# CHB Narrative 01 (It is split in half so it displays in two nice columns on the UI
chb_raw_01 <- chb_raw[1:33,]
chb_raw_01$CHB <-  paste("<b>", chb_raw_01$CHB, "</b>") #Bold CHBs so it is easier to understand narrative

# Create chb data frame for global narrative reference
# CHB Narrative 02 (It is split in half so it displays in two nice columns on the UI
chb_raw_02 <- chb_raw[34:nrow(chb_raw),] # :nrow means it goes to the end of the data frame
chb_raw_02$CHB <-  paste("<b>", chb_raw_02$CHB, "</b>") #Bold CHBs so it is easier to understand narrative


# Server ----------------------------------------------------------------------

# Region & County ---------------------------------------------------------

output$region_narrative <- renderUI({
  # Replace the values that are equal to input county by adding the font tag
  schsac_raw$County[schsac_raw$County == input$parGlobal_county] <-  paste("<font color=red>", schsac_raw$County[schsac_raw$County == input$parGlobal_county], "</font>")

  # Split the data frame by the group column
  schsac_split <- split(schsac_raw , schsac_raw$Region)

  # Apply a function to each subset to create a new column with comma-separated values
  schsac_character <- sapply(schsac_split, function(x) paste(x$County, collapse = ", "))

  # Convert the result to a data frame
  schsac_df <- as.data.frame(schsac_character)

  # Create an empty character vector to store the results
  schsac_result <- character()

  # Loop through each row of schsac_df
  for (i in 1:nrow(schsac_df)) {
    # Get the row name and schsac_character_placeholder as strings
    rowname <- as.character(rownames(schsac_df)[i])
    schsac_character_placeholder <- as.character(schsac_df$schsac_character[i])

    # Concatenate them with a separator
    concat <- paste0(rowname, ":: ", schsac_character_placeholder)

    # Append the result to the vector
    schsac_result <- c(schsac_result, concat)
  }

  # Paste the result vector and create a new line after each Region
  HTML(paste(schsac_result, collapse=  "<br/>"))

})

output$chb_narrative_01 <- renderUI({
  # Replace the values that are equal to input county by adding the font tab
  chb_raw_01$County[chb_raw_01$County == input$parGlobal_county] <-  paste("<font color=red>", chb_raw_01$County[chb_raw_01$County == input$parGlobal_county], "</font>")

  # Split the data frame by the chb
  chb_split <- split(chb_raw_01 , chb_raw_01$CHB)

  # Apply a function to each subset to create a new column with comma-separated values
  chb_character <- sapply(chb_split, function(x) paste(x$County, collapse = ", "))

  # Convert the result to a data frame
  chb_df <- as.data.frame(chb_character)

  # Create an empty character vector to store the results
  chb_result <- character()

  # Loop through each row of chb_df
  for (i in 1:nrow(chb_df)) {
    # Get the row name and chb_character_placeholder as strings
    rowname <- as.character(rownames(chb_df)[i])
    chb_character_placeholder <- as.character(chb_df$chb_character[i])

    # Concatenate them with a separator
    concat <- paste0(rowname, ":: ", chb_character_placeholder)

    # Append the result to the vector
    chb_result <- c(chb_result, concat)
  }

  # Paste the result vector and create a new line after each CHB
  HTML(paste(chb_result, collapse =  "<br/>"))
})

output$chb_narrative_02 <- renderUI({
  # Replace the values that are equal to input county by adding the font tab
  chb_raw_02$County[chb_raw_02$County == input$parGlobal_county] <-  paste("<font color=red>", chb_raw_02$County[chb_raw_02$County == input$parGlobal_county], "</font>")

  # Split the data frame by the chb
  chb_split <- split(chb_raw_02 , chb_raw_02$CHB)

  # Apply a function to each subset to create a new column with comma-separated values
  chb_character <- sapply(chb_split, function(x) paste(x$County, collapse = ", "))

  # Convert the result to a data frame
  chb_df <- as.data.frame(chb_character)

  # Create an empty character vector to store the results
  chb_result <- character()

  # Loop through each row of chb_df
  for (i in 1:nrow(chb_df)) {
    # Get the row name and chb_character_placeholder as strings
    rowname <- as.character(rownames(chb_df)[i])
    chb_character_placeholder <- as.character(chb_df$chb_character[i])

    # Concatenate them with a separator
    concat <- paste0(rowname, ":: ", chb_character_placeholder)

    # Append the result to the vector
    chb_result <- c(chb_result, concat)
  }

  # Paste the result vector and create a new line after each CHB
  HTML(paste(chb_result, collapse =  "<br/>"))
})

# Update the region input based on the county input
# In case data source doesn't have the same number of counties Null, Blank, Minnesota are examples.
# Currently, the county source is from the region data source but if it ever changes this should help capture it
observeEvent(input$parGlobal_county, {
  if(input$parGlobal_county %in% mn_region_raw$County) {
    updateSelectInput(
      session, "parGlobal_region",
      choices =  unique(mn_region_raw$Region[order(mn_region_raw$County == input$parGlobal_county, decreasing = TRUE)]),
      selected = unique(mn_region_raw$Region[order(mn_region_raw$County == input$parGlobal_county, decreasing = TRUE)])[1] #Default to the first choice, which will automatically change the cascading results when the parameter is mapped to a ggplot
    )
  }
  else {
    updateSelectInput(
      session, "parGlobal_region",
      choices = "",
      selected = ""
    )
  }
}
)

# Update the chb input based on the county input
# In case data source doesn't have the same number of counties Null, Blank, Minnesota are examples.
# Currently, the county source is from the chb data source but if it ever changes this should help capture it
observeEvent(input$parGlobal_county, {
  if(input$parGlobal_county %in% chb_raw$County) {
    updateSelectInput(
      session, "parGlobal_chb",
      choices =  unique(chb_raw$CHB[order(chb_raw$County == input$parGlobal_county, decreasing = TRUE)]),
      selected = unique(chb_raw$CHB[order(chb_raw$County == input$parGlobal_county, decreasing = TRUE)])[1] #Default to the first choice, which will automatically change the cascading results when the parameter is mapped to a ggplot
    )
  }
  else {
    updateSelectInput(
      session, "parGlobal_chb",
      choices = "",
      selected = ""
    )
  }
}
)

# Observe what sidebar the user is on and only allow for read only access to global parameters
observe({
  if(input$smID %in% c("tn_homePage","tn_regionChbDefinations")) # it requires an ID of sidebarMenu (in this case)
  {
    # shinyjs::disable("parGlobal_region")
    # shinyjs::disable("parGlobal_chb")
    shinyjs::hide("parGlobal_region")
    shinyjs::hide("parGlobal_chb")
  }
  else
  {
    shinyjs::enable("parGlobal_region")
    shinyjs::enable("parGlobal_chb")
  }
})

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
    filtered_region <- mn_region_raw %>%
      filter(County == input$parGlobal_county)
    HTML(
      paste0(
        "<b>", unique(filtered_region$RegionName), " Region</b> is made up of the following counties: ",
        paste(unique(filtered_region$County), collapse = ", "), "."
      )
    )
  })

  output$chb_narrative_01 <- renderUI({
    filtered_chb <- chb_raw %>%
      filter(County == input$parGlobal_county)
    HTML(
      paste0(
        "<b>", unique(filtered_chb$CHBName), " Community Health Board</b> includes: ",
        paste(unique(filtered_chb$County), collapse = ", "), "."
      )
    )
  })

  # Reactive Data
  reactive_CHD_data <- reactive({
    CHD_MN21 %>% filter(CTYNAME == input$parGlobal_county)
  })

  output$plot_chdEstimate <- renderPlot({
    data <- reactive_CHD_data()
    ggplot(data, aes(x = CTYNAME, y = Aggregate_Data_Value, fill = Data_Value_Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Coronary Heart Disease Exposure Estimate", x = "County", y = "Estimate")
  })

  output$plot_confidenceInterval <- renderPlot({
    data <- reactive_CHD_data()
    ggplot(data, aes(x = CTYNAME, y = Aggregate_Data_Value, color = Data_Value_Type)) +
      geom_errorbar(aes(ymin = Aggregate_Low_Confidence_Limit, ymax = Aggregate_High_Confidence_Limit), width = 0.2) +
      labs(title = "Confidence Interval for Coronary Heart Disease Estimate", x = "County", y = "Estimate")
  })
}
#
# # App Initialization ----------------------------------------------------------
shinyApp(ui = ui, server = server)
#
