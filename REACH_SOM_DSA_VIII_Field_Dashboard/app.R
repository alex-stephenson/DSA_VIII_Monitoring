
library(readxl)
library(dplyr)
library(tidyr)
##############
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(reactable)
library(leaflet)
library(sf)
library(base64enc)
library(reactablefmtr)

# Load the data from the Excel file
clogs_path <- "data/combined_clogs.xlsx"
site_file_path <- "data/DSA_App_idp-site-master-list-sep-2024.xlsx"
dashboard_file_path <- "data/dashboard_data.csv"
fo_data_file_path <- "data/Field team work distribution.xlsx"


# Section 1: Load and prepare clogs data
clogs <- read_excel(clogs_path, sheet = "Sheet1") %>%
  rename_with(~ make.names(.)) %>%  # Make column names R-friendly
  mutate(across(c(uuid, question, old_value, issue, check_id, check_binding, change_type, new_value, idp_code, district_name, ki_contact, ki_name, Responsible_FO), as.character),
         across(c(enum_name, ki_contact), as.integer))  # Change data types

# Section 2: Load and prepare pending clogs data
pending_clogs_FO <- clogs %>%
  filter(is.na(change_type)) %>%
  group_by(Responsible_FO) %>%
  summarise(Pending_Clogs = n(), .groups = 'drop')  # Group by Responsible_FO and count

# Section 3: Load and prepare complete clogs data
complete_clogs_FO <- clogs %>%
  filter(!is.na(change_type)) %>%
  group_by(Responsible_FO) %>%
  summarise(Complete_Clogs = n(), .groups = 'drop')  # Count complete clogs

clogs_table <- pending_clogs_FO %>%
  left_join(complete_clogs_FO, by = c("Responsible_FO"))

# Section 4: Load and prepare dashboard data
dashboard_data <- read.csv(dashboard_file_path, 
                           stringsAsFactors = FALSE)

# Change column types for dashboard data
dashboard_data <- dashboard_data %>%
  mutate(across(c(start, end, today), as.POSIXct),
         across(c(deviceid, Responsible_FO, enum_name, consent, district_name, localisation_region_label, idp_code, idp_site, idp_code_verification, 
                  idp_code_districti_verification, ki_role, ki_role_other, ki_gender), as.character),
         across(c(`X_observation_gps_latitude`, `X_observation_gps_longitude`), as.numeric))

# Section 5: Group dashboard data by region
dashboard_data_grouped_region <- dashboard_data %>%
  group_by(localisation_region_label) %>%
  summarise(Survey_Per_Region = n(), .groups = 'drop')  # Count surveys per region

# Section 6: Group dashboard data by district
dashboard_data_grouped_district <- dashboard_data %>%
  group_by(district_name) %>%
  summarise(Survey_Per_District = n(), .groups = 'drop')  # Count surveys per district


# Load FO Data sheet and format columns
fo_data <- read_excel(fo_data_file_path, sheet = "Sheet1") %>%
  rename_with(~ gsub(" ", "_", .)) %>%
  mutate(Locations = stringr::str_to_title(Locations)) %>%
  mutate(Locations = case_when(
    Locations %in% c("Mogadishu Dayniile", "Mogadishu Khada") ~ "Banadir", .default = Locations)
  ) %>%
  unique()

# Load "Sites for Field" sheet and format columns
sites_for_field <- read_excel(site_file_path, sheet = "CCCM IDP Site List (Verified)") %>%
  filter(Sampled == "To visit") %>%
  mutate(District = case_when(
    District %in% c("Mogadishu Dayniile", "Mogadishu Khada") ~ "Banadir", .default = District)
  ) %>% 
  select(-c("Source (Q1-2024)", "SWState Y/N")) %>%
  rename_with(~ gsub(" ", "_", .)) %>%
  mutate(across(c(CCCM_IDP_Site_Code, Region, District, IDP_Site, Neighbourhood, Neighbourhood_Type, Site_Accessible, Excluded_based_on_HH_Size, Sampled), as.character),
         across(c("HH_(Q1-2024)", "Individual_(Q1-2024)"), as.integer),
         Date_IDP_site_Established = as.Date(Date_IDP_site_Established, format = "%Y-%m-%d")) %>%
  left_join(fo_data, by = c("District" = "Locations"))



# Load and group "Sites Visited" data from the dashboard CSV
sites_visited <- read.csv(dashboard_file_path) %>%
  mutate(start = as.POSIXct(start),
         end = as.POSIXct(end),
         today = as.Date(today, format = "%Y-%m-%d")) %>%
  group_by(idp_code, Responsible_FO) %>%
  summarise(Count = n(), .groups = "drop")

# Process "District List"
District_List <- sites_for_field %>%
  select(Region, District, Responsible_FO) %>%
  group_by(Region, District, Responsible_FO) %>%
  summarise(Total_Sites = n()) %>%
  ungroup()

# Join with other tables and create custom columns
District_List <- District_List %>%
  left_join(dashboard_data_grouped_district, by = c("District" = "district_name")) %>%
  rename(Surveys_Complete = Survey_Per_District) %>%
  mutate(Surveys_Complete = replace_na(Surveys_Complete, 0),
         Total_Surveys = Total_Sites * 3,
         Survey_ratio = paste0(Surveys_Complete, " / ", Total_Surveys),
         Survey_Percent = Surveys_Complete / Total_Surveys) %>%
  arrange(Region)

# "Region_pct_done" calculation
region_pct_done <- District_List %>%
  group_by(Region, Responsible_FO) %>%
  summarise(Count_Region_Surv_Done = sum(Surveys_Complete, na.rm = TRUE),
            Count_Region_Surv_Total = sum(Total_Surveys, na.rm = TRUE)) %>%
  mutate(region_pct_done = (Count_Region_Surv_Done / Count_Region_Surv_Total)*100) %>%
  ungroup()

#District_List <- District_List %>%
#  left_join(region_pct_done, by = c("Region"))


# Process "Site List" sheet
site_list <- sites_for_field %>%
  select(-c(Date_IDP_site_Established, "HH_(Q1-2024)", "Individual_(Q1-2024)",
            Site_Accessible, Excluded_based_on_HH_Size, Sampled)) %>%
  left_join(sites_visited %>% select(-c(Responsible_FO)), by = c("CCCM_IDP_Site_Code" = "idp_code")) %>%
  rename(Surveys_Per_Site = Count,
         Total_Surveys = `#_Surveys`) %>%
  mutate(Surveys_Per_Site = ifelse(is.na(Surveys_Per_Site), 0, Surveys_Per_Site),
         Surveys_Complete = paste0(Surveys_Per_Site, " / ", Total_Surveys))


# Final processing on Sites with Coordinates
sites_with_coords <- read_excel(site_file_path, sheet = "CCCM IDP Site List (Verified)") %>%
  filter(Sampled == "To visit")

# Site level Data

## geo data


shp_path <- "data/gis_data/som_admbnda_adm2_ocha_20230308.shp"

regions_sf <- st_read(shp_path)  # or use st_read("path_to_regions.shp")
regions_sf <- regions_sf %>%
  left_join(District_List, by = c("ADM2_EN" = "District"))


#image_path <- "dashboard_landing_page.png"
#base64_image <- base64enc::dataURI(file = image_path, mime = "image/png")


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "DSA Field Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("District Data", tabName = "district_data", icon = icon("table")),
      menuItem("Site Data", tabName = "site_data", icon = icon("tent")),
      menuItem("Clogs Data", tabName = "clogs_data", icon = icon("exclamation-triangle")),
      menuItem("Survey Locations", tabName = "survey_locations", icon = icon("map-marker-alt")),
      menuItem("Enumerator Responses", tabName = "enum_responses", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(paste0("
       .content-wrapper {
        background-color: #f9f9f9; /* Light background */
      }
      .box {
        border-radius: 5px;
        background-color: #ffffff; /* White boxes for contrast */
      }
      .box-title {
        font-weight: bold;
        color: #333; /* Darker title color */
      }
      .leaflet {
        border-radius: 5px; /* Rounded map corners */
      }
      /* Overview tab background styling */
      .overview-background {
        background-image: url('https://www.impact-initiatives.org/wp-content/uploads/2018/07/header1.jpg');
        background-size: cover;
        background-position: center;
        color: white;
        height: 100vh;
        display: flex;
        align-items: center;
        justify-content: center;
        text-align: center;
        font-size: 20px;
      }
      /* Centered content box */
      .centered-text-box {
        background-color: rgba(0, 0, 0, 0.7); /* Semi-transparent dark background */
        padding: 30px 40px;
        border-radius: 10px;
        max-width: 600px;
        margin: 0 auto;
      }
      .centered-text-box h2 {
        font-size: 28px;
        font-weight: bold;
        margin-bottom: 20px;
        color: #FFFFFF;
      }
      .centered-text-box p {
        font-size: 18px;
        line-height: 1.6;
        color: #FFFFFF;
      }
      ")))
    ),
    tabItems(
      tabItem(tabName = "overview",
              div(class = "overview-background",
                  div(class = "centered-text-box",
                      h2("Welcome to the DSA VIII Field Dashboard"),
                      p("Use the Navigation menu (☰) to explore the district completion data, the clogs, response location data, and enumerator data."),
                      p("🐛🔧 If there are any bugs, please reach out to Alex Stephenson.")
                  )
              )
      )
    ,
      tabItem(tabName = "district_data",
              fluidRow(
                box(width = 3,
                    selectInput("region", "Select Region:", choices = c("All", unique(District_List$Region)), selected = "All")
                ),
                box(width = 3,
                    selectInput("fo", "Field Officer:", choices = c("All", unique(District_List$Responsible_FO)), selected = "All")
                )
              ),
              fluidRow(
                box(width = 6, title = "District Data", status = "primary", solidHeader = TRUE, reactableOutput("district_table")),
                box(width = 6, title = "Region", leafletOutput("region_map"))
              )
      ),
    tabItem(tabName = "site_data",
            fluidRow(
              box(width = 3,
                  selectInput("region_site", "Select Region:", choices = c("All", unique(District_List$Region)), selected = "All")
              ),
              box(width = 3,
                  selectInput("fo_site", "Field Officer:", choices = c("All", unique(District_List$Responsible_FO)), selected = "All")
              ),
              box(width = 3,
                  selectInput("district_site", "District:", choices = c("All", unique(site_list$District)), selected = "All")
              )
            ),
            fluidRow(
              box(width = 12, title = "Site Data", status = "primary", solidHeader = TRUE, reactableOutput("site_data"))
            )
    ),   
    
      tabItem(tabName = "clogs_data",
              fluidRow(
                box(width = 2,
                    selectInput("fo_clogs", "Field Officer:", choices = c("All", unique(clogs_table$Responsible_FO)), selected = "All")
                ),
                box(width = 10, title = "Clogs Data Overview", status = "primary", solidHeader = TRUE, reactableOutput("clogs_table"))
              ),
              fluidRow(
                box(width = 12, title = "Clogs Data Detailed", status = "primary", solidHeader = TRUE, reactableOutput("clogs_detailed"))
              )
              
      ),
      tabItem(tabName = "survey_locations",
              fluidRow(
                box(width = 12, title = "Survey Locations", status = "primary", solidHeader = TRUE, leafletOutput("location_map", height = 600))
              )
      ),
      tabItem(tabName = "enum_responses",
              fluidRow(
                box(width = 12, title = "Enumerator Responses", status = "primary", solidHeader = TRUE, plotOutput("enum_response_chart"))
              )
      )
    )
  )
)



server <- function(input, output, session) {
  # Filtered District List table
  output$district_table <- renderReactable({
    
    District_List_table_df <- District_List %>%
      filter(Region == input$region | input$region == "All") %>%
      filter(Responsible_FO == input$fo| input$fo == "All") %>%
      select(Region, District, Responsible_FO, Survey_ratio, Survey_Percent) %>%
      mutate(Survey_Percent = (Survey_Percent * 100),
             `% Complete` = round(Survey_Percent, 1)) %>%
      select(-c(Survey_Percent))
      

#    color_palette <- colorNumeric(palette = c("lightblue", "darkblue"), 
#                                  domain = District_List_table_df$Survey_Percent)
  
    reactable(
      District_List_table_df,
      defaultColDef = colDef(
        cell = data_bars(District_List_table_df,
                         text_position = "outside-base",
                         max_value = 100)
      )
    ) 

  })
  
  
  observeEvent(input$region, {
    # Filter districts based on the selected region
    districts <- if (input$region == "All") {
      unique(site_list$District)  # Show all districts
    } else {
      unique(site_list$District[site_list$Region == input$region])  # Filter districts by selected region
    }
    
    # Update the district select input
    updateSelectInput(session, "district", choices = c("All", districts))
  })
  
  observeEvent(input$region, {
    # Filter districts based on the selected field officer
    fos <- if (input$region == "All") {
      unique(site_list$Responsible_FO)  # Show all districts
    } else {
      unique(site_list$Responsible_FO[site_list$Region == input$region])  # Filter districts by selected FO
    }
    
    # Update the district select input
    updateSelectInput(session, "fo", choices = c("All", fos))
  })

  
  output$site_data <- renderReactable({
    reactable(
      (site_list %>% select(CCCM_IDP_Site_Code, IDP_Site, Region, District, Responsible_FO, Surveys_Complete) %>%
         rename(Site_Name = CCCM_IDP_Site_Code) %>%
         filter(Region == input$region_site | input$region_site == "All") %>% ## change filters here
         filter(Responsible_FO == input$fo_site| input$fo_site == "All") %>%
         filter(District == input$district_site | input$district_site == "All")
    ))
    
  })
  
  observeEvent(input$region_site, {
    # Filter districts based on the selected region
    districts <- if (input$region_site == "All") {
      unique(site_list$District)  # Show all districts
    } else {
      unique(site_list$District[site_list$Region == input$region_site])  # Filter districts by selected region
    }
    
    # Update the district select input
    updateSelectInput(session, "district_site", choices = c("All", districts))
  })
  
  observeEvent(input$fo_site, {
    # Filter districts based on the selected field officer
    districts <- if (input$fo_site == "All") {
      unique(site_list$District)  # Show all districts
    } else {
      unique(site_list$District[site_list$Responsible_FO == input$fo_site])  # Filter districts by selected FO
    }
    
    # Update the district select input
    updateSelectInput(session, "district_site", choices = c("All", districts))
  })
  
  # Filtered Clogs table
  output$clogs_table <- renderReactable({
    clogs_table %>%
      filter((Responsible_FO == input$fo_clogs | input$fo_clogs == "All")) %>%
      #      filter((Region == input$region | input$region == "All")) %>%
      select(Responsible_FO, Pending_Clogs, Complete_Clogs) %>%
      reactable()
  })
  
  # detailed clogs table
  
  output$clogs_detailed <- renderReactable({
    clogs %>%
      filter((Responsible_FO == input$fo_clogs | input$fo_clogs == "All")) %>%
      reactable()
  })  
  
  # Enumerator Responses bar chart
  output$enum_response_chart <- renderPlot({
    dashboard_data %>%
      filter(localisation_region_label == input$region | input$region == "All") %>%
      ggplot(aes(x = enum_name, fill = localisation_region_label)) +
      geom_bar() +
      labs(x = "Enum Code", y = "Count", fill = "Region") +
      theme_minimal()
  })
  
  
  
  color_palette <- colorNumeric(palette = c("lightblue", "darkblue"), 
                                domain = c(0,0.1))
  
  output$region_map <- renderLeaflet({
    # Set the domain for the color scale based on Survey_Percent
    
    
    leaflet(data = regions_sf) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~color_palette(Survey_Percent), # Use the color scale
        color = "darkblue",
        weight = 1,
        opacity = 0.8,
        fillOpacity = 0.7,
        label = ~paste0("District: ", ADM2_EN, r"(.  )", "Surveys Complete: ", Survey_ratio),
        highlightOptions = highlightOptions(
          color = "white",
          weight = 2,
          bringToFront = TRUE
        )
      )
  })
  
  
  # Individual Survey Location Map
  output$location_map <- renderLeaflet({
    leaflet(data = dashboard_data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~X_observation_gps_longitude,
        ~X_observation_gps_latitude,
        radius = 2,
        color = "blue",
        fillOpacity = 0.6,
        label = ~paste0("IDP Site: ", idp_site, ", District: ", district_name  )
        
      )
  })
}

shinyApp(ui, server)





