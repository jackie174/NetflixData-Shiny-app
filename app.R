library(shiny)
library(ggplot2)
library(ggiraph)
library(ggtext)
library(leaflet)
library(dplyr)
library(tidyr)
library(tidyverse)
library(maps)
library(rnaturalearth)
library(readr)
library(sf)
library(RColorBrewer)
library(remotes)
library(shinythemes)
library(plotly)
library(htmlwidgets)
library(shinyjs)
library(gganimate)

############################################################################################################
############################################ CLEAN DATA START ##############################################
############################################################################################################

################################# CLEANING DATA START ######################################################
# Load world coordinates data
world_coordinates <-
  ne_countries(scale = "medium", returnclass = "sf")

# Rename some country names in the 'world_coordinates' dataset
world_coordinates <- world_coordinates %>%
  mutate(name_long = str_replace_all(
    name_long,
    c(
      'Russian Federation' = 'Russia',
      'Republic of Korea' = 'South Korea',
      'Dem. Rep. Korea' = 'North Korea',
      'Taiwan' = 'Taiwan Province'
    )
  ))

# Load the Netflix dataset and replace empty strings with 'Unknown'
netflix <-
  read_csv("netflix_titles.csv", show_col_types = FALSE) %>% replace(. == "", "Unknown")

# Clean the 'netflix' dataset: replacing country names, filling NA values, and correcting 'rating' values
netflix <- netflix %>%
  mutate(
    country = str_replace_all(
      country,
      c(
        'West Germany' = 'Germany',
        'East Germany' = 'Germany',
        'Vatican City' = 'Vatican',
        'Hong Kong' = 'China',
        'Taiwan' = 'China'
      )
    ),
    country = replace_na(country, "unknown country"),
    rating = str_replace_all(
      rating,
      c(
        '66 min' = 'unknown rating',
        '74 min' = 'unknown rating',
        '84 min' = 'unknown rating'
      )
    ),
    rating = replace_na(rating, "unknown rating"),
    director = replace_na(director, "unknown director"),
    cast = replace_na(cast, "unknown cast"),
    duration = replace_na(duration, "unknown duration"),
    date_added = replace_na(date_added, "unknown date_added")
  )

# Ensure each 'country' entry in the 'netflix' dataset is unique
netflix$country <-
  sapply(netflix$country, function(x)
    paste(unique(unlist(str_split(
      x, ", "
    ))), collapse = ","))

# Reduce the size of the 'netflix' dataset by selecting specific columns
netflix_country_indp <-
  netflix[, c('country', 'title', 'rating', 'type', 'listed_in')]

# Separate rows in the 'netflix_country_indp' dataset for each country in collaborative production
netflix_country_coop <-
  netflix_country_indp %>% separate_rows(country, sep = ",")

# Calculate the number of collaborative productions for each country
country_frequency <-
  netflix_country_coop %>% group_by(country) %>% summarise(number_coop = n())

# Calculate the number of independent productions for each country
unique_country_counts_temp <-
  netflix_country_indp %>% group_by(country) %>% summarise(number = n())

# Merge the independent and collaborative production numbers
netflix_country_comb <-
  merge(unique_country_counts_temp,
        country_frequency,
        by = "country",
        all.x = TRUE) %>%
  mutate(number = replace_na(number, 0),
         number_coop = replace_na(number_coop, 0))

# Merge the production numbers into the 'world_coordinates' spatial data
world_coordinates <-
  merge(
    world_coordinates,
    netflix_country_comb,
    by.x = "name_long",
    by.y = "country",
    all.x = TRUE
  )

# Transform the CRS of 'world_coordinates' to WGS 84 and replace NA values with 0
world_coordinates <-
  st_transform(world_coordinates, crs = 4326) %>%
  mutate(number = replace_na(number, 0),
         number_coop = replace_na(number_coop, 0))

#################################CLEANING DATA DONE#########################################################

################################ ADD NEW COLUMN START#######################################################
# Create a copy of 'world_coordinates' named 'country_centers'
country_centers <- world_coordinates

# Calculate the centroid for each country and store it in 'country_centers'
country_centers <- st_point_on_surface(country_centers)

# Extract longitude and latitude of each centroid and store them in new columns 'long' and 'lat'
country_centers$long <- st_coordinates(country_centers)[, 1]
country_centers$lat <- st_coordinates(country_centers)[, 2]

# Calculate the radius for each country based on the number of productions
country_centers$radius <- sqrt(country_centers$number) * 0.4
country_centers$radius_coop <-
  sqrt(country_centers$number_coop) * 0.4

# Define bins for color scaling based on production numbers
bin_number <- country_centers$number[country_centers$number > 0]
bin_number_coop <-
  country_centers$number_coop[country_centers$number_coop > 0]
bins <-
  10 ^ (seq(floor(log10(min(
    bin_number
  ))), ceiling(log10(max(
    bin_number
  ))), length.out = 10))
bins <- round(bins)
binss <-
  10 ^ (seq(floor(log10(
    min(bin_number_coop)
  )), ceiling(log10(
    max(bin_number_coop)
  )), length.out = 10))
binss <- round(binss)

# Define color palettes for independent and cooperative productions
pal_indp <-
  colorBin(
    palette = c("white", rgb(229 / 255, 9 / 255, 19 / 255)),
    domain = country_centers$number,
    bins = c(0, bins)
  )
pal_coop <-
  colorBin(
    palette = c("white", rgb(229 / 255, 9 / 255, 19 / 255)),
    domain = country_centers$number_coop,
    bins = c(0, binss)
  )
################################ ADD NEW COLUMN DONE########################################################

#################################CREATE NEW DATASET STRAT ##################################################
# Extract country names from the 'world_coordinates' dataset
country_names <- world_coordinates$name_long
new_data <- data.frame(country = country_names)

# Add an entry for 'unknown country' to the dataset
na_countries <- data.frame(country = 'unknown country')
new_data <- rbind(new_data, na_countries)

# Join the 'new_data' with 'netflix_country_indp' to get the full dataset for independent production
netflix_country_indp_full <- new_data %>%
  inner_join(netflix_country_indp, by = c("country" = "country"))

# Group by 'country' and calculate the number of occurrences for each country in the independent production dataset
netflix_country_indp_full <- netflix_country_indp_full %>%
  group_by(country) %>%
  mutate(number = n()) %>%
  ungroup()

# Group by 'country' and calculate the number of occurrences for each country in the cooperative production dataset
netflix_country_coop_full <- netflix_country_coop %>%
  group_by(country) %>%
  mutate(number_coop = n()) %>%
  ungroup()

# Generate summary statistics for independent production by different grouping
type_counts_indp <-
  netflix_country_indp_full %>% group_by(type) %>% summarise(number = n(), .groups = 'drop')
type_counts_indp_country <-
  netflix_country_indp_full %>% group_by(country, type) %>% summarise(number = n(), .groups = 'drop')
type_counts_indp_type <-
  netflix_country_indp_full %>% group_by(type, rating) %>% summarise(number = n(), .groups = 'drop')
listed_counts_indp_type <- netflix_country_indp_full %>%
  separate_rows(listed_in, sep = ",")
listed_counts_indp_type <-
  listed_counts_indp_type %>% group_by(type, listed_in) %>% summarise(number = n(), .groups = 'drop')
listed_counts_indp_type1 <- netflix_country_indp_full %>%
  separate_rows(listed_in, sep = ",")
listed_counts_indp_type <-
  listed_counts_indp_type1 %>% group_by(type, listed_in) %>% summarise(number = n(), .groups = 'drop')
listed_counts_indp_type_country <-
  listed_counts_indp_type1 %>% group_by(type, country, listed_in) %>% summarise(number = n(), .groups = 'drop')

listed_counts_coop_type1 <- netflix_country_coop_full %>%
  separate_rows(listed_in, sep = ",")
listed_counts_coop_type <-
  listed_counts_coop_type1 %>% group_by(type, listed_in) %>% summarise(number = n(), .groups = 'drop')
listed_counts_coop_type_country <-
  listed_counts_coop_type1 %>% group_by(type, country, listed_in) %>% summarise(number = n(), .groups = 'drop')
listed_counts_coop_type <- netflix_country_coop_full %>%
  separate_rows(listed_in, sep = ",")
listed_counts_coop_type <-
  listed_counts_coop_type %>% group_by(type, country, listed_in) %>% summarise(number = n(), .groups = 'drop')
type_counts_coop <-
  netflix_country_coop_full %>% group_by(type) %>% summarise(number = n(), .groups = 'drop')
type_counts_coop_country <-
  netflix_country_coop_full %>% group_by(country, type) %>% summarise(number = n(), .groups = 'drop')
type_counts_coop_type <-
  netflix_country_coop_full %>% group_by(type, rating) %>% summarise(number = n(), .groups = 'drop')

rating_counts_country_indp_ <- netflix_country_indp_full %>%
  group_by(country, type, rating) %>%
  summarise(number = n(), .groups = 'drop')
rating_counts_country_coop_ <- netflix_country_coop_full %>%
  group_by(country, type, rating) %>%
  summarise(number = n(), .groups = 'drop')

#################################CREATE NEW DATASET DONE ###################################################

############################# HELP FUNCTION START ###########################################################
# Define a function to get the coordinates for setting the map view
getSelectedCoords <- function(data, default = c(0, 0)) {
  # Check the number of rows in the data
  if (nrow(data) > 1 | nrow(data) == 0) {
    # If more than one country is selected or no country is selected, use default coordinates and zoom level
    lat <- default[1]
    lng <- default[2]
    zoom <- 2
  } else {
    # If a specific country is selected, use its coordinates and adjust the zoom level based on the number of productions
    lat <- data$lat[1]
    lng <- data$long[1]
    zoom <- if (data$number > 0)
      4
    else
      6
  }
  # Return a list containing the latitude, longitude, and zoom level
  return(list(lat = lat, lng = lng, zoom = zoom))
}

# Define a function to create HTML labels for the map
getLabel <- function(name_long, number) {
  # Ensure the lengths of name_long and number match
  min_length <- min(length(name_long), length(number))
  name_long <- name_long[1:min_length]
  number <- number[1:min_length]
  
  # Create and return HTML labels
  return(
    sprintf(
      "Country: <strong>%s</strong><br/>Number: %g",
      name_long,
      number
    ) %>% lapply(htmltools::HTML)
  )
}

# Define a function to display a warning message when no countries are available to display
warning_message <- function(name) {
  return(tags$div(class = "card", h3(
    sprintf(
      "There are no countries available to display on the %s.<br/>Please use the reset button to reset the data.",
      name
    ) %>% lapply(htmltools::HTML)
  )))
}

# Define a function to process and sort country data based on the sorting state and selected production type
processData <- function(countryData, sort_state, input_sector) {
  selected_production <-
    if (input_sector == 'Collaborative Production')
      countryData$number_coop
  else
    countryData$number
  # Sort data based on sort state and selected production
  countryData <- if (sort_state == "number") {
    countryData %>% arrange(selected_production)
  } else if (sort_state == "name_long") {
    countryData %>% arrange(desc(name_long))
  }
  return(countryData)
}
# Define a function to generate a color gradient for Movie visualizations
mv_color <- function(start, end) {
  original_color <- rgb(229 / 255, 9 / 255, 19 / 255, 1)
  num_categories <- 15
  alpha_values <- seq(start, end, length.out = num_categories)
  mv_color <-
    sapply(alpha_values, function(alpha)
      adjustcolor(original_color, alpha.f = alpha))
}

# Define a function to generate a color gradient for TV Show visualizations
tv_color <- function(start, end) {
  original_color <- rgb(64 / 255, 64 / 255, 64 / 255, 1)
  num_categories <- 15
  alpha_values <- seq(start, end, length.out = num_categories)
  tv_color <-
    sapply(alpha_values, function(alpha)
      adjustcolor(original_color, alpha.f = alpha))
}
############################# HELP FUNCTION DONE ###########################################################


############################################################################################################
############################################ CLEAN DATA DONE ###############################################
############################################################################################################

##################
# USER INTERFACE #
##################

######################### First Nav Tab Start ##############################################################
countries_tab <- tabPanel(title = 'Movie & TV Show Number',
                          sidebarLayout(
                            sidebarPanel(
                              id = "mySidebar",
                              width = 2,
                              selectInput(
                                'region',
                                label = 'Country or Region',
                                choices = c(
                                  'All',
                                  country_centers %>%
                                    arrange(desc(number)) %>%
                                    pull(name_long) %>%
                                    unique()
                                ),
                                selected = 'All',
                              ),
                              radioButtons(
                                'sector',
                                label = 'Production Method',
                                choices = c('Collaborative Production', 'Independent Production'),
                                selected = 'Collaborative Production',
                              ),
                              sliderInput(
                                "number",
                                "Minimum number of production on Netflix",
                                1,
                                3695,
                                value = 100,
                                sep = ""
                              ),
                              actionButton(
                                "reset_button",
                                label = HTML('<i class="fa fa-refresh"></i>'),
                                title = "Reset all data to refresh page."
                              ),
                              downloadButton('down_hist_countries', title =
                                               "Download image for bar chart.", '')
                              
                            ),
                            mainPanel(
                              id = 'map',
                              width = 10,
                              tabsetPanel(
                                id = "map_bar",
                                tabPanel(
                                  "Number in Map",
                                  tags$div(
                                    style = "text-align:center;font-size:16px;",
                                    tags$b("The Number of Movie and TV Show in Netflix by Country")
                                  ),
                                  
                                  uiOutput("map_countries_final")
                                ),
                                # https://stackoverflow.com/q/58510443
                                # Get to know how to put icon and text in same line
                                tabPanel(
                                  "Number in Bar Chart",
                                  div(
                                    style = "position:relative;",
                                    
                                    div(HTML(
                                      paste0(
                                        "<div style='position:absolute; top:0px; right:40px; font-size:14px;'title='Sort the production number by country and number'>",
                                        as.character(actionLink(
                                          inputId = "sort_button",
                                          label = "",
                                          icon = icon("sort")
                                        )),
                                        "</div>"
                                      )
                                    )),
                                    uiOutput("dynamic_plot", click = "plot_click")
                                    
                                  )
                                )
                              )
                            )
                          ))
######################### First Nav Tab Done ###############################################################

######################### Second Nav Tab Start #############################################################
# Define a tab panel titled 'Type and Rating'
type_tab <- tabPanel(title = 'Type and Rating',
                     sidebarLayout(
                       # Define a sidebar panel with input controls
                       sidebarPanel(
                         id = "mySidebar",
                         width = 2,
                         # Define a dropdown menu for selecting a country or region
                         selectInput(
                           'region2',
                           label = 'Country or Region',
                           choices = c(
                             'All',
                             country_centers %>%
                               arrange(desc(number)) %>%
                               pull(name_long) %>%
                               unique()
                           ),
                           selected = 'All',
                         ),
                         # Define radio buttons for selecting the production method
                         radioButtons(
                           'sector2',
                           label = 'Production Method',
                           choices = c('Collaborative Production', 'Independent Production'),
                           selected = 'Collaborative Production',
                         ),
                         # Define a reset button to refresh the page data
                         actionButton(
                           "reset_button2",
                           label = HTML('<i class="fa fa-refresh"></i>'),
                           title = "Reset all data to refresh page."
                         ),
                       ),
                       # Define the main panel containing the chart plots
                       mainPanel(
                         id = "pie",
                         width = 9,
                         # Define a set of tabs within the main panel
                         tabsetPanel(
                           # Define the first tab panel containing Pie Charts
                           tabPanel(
                             "Pie Chart",
                             fluidRow(column(3),
                                      column(
                                        6, plotlyOutput("tv_mv_pie_chart", height = 350)
                                      ),
                                      column(3)),
                             fluidRow(column(
                               6, plotlyOutput("mv_pie_chart", height = 300)
                             ),
                             column(
                               6, plotlyOutput("tv_pie_chart", height = 300)
                             ))
                           ),
                           # Define the second tab panel containing Bar Charts
                           tabPanel(
                             "Bar Chart",
                             fluidRow(column(3),
                                      column(
                                        6, plotlyOutput("tv_mv_bar_chart", height = 350)
                                      ),
                                      column(3)),
                             fluidRow(column(
                               6, plotlyOutput("mv_bar_chart", height = 300)
                             ),
                             column(
                               6, plotlyOutput("tv_bar_chart", height = 300)
                             ))
                           )
                         )
                       )
                     ))

######################### Second Nav Tab Done ##############################################################
######################### Third Nav Tab Start #############################################################
# Define a tab panel titled 'Type and Rating'
genre_tab <- tabPanel(title = 'Type and Genre',
                      sidebarLayout(
                        # Define a sidebar panel with input controls
                        sidebarPanel(
                          id = "mySidebar",
                          width = 2,
                          # Define a dropdown menu for selecting a country or region
                          selectInput(
                            'region3',
                            label = 'Country or Region',
                            choices = c(
                              'All',
                              country_centers %>%
                                arrange(desc(number)) %>%
                                pull(name_long) %>%
                                unique()
                            ),
                            selected = 'All',
                          ),
                          # Define radio buttons for selecting the production method
                          radioButtons(
                            'sector3',
                            label = 'Production Method',
                            choices = c('Collaborative Production', 'Independent Production'),
                            selected = 'Collaborative Production',
                          ),
                          sliderInput(
                            "number1",
                            "Minimum number of production on Netflix",
                            0,
                            2250,
                            value = c(300, 1000),
                            sep = ""
                          ),
                          # Define a reset button to refresh the page data
                          actionButton(
                            "reset_button3",
                            label = HTML('<i class="fa fa-refresh"></i>'),
                            title = "Reset all data to refresh page."
                          ),
                        ),
                        # Define the main panel containing the chart plots
                        mainPanel(
                          id = "genre",
                          width = 9,
                          # Define a set of tabs within the main panel
                          tabsetPanel(
                            # Define the first tab panel containing Pie Charts
                            id = "thirdpanel",
                            tabPanel("Total",
                                     br(),
                                     plotlyOutput("tv_mv_bar_chart_g", height = 500)),
                            tabPanel(
                              "TV Show Chart by Genre",
                              br(),
                              plotlyOutput("tv_bar_chart_g", height = 500)
                            ),
                            
                            tabPanel(
                              "Movie Chart by Genre",
                              br(),
                              
                              plotlyOutput("mv_bar_chart_g", height = 500)
                              
                              
                            )
                            
                            
                            
                          )
                        )
                      ))

######################### Third Nav Tab Done ##############################################################
ui <- navbarPage(
  id = 'mypage',
  # this is needed to be able to change the selected tab from code
  title = 'NETFLIX Movie and TV Show',
  theme = shinythemes::shinytheme("paper"),
  fluid = TRUE,
  collapsible = TRUE,
  ######################## CSS  START #######################
  header = tags$head(tags$style(
    HTML(
      "
             .navbar .navbar-brand {
              color: red !important;
              font-weight: bold !important;
              font-size: 23px !important;
            }

            .nav-tabs>li.active>a,
            .nav-tabs>li.active>a:focus {
              box-shadow: inset 0 -2px 0 #E50913;
              color: #E50913;
              border: 1px solid #ddd;
            }

            .nav-tabs>li>a:hover,
            .nav-tabs>li>a:focus:hover {
              box-shadow: inset 0 -2px 0 #E50913;
              color: #E50913;
              border: 1px solid #ddd;
            }

            .nav-tabs>li.active>a:hover,
            .nav-tabs>li.active>a:focus:hover {
              border: 1px solid #ddd;
              color: #E50913;
            }


            .control-label {
              color: grey !important;
              font-weight: bold !important;
              font-size: 18px !important;
            }

            #sort_button {
              display: inline !important;
              margin: 0px !important;
              color: grey !important;
              text-align: right !important;

            }

            input[type=radio]:after {
              background-color: white;

            }

            input[type='radio']:checked:after {
              background-color: #E50913;

            }

            input[type=radio]:checked:after,
            .radio input[type=radio]:checked:after,
            .radio-inline input[type=radio]:checked:after {
              border-color: white;
            }


            #mySidebar {
              min-width: 130px !important;
            }

            #map {
              max-width: 1150px !important;
            }
            #pie {
              max-width: 1150px !important;
            }

            .js-irs-0 .irs-single,
            .js-irs-0 .irs-bar-edge,
            .js-irs-0 .irs-bar {
              background-color: #E50913;
              border-color: #E50913;
            }

            .js-irs-0 .irs-to {
              background-color: #E50913;
              border-color: #E50913;
            }

            .js-irs-0 .irs-from {
              background-color: #E50913;
              border-color: #E50913;
            }
            .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
              color: #fff;
              background-color: #E50913;
            }
            .irs--shiny .irs-bar {
                background: #E50913;
                border-color: #E50913;
            }
            .irs--shiny .irs-bar {
                background: #E50913;
                border-color: #E50913;
            }
            .card {
              justify-content: center;
              align-items: center;
              border: 1px solid #ddd;
              border-radius: 8px;
              padding: 20px;
              box-shadow: 2px 2px 12px #aaa;
              margin: 20px;
              text-align: center;
              background-color: #e0e0e0;
              color: black;
            }
             .summarycard {
              justify-content: center;
              align-items: center;
              border: 1px solid #ddd;
              border-radius: 8px;
              padding: 20px;
              box-shadow: 2px 2px 12px #aaa;
              margin: 20px;
              text-align: center;
              background-color: #f9f9f9;
              color: black;
            }

    "
    )
  )),
  ######################## CSS  DONE #########################################################################
  countries_tab,
  type_tab,
  genre_tab,
  # To avoid Waring 
  footer = tags$div(""),
  
  navbarMenu(
    "More",
    tabPanel("Data Table",  mainPanel(dataTableOutput("viewTable")))
  )
)

################
# SHINY SERVER #
################
server <- function(input, output, session) {

  ######################  Help Function on First Tab Start ####################
  # Define a reactive expression to filter country data based on user input
  getFilteredCountryData <- reactive({
    number_range <- input$number
    selected_production <-
      if (input$sector == 'Collaborative Production')
        country_centers$number_coop
    else
      country_centers$number
    country_centers %>% filter((input$region == 'All' |
                                  name_long == input$region) & selected_production >= number_range
    )
  })
  
  # Define a reactive expression to get production details based on filtered country data
  productionDetails <- reactive({
    countryData <- getFilteredCountryData()
    selected_production <-
      if (input$sector == 'Collaborative Production')
        countryData$number_coop
    else
      countryData$number
    selected_productionn <-
      if (input$sector == 'Collaborative Production')
        world_coordinates$number_coop
    else
      world_coordinates$number
    labels <- getLabel(countryData$name_long, selected_production)
    labelss <-
      getLabel(world_coordinates$name_long, selected_productionn)
    pall <-
      if (input$sector == 'Collaborative Production')
        pal_coop
    else
      pal_indp
    radius <-
      if (input$sector == 'Collaborative Production')
        countryData$radius_coop
    else
      countryData$radius
    list(
      selected_production = selected_production,
      selected_productionn = selected_productionn,
      labels = labels,
      labelss = labelss,
      pall = pall,
      radius = radius
    )
  })
  
  # Define a reactive expression to get production details based on sorted country data
  productionDetails_hist <- reactive({
    countryData <- sortedCountryData()
    selected_production <-
      if (input$sector == 'Collaborative Production')
        countryData$number_coop
    else
      countryData$number
    labels <- getLabel(countryData$name_long, selected_production)
    list(selected_production = selected_production, labels = labels)
  })
  
  # Define a reactive expression to dynamically set the height of bar chart
  plot_height <- reactive({
    if (length(unique(sortedCountryData()$name_long)) > 30) {
      return(1000)
    } else {
      return(600)
    }
  })
  
  # Initialize a reactive value to store the state of sorting
  sort_state <- reactiveVal("number")
  
  # Define a reactive expression to get sorted country data based on user input and sorting state
  sortedCountryData <- reactive({
    countryData <- getFilteredCountryData()
    processData(countryData, sort_state(), input$sector)
  })
  
  ###################### Help Function on First Tab Done ###################################################
  
  ################################# Draw Bar Chart on first tab Start ###############################
  
  output$dynamic_plot <- renderUI({
    countryData_sort <- sortedCountryData()
    if (nrow(countryData_sort) == 0) {
      # If length is zero, display a sentence
      warning_message("bar chart")
    } else {
      # Otherwise, render the bar chart
      div(
        style = sprintf(
          'width:auto;height:%spx;overflow-y: scroll;',
          ifelse(plot_height() > 600, 600, plot_height())
        ),
        plotOutput(
          "hist_countries",
          height = plot_height(),
          click = "plot_click"
        )
      )
    }
    
  })
  # https://stackoverflow.com/a/49978505 
  # To download the image
  output$down_hist_countries <- downloadHandler(
    filename = function() {
      paste("bar chart", '.png', sep = '')
    },
    content = function(file) {
      ggsave(file,
             plot = plotOutputReactive(),
             width = 13,
             height = 18)
    }
  )
  output$hist_countries <- renderPlot({
    plotOutputReactive()
    
  })
  plotOutputReactive <- reactive({
    countryData_sort <- sortedCountryData()
    prodDetails <- productionDetails_hist()
    countryData_sort$name_long <-
      factor(countryData_sort$name_long, levels = countryData_sort$name_long)
    # https://r-graph-gallery.com/301-custom-lollipop-chart.html#horiz 
    # How to draw lollopop chart
    ggplot(countryData_sort,
           aes(x = name_long,
               y = prodDetails$selected_production, )) +
      geom_segment(
        aes(
          x = name_long,
          xend = name_long,
          y = 0,
          yend = prodDetails$selected_production,
          color = "#FFCCCC",
        )
      ) +
      labs(title = "The Number of Movie and TV Show in Netflix by Country",
           y = "Production Number",
           x = "Country") +
      geom_point(size = 4,
                 color = '#FFCCCC',
                 alpha = 0.8,) +
      geom_text(
        aes(
          label = sprintf("%d", prodDetails$selected_production),
          y = prodDetails$selected_production,
          x = name_long
        ),
        hjust = -0.5,
        color = 'red'
      ) +
      theme_light() +
      coord_flip() +
      guides(color = FALSE) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(
          size = 16,
          face = "bold",
          hjust = 0.5,
          color = "#606060"
        ),
        plot.title.position = "panel",
        axis.title.x = element_text(
          color = "black",
          size = 16,
          face = "bold",
          margin = margin(
            t = 10,
            r = 0,
            b = 0,
            l = 0
          )
        ),
        axis.title.y = element_text(
          color = "black",
          size = 16,
          face = "bold",
          margin = margin(
            t = 0,
            r = 10,
            b = 0,
            l = 0
          )
        ),
        axis.text.x = element_text(
          color = "#606060",
          size = 12,
          face = "bold",
          margin = margin(
            t = 10,
            r = 0,
            b = 0,
            l = 0
          )
        ),
        axis.text.y = element_text(
          color = "#606060",
          size = 12,
          angle = 0,
          hjust = 0,
          face = "bold",
          margin = margin(
            t = 0,
            r = -20,
            b = 0,
            l = 10
          )
        )
      )
    
  })
  ################################# Draw Bar Chart on first tab Done #######################################
  
  ##################### Draw Map on first tab Start ###################################################
  # Define a UI output to render the Leaflet map or a warning message
  output$map_countries_final <- renderUI({
    # Obtain filtered country data
    countryData <- getFilteredCountryData()
    # Further filter the country data where the 'number' column is greater than 0 and arrange by 'name_long'
    countryData_filtered <-
      countryData %>% arrange(name_long) %>% filter(number > 0)
    
    # Check if there are any rows in the filtered country data
    if (nrow(countryData_filtered) == 0) {
      # If no rows are present, display a warning message
      warning_message("map")
    } else {
      # Otherwise, render the Leaflet map with a specified height
      leafletOutput("map", height = '600px')
    }
  })
  
  # Define a Leaflet output to render the map
  output$map <- renderLeaflet({
    map_countries()
  })
  
  # Define a reactive expression to create a Leaflet map with polygons, legend, buttons, and other elements
  map_countries <- reactive({
    # Obtain filtered country data and further filter it
    countryData <- getFilteredCountryData()
    countryData_filtered <-
      countryData %>% arrange(name_long) %>% filter(number > 0)
    
    # Initialize a reactive value for sorting order
    sorting_order <- reactiveVal("name_long")
    
    # Obtain coordinates for the selected country
    coords <- getSelectedCoords(countryData)
    
    # Determine the currently selected country
    selected_country <-
      if (nrow(countryData) == 1)
        countryData$name_long[1]
    else
      'NoCountry'
    
    # Obtain production details that contain pal, selected_productionn, labelss
    prodDetails <- productionDetails()
    
    # Create a Leaflet map with various elements and layers
    leaflet(countryData) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(lng = coords$lng,
              lat = coords$lat,
              zoom = coords$zoom) %>%
      setMaxBounds(
        lng1 = -180,
        lat1 = -55,
        lng2 = 180,
        lat2 = 85
      ) %>%
      # Add polygons to represent countries
      addPolygons(
        data = world_coordinates,
        fillColor = ~ prodDetails$pall(prodDetails$selected_productionn),
        color = ~ ifelse(name_long == selected_country, 'white', 'grey'),
        weight = ~ ifelse(name_long == selected_country, 1, 1),
        opacity = ~ ifelse(name_long == selected_country, 0.8, 0.2),
        smoothFactor = 0.2,
        fillOpacity = 0.6,
        highlight = highlightOptions(
          color = "white",
          weight = 1,
          opacity = 0.8,
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~ prodDetails$labelss,
        layerId = ~ name_long,
        labelOptions = labelOptions(
          direction = 'top',
          textOnly = TRUE,
          noHide = FALSE,
          style = list(
            "font-weight" = "normal",
            "font-size" = "14px",
            "color" = "black",
            padding = "3px 8px"
          )
        )
      ) %>%
      # Add a reset view button
      addEasyButton(easyButton(
        icon = "fa-globe",
        title = "Reset view",
        onClick = JS("function(btn, map){ map.setView([0,0], 2); }")
      )) %>%
      # Add a legend to the map
      addLegend(
        position = "bottomleft",
        pal = prodDetails$pall,
        values = ~ prodDetails$selected_productionn,
        title = "Total Numbers"
      )
  })
  
  ##################### Draw Map on first tab Done #########################################################
  
  ###################### Help function on Second Tab Start ####################
  # Define a reactive expression to get filtered country data based on user input
  getFilteredCountryData2 <- reactive({
    # Check the value of 'region2' input
    if (input$region2 == "All") {
      # If 'region2' is "All", check the value of 'sector2' input
      if (input$sector2 == 'Collaborative Production') {
        # Return the collaborative production type counts, arranged by type
        type_counts_coop %>% arrange(desc(type))
      } else {
        # Return the independent production type counts, arranged by type
        type_counts_indp %>% arrange(desc(type))
      }
    } else {
      # If 'region2' is not "All", filter the data by the selected country and 'sector2' input
      if (input$sector2 == 'Collaborative Production') {
        type_counts_coop_country %>%
          filter((input$region2 == 'All' |
                    country == input$region2)) %>%
          arrange(number)
      } else {
        type_counts_indp_country %>%
          filter((input$region2 == 'All' |
                    country == input$region2)) %>%
          arrange(number)
      }
    }
  })
  
  ###################### Help function on Second Tab Done ##################################################

  ##################################### Draw pie chart on second nav tab Start  ################################
  # Define a function to create a pie chart using Plotly
  create_pie_chart <-
    function(data,
             type_input,
             original_color,
             hole,
             title) {
      if (type_input == "type") {
        data <- data.frame(category = data$type, count = data$number)
      } else {
        temp <- data %>%
          filter((type == type_input)) %>%
          arrange(number)
        data <-
          data.frame(category = temp$rating, count = temp$number)
      }
      # Calculate the fraction of each category
      data$fraction = data$count / sum(data$count)
      # If threshold less than 5%, do not display the label
      threshold <- 0.05
      # Create a Plotly pie chart with the processed data
      # https://plotly.com/r/pie-charts/ 
      p_plotly <- plot_ly(
        data,
        labels = ~ category,
        values = ~ count,
        type = 'pie',
        hole = hole,
        text = ~ ifelse(fraction < threshold, '', paste(
          category, '\n', sprintf("%.1f%%", fraction * 100)
        )),
        # Set the text displayed on the pie chart and on hover
        hoverinfo = 'text+value',
        textinfo = 'text',
        insidetextorientation = 'radial',
        marker = list(
          colors = original_color,
          line = list(color = '#FFFFFF', width = 2)
        )
      )
      # Set the layout for the pie chart
      p_plotly %>% layout(
        title = title,
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        )
      )
    }
  # Render the pie chart for Movie vs TV Show in Netflix
  output$tv_mv_pie_chart <- renderPlotly({
    original_color <-
      c(rgb(229 / 255, 9 / 255, 19 / 255, 0.7),
        rgb(64 / 255, 64 / 255, 64 / 255, 0.8))
    filterData <- getFilteredCountryData2()
    
    create_pie_chart(filterData,
                     "type",
                     original_color,
                     hole = 0,
                     'Movie vs TV Show in Netflix')
  })
  # Render the pie chart for Statistics of Movie Ratings on Netflix
  output$tv_pie_chart <- renderPlotly({
    tv_color <- tv_color(0.1, 0.9)
    filterData <- getFilteredRatingCounts()
    create_pie_chart(filterData,
                     "Movie",
                     tv_color,
                     hole = 0.5,
                     'Statistics of Movie Ratings on Netflix')
  })
  # Render the pie chart for Statistics of TV Show Ratings on Netflix
  output$mv_pie_chart <- renderPlotly({
    mv_color <- mv_color(0.1, 0.9)
    filterData <- getFilteredRatingCounts()
    create_pie_chart(filterData,
                     "TV Show",
                     mv_color,
                     hole = 0.5,
                     'Statistics of TV Show Ratings on Netflix')
  })
  ##################################### Draw pie chart on second nav tab Done ##############################
  
  ##################################### Draw 3 similar bar chart #######################################
  # Define a function to create a bar chart using Plotly
  create_bar_chart <-
    function(data,
             type_input,
             original_color,
             title,
             angle) {
      # Process data
      if (type_input == "type") {
        # If 'type_input' is "type", rearrange the data by 'type' and 'number'
        data <-
          data.frame(category = data$type, count = data$number) %>%
          arrange(count)
        data$category <- factor(data$category, levels = data$category)
      } else {
        temp <-
          data %>% filter((type == type_input)) %>% arrange(desc(number))
        data <-
          data.frame(category = temp$rating, count = temp$number)
        data$category <- factor(data$category, levels = data$category)
      }
      
      # Create a Plotly bar chart with the processed data
      p_plotly <- plot_ly(
        data,
        x = ~ category,
        y = ~ count,
        type = 'bar',
        marker = list(color = original_color, line = list(color = 'black'))
      ) %>% layout(
        title = title,
        xaxis = list(title = "Country", tickangle = angle),
        yaxis = list(title = "Production Number")
      ) %>% onRender(
        "
                   function(el) {
                 el.on('plotly_click', function(data) {
                   var yValue = data.points[0].x;
                   Shiny.setInputValue('pointClicked', data.points[0].x);
                 });
                   }
                   "
      )
      
      return(p_plotly)
    }
  
  # Render the bar chart for Movie vs TV Show in Netflix
  output$tv_mv_bar_chart <- renderPlotly({
    original_color <-
      c(rgb(229 / 255, 9 / 255, 19 / 255, 0.7),
        rgb(64 / 255, 64 / 255, 64 / 255, 0.8))
    filterData <- getFilteredCountryData2()
    create_bar_chart(filterData,
                     "type",
                     original_color,
                     'Movie vs TV Show in Netflix',
                     0)
  })
  
  # Render the bar chart for Statistics of Movie Ratings on Netflix
  output$tv_bar_chart <- renderPlotly({
    tv_color <- tv_color(0.8, 0.1)
    filterData <- getFilteredRatingCounts()
    create_bar_chart(filterData,
                     "Movie",
                     tv_color,
                     'Statistics of Movie Ratings on Netflix',
                     45)
  })
  
  # Render the bar chart for Statistics of TV Show Ratings on Netflix
  output$mv_bar_chart <- renderPlotly({
    mv_color <- mv_color(0.8, 0.1)
    filterData <- getFilteredRatingCounts()
    create_bar_chart(filterData,
                     "TV Show",
                     mv_color,
                     'Statistics of TV Show Ratings on Netflix',
                     45)
  })
  ##################################### Draw 3 similar bar chart ###########################################
  
  ###################### Help function on Third Tab Start ##################################################
  # Define a reactive expression to get filtered country data based on user input
  getFilteredCountryData3 <- reactive({
    # Check the value of 'region2' input
    if (input$region3 == "All") {
      # If 'region2' is "All", check the value of 'sector2' input
      if (input$sector3 == 'Collaborative Production') {
        # Return the collaborative production type counts, arranged by type
        listed_counts_indp_type %>% filter(number >= input$number1[1] &
                                             number <= input$number1[2]) %>%
          arrange(number)
        
      } else {
        # Return the independent production type counts, arranged by type
        listed_counts_coop_type %>% filter(number >= input$number1[1] &
                                             number <= input$number1[2]) %>%
          arrange(number)
      }
    } else {
      # If 'region2' is not "All", filter the data by the selected country and 'sector2' input
      if (input$sector3 == 'Collaborative Production') {
        listed_counts_coop_type_country %>%
          filter((input$region3 == 'All' |
                    country == input$region3) &
                   number >= input$number1[1] & number <= input$number1[2]
          ) %>%
          arrange(number)
      } else {
        listed_counts_indp_type_country %>%
          filter((input$region3 == 'All' |
                    country == input$region3) &
                   number >= input$number1[1] & number <= input$number1[2]
          ) %>%
          arrange(number)
      }
    }
  })
  
  # Define a reactive expression to get filtered rating counts based on user input
  getFilteredRatingCounts <- reactive({
    # Similar logic as above, but applied to rating counts data
    if (input$region2 == "All") {
      if (input$sector2 == 'Collaborative Production') {
        type_counts_coop_type
      } else {
        type_counts_indp_type
      }
    } else {
      if (input$sector2 == 'Collaborative Production') {
        rating_counts_country_coop_ %>%
          filter((input$region2 == 'All' |
                    country == input$region2)) %>%
          arrange(number)
      } else {
        rating_counts_country_indp_ %>%
          filter((input$region2 == 'All' |
                    country == input$region2)) %>%
          arrange(number)
      }
    }
  })
  
  ###################### Help function on Thrid Tab Done ###################################################
  
  ##################################### Draw 3 bigger bar chart on 3rd nav tab #############################
  
  
  create_bar_chart1 <-
    function(data,
             type_input,
             original_color,
             title,
             angle) {
      # Process data
      if (type_input == "type") {
        # If 'type_input' is "type", rearrange the data by 'type' and 'number'
        data <-
          data.frame(category = data$listed_in,
                     count = data$number) %>%
          arrange(desc(count))
        data <- data[!duplicated(data$category),]
        data$category <- factor(data$category, levels = data$category)
      } else {
        temp <-
          data %>% filter((type == type_input)) %>% arrange(desc(number))
        data <-
          data.frame(category = temp$listed_in,
                     count = temp$number)
        data <- data[!duplicated(data$category),]
        data$category <- factor(data$category, levels = data$category)
      }
      
      data <- data %>% arrange(desc(count))
      
      # Create a Plotly bar chart with the processed data
      p_plotly <- plot_ly(
        data,
        x = ~ category,
        y = ~ count,
        type = 'bar',
        marker = list(color = original_color, line = list(color = 'black'))
      ) %>% layout(
        title = title,
        xaxis = list(title = "Genre", tickangle = angle),
        yaxis = list(title = "Production Number")
      )
      
      return(p_plotly)
    }
  # Render the bar chart for Movie vs TV Show in Netflix
  output$tv_mv_bar_chart_g <- renderPlotly({
    original_color <- rgb(64 / 255, 64 / 255, 64 / 255, 1)
    filterData <- getFilteredCountryData3()
    create_bar_chart1(filterData,
                      "type",
                      original_color,
                      'Movie and TV Show by Genre in Netflix',
                      45)
  })
  
  # Render the bar chart for Statistics of Movie Ratings on Netflix
  output$mv_bar_chart_g <- renderPlotly({
    tv_color <- tv_color(0.8, 0.1)
    filterData <- getFilteredCountryData3()
    create_bar_chart1(
      filterData,
      "Movie",
      rgb(64 / 255, 64 / 255, 64 / 255, 0.8),
      'Statistics of Movie Genre on Netflix',
      45
    )
  })
  
  # Render the bar chart for Statistics of TV Show Ratings on Netflix
  output$tv_bar_chart_g <- renderPlotly({
    mv_color <- mv_color(0.8, 0.1)
    filterData <- getFilteredCountryData3()
    create_bar_chart1(
      filterData,
      "TV Show",
      rgb(229 / 255, 9 / 255, 19 / 255, 0.7),
      'Statistics of TV Show Genre on Netflix',
      45
    )
  })
  
  
  ##################################### Draw 3 bigger bar chart on 3rd nav tab #############################
  
  
  #################################### Draw Table Start ####################################################
  # Render the table
  output$viewTable <- renderDataTable({
    netflix
  })
  #################################### Draw Table Start ####################################################
  
  ######################################  Observe Event Start ##############################################
  # Observe for a click event on the plot
  observeEvent(input$plot_click, {
    # Check if the y-coordinate of the click is numeric
    if (is.numeric(input$plot_click$y)) {
      # Round the y-coordinate to get the index of the selected country
      index <- round(input$plot_click$y)
      # Retrieve the selected country using the index
      selected_country <-
        input$plot_click$domain$discrete_limits$y[index]
      # Update the 'region2' select input with the selected country
      updateSelectInput(session, 'region2', selected = selected_country)
      # Navigate to the "Type and Rating" tab
      updateNavbarPage(session, "mypage", selected = "Type and Rating")
    }
  })
  
  # Observe if 'pointClicked' input value changes
  observeEvent(input$pointClicked, {
    # Update 'region3' select input to 'All' when a point is clicked
    updateSelectInput(session, 'region3', selected = 'All')
    # Check the value of 'pointClicked'
    if (input$pointClicked == 'TV Show') {
      # If 'pointClicked' is 'TV Show', navigate to "Type and Genre" tab of 'navbarPage'
      # and select "TV Show Chart by Genre" tab in 'tabsetPanel'
      updateNavbarPage(session, "mypage", selected = "Type and Genre")
      updateTabsetPanel(session, "thirdpanel", selected = "TV Show Chart by Genre")
    } else {
      # If 'pointClicked' is not 'TV Show', navigate to "Type and Genre" tab of 'navbarPage'
      # and select "Movie Chart by Genre" tab in 'tabsetPanel'
      updateNavbarPage(session, "mypage", selected = "Type and Genre")
      updateTabsetPanel(session, "thirdpanel", selected = "Movie Chart by Genre")
      
    }
  })
  
  
  # Observe for a click event on the sort button
  observeEvent(input$sort_button, {
    # Check the current state of sorting
    if (sort_state() == "number") {
      # If currently sorted by number, change the sorting to name_long
      sort_state("name_long")
    } else {
      # If not sorted by number, change the sorting to number
      sort_state("number")
    }
  })
  
  
  # Observe for a click event on the reset button in first tab
  observeEvent(input$reset_button, {
    # Reset the 'number' slider input to 100
    updateSliderInput(session, "number", value = 100)
    # Reset the 'region' select input to "All"
    updateSelectInput(session, "region", selected = "All")
    # Reset the 'sector' radio buttons to "Collaborative Production"
    updateRadioButtons(session, "sector", selected = "Collaborative Production")
  })
  
  # Observe for a click event on the reset button in second tab
  observeEvent(input$reset_button2, {
    # Reset the 'region2' select input to "All"
    updateSelectInput(session, "region2", selected = "All")
    # Reset the 'sector2' radio buttons to "Collaborative Production"
    updateRadioButtons(session, "sector2", selected = "Collaborative Production")
  })
  observeEvent(input$reset_button3, {
    # Reset the 'number1' slider input to 100
    updateSliderInput(session, "number1", value = c(300, 1000))
    # Reset the 'region2' select input to "All"
    updateSelectInput(session, "region3", selected = "All")
    # Reset the 'sector2' radio buttons to "Collaborative Production"
    updateRadioButtons(session, "sector3", selected = "Collaborative Production")
  })
  ######################################  Observe Event Done ###############################################
}
#############
# Run Shiny #
#############

shinyApp(ui = ui,
         server = server)
