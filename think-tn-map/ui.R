#' Think TN metric map
#' By: Andrew Bell
#'
#' On behalf of: Market Retrievers Consulting / Think Tennessee

library(shiny)
library(bslib)
library(shinyjs)
library(readr)
library(dplyr)
library(here)
library(leaflet)
library(plotly)
library(janitor)
library(shinycssloaders)
options(scipen=999,
        readr.show_col_types = F)

# Data -------------------------------------------------------------------------
tn_county_list <- read_csv(here("data", "tn-counties-list.csv")) |>
  arrange(`County Name`)

info_ds <- here("data", "data-info.csv") |>
  read_csv() |>
  clean_names()

categories <- info_ds |>
  distinct(category) |>
  pull(var = category)

# UI code ----------------------------------------------------------------------
ui <- fluidPage(

  theme = bs_theme(bootswatch = "journal",
                   primary = "#1a884a",
                   secondary = "#f8b43c"),
  useShinyjs(),
  
  # Application title ----------------------------------------------------------
  titlePanel(div(
    tags$a(img(src = "think-tn-logo.png",
               height = "3%", width = "8%"),
           href = "https://www.thinktennessee.org/"),
    a("Think Tennessee Data Map [DRAFT / NOT FOR PUBLICATION]", href = "https://www.thinktennessee.org/")
  ),
  windowTitle = "[DRAFT] Think Tennessee Data Map"),
  
  tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
  
  # Input sidebar --------------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Choose first statistic (map fill)
      tags$style(
        type = "text/css", 
        "h3 {
          padding: 0px; margin: 0px; padding-bottom: 5px;
        }"
        ),
      h3("Mapping Tools"),
      p("Select a category and a metric below to see it displayed on the county map."),
      selectInput(inputId = "fill_stat_cat",
                  label = "Map Metric Category:",
                  choices = categories,
                  selected = "Children",
                  multiple = FALSE,
                  selectize = TRUE),
      uiOutput("fill_stat_ui"),
      # htmlOutput('description_text_fill'),
      # Metric reports
      p("The PDF report below provides an overview of the selected metric across all counties."),
      downloadButton(outputId = "pdf_download_all_counties",
                     label = "All Counties Summary"),
      hr(),
      # County reports
      h4("County Summaries"),
      p("The first PDF report below provides an overview of the selected county. 
        The second lists all metrics for the selected county."),
      selectInput(inputId = "pdf_select_county",
                  label = "Selected County",
                  # choices = tn_county_list, # for DEMO!!!
                  choices = "Shelby",
                  selected = "Shelby",
                  multiple = FALSE,
                  selectize = TRUE),
      downloadButton(outputId = "pdf_download_county_summary",
                     label = "County Overview"),
      downloadButton(outputId = "pdf_download_all_metrics",
                     label = "All Metrics"),

      # hr(),
      # div("This interactive tool allows you to visualize OK Policy's Child Well-being metrics on a map of Oklahoma's 77 counties.
      #   You can also click the 'Compare Stats' button in the top right corner to select an additional variable for comparison. The
      #   map is designed to display more negative outcomes (such as a high unemployment rate or a low median income) with darker colors.",
      #   HTML("<br><br>"),
      #   "This tool was developed as part of the Oklahoma Policy Institute's KIDS COUNT research. For more information about OK Policy's KIDS COUNT work, visit its",
      #   a("KIDS COUNT website", href = "https://okpolicy.org/topic/kids-count/"),
      #   "All questions, suggestions, and feedback for this tool may be addressed to our research team using the following email address:",
      #   HTML("<a href='abell@okpolicy.org' target='_blank'>abell@okpolicy.org</a>"),
      #   style = "font-size:14px;" # <- this font size is only for the paragraph at the bottom, not the whole sidebar
      # )
      ),
    
    # Main panel ---------------------------------------------------------------
    mainPanel(
      width = 9,
      # Leaflet output ---------------------------------------------------------
      tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"), # This makes the map fill up the page vertically
      tags$style(type = "text/css", "#show-panel {height: 30px; width: 100px;"), # This makes the "compare stats" easybutton bigger
      tags$style(type = "text/css", ".info.legend.leaflet-control {max-width: 40vw"),
      withSpinner(
        leafletOutput('map')
      ),
      # Absolute / compare stats panel -----------------------------------------
      tags$style("#plotly_panel {background-color: #FFFFFF; opacity: 0.9;}"), # Sets background color / alpha of absolute panel
      hidden(
        absolutePanel(
          id = "plotly_panel",
          class = "panel panel-default",
          fixed = FALSE,
          draggable = FALSE,
          top = "11%",
          left = "auto",
          right = 30,
          bottom = "auto",
          width = 400,
          # height = 700,
          wellPanel(
            plotlyOutput('plotly'),
            checkboxInput(inputId = "add_stat2",
                          label = "Add comparison metric?",
                          value = FALSE),
            # Comparison statistic
            selectInput(inputId = "stat2_cat",
                        label = "Comparison Metric Category:",
                        choices = categories,
                        selected = "Economy",
                        multiple = FALSE,
                        selectize = TRUE),
            uiOutput("stat2_ui"),
            # Stat2 nice description
            htmlOutput('description_text_stat2'),
            hr(),
            # Trend line toggle
            shiny::checkboxInput(inputId = "toggle",
                                 label = "Toggle Chart Trend Line"),
            # set axes to begin at 0 toggle
            shiny::checkboxInput(inputId = "lims",
                                 label = "Set X and Y axis to begin at zero")
          )
        )
      )
    )
  )
)

