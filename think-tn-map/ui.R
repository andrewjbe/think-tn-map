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
options(scipen=999,
        readr.show_col_types = F)

# Data -------------------------------------------------------------------------
tn_county_list <- read_csv(here("think-tn-map", "data", "tn-counties-list.csv")) |>
  arrange(`County Name`)

# TODO: placeholder, add groups
choices_grouped <- read_csv(here("think-tn-map", "data", "data-info.csv")) |>
  clean_names() |>
  pull(var = variable)

names(choices_grouped) <- read_csv(here("think-tn-map", "data", "data-info.csv")) |>
  clean_names() |>
  pull(var = metric_title)

# UI code ----------------------------------------------------------------------
ui <- fluidPage(

  theme = bs_theme(bootswatch = "journal",
                   primary = "#1a884a",
                   secondary = "#f8b43c"),
  useShinyjs(),
  
  # Application title ----------------------------------------------------------
  titlePanel(div(
    tags$a(img(src = "think-tn-logo.png",
               heigh = "3%", width = "8%"),
           href = "https://www.thinktennessee.org/"),
    a("Think Tennessee Data Map", href = "https://www.thinktennessee.org/")
  ),
  windowTitle = "Think Tennessee Data Map"),
  
  # Input sidebar --------------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Choose first statistic (map fill)
      tags$style(type = "text/css", "h3{padding: 0px; margin: 0px; padding-bottom: 5px;}"), # This just customizes the padding around h3()
      h3("Mapping Tools"),
      selectInput(inputId = "fill_stat",
                  label = "Fill Statistic",
                  choices = choices_grouped,
                  selected = "uninsured_adults",
                  multiple = FALSE,
                  selectize = TRUE),
      htmlOutput('description_text_fill'),
      hr(),
      h3("County Summaries"),
      p("Download a PDF report detailing all child well-being metrics for a specific county below."),
      selectInput(inputId = "pdf_select",
                  label = "Selected County",
                  choices = tn_county_list,
                  selected = "Tulsa",
                  multiple = FALSE,
                  selectize = TRUE),
      downloadButton(outputId = "pdf_download",
                     label = "Download"),
      hr(),
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
      leafletOutput('map'),
      # Absolute / compare stats panel -----------------------------------------
      tags$style("#plotly_panel {background-color: #FFFFFF; opacity: 0.9;}"), # Sets background color / alpha of absolute panel
      hidden(
        absolutePanel(
          id = "plotly_panel",
          class = "panel panel-default",
          fixed = TRUE,
          draggable = FALSE,
          top = 120,
          left = "auto",
          right = 30,
          bottom = "auto",
          width = 400,
          # height = 700,
          plotlyOutput('plotly'),
          wellPanel(
            # Comparison statistic
            selectInput(inputId = "stat2",
                        label = "Comparison Statistic",
                        choices = choices_grouped,
                        selected = "poverty_rate_all",
                        multiple = FALSE,
                        selectize = TRUE),
            # Stat2 nice description
            htmlOutput('description_text_stat2'),
            hr(),
            # Trend line toggle
            shiny::checkboxInput(inputId = "toggle",
                                 label = "Toggle Chart Trend Line"),
            # set axes to begin at 0 toggle
            shiny::checkboxInput(inputId = "lims",
                                 label = "Set X and Y axis to begin at zero")
          ),
        )
      )
    )
  )
)