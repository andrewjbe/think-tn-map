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
    # a(paste("Think", em("Tennessee"), "State of the Counties Dashboard [DRAFT / NOT FOR PUBLICATION]"), href = "https://www.thinktennessee.org/")
    HTML("<a href='https://www.thinktennessee.org/'>Think<em>Tennessee</em> State of the Counties Dashboard [DRAFT / NOT FOR PUBLICATION]</em></a>")
  ),
  windowTitle = "[DRAFT] ThinkTennessee State of the Counties Dashboard"
  ),
  
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
        }
        .btn {
          margin-bottom: 5px;
          padding-top: 0.1em;
          padding-bottom: 0.1em;
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
      checkboxInput(inputId = "show_labels",
                    label = "Show County Labels?",
                    value = FALSE),
      # htmlOutput('description_text_fill'),
      # Metric reports
      p("Use the buttons below to download a PDF report with the selected metric across all counties, or a snapshot of the current map."),
      downloadButton(outputId = "pdf_download_all_counties",
                     label = "All Counties Summary"),
      br(),
      # downloadButton("snapshot_download",
      #                "Map Snapshot"),
      hr(),
      # County reports
      h4("County Summaries"),
      p("The first PDF report below provides an overview of the selected county. 
        The second lists all metrics for the selected county."),
      selectInput(inputId = "pdf_select_county",
                  label = "Selected County",
                  # choices = tn_county_list, # for DEMO!!!
                  choices = "Anderson",
                  selected = "Anderson",
                  multiple = FALSE,
                  selectize = TRUE),
      downloadButton(outputId = "pdf_download_county_summary",
                     label = "County Overview"),
      downloadButton(outputId = "pdf_download_all_metrics",
                     label = "All Metrics"),
      # Copy for the sidebar can go here if room allows.
      ),
    
    # Main panel ---------------------------------------------------------------
    mainPanel(
      width = 9,
      # Leaflet output ---------------------------------------------------------
      tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"), # This makes the map fill up the page vertically
      tags$style(type = "text/css", "#show-panel {height: 30px; width: 100px;"), # This makes the "compare stats" easybutton bigger
      withSpinner(
        leafletOutput('map')
      ),
      # Absolute / compare stats panel -----------------------------------------
      tags$style("#plotly_panel {
                 background-color: #FFFFFF; 
                 opacity: 0.9;
                 z-index: 1000;
                 }"), # Sets background color / alpha of absolute panel, moves to front
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
            checkboxInput(inputId = "add_stat2",
                          label = "Enable Comparison Tool",
                          value = FALSE),
            plotlyOutput('plotly'),
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

