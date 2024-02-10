#' Think TN metric map
#' By: Andrew Bell
#'
#' On behalf of: Market Retrievers Consulting / Think Tennessee

# Dependencies and Configuration ===============================================
library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(here)
library(janitor)
library(leaflet)
library(sf)
library(plotly)
library(htmltools)
library(htmlwidgets)
options(scipen=999,
        readr.show_col_types = F)

library(reactlog)

format_num <- function(num){
  
  # Reformats numbers from 0-1 as %s, anything > 1 with a comma, and anything NA as NA
  if(!is.na(num)) {
    if(num < 1){
      return(paste0(format(round(100 * num, 2), big.mark = ","), "%"))
    } else {
      return(format(round(num, 2), big.mark = ","))
    }
  } else {
    return(NA)
  }
  
}

format_num_vec <- base::Vectorize(format_num)

# Data =========================================================================
# Main dataset with all metrics ------------------------------------------------
# main_ds <- here("think-tn-map", "data", "data-clean.csv") |>
main_ds <- here("data", "data-clean.csv") |>
  read_csv()

# Dataset with info / details on all metrics -----------------------------------
# info_ds <- here("think-tn-map", "data", "data-info.csv") |>
info_ds <- here("data", "data-info.csv") |>
  read_csv() |>
  clean_names()

# Variable names 
# var_names <- main_ds |>
#   select(!c(county)) |>
#   names() |>
#   as_tibble() |>
#   rename(variable = value) |>
#   left_join(info_ds |> select(variable, metric_title), by = "variable")
var_names <- info_ds |> 
  select(variable, metric_title)

# County shapefile -------------------------------------------------------------
# county_shape <- read_rds(here("think-tn-map", "data", "tn-counties.rds")) |>
county_shape <- read_rds(here("data", "tn-counties.rds")) |>
  clean_names() |>
  rename(county = name)
  
# Server logic =================================================================
function(input, output, session) {
  
  reactlog_enable()

  # Reactive variables / dataframes ============================================
  # Mapping Data reactive dataframe --------------------------------------------
  map_data.react <- reactive({
    z <- main_ds |>
      select(county,
             fill_stat = input$fill_stat,
             stat2 = input$stat2
      ) |>
      mutate(fill_stat.toggle = as.numeric(NA),
             stat2.toggle = as.numeric(NA)) |>
      filter(county %in% county_shape$county)

    if(input$toggle){
      z <- z |>
        mutate(
          fill_stat.toggle = as.numeric(fill_stat),
          stat2.toggle = as.numeric(stat2)
        )
    }
    return(z)
  })
  
  # Reactive values with currently selected fill stat info ---------------------
  fill_stat_info <- reactive({
    
    info_ds |>
      filter(variable == input$fill_stat)
    
  })
  
  stat2_info <- reactive({
    
    info_ds |>
      filter(variable == input$stat2)
    
  })
  
  # Nicely formatted reactive description of fill stat -------------------------
  output$description_text_fill <- renderUI({
    str <- paste0(
      "<b>", fill_stat_info()$metric_title, ":</b> ",
      fill_stat_info()$description,
      "<br><b>Source: </b>", 
      fill_stat_info()$source,
      " (", 
      fill_stat_info()$years, 
      ")"
    )
    HTML(str)
    
  })

  # Nicely formatted reactive description of stat2 -----------------------------
  output$description_text_stat2 <- renderUI({
    str <- paste0(
      "<b>", stat2_info()$metric_title, ":</b> ",
      stat2_info()$description,
      "<br><b>Source: </b>", 
      stat2_info()$source,
      " (", 
      stat2_info()$years,
      ")"
    )
    HTML(str)
  })

  # Leaflet Map ================================================================
  output$map <- renderLeaflet({

    # Map geometry -------------------------------------------------------------
    m <- left_join(county_shape, map_data.react(), by = "county") |>
      st_transform(4326)

    # Generates map color scale based on selected stat -------------------------
    pal <- colorBin(
      # palette = info_ds[which(info_ds$variable == input$fill_stat),] |>
      #   pull(var = "color_scheme"),
      palette = "Greens",
      # reverse = if_else(info_ds[which(info_ds$variable == input$fill_stat),] |>
      #                     pull(var = "good_outcomes") == "low", FALSE, TRUE),
      domain = map_data.react()$fill_stat,
      na.color = "grey",
      bins = 6,
      pretty = TRUE
    )

    # The map itself
    leaflet(options = leafletOptions(minZoom = 7, maxZoom = 10)) |>
      # addTiles() |>
      setView(lng = -86, lat = 35.51, zoom = 8) |>
      # Teneessee Shape File
      addPolygons(data = m,
                  layerId = ~paste0(county),
                  label = ~county,
                  weight = 0.1,
                  smoothFactor = 0.8,
                  fillOpacity = 0.7,
                  fillColor = ~pal(fill_stat),
                  popup = ~paste0("<b>County</b>: ", county, "<br><b>",
                                  fill_stat_info()$metric_title, "</b>: ", 
                                  format_num_vec(fill_stat), "<br><b>",
                                  stat2_info()$metric_title, "</b>: ", 
                                  format_num_vec(stat2)
                  )
      ) |>
      addEasyButton(
        easyButton(
          position = "topright",
          icon = span(class = "p", HTML("<b>Compare Stats</b>")),
          title = "Show Details / Compare Statistics",
          id = "show-panel",
          onClick = JS("function(btn, map) {Shiny.onInputChange('eb_show_panel', Math.random());}")
        )
      )
  })
  
  # observeEvent(input$fill_stat, {
  #   leafletProxy("map") |>
  #     clearShapes()
  # })

  # Selected leaflet zip handler -----------------------------------------------
  click_county <- reactiveVal()
  click_county("Oklahoma")

  observeEvent(input$map_shape_click, {
    # Capture the county name of the clicked polygon
    click_county(input$map_shape_click$id)
  })

  # Toggle plotly-panel handler ------------------------------------------------
  observeEvent(input$eb_show_panel, {
    toggle('plotly_panel')
  })

  # Comparison plotly ----------------------------------------------------------
  output$plotly <- renderPlotly({
    
    plot_limits <- if_else(input$lims == TRUE, 0, as.numeric(NA))

    # Multiplies values by 100 if they're percentages (between 0 and 1)
    plotly_data <- map_data.react() |>
      mutate(
        fill_stat = if_else(fill_stat > 0 & fill_stat < 1, fill_stat * 100, fill_stat),
        stat2 = if_else(stat2 > 0 & stat2 < 1, stat2 * 100, stat2),
        fill_stat.toggle = if_else(fill_stat.toggle > 0 & fill_stat.toggle < 1, fill_stat.toggle * 100, fill_stat.toggle),
        stat2.toggle = if_else(stat2.toggle > 0 & stat2.toggle < 1, stat2.toggle * 100, stat2.toggle)
      )

    # The ggplot
    p1 <- ggplot(plotly_data,
                 aes(x = fill_stat,
                     y = stat2,
                     group = 1,
                     text = paste0("County: ", county, "\n<b>",
                                   fill_stat_info()$metric_title, "</b>: ", format_num_vec(fill_stat), "<br><b>",
                                   stat2_info()$metric_title, "</b>: ", format_num_vec(stat2)
                     ))) +
      # The normal points
      geom_point() +
      # The highlighted red point
      geom_point(data = plotly_data |>
                   mutate(
                     fill_stat = if_else(fill_stat > 0 & fill_stat < 1, fill_stat * 100, fill_stat),
                     stat2 = if_else(stat2 > 0 & stat2 < 1, stat2 * 100, stat2)
                   ) |>
                   filter(county == click_county()),
                 size = 3,
                 color = "red") +
      # Trendline
      geom_smooth(aes(y = stat2.toggle), method = "lm", color = "red", se = F) +
      scale_x_continuous(labels = scales::comma,
                         limits = c(plot_limits, NA)) +
      scale_y_continuous(labels = scales::comma,
                         limits = c(plot_limits, NA)) +
      labs(
        x = fill_stat_info()$metric_title,
        y = stat2_info()$metric_title
      ) +
      theme_minimal()

    ggplotly(p1, tooltip = "text") |>
      layout(xaxis = list(fixedrange = TRUE),
             yaxis = list(fixedrange = TRUE))
  })

  # PDF download handler -- gets from www file
  output$pdf_download <- downloadHandler(
    filename = function() {
      paste("county-report-", tolower(input$pdf_select), ".pdf", sep = "")
    },
    content = function(file) {
      file.copy(paste0("www/pdfs/kids-count-factsheet-", tolower(input$pdf_select), "-county.pdf"), file)
    }
  )

}
