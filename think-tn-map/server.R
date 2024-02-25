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
library(shinyjs)
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
main_ds <- here("data", "data-clean.csv") |>
  read_csv()

# Dataset with info / details on all metrics -----------------------------------
info_ds <- here("data", "data-info.csv") |>
  read_csv() |>
  clean_names()

# Variable names 
var_names <- info_ds |> 
  select(variable, metric_title)

# County shapefile -------------------------------------------------------------
# county_shape <- read_rds(here("think-tn-map", "data", "tn-counties.rds")) |>
county_shape <- read_rds(here("data", "tn-counties.rds")) |>
  clean_names() |>
  rename(county = name)
  
# Server logic =================================================================
function(input, output, session) {
  
  # reactlog_enable()
  
  # Reactive variables / dataframes ============================================
  # Mapping Data reactive dataframe --------------------------------------------
  map_data.react <- reactive({

    # If you're using a stat2...
    if(input$add_stat2){
      z <- main_ds |>
        select(county,
               fill_stat = input$fill_stat,
               fill_stat_rank = paste0(input$fill_stat, "_rank"),
               stat2 = input$stat2,
               stat2_rank = paste0(input$stat2, "_rank")
        ) |>
        mutate(
          fill_stat.toggle = as.numeric(NA),
          stat2.toggle = as.numeric(NA)
        ) |>
        filter(county %in% county_shape$county)
      
      if(input$toggle){
        z <- z |>
          mutate(
            fill_stat.toggle = as.numeric(fill_stat),
            stat2.toggle = as.numeric(stat2)
          )
      }
      
    # If you're not (default)...
    } else {
      z <- main_ds |>
        select(county,
               fill_stat = input$fill_stat,
               fill_stat_rank = paste0(input$fill_stat, "_rank"),
        ) |>
        mutate(
          fill_stat.toggle = as.numeric(NA),
        ) |>
        filter(county %in% county_shape$county)
      
      if(input$toggle){
        z <- z |>
          mutate(
            fill_stat.toggle = as.numeric(fill_stat)
          )
      }
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
    
    validate(need(input$fill_stat, message = F), 
             need(input$fill_stat_cat, message = F))
    
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
    
    validate(need(input$stat2, message = F), 
             need(input$stat2_cat, message = F))
    
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

  # UI elements ================================================================
  # Metric ---------------------------------------------------------------------
  output$fill_stat_ui <- renderUI({
    choices_grouped <- info_ds |>
      filter(category == input$fill_stat_cat) |>
      pull(var = variable)
    
    names(choices_grouped) <- info_ds |>
      filter(category == input$fill_stat_cat) |>
      pull(var = metric_title)
    
    selectInput(inputId = "fill_stat",
                label = "Map Metric:",
                choices = choices_grouped,
                selected = "child_food_insecurity",
                multiple = FALSE,
                selectize = TRUE)
  })
  

  # stat2 metric ---------------------------------------------------------------
  output$stat2_ui <- renderUI({
    choices_grouped_2 <- info_ds |>
      filter(category == input$stat2_cat) |>
      pull(var = variable)
    
    names(choices_grouped_2) <- info_ds |>
      filter(category == input$stat2_cat) |>
      pull(var = metric_title)
    
    disabled(
    selectInput(inputId = "stat2",
                label = "Comparison Metric",
                choices = choices_grouped_2,
                selected = "poverty_rate_child",
                multiple = FALSE,
                selectize = TRUE)
    )
    
  })
  
  observe({
    if(input$add_stat2){
      shinyjs::enable("stat2")
      shinyjs::enable("stat2_cat")
    } else {
      shinyjs::disable("stat2")
      shinyjs::disable("stat2_cat")
    }
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
      palette = "RdYlGn",
      # reverse = if_else(info_ds[which(info_ds$variable == input$fill_stat),] |>
      #                     pull(var = "good_outcomes") == "low", FALSE, TRUE),
      domain = map_data.react()$fill_stat,
      na.color = "grey",
      bins = 6,
      pretty = TRUE
    )
    
    # The map popups
    generate_popup <- function(county, fill_stat = NA, stat2 = NA, fill_stat_rank = NA, stat2_rank = NA,
                               include_stat2 = FALSE) {

      if(include_stat2) {
        paste0("<b>County</b>: ", county, "<br><b>",
               fill_stat_info()$metric_title, "</b>: ", 
               format_num_vec(fill_stat), 
               " (#", fill_stat_rank, ")<br>",
               "<b>Avg. TN county:</b> ", fill_stat_info()$average_tn_county, 
               "<hr><b>",
               stat2_info()$metric_title, "</b>: ",
               format_num_vec(stat2),
               " (#", stat2_rank, ")<br>",
               "<b>Avg. TN county:</b> ", stat2_info()$average_tn_county
        )
      } else {
        paste0("<b>County</b>: ", county, "<br><b>",
               fill_stat_info()$metric_title, "</b>: ",
               format_num_vec(fill_stat),
               " (#", fill_stat_rank, ")<br>",
               "<b>Avg. TN county:</b> ", fill_stat_info()$average_tn_county
        )
      }

    }

    # The map itself
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     zoomSnap = 0.1,
                                     zoomDelta = 0.1,
                                     minZoom = 7, 
                                     maxZoom = 10)) |>
      # addTiles() |>
      setView(lng = -86, lat = 35.51, zoom = 7.8) |>
      addPolygons(data = m,
                  layerId = ~paste0(county),
                  label = ~county,
                  weight = 0.1,
                  smoothFactor = 0.8,
                  fillOpacity = 0.7,
                  fillColor = ~pal(fill_stat),
                  popup = ~generate_popup(county = county,
                                          fill_stat = fill_stat,
                                          stat2 = stat2,
                                          fill_stat_rank = fill_stat_rank,
                                          stat2_rank = stat2_rank,
                                          include_stat2 = input$add_stat2)
      ) |>
      addEasyButton(
        easyButton(
          position = "topright",
          icon = span(class = "p", HTML("<b>Compare Stats</b>")),
          id = "show-panel",
          onClick = JS("function(btn, map) {Shiny.onInputChange('eb_show_panel', Math.random());}")
        )
      ) |>
      onRender(
        "function(el, x) {
          L.control.zoom({position:'bottomleft'}).addTo(this);
        }") |>
      addControl(
        h2(paste0(
          fill_stat_info()$metric_title
        )),
        position = "topleft"
      )
    
  })
  

  # Selected leaflet zip handler -----------------------------------------------
  click_county <- reactiveVal()
  click_county("Shelby")

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
    
    validate(need(input$add_stat2 == TRUE,
             message = F)
             )
    
    plot_limits <- if_else(input$lims == TRUE, 0, as.numeric(NA))

    plotly_data <- map_data.react()

    # The ggplot
    p1 <- ggplot(plotly_data,
                 aes(x = stat2,
                     y = fill_stat,
                     group = 1,
                     text = paste0("County: ", county, "\n<b>",
                                   fill_stat_info()$metric_title, "</b>: ", format_num_vec(fill_stat), "<br><b>",
                                   stat2_info()$metric_title, "</b>: ", format_num_vec(stat2)
                     ))) +
      # The normal points
      geom_point() +
      # The highlighted red point
      geom_point(data = plotly_data |>
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
        x = stat2_info()$metric_title,
        y = fill_stat_info()$metric_title
      ) +
      theme_minimal()

    ggplotly(p1, tooltip = "text") |>
      layout(xaxis = list(fixedrange = TRUE),
             yaxis = list(fixedrange = TRUE))
  })

  # PDF download handler -- gets from www file
  output$pdf_download_all_counties <- downloadHandler(
    filename = function() {
      paste("Think TN Data Map single metric report - ", tolower(fill_stat_info()$metric_title), ".pdf", sep = "")
    },
    content = function(file) {
      # print(paste0("docs/one-metric-all-counties/one-metric-all-counties-", fill_stat_info()$metric_title, ".pdf"))
      file.copy(paste0("docs/one-metric-all-counties/one-metric-all-counties-", 
                       fill_stat_info()$metric_title, ".pdf"),
                file)
    }
  )
  
  output$pdf_download_county_summary <- downloadHandler(
    filename = function() {
      paste("Think TN Data Map ", input$pdf_select_county, " County Summary.pdf", sep = "")
    },
    content = function(file) {
      file.copy(paste0("docs/county-summary/county-summary-", 
                       input$pdf_select_county, ".pdf"), file)
    }
  )
  
  output$pdf_download_all_metrics <- downloadHandler(
    filename = function() {
      paste("Think TN Data Map all metrics report - ", input$pdf_select_county, " County.pdf", sep = "")
    },
    content = function(file) {
      file.copy(paste0("docs/one-county-all-metrics/one-county-all-metrics-", 
                       input$pdf_select_county, ".pdf"), file)
    }
  )

}
