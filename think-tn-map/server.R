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
library(stringr)
library(mapview)
library(webshot)
options(scipen=999,
        readr.show_col_types = F)

# Sys.setenv("OPENSSL_CONF"="/dev/null") # This is necessary for mapview::mapshot()

# library(reactlog)

format_metric <- function(x, format, diff = FALSE) {
  
  if (is.na(x)) {
    y <- x
  } else if (format == "percent") {
    y <- sprintf("%.1f%%", 100 * x)
  } else if (format == "number") {
    y <- format(round(x, 2), big.mark = ",")
  } else if (format == "dollar" & x < 0) {
    y <- paste0("-$", format(round(-x, 2), big.mark = ","))
  } else if (format == "dollar" & x >= 0) {
    y <- paste0("$", format(round(x, 0), big.mark = ","))
  } else if (format == "per_1") {
    y <- paste0(format(round(x, 2), big.mark = ","), " to 1")
  } else if (format == "per_1k") {
    y <- paste0(format(round(x, 2), big.mark = ","), " per 1k")
  } else if (format == "per_100k") {
    y <- paste0(format(round(x, 2), big.mark = ","), " per 100k")
  } else if (format == "ratio") {
    y <- paste0("1 to ", format(round(x, 2), big.mark = ","))
  } else {
    y <- "ERROR!"
  }
  
  if ((x > 0 | is.na(x)) & diff) {
    y <- paste0("+", y)
  }
  
  return(y)
  
}

format_metric <- format_metric |>
  Vectorize()

# Data =========================================================================
# Main dataset with all metrics ------------------------------------------------
main_ds <- here("data", "data-clean.csv") |>
  read_csv()

# Dataset with info / details on all metrics -----------------------------------
info_ds <- here("data", "data-info.csv") |>
  read_csv() |>
  clean_names()

# Dataset with supplemental numbers --------------------------------------------
supplemental_ds <- here("data", "data-supplements.csv") |>
  read_csv() |>
  clean_names()

# Variable names 
var_names <- info_ds |> 
  select(variable, metric_title)

# County shapefile -------------------------------------------------------------
# county_shape <- read_rds(here("think-tn-map", "data", "tn-counties.rds")) |>
county_shape <- read_rds(here("data", "tn-counties.rds")) |>
  clean_names() |>
  rename(county = name) |>
  st_transform(4326) 
  
# Server logic =================================================================
function(input, output, session) {
  
  # reactlog_enable() # For debugging
  
  # Reactive variables / dataframes ============================================
  # Mapping Data reactive dataframe --------------------------------------------
  map_data.react <- reactive({
    
    validate(need(input$fill_stat, label = "fill stat"))

    # If you're using a stat2...
    if(input$add_stat2){
      z <- main_ds |>
        select(county,
               fill_stat = input$fill_stat,
               fill_stat_rank = paste0(input$fill_stat, "_rank"),
               stat2 = input$stat2,
               stat2_rank = paste0(input$stat2, "_rank")
        ) |>
        filter(county %in% county_shape$county)
      
    # If you're not (default)...
    } else {
      z <- main_ds |>
        select(county,
               fill_stat = input$fill_stat,
               fill_stat_rank = paste0(input$fill_stat, "_rank"),
        ) |>
        filter(county %in% county_shape$county)
    }
    
    return(z)
    
  })
  
  # Reactive values with currently selected fill stat info ---------------------
  fill_stat_info <- reactive({
    
    validate(need(input$fill_stat, message = F))
    
    info_ds |>
      filter(variable == input$fill_stat)
  })
  
  stat2_info <- reactive({
    
    validate(need(input$add_stat2, message = F))
    
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
                selected = "gdp_growth",
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
    
    if(!input$add_stat2){
      disabled(
        selectInput(inputId = "stat2",
                    label = "Comparison Metric",
                    choices = choices_grouped_2,
                    selected = "poverty_rate_child",
                    multiple = FALSE,
                    selectize = TRUE)
      )
    } else {
      selectInput(inputId = "stat2",
                  label = "Comparison Metric",
                  choices = choices_grouped_2,
                  selected = "voter_registration_2020",
                  multiple = FALSE,
                  selectize = TRUE)
    }
    
  }) 
  
  # Grey out these UI elements when they're inaccessible
  observeEvent(input$add_stat2, {
    if(input$add_stat2 == TRUE){
      shinyjs::enable("stat2")
      shinyjs::enable("stat2_cat")
    } else {
      shinyjs::disable("stat2")
      shinyjs::disable("stat2_cat")
    }
  })
  
  # Leaflet Map ================================================================
  map_reactive <- reactive({

    # Map geometry -------------------------------------------------------------
    m <- left_join(county_shape, map_data.react(), by = "county")
    
    # Add fill stat supplemental number if needed...
    if (fill_stat_info()$supplemental_number == "Y") {
      fill_stat_supplemental_numbers <- supplemental_ds |>
        select(fill_stat_supp = paste0(input$fill_stat, "_sup"),
               county) |>
        mutate(fill_stat_supp = paste0(" (", format(fill_stat_supp, big.mark = ",", trim = TRUE), ")"))
          
      m <- m |>
        left_join(fill_stat_supplemental_numbers, by = "county") 
    } else {
      m <- m |>
        mutate(fill_stat_supp = "")
    }
    
    # Also add stat2 supplemental number if needed...
    if (input$add_stat2) {
      if (stat2_info()$supplemental_number == "Y") {
        stat2_supplemental_numbers <- supplemental_ds |>
          select(stat2_supp = paste0(input$stat2, "_sup"),
                 county) |>
          mutate(stat2_supp = paste0(" (", format(stat2_supp, big.mark = ",", trim = TRUE), ")"))
        
        m <- m |>
          left_join(stat2_supplemental_numbers, by = "county") 
      } else {
        m <- m |>
          mutate(stat2_supp = "")
      }
    }
    
    # Generates map color scale based on selected stat -------------------------
    # This part is very hack-y because I'm trying to address very specific requests
    # related to how the scales look. There's definitely a better way to do this, sorry!
    # Pick the colors for the palette
    if(input$fill_stat %in% c("new_home_sales")){
      pal_colors <- "YlGn"
    } else if (input$fill_stat %in% c("employment_growth", "renewable_energy_infr")) {
      pal_colors <- c("#F46D43", "#D9EF8B", "#A6D96A", "#66BD63", "#1A9850")
    } else {
      pal_colors <- "RdYlGn"
    }
    
    na_color <- "gray"

    
    # These couple of metrics look better as numeric scales...
    if(input$fill_stat %in% c("renter_growth_black", "renter_growth_hispanic",
                              "nonprofit_giving_per_captia",
                              "provisional_ballots_rejected", "avg_home_price",
                              "mental_healthcare_provider_ratio")) {
      pal <- colorNumeric(
        palette = pal_colors,
        reverse = if_else(fill_stat_info()$good_outcomes == 1, FALSE, TRUE),
        domain = map_data.react()$fill_stat,
        na.color = na_color
      )
    # We want this one to have zero as one color, and all others binned...
    } else if (input$fill_stat %in% c("renewable_energy_infr")){
      pal <- colorBin(
        palette = pal_colors,
        domain = map_data.react()$fill_stat,
        bins = c(0, 25, 50, 75, 100, 125, 150, 300, 1800),
        na.color = na_color,
        reverse = if_else(fill_stat_info()$good_outcomes == 1, FALSE, TRUE)
      )
      # We want this one to bin all values above 95
    } else if (input$fill_stat %in% c("voter_registration_2022", "voter_registration_2020")){
      # labels <- c("50%", "55%", "60%", "65%", "70%", "75%", "80%", "85%", "90%", "95%+")
      pal <- colorBin(
        palette = pal_colors,
        domain = map_data.react()$fill_stat,
        bins = c(.5, .55, .6, .65, .7, .75, .8, .85, .9, .95, 1.2),
        na.color = na_color,
        reverse = if_else(fill_stat_info()$good_outcomes == 1, FALSE, TRUE)
      )
    } else {
      # ...but for most of the rest, use a binned scale 
      n_bins <- 9
      
      # Manual adjustments by request
      if (input$fill_stat %in% c("post_hs_attainment", "employment_growth", "gdp_growth",
                                 "disconnected_youth")) {
        n_bins <- 5
      }

      pal <- colorBin(
        palette = pal_colors,
        reverse = if_else(fill_stat_info()$good_outcomes == 1, FALSE, TRUE),
        domain = map_data.react()$fill_stat,
        na.color = na_color,
        bins = n_bins,
        pretty = TRUE
      )
    }
    
    # Generate the map popups --------------------------------------------------
    generate_popup <- function(county, 
                               fill_stat = NA, stat2 = NA, 
                               fill_stat_rank = NA, stat2_rank = NA, 
                               fill_stat_supp = NA, stat2_supp = NA,
                               include_stat2 = FALSE) {

      if(include_stat2) {
        paste0("<b>County</b>: ", county, "<br><b>",
               fill_stat_info()$metric_title, "</b>: ", 
               format_metric(fill_stat,
                             format = fill_stat_info()$format), fill_stat_supp,
               "<br>", 
               "<b>Rank:</b> #", fill_stat_rank, "<br>",
               "<b>Avg. TN county:</b> ", fill_stat_info()$average_tn_county, 
               "<hr><b>",
               stat2_info()$metric_title, "</b>: ",
               format_metric(stat2,
                             format = stat2_info()$format), stat2_supp,
               "<br>",
               "<b>Rank:</b> #", stat2_rank, "<br>",
               "<b>Avg. TN county:</b> ", stat2_info()$average_tn_county
        )
      } else {
        paste0("<b>County</b>: ", county, "<br><b>",
               fill_stat_info()$metric_title, "</b>: ",
               format_metric(fill_stat,
                             format = fill_stat_info()$format), fill_stat_supp,
               "<br>",
               "<b>Rank:</b> #", fill_stat_rank, "<br>",
               "<b>Avg. TN county:</b> ", fill_stat_info()$average_tn_county
        )
      }

    }
    
    # This creates the label for the legend.
    get_label_format <- function(format) {
      # They were very particular about this one so it needs a lot of special stuff
      if (input$fill_stat %in% c("renewable_energy_infr")) {
        return(labelFormat(transform = function(x) round(x, 1),
                           suffix = " MW", ))
      } else if (format == "percent") {
        return(labelFormat(suffix = "%", between = "% — ", transform = function(x) round(100 * x, 1)))
      } else if (format == "number") {
        return(labelFormat(transform = function(x) round(x, 2)))
      } else if (format == "dollar") {
        return(labelFormat(prefix = "$", between = " — $", transform = function(x) round(x, 2)))
      } else if (format == "per_1") {
        return(labelFormat(suffix = " to 1", transform = function(x) round(x, 2)))
      } else if (format == "per_1k") {
        return(labelFormat(suffix = " per 1k", transform = function(x) round(x, 2)))
      } else if (format == "per_100k") {
        return(labelFormat(suffix = " per 100k", transform = function(x) round(x, 2)))
      } else if (format == "ratio") {
        return(labelFormat(prefix = "1 to ", transform = function(x) round(x, 2)))
      } else {
        stop("Error!")
      }
    }
    

    # The map itself
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     zoomSnap = 0.1,
                                     zoomDelta = 0.3,
                                     minZoom = 7, 
                                     maxZoom = 10)) |>
      addPolygons(data = m,
                  layerId = ~paste0(county),
                  label = ~county,
                  weight = 0.5,
                  color = "white",
                  smoothFactor = 0.8,
                  fillOpacity = 0.7,
                  fillColor = ~pal(fill_stat),
                  popup = ~generate_popup(county = county,
                                          fill_stat = fill_stat,
                                          stat2 = stat2,
                                          fill_stat_rank = fill_stat_rank,
                                          stat2_rank = stat2_rank,
                                          include_stat2 = input$add_stat2,
                                          fill_stat_supp = fill_stat_supp,
                                          stat2_supp = stat2_supp)
      ) |>
      # Add javascript button for comparison stat
      addEasyButton(
        easyButton(
          position = "topright",
          icon = span(class = "p", HTML("<b>Compare Stats</b>")),
          id = "show-panel",
          onClick = JS("function(btn, map) {Shiny.onInputChange('eb_show_panel', Math.random());}")
        )
      ) |>
      # Add the color scale legend, uses the get_label_format function above
      addLegend(
        data = m,
        position = "bottomleft",
        pal = pal,
        values = ~fill_stat,
        title = fill_stat_info()$metric_title,
        opacity = 0.7,
        labFormat = get_label_format(format = fill_stat_info()$format)
      ) |>
      # Have to add the zoom back in, for some reason you can't control the position otherwise
      onRender(
        "function(el, x) {
          L.control.zoom({position:'topright'}).addTo(this);
        }")
    
  })
  
  # Render the map in the Shiny application
  output$map <- renderLeaflet({
    map <- map_reactive() |>
      setView(lng = -86, lat = 36, zoom = 7) |>
      # Title
      addControl(
        div(
          style = "max-width: 40vw",
          h2(paste0(
            fill_stat_info()$metric_title
          ))
        ),
        position = "topleft"
      ) |>
      # Source / description text
      addControl(
        div(style = "max-width: 50vw",
            HTML(paste0(
              "<b>", fill_stat_info()$metric_title, ": </b>",
              fill_stat_info()$description, "<br>",
              "<b>Source: </b>",
              fill_stat_info()$source,
              " (", 
              fill_stat_info()$years, 
              ")"
            ))
        ),
        position = "topleft"
      )
    
  })
  
  # Observe the show_labels input and add/remove labels accordingly
  observe({
    if (input$show_labels & !is.null(input$fill_stat)) {
      leafletProxy("map") |>
        addLabelOnlyMarkers(
          data = suppressWarnings(st_centroid(county_shape, of_largest_polygon = TRUE)),
          label = ~county,
          group = "label",
          labelOptions = labelOptions(
            noHide = TRUE,
            style = c("padding" = "0", "font-weight" = "bold"),
            direction = "center",
            textOnly = TRUE
          )
        )
    } else {
      leafletProxy("map") |>
        clearGroup("label")
    }
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
             message = F),
             need(input$fill_stat, message = F),
             need(input$stat2, message = F)
             )
    
    plot_limits <- if_else(input$lims == TRUE, 0, as.numeric(NA))

    plotly_data <- map_data.react()

    # The ggplot
    p1 <- ggplot(plotly_data,
                 aes(x = stat2,
                     y = fill_stat,
                     group = 1,
                     text = paste0("County: ", county, "\n<b>",
                                   fill_stat_info()$metric_title, "</b>: ", 
                                   format_metric(fill_stat,
                                                 format = fill_stat_info()$format), "<br><b>",
                                   stat2_info()$metric_title, "</b>: ", 
                                   format_metric(stat2,
                                                 format = stat2_info()$format)
                     )
                 )) +
      # The normal points
      geom_point() +
      # The highlighted red point
      geom_point(data = plotly_data |>
                   filter(county == click_county()),
                 size = 3,
                 color = "red") +
      scale_x_continuous(labels = scales::comma,
                         limits = c(plot_limits, NA)) +
      scale_y_continuous(labels = scales::comma,
                         limits = c(plot_limits, NA)) +
      labs(
        x = stat2_info()$metric_title,
        y = fill_stat_info()$metric_title
      ) +
      theme_minimal()
    
    if(input$toggle) {
      # Trendline
      p1 <- p1 + 
        geom_smooth(method = "lm", color = "red", se = F)
    }

    ggplotly(p1, tooltip = "text") |>
      layout(xaxis = list(fixedrange = TRUE),
             yaxis = list(fixedrange = TRUE))
  })

  # Download handlers ==========================================================
  # PDF download handlers -- gets from docs file
  output$pdf_download_all_counties <- downloadHandler(
    filename = function() {
      paste("Think TN Data Map single metric report - ", tolower(fill_stat_info()$metric_title), ".pdf", sep = "")
    },
    content = function(file) {
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
  
  # # Handler for downloading the map snapshots ----------------------------------
  # output$snapshot_download <- downloadHandler(
  #   filename = function() {
  #     paste("Think TN Data Map snapshot - ", fill_stat_info()$metric_title, ".png", sep = "")
  #   },
  #   content = function(file) {
  #     shiny::showNotification(paste0("Saving snapshot of current map..."),
  #                             duration = 10, type = "message")
  #     
  #     z <- map_reactive() |>
  #       setView(lng = -86, lat = 36.2, zoom = 2) |>
  #       # Title
  #       addControl(
  #         div(
  #           style = "max-width: 500px; font-size: large; font-weight: bold;",
  #           fill_stat_info()$metric_title
  #         ),
  #         position = "topleft"
  #       ) |>
  #       # Source
  #       addControl(
  #         div(style = "max-width: 400px; font-size: x-small;",
  #             HTML(paste0(
  #               "<b>", fill_stat_info()$metric_title, ": </b>",
  #               fill_stat_info()$description, "<br>",
  #               "<b>Source: </b>",
  #               fill_stat_info()$source,
  #               " (", 
  #               fill_stat_info()$years, 
  #               ")"
  #             ))
  #         ),
  #         position = "topleft"
  #       )
  #     
  #     if(input$show_labels) {
  #       z <- z |>
  #         addLabelOnlyMarkers(
  #           data = st_centroid(county_shape),
  #           label = ~county,
  #           group = "label",
  #           labelOptions = labelOptions(
  #             noHide = TRUE,
  #             style = c("padding" = "0", "font-weight" = "bold"),
  #             direction = "center",
  #             textOnly = TRUE
  #           )
  #         )
  #     }
  #     
  #     mapview::mapshot(
  #       z,
  #       file = file,
  #       remove_controls = c("easyButton"),
  #       # vwidth = 1600,
  #       # vheight = 1000
  #       vwidth = 800,
  #       vheight = 500
  #     )
  #     
  #   }
  # )

}

