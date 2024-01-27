#' Think TN metric map
#' By: Andrew Bell
#'
#' On behalf of: Market Retrievers Consulting / Think Tennessee

library(shiny)
library(readr)

function(input, output, session) {

  # # Mapping Data reactive df
  # map_data.react <- reactive({
  #   z <- ds |>
  #     select(county = 'county',
  #            fill_stat = input$fill_stat,
  #            stat2 = input$stat2
  #     ) |>
  #     mutate(fill_stat.toggle = as.numeric(NA),
  #            stat2.toggle = as.numeric(NA)) |>
  #     filter(county %in% county_shape$county)
  #   
  #   if(input$toggle){
  #     z <- z |>
  #       mutate(
  #         fill_stat.toggle = as.numeric(fill_stat),
  #         stat2.toggle = as.numeric(stat2)
  #       )
  #   }
  #   return(z)
  # })
  # 
  # # Reactive variable w/ selected nice names, for convenience
  # fill_stat_nice_name.react <- reactive({var_names[which(var_names$variable == input$fill_stat),] |>
  #     pull(var = nice_name)})
  # 
  # stat2_nice_name.react <- reactive({var_names[which(var_names$variable == input$stat2),] |>
  #     pull(var = nice_name)})
  # 
  # # Toggles ggplot axes to start at zero; hooked up to input$lims
  # lims.react <- reactive({if_else(input$lims == TRUE, 0, as.numeric(NA))})
  # 
  # # Nicely formatted reactive description of fill stat
  # output$description_text_fill <- renderUI({
  #   str <- paste0(
  #     "<b>", fill_stat_nice_name.react(), ":</b> ",
  #     var_info[which(var_info$variable == input$fill_stat),] |> pull(var = "description"),
  #     "<br><b>Source: </b>", var_info[which(var_info$variable == input$fill_stat),] |> pull(var = "source"),
  #     " (", var_info[which(var_info$variable == input$fill_stat),] |> pull(var = "years"), ")"
  #   )
  #   HTML(str)
  # })
  # 
  # # Nicely formatted reactive description of stat2
  # output$description_text_stat2 <- renderUI({
  #   str <- paste0(
  #     "<b>", stat2_nice_name.react(), ":</b> ",
  #     var_info[which(var_info$variable == input$stat2),] |> pull(var = "description"),
  #     "<br><b>Source: </b>", var_info[which(var_info$variable == input$fill_stat),] |> pull(var = "source"),
  #     " (", var_info[which(var_info$variable == input$fill_stat),] |> pull(var = "years"), ")"
  #   )
  #   HTML(str)
  # })
  # 
  # # Leaflet Map -------------
  # output$map <- renderLeaflet({
  #   
  #   # Map geometry
  #   m <- left_join(county_shape, map_data.react(), by = "county") |>
  #     st_transform(4326)
  #   
  #   # Generates map color scale based on selected stat
  #   pal <- colorBin(
  #     palette = var_info[which(var_info$variable == input$fill_stat),] |>
  #       pull(var = "color_scheme"),
  #     reverse = if_else(var_info[which(var_info$variable == input$fill_stat),] |>
  #                         pull(var = "good_outcomes") == "low", FALSE, TRUE),
  #     domain = map_data.react()$fill_stat,
  #     na.color = "grey",
  #     bins = 6,
  #     pretty = TRUE
  #   )
  #   
  #   # The map itself
  #   leaflet(options = leafletOptions(minZoom = 7)) |>
  #     addTiles() |>
  #     setView(lng = -97.0, lat = 35.5, zoom = 7) |>
  #     # OKC Shape File
  #     addPolygons(data = m,
  #                 layerId = ~paste0(county),
  #                 label = ~county,
  #                 weight = 0.1,
  #                 smoothFactor = 0.8,
  #                 fillOpacity = 0.7,
  #                 fillColor = ~pal(fill_stat),
  #                 popup = ~paste0("<b>County</b>: ", county, "<br><b>",
  #                                 fill_stat_nice_name.react(), "</b>: ", format_num_vec(fill_stat), "<br><b>",
  #                                 stat2_nice_name.react(), "</b>: ", format_num_vec(stat2)
  #                 )
  #     ) |>
  #     addEasyButton(easyButton(
  #       position = "topright",
  #       icon = span(class = "p", HTML("<b>Compare Stats</b>")),
  #       title = "Show Details / Compare Statistics",
  #       id = "show-panel",
  #       onClick = JS("function(btn, map) {Shiny.onInputChange('eb_show_panel', Math.random());}")
  #     ))
  # })
  # 
  # # Selected leaflet zip handler ===============
  # click_county <- reactiveVal()
  # click_county("Oklahoma")
  # 
  # observeEvent(input$map_shape_click, {
  #   # Capture the county name of the clicked polygon
  #   click_county(input$map_shape_click$id)
  # })
  # 
  # # Toggle plotly-panel handler ===============
  # observeEvent(input$eb_show_panel, {
  #   toggle('plotly_panel')
  # })
  # 
  # # DT Datatable ===================
  # output$table <- renderDataTable({
  #   d <- map_data.react() |>
  #     mutate(
  #       fill_stat = format_num_vec(fill_stat),
  #       stat2 = format_num_vec(stat2),
  #     ) |>
  #     select(county, fill_stat, stat2) #, total_pop)
  #   
  #   DT::datatable(
  #     data = d,
  #     rownames = F,
  #     options = list(pageLength = 5),
  #     colnames = c(
  #       "County",
  #       fill_stat_nice_name.react(),
  #       stat2_nice_name.react()
  #     )
  #   )
  #   
  # })
  # 
  # # Comparison plotly -----
  # output$plotly <- renderPlotly({
  #   
  #   # Multiplies values by 100 if they're percentages (between 0 and 1)
  #   plotly_data <- map_data.react() |>
  #     mutate(
  #       fill_stat = if_else(fill_stat > 0 & fill_stat < 1, fill_stat * 100, fill_stat),
  #       stat2 = if_else(stat2 > 0 & stat2 < 1, stat2 * 100, stat2),
  #       fill_stat.toggle = if_else(fill_stat.toggle > 0 & fill_stat.toggle < 1, fill_stat.toggle * 100, fill_stat.toggle),
  #       stat2.toggle = if_else(stat2.toggle > 0 & stat2.toggle < 1, stat2.toggle * 100, stat2.toggle)
  #     )
  #   
  #   # The ggplot
  #   p1 <- ggplot(plotly_data,
  #                aes(x = fill_stat,
  #                    y = stat2,
  #                    group = 1,
  #                    text = paste0("County: ", county, "\n",
  #                                  fill_stat_nice_name.react(), "</b>: ", format_num_vec(fill_stat), "<br><b>",
  #                                  stat2_nice_name.react(), "</b>: ", format_num_vec(stat2)
  #                    ))) +
  #     # The normal points
  #     geom_point() +
  #     # The highlighted red point
  #     geom_point(data = plotly_data |>
  #                  mutate(
  #                    fill_stat = if_else(fill_stat > 0 & fill_stat < 1, fill_stat * 100, fill_stat),
  #                    stat2 = if_else(stat2 > 0 & stat2 < 1, stat2 * 100, stat2)
  #                  ) |>
  #                  filter(county == click_county()),
  #                size = 3,
  #                color = "red") +
  #     # Trendline
  #     geom_smooth(aes(y = stat2.toggle), method = "lm", color = "red", se = F) +
  #     scale_x_continuous(labels = scales::comma,
  #                        limits = c(lims.react(), NA)) +
  #     scale_y_continuous(labels = scales::comma,
  #                        limits = c(lims.react(), NA)) +
  #     labs(x = fill_stat_nice_name.react(),
  #          y = stat2_nice_name.react()) +
  #     theme_minimal()
  #   
  #   ggplotly(p1, tooltip = "text") |>
  #     layout(xaxis = list(fixedrange = TRUE),
  #            yaxis = list(fixedrange = TRUE))
  # })
  # 
  # # PDF download handler -- gets from www file, might want to grab from Google Cloud
  # output$pdf_download <- downloadHandler(
  #   filename = function() {
  #     paste("county-report-", tolower(input$pdf_select), ".pdf", sep = "")
  #   },
  #   content = function(file) {
  #     file.copy(paste0("www/pdfs/kids-count-factsheet-", tolower(input$pdf_select), "-county.pdf"), file)
  #   }
  # )

}
