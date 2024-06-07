library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(mapview)
library(here)

options(tigris_use_cache = T)

# Shape files ==================================================================
# TN shape files ---------------------------------------------------------------
# tn_counties <- tigris::counties(state = "TN",
#                                 resolution = "500k",
#                                 cb = FALSE)
# 
# # mapview(tn_counties)
# 
# write_rds(tn_counties, here("think-tn-map", "data", "tn-counties.rds"))
# 
# tn_counties |> 
#   select(name = NAME) |>
#   st_drop_geometry() |>
#   as_tibble() |>
#   write_csv(here("think-tn-map", "data", "tn-counties-list.csv"))

tictoc::tic()

# PDFs =========================================================================

main_ds <- here("data", "data-clean.csv") |>
  read_csv()

ds_info <- here("data", "data-info.csv") |>
  read_csv() |>
  janitor::clean_names()

# List of counties
county_list <- main_ds$county


for(county in c(county_list)) {
  
  # County Summaries -----------------------------------------------------------
  cli::cli_alert("Rendering report...")
  
  # system(
  #   paste0("quarto render docs/county-summary-pdf/county-summary-pdf.qmd ",
  #          "-P county:", county)
  # )
  # 
  # cli::cli_alert("Report rendered successfully! Saving as PDF...")
  # 
  # pagedown::chrome_print(here("docs", "county-summary-pdf", "county-summary-pdf.html"),
  #                        here("think-tn-map", "docs", "county-summary", 
  #                             paste0("county-summary-", county, ".pdf")),
  #                        options = c(pageRanges = "1-5"),
  #                        format = "pdf"
  # )
  
  cli::cli_alert_success(paste0(county, " County summary .pdf saved successfully!"))
  
  # County all metrics ---------------------------------------------------------
  cli::cli_alert("Rendering report...")
  
  system(
    paste0("quarto render docs/one-county-all-metrics/one-county-all-metrics.qmd ",
           "-P county:", county)
  )
  
  cli::cli_alert("Report rendered successfully! Saving as PDF...")
  
  pagedown::chrome_print(here("docs", "one-county-all-metrics", "one-county-all-metrics.html"),
                         here("think-tn-map", "docs", "one-county-all-metrics", 
                              paste0("one-county-all-metrics-", county, ".pdf")),
                         format = "pdf"
  )
  
  cli::cli_alert_success(paste0(county, " County 'All Metrics' .pdf saved successfully!"))
  
  
}

# Then the metrics one
metric_list <- ds_info$metric_title

for(metric in c(metric_list)) {
  
  cli::cli_alert("Rendering report...")
  
  system(
    paste0("quarto render docs/one-metric-all-counties/one-metric-all-counties.qmd ",
           "-P metric:'", metric, "'")
  )
  
  cli::cli_alert("Report rendered successfully! Saving as PDF...")
  
  pagedown::chrome_print(here("docs", "one-metric-all-counties", "one-metric-all-counties.html"),
                         here("think-tn-map", "docs", "one-metric-all-counties", 
                              paste0(
                                "one-metric-all-counties-",
                                str_replace_all(metric, "/", ""), 
                                ".pdf")),
                         format = "pdf"
  )
  
  cli::cli_alert_success(paste0(metric, " 'All Counties' .pdf saved successfully!"))
  
}

tictoc::toc()
