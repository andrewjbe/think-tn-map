library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(mapview)
library(here)

options(tigris_use_cache = T)

# Shape files ==================================================================
# TN shape files ---------------------------------------------------------------
tn_counties <- tigris::counties(state = "TN",
                                resolution = "500k",
                                cb = FALSE)

# mapview(tn_counties)

write_rds(tn_counties, here("think-tn-map", "data", "tn-counties.rds"))

tn_counties |> 
  select(name = NAME) |>
  st_drop_geometry() |>
  as_tibble() |>
  write_csv(here("think-tn-map", "data", "tn-counties-list.csv"))





# PDFs =========================================================================

# TODO: adapt this

# List of counties
county_list <- read_rds("/home/andrew/Documents/GitHub/kids-count-map/kids-count-map/data/OK-county-shape.RDS") |>
  rename(county = NAME) |>
  pull(var = "county") |>
  sort()


for(county in c(county_list)) {
  # This is necessary to knit each one in a new R session; otherwise latex gets confused
  xfun::Rscript_call(
    rmarkdown::render,
    list(input = "./kids-count-reports/kids-count-pdf.Rmd",
         params = list(given_county = county),
         output_dir = paste0("/home/andrew/Documents/GitHub/kids-count-map/kids-count-reports/finished-pdfs-new/"),
         output_file = paste0("kids-count-factsheet-", tolower(county), "-county.pdf"),
         clean = TRUE,
         quiet = TRUE)
  )
}



