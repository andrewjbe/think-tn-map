# Think*Tenessee* State of the Counties Dashboard

## Repository Structure

This repo consists of the following directories:

* `/data/` is a backup of all the data sources used throughout. It is not directly referenced by the Shiny app, or the PDF templates; it just exists as a store of files for convenience.
* `/inst/` contains the PDF report templates, plus the `resources.R` script that I used to generate the final PDFs using the templates.
   * Each subdirectory (`county-summary-pdf/`, `one-county-all-metrics/`, etc.) contains the Quarto report template (the `.qmd` file), the data used therein (`/data/`), as well as some aesthetic / theming resources (`/www/`, etc.)
* `/renv/` and `renv.lock` are used by the `{renv}` package for R to maintain a consistent development environment.
* `/think-tn-map/` is the Shiny application itself.
   * `server.R` is the "behind the scenes" server logic of the Shiny app.
   * `ui.R` is the user interface of the Shiny app.
   * `/data/` contains copies of the source data.
   * `/www/` contains aesthetic / themeing files for the map site.
* `.Rprofile`, `.gitignore`, and `think-tn-map.Rproj` are miscellaneous R / git helper files.
* `LICENSE` has the license for the software (it's using the GNU General Public License, which is a very permissive open source license)
* `README.md` is this file.
