# PG&E residential electricity pricing

A native R analysis of lower volumetric prices and income-sensitive fixed charges, using synthetic billing data from 2015–2023. The report estimates a demand response to the 2019 PG&E price cut and simulates a revenue-neutral 2023 tariff with distributional, surplus, and emissions effects.

**[View the full report](https://usama1208.github.io/retail-electricity-pricing/)**

- [R Markdown source](analysis.Rmd)
- [Full R analysis](analysis.R)
- [Summary memo](RESULTS_SUMMARY.md)
- [Changelog](CHANGELOG.md)
- [Numerical tables](tables/)

**Note:** `bills.csv` (~111MB, 4.32M rows), the monthly billing panel this analysis relies on, is excluded here due to GitHub's 100MB file size limit. `households.csv`, `tracts.csv`, and `weather.csv` are included.

## Reproduce in R

Open this folder in RStudio and knit `analysis.Rmd`, or run from this directory:

```r
source("install_dependencies.R")
```

Then use a terminal:

```bash
Rscript run.R data/ca_electricity_data
```

The four inputs are households.csv, tracts.csv, bills.csv, and weather.csv. Since `bills.csv` isn't included in this repo, reproducing the run requires supplying your own copy in `data/ca_electricity_data/` (or pointing `run.R` / the `data_dir` parameter at another directory containing all four files).

The workflow uses data.table, fixest, ggplot2, rmarkdown, knitr, and jsonlite. Pandoc is required for HTML rendering; `run.R` can locate the RStudio-bundled version on macOS. Packages install into `.R-library` without modifying system libraries. The R Markdown setup automatically loads the project-local library when present. Allow several minutes and several GB of memory for the 4.32-million-row panel.

Every statistical estimate, simulation, table, and figure is produced in R. The command regenerates the HTML, summary, CSV tables, and run metadata. The narrative changelog documents the supplied extract. Source data remain unchanged. The separate profit-neutral variant depends on an assumed private marginal cost; exact revenue equality and profit neutrality are not interchangeable. No totals are expanded to the utility population.
