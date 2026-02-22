# Advanced-Statistical-Modeling
Mentored Advanced Projects ([MAP](https://www.grinnell.edu/academics/experience/research/map)) Spring 2026

## Authors

- [Khanh Do](https://github.com/khanhdo05)
- [Joyce Gill](https://github.com/joycegill)
- [Professor Shonda Kuiper](https://en.wikipedia.org/wiki/Shonda_Kuiper)

---

## Objective

The goal of this project is to explore U.S. higher education data using government sources, such as **IPEDS** and **College Scorecard**, and to build **advanced statistical models** that provide new insights into college and university performance, enrollment trends, and rankings. 

## Key Links

Code Book [Private Link](https://docs.google.com/spreadsheets/d/15nn2SjonRKGXaUnBLJxZs6E04GZDL0sNgCyN5-q8Vig/edit?gid=0#gid=0)

Shared Document [Private Link](https://docs.google.com/document/d/1MQ_aKizFGp35PtHlDsg5jPPBlRRzj7LNrtDTnOQaHXo/edit?tab=t.b5h7exbr8cju#heading=h.5b4lqfvc832d)

## Repository Structure
```yaml
Advanced-Statistical-Modeling/
├── data/
│ ├── raw/     # Raw local CSV files (NOT tracked in Git)
│ ├── cleaned/ # obsolete
│ └── custom_data/ # FILTERED DATA RAW FROM IPEDS <- using this
├── app.R # Shiny app for interactive visualizations
├── graphs.Rmd # obsolete: R Markdown for static/exploratory graphs
├── data_cleaning.Rmd # obsolete: R scripts for cleaning data
├── data_cleaning.pdf # obsolete: Pdf report of data cleaning script
├── README.md # This file
├── DataCollection.md # Detailed how we get custom_data/
└── .gitignore # Ignores raw data and other unnecessary files
```

## Reproducibility Note

IPEDS data downloads generate non-deterministic filenames and require
manual UI-based selection. Original filenames were not preserved.
Instead, this repository documents the exact IPEDS tables, components,
survey years, and variable selections used to recreate each dataset.

For more information, please refer to the [Data Collection Guide](./DataCollection.md).

---

## Getting Started

1. Clone the repo:
```bash
git clone git@github.com:joycegill/Advanced-Statistical-Modeling.git
```

2. Install dependencies in R
```r
install.packages(c("shiny", "plotly", "tidyverse", "ggplot2", "effectsize", "corrplot", "patchwork", "readr", "stringr", "stringdist"))
```

3. Run the Shiny app (from the project root)
```r
shiny::runApp("app.R")
```


