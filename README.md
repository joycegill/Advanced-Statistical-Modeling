# Advanced-Statistical-Modeling
Mentored Advanced Projects ([MAP](https://www.grinnell.edu/academics/experience/research/map)) Spring 2026

## Authors

- [Khanh Do](https://github.com/khanhdo05)
- [Joyce Gill](https://github.com/joycegill)
- [Professor Shonda Kuiper](https://en.wikipedia.org/wiki/Shonda_Kuiper)

---

## Objective

The goal of this project is to explore U.S. higher education data using government sources, such as **IPEDS** and **College Scorecard**, and to build **advanced statistical models** that provide new insights into college and university performance, enrollment trends, and rankings. 

Code Book [Private Link](https://docs.google.com/spreadsheets/d/15nn2SjonRKGXaUnBLJxZs6E04GZDL0sNgCyN5-q8Vig/edit?gid=0#gid=0)

Shared Document [Private Link](https://docs.google.com/document/d/1MQ_aKizFGp35PtHlDsg5jPPBlRRzj7LNrtDTnOQaHXo/edit?tab=t.b5h7exbr8cju#heading=h.5b4lqfvc832d)

## Repository Structure

```yaml
Advanced-Statistical-Modeling/
├── data/
│ ├── raw/     # Raw local CSV files (NOT tracked in Git)
│ └── cleaned/ # Processed/cleaned CSV files used in analysis
├── data_cleaning.Rmd # R scripts for cleaning data
├── data_cleaning.pdf # Pdf report of data cleaning script
├── README.md # This file
└── .gitignore # Ignores raw data and other unnecessary files
```

## Data Instructions

1. **Download raw data**: Visit the [IPEDS Data Center](https://nces.ed.gov/ipeds/datacenter/) and download the necessary files (e.g., `HD2024`, `EF2024D`, etc.).  
2. **Place files**: Extract the CSVs and save them in `data/raw/`.  
3. **Do not commit raw data**: The `data/raw/` folder is ignored by Git due to file size and privacy considerations.  

The repository includes scripts that **clean and filter the raw data** (e.g., removing two-year schools and institutions with fewer than 500 students). After running these scripts, **cleaned data** is saved to `data/cleaned/` for analysis.

---

## Getting Started

1. Clone the repo:
```bash
git clone git@github.com:joycegill/Advanced-Statistical-Modeling.git
```

2. Install dependencies in R
```bash
install.packages(c("dplyr", "tidyr", "mosaic", "ggplot2"))
```


