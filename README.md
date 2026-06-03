# Energy Efficiency Policy Network Analysis

## Overview
This project examines how hyperlink networks among state-level energy policy actors 
(nonprofits, utilities, public agencies, legislatures) relate to state energy efficiency 
(EE) outcomes, using network analysis and OLS regression across all 50 U.S. states.

## Research Questions
- How are energy policymaking actors connected through online hyperlink networks?
- Do network structural properties (degree centrality, clustering, network size) 
  predict state EE scores and clean energy job growth?

## Methods
- **Web scraping** of hyperlinks from ~6 categories of actors per state (50 states)
- **Network construction** using `igraph`; measures include degree centrality, 
  clustering coefficients, and network size
- **OLS regression** with controls for GDP per capita, population, political ideology, 
  industry composition, and e-government index

## Scripts (run in order)
| Script | Description |
|--------|-------------|
| `00_organization.R` | Data collection and cleaning for all actor types |
| `01_hyperlink_collection.R` | Web scraping of hyperlinks per state using `httr`/`rvest` |
| `02_network_measures.R` | Graph construction and network measure calculation |
| `03_network_visualization.R` | Network visualization using `igraph` |
| `04_data_preprocessing_regression.R` | Merging network data with policy/economic covariates |
| `05_regression.R` | OLS regression, assumption diagnostics, and table export |

## Key R Packages
`igraph`, `rvest`, `httr`, `tidyverse`, `stargazer`, `car`, `lmtest`, `readxl`, `usdata`

## Data Sources
- ACEEE State Energy Efficiency Scorecard (2024/2025)
- IRS NTEE nonprofit registry (C35 energy nonprofits)
- EIA Form 861 utility data
- BLS clean energy employment data
- MRP state-level political ideology estimates
- U.S. BEA Real GDP by State

> Raw data files are not included due to licensing. See each data source link above.

## Results
Regression outputs are exported via `stargazer` to `/output/` as `.txt` and `.tex` files.

## Project Structure
```
│
├── README.md
├── .gitignore
│
├── R/
│   ├── 00_organization.R
│   ├── 01_hyperlink_collection.R
│   ├── 02_network_measures.R
│   ├── 03_network_visualization.R
│   ├── 04_data_preprocessing_regression.R
│   └── 05_regression.R
│
├── data/
│   └── README.md          
│
└── output/
    └── README.md       

```

