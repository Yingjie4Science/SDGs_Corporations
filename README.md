![Visitor Badge](https://visitor-badge.laobi.icu/badge?page_id=yingjie4science.SDGs_Corporations)

# Global Business Giants’ Commitment to SDGs



## Introduction



## System Requirements

- R version: 4.3.2

- Operating system: Windows 11 Pro

- No special hardware or proprietary software is required.


## Quick Start / Installation
Instructions for getting started quickly, especially for non-R users.

* Clone the repository
If you're working from your Terminal or Git Bash, you can run:
```
git clone https://github.com/Yingjie4Science/SDGs_Corporations.git /path/to/your/target-folder
```

* Setting the Working Directory

This project is designed to be run directly from the project folder.

If you're using RStudio, opening the .Rproj file will automatically set the working directory.

If you are working manually, please set the working directory first:
```
setwd("/path/to/your/cloned/project")
```


## Required R Packages

The following packages are required to run the analysis. 
Please install them prior to running the code using install.packages() or via a package manager like renv. 
Version numbers used in our analysis are shown below for reproducibility:

```         
library(readr)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)

library(ggplot2)
library(cowplot)
library(ggpubr)
library(RColorBrewer)
```


## Directory Structure

```
├── data/
│   ├── 
│   ├── 
│   └── data_tm/                          # text mining (tm) folder
│       ├── database                      # regex database for tm 
│       ├── annual_reports                # raw PDF data by year
│       ├── coded_results                 # tm results
│       |   ├── coded_combined_with_raw_351.csv
|       |
|       ├── 
│
├── code/
│   ├── 
│   ├── 
│   ├── 20_PDF_to_df_batch.Rmd            # convert PDF to df
│   ├── 21_Text_mining_batch.Rmd          # tm - add sdg labels
│   ├── 21a_format_tm_data.Rmd
│   ├── 
│   ├── 30_Compare_human_tm.Rmd           # compare tm to human coded results
│   ├──
│   ├── 
│   ├── 50_coded_analysis_humanData.Rmd
│   ├── 51_coded_analysis_newPlots.Rmd
│   ├──
│   ├──
│   ├── 
│   └── function_*.R                      # Various data processing functions
│      

```


## How to Run

You can replicate the main results and figures using the `.Rmd` files in the `code/` folder. 
Each script contains inline comments describing its purpose and required inputs. 




