# project2-NFL-APP
# NFL Play-by-Play Data Explorer (2009–2018)

**Author:** Evie Kenna  
**Course:** ST 558 – Data Science for Statisticians  
**Live App:** [eviekenna.shinyapps.io/nfl-playbyplay](https://eviekenna.shinyapps.io/nfl-playbyplay/)  

---

## Overview

This project presents an interactive R Shiny web application designed to explore detailed NFL play-by-play data from the 2009–2018 seasons.  
The app allows users to dynamically subset, summarize, and visualize key aspects of NFL game play. 

The application is deployed publicly on **[shinyapps.io](https://www.shinyapps.io)** and provides both numerical summaries and visual insights.  

---

## Purpose

The goal of this app is to let users:
- Explore trends in NFL offensive play-calling over multiple seasons  
- Subset plays by team, down, play type, and numeric conditions  
- Generate both categorical and numeric summaries  
- Visualize how yards gained vary across situations and seasons  
- Download customized subsets of the data for further analysis  

---

## Features

### Sidebar Filters
Users can:
- Select **play types** (e.g., pass, run, punt, field goal)
- Choose **downs (1–4)**
- Filter by **offensive team**
- Subset by up to **two numeric variables** using dynamic range sliders  
  (examples: `yards_gained`, `yardline_100`, `score_differential`, `season`)

All filters are applied only when the “Apply Filters” button is clicked — ensuring smooth, controlled updates.

---

### Tabs in the Main Panel

#### **1. About**
- Describes the purpose of the app, its structure, and the dataset source  
- Includes links and a brief tutorial for new users  
- Displays an NFL logo image for context and presentation polish  

#### **2. Data Download**
- Displays the filtered dataset using an interactive **DT table**
- Allows CSV download of the subsetted data for external analysis  

#### **3. Data Exploration**
- Toggle between **Categorical Summaries** and **Numeric Summaries**
- Categorical summaries include:
  - One- and two-way contingency tables  
  - Frequency bar charts and stacked proportion bars  
- Numeric summaries include:
  - Mean, median, SD, and range grouped by a categorical variable  
  - Histograms and boxplots with color grouping  

#### **4. Team Averages**
- Displays **average yards gained per team** for both **passing** and **running plays**  
- Visualized with ranked horizontal bar charts and labeled averages  

---

##  Data Source

The dataset comes from **Kaggle**, specifically the public dataset:

> [Detailed NFL Play-by-Play Data 2009–2018](https://www.kaggle.com/datasets/maxhorowitz/nflplaybyplay2009to2016)

The original data (≈1.2 GB) includes every recorded NFL play from 2009–2018.  
For deployment efficiency, this project uses a **reduced version (≈13.8 MB)** containing the most relevant columns:

- `play_type`, `down`, `posteam`, `yards_gained`, `yardline_100`, `season`, etc.  
- Nonessential or high-missing-value columns were removed  

This smaller CSV file — **`NFL Play by Play 2009–2018 (v5).csv`** — allows faster loading and compatibility with `shinyapps.io`.

---

## Technologies Used

- **R / Shiny** – App framework and interactivity  
- **tidyverse** – Data wrangling and summarization  
- **ggplot2** + **viridis** – Visualization  
- **DT** – Interactive data tables  
- **bslib** – Modernized Bootstrap 5 interface  
- **rsconnect** – Deployment to shinyapps.io  

---

## Deployment

The live application is available at: [https://eviekenna.shinyapps.io/nfl-playbyplay/](https://eviekenna.shinyapps.io/nfl-playbyplay/)

To run locally:

```r
# Clone this repository
git clone https://github.com/eviekenna/project2-NFL-APP.git
setwd("project2-NFL-APP/NFL")

# Install required packages
install.packages(c("shiny", "bslib", "tidyverse", "DT", "ggplot2", "readr"))

# Launch the app
shiny::runApp("app.R")
