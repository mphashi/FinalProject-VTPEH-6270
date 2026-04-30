# FinalProject-VTPEH-6270

> **Course:** VTPEH 6270 | **Author:** Ashita Singhal | **Institution:** Cornell University

---

## Overview

This project is the culmination of what I learnt in my R Class (VTPEH 6270) through the exploration of a BRFSS Dataset (from the CDC) about Prevalence of Obesity in the United States of America.

Using 14 years of surveillance data (2011–2024), I investigate one central question:

> **If a state has low obesity, is that because its residents have higher incomes, are more physically active — or both?**

The analysis shows that **both income and physical inactivity are independent predictors of state-level obesity**, with the effect of physical inactivity persisting even after controlling for income and year.

---

## Research Questions

1. Is obesity prevalence associated with income category among U.S. adults, such that higher income categories have lower obesity prevalence?
2. What is the state-level relationship between income and obesity — do higher-income states tend to have lower obesity rates?
3. How does physical inactivity relate to state-level obesity prevalence, and does the association persist after controlling for income?

---

## Key Findings

- **Income gradient:** Mean obesity prevalence decreases monotonically from ~36% in adults earning <$15,000 to ~27% in those earning ≥$75,000. Each income step up is associated with ~1.8 percentage points lower obesity (p < 0.001).
- **State-level income:** States with a wealthier population composition have significantly lower obesity rates (r = −0.38, p < 0.001).
- **Physical inactivity:** A 1 percentage point increase in physical inactivity is associated with ~0.7 percentage point higher obesity (p < 0.001), explaining ~53% of variance in state-level obesity.
- **Joint model:** Both income and physical inactivity contribute **independently** — a state with low obesity has both a wealthier *and* a more active population.

---

## Data

| Source | Description |
|--------|-------------|
| [CDC BRFSS — Nutrition, Physical Activity, and Obesity](https://catalog.data.gov/dataset/nutrition-physical-activity-and-obesity-behavioral-risk-factor-surveillance-system) | Annual telephone survey of U.S. adults (≥18 years) in all 50 states, DC, and territories. 110,880 state-year-stratification records spanning **2011–2024**. |

The raw dataset is stored in [`data/`](data/).

---

## Interactive Shiny App

Explore the data interactively:

🔗 **[Launch the Shiny App](https://mphashita.shinyapps.io/RFINALSHINYAPP/)**

The app allows you to filter by year, state, and income group to explore the relationships between income, physical inactivity, and obesity prevalence across U.S. states.

---

## Repository Structure

```
FinalProject-VTPEH-6270/
│
├── data/
│   └── Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv
│
├── scripts/
│   ├── RFinalProject6270.R          # Main analysis script
│   └── figures_nature_style.R       # Standalone Nature-style figure generator
│
├── outputs/
│   ├── reports/
│   │   └── RFinalProject6270.pdf    # Full written report
│   └── figures/
│       ├── Fig1_obesity_by_income_boxplot.pdf
│       ├── Fig2_mean_obesity_by_income_bar.pdf
│       ├── Fig3_obesity_trends_by_income.pdf
│       ├── Fig4_obesity_violin_by_income.pdf
│       ├── Fig5_state_income_vs_obesity.pdf
│       ├── Fig6_inactivity_vs_obesity.pdf
│       ├── Fig7_national_trends_inactivity_obesity.pdf
│       └── Fig8_combined_panel.pdf
│
├── RFinalProject6270.Rmd            # R Markdown source
├── references.bib                   # Bibliography
├── apa.csl                          # Citation style
└── README.md
```

---

## How to Reproduce

1. Clone the repo:
   ```bash
   git clone https://github.com/mphashi/FinalProject-VTPEH-6270.git
   cd FinalProject-VTPEH-6270
   ```

2. Open `FinalProject VTPEH 6270.Rproj` in RStudio.

3. Install dependencies:
   ```r
   install.packages(c("readr","dplyr","tidyr","ggplot2","scales","forcats",
                       "broom","knitr","ggrepel","patchwork"))
   ```

4. To regenerate figures only, run:
   ```r
   source("scripts/figures_nature_style.R")
   ```

5. To knit the full report:
   ```r
   rmarkdown::render("RFinalProject6270.Rmd")
   ```

---

## AI Use Disclosure

This project used **Claude (Anthropic)** to assist with R code generation, figure styling, and R Markdown formatting. All code was reviewed, tested, and adjusted by the author.

---

## License

For academic use only. Data sourced from the CDC and subject to their terms of use.
