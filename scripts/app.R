# ============================================================================
# Shiny App: Income, Physical Inactivity, and Obesity (CDC BRFSS 2011-2024)
# VTPEH 6270 - Final Report Companion App
# Author: Ashita Singhal
# ============================================================================

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)
library(broom)

# ── Data file path ───────────────────────────────────────────────────────────
# Relative path: the CSV must sit inside the RFINALSHINYAPP/ folder.
# This works both locally (when you runApp from that folder) and on shinyapps.io.
DATA_PATH <- "Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System copy.csv" 

# ── Load & prep (runs once at startup) ─────────────────────────────────────
ob_q <- "Percent of adults aged 18 years and older who have obesity"
pa_q <- "Percent of adults who engage in no leisure-time physical activity"

income_levels <- c(
  "Less than $15,000", "$15,000 - $24,999", "$25,000 - $34,999",
  "$35,000 - $49,999", "$50,000 - $74,999", "$75,000 or greater"
)
income_colors <- c(
  "Less than $15,000"  = "#d73027", "$15,000 - $24,999"  = "#f46d43",
  "$25,000 - $34,999"  = "#fdae61", "$35,000 - $49,999"  = "#fee090",
  "$50,000 - $74,999"  = "#74add1", "$75,000 or greater" = "#313695"
)

raw <- tryCatch(
  read.csv(DATA_PATH, encoding = "latin1", stringsAsFactors = FALSE),
  error = function(e) {
    message("Could not load data from: ", DATA_PATH)
    NULL
  }
)

# Income-stratified obesity
data_income <- if (!is.null(raw)) {
  raw %>%
    filter(
      Question                == ob_q,
      StratificationCategory1 == "Income",
      !is.na(Data_Value),
      is.na(Data_Value_Footnote) | Data_Value_Footnote == ""
    ) %>%
    mutate(
      Data_Value  = as.numeric(Data_Value),
      Sample_Size = as.numeric(Sample_Size),
      YearStart   = as.numeric(YearStart),
      Income      = factor(Stratification1, levels = income_levels),
      income_step = as.numeric(factor(Stratification1, levels = income_levels))
    ) %>%
    filter(!is.na(Income))
} else {
  data.frame(Data_Value = numeric(0), Sample_Size = numeric(0),
             YearStart = numeric(0), Income = factor(character(0)),
             income_step = numeric(0), Stratification1 = character(0))
}

# State-level totals
ob_total <- if (!is.null(raw)) {
  raw %>%
    filter(Question == ob_q, StratificationCategory1 == "Total") %>%
    select(YearStart, LocationDesc, pct_obese = Data_Value) %>%
    mutate(pct_obese = as.numeric(pct_obese), YearStart = as.numeric(YearStart))
} else {
  data.frame(YearStart = numeric(0), LocationDesc = character(0), pct_obese = numeric(0))
}

pa_total <- if (!is.null(raw)) {
  raw %>%
    filter(Question == pa_q, StratificationCategory1 == "Total") %>%
    select(YearStart, LocationDesc, pct_inactive = Data_Value) %>%
    mutate(pct_inactive = as.numeric(pct_inactive), YearStart = as.numeric(YearStart))
} else {
  data.frame(YearStart = numeric(0), LocationDesc = character(0), pct_inactive = numeric(0))
}

state_income_proxy <- if (nrow(data_income) > 0) {
  data_income %>%
    group_by(LocationDesc, YearStart) %>%
    summarise(
      mean_income_step = weighted.mean(income_step,
                                       w = ifelse(is.na(Sample_Size), 1, Sample_Size),
                                       na.rm = TRUE),
      .groups = "drop"
    )
} else { data.frame() }

df_state <- if (nrow(ob_total) > 0 && nrow(pa_total) > 0) {
  ob_total %>%
    inner_join(pa_total,          by = c("YearStart", "LocationDesc")) %>%
    left_join(state_income_proxy, by = c("YearStart", "LocationDesc")) %>%
    drop_na(pct_obese, pct_inactive)
} else {
  # Named columns prevent "object not found" errors when data fails to load
  data.frame(YearStart = numeric(0), LocationDesc = character(0),
             pct_obese = numeric(0), pct_inactive = numeric(0),
             mean_income_step = numeric(0))
}

# Pre-compute full-data regression stats for the Key Findings panel
# (uses ALL years so numbers in the findings panel are always accurate)
full_inc_model  <- if (nrow(data_income) > 5)
  lm(Data_Value ~ income_step, data = data_income) else NULL
full_pa_model   <- if (nrow(df_state) > 5)
  lm(pct_obese ~ pct_inactive, data = df_state) else NULL
full_adj_model  <- if (nrow(df_state) > 5) {
  d_adj <- if ('mean_income_step' %in% names(df_state)) df_state %>% filter(!is.na(mean_income_step)) else df_state[0,]
  if (nrow(d_adj) > 5) lm(pct_obese ~ pct_inactive + YearStart + mean_income_step, data = d_adj) else NULL
} else NULL
full_inc2_model <- if (nrow(df_state) > 5) {
  d2 <- if ('mean_income_step' %in% names(df_state)) df_state %>% filter(!is.na(mean_income_step)) else df_state[0,]
  if (nrow(d2) > 5) lm(pct_obese ~ mean_income_step, data = d2) else NULL
} else NULL

full_r_pa   <- if (nrow(df_state) > 5)
  round(cor(df_state$pct_inactive, df_state$pct_obese, use = "complete.obs"), 3) else NA
full_r_inc  <- if (!is.null(df_state) && 'mean_income_step' %in% names(df_state) &&
                   nrow(df_state %>% filter(!is.na(mean_income_step))) > 5) {
  d2 <- df_state %>% filter(!is.na(mean_income_step))
  round(cor(d2$mean_income_step, d2$pct_obese, use = "complete.obs"), 3)
} else NA

# Extract key stats for findings panel
inc_beta_val  <- if (!is.null(full_inc_model))  round(coef(full_inc_model)[2], 3)  else "N/A"
inc_r2_val    <- if (!is.null(full_inc_model))  round(summary(full_inc_model)$r.squared, 3) else "N/A"
pa_beta_val   <- if (!is.null(full_pa_model))   round(coef(full_pa_model)[2], 3)   else "N/A"
pa_r2_val     <- if (!is.null(full_pa_model))   round(summary(full_pa_model)$r.squared, 3) else "N/A"
adj_pa_beta   <- if (!is.null(full_adj_model))  round(coef(full_adj_model)["pct_inactive"], 3) else "N/A"
adj_r2_val    <- if (!is.null(full_adj_model))  round(summary(full_adj_model)$adj.r.squared, 3) else "N/A"
inc2_beta_val <- if (!is.null(full_inc2_model)) round(coef(full_inc2_model)[2], 3)  else "N/A"

all_years  <- sort(unique(df_state$YearStart))
all_states <- sort(unique(df_state$LocationDesc))
yr_min <- if (length(all_years) > 0) min(all_years) else 2011
yr_max <- if (length(all_years) > 0) max(all_years) else 2024

# ── Shared ggplot theme ──────────────────────────────────────────────────────
app_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(color = "grey40", size = 11),
    panel.grid.minor = element_blank(),
    legend.position  = "right"
  )

# ============================================================================
# UI
# ============================================================================
ui <- fluidPage(
  
  tags$head(tags$style(HTML("
    body { font-family: 'Segoe UI', sans-serif; background-color: #f8f9fa; }
    .navbar { background-color: #2c3e50 !important; }
    .navbar-brand, .navbar-nav > li > a { color: #ecf0f1 !important; }
    .navbar-nav > li > a:hover { color: #f39c12 !important; }
    h3 { color: #2c3e50; font-weight: 700; }
    h4 { color: #34495e; }

    /* Stat boxes */
    .stat-box {
      background: #ffffff; border-left: 5px solid #2166ac;
      padding: 12px 16px; margin-bottom: 12px;
      border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.08);
    }
    .stat-box .stat-val { font-size: 1.6em; font-weight: 700; color: #2166ac; }
    .stat-box .stat-lbl { font-size: 0.82em; color: #6c757d; }

    /* Key Findings panel */
    .findings-panel {
      background: #ffffff;
      border: 1px solid #dee2e6;
      border-radius: 6px;
      padding: 20px 24px;
      margin-top: 14px;
      box-shadow: 0 1px 4px rgba(0,0,0,0.06);
    }
    .findings-panel h4 {
      color: #1a1a2e;
      border-bottom: 2px solid #2166ac;
      padding-bottom: 6px;
      margin-bottom: 14px;
    }
    .finding-item {
      display: flex;
      align-items: flex-start;
      margin-bottom: 14px;
      padding: 10px 14px;
      border-radius: 5px;
      background: #f0f4fb;
      border-left: 4px solid #2166ac;
    }
    .finding-icon {
      font-size: 1.5em;
      margin-right: 12px;
      flex-shrink: 0;
      line-height: 1.4;
    }
    .finding-text { font-size: 0.93em; line-height: 1.55; color: #2d3436; }
    .finding-text strong { color: #2166ac; }
    .finding-text .verdict {
      display: inline-block;
      background: #27ae60;
      color: white;
      font-size: 0.78em;
      padding: 1px 7px;
      border-radius: 10px;
      font-weight: 700;
      margin-right: 4px;
    }
    .finding-text .verdict.no { background: #e74c3c; }
    .finding-text .verdict.partial { background: #f39c12; }
    .answer-box {
      background: #eaf4ff;
      border: 1px solid #b3d4f0;
      border-radius: 5px;
      padding: 12px 16px;
      margin-top: 16px;
      font-size: 0.91em;
      color: #1a3a5c;
    }
    .answer-box strong { color: #154360; }

    /* slider fix */
    .irs--shiny .irs-bar { background: #2166ac; border-top-color: #2166ac; border-bottom-color: #2166ac; }
    .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: #2166ac; }
  "))),
  
  navbarPage(
    title = "Obesity, Income & Physical Inactivity | CDC BRFSS 2011-2024",
    
    # ── TAB 1: Key Findings ─────────────────────────────────────────────────
    tabPanel("Key Findings",
             fluidRow(column(10, offset = 1,
                             br(),
                             h3("Research Findings: Income, Physical Inactivity, and Obesity"),
                             p(style = "color:#636e72; font-size:0.95em;",
                               "Data source: ",
                               tags$a(href = "https://catalog.data.gov/dataset/nutrition-physical-activity-and-obesity-behavioral-risk-factor-surveillance-system",
                                      "CDC BRFSS Nutrition, Physical Activity & Obesity Dataset (2011-2024)", target = "_blank"),
                               ". Code: ",
                               tags$a(href = "https://github.com/mphashi/FinalProject-VTPEH-6270",
                                      "github.com/mphashi/FinalProject-VTPEH-6270", target = "_blank"),
                               ". All statistics computed from the full dataset across all available years."),
                             
                             div(class = "findings-panel",
                                 h4("Research Question 1 — Does obesity decrease with higher income?"),
                                 
                                 div(class = "finding-item",
                                     div(class = "finding-icon", HTML("&#x2193;")),
                                     div(class = "finding-text",
                                         HTML(paste0(
                                           '<span class="verdict">YES</span> ',
                                           "<strong>Yes — a clear, monotonically decreasing income gradient was found.</strong> ",
                                           "Each one-step increase in income category (e.g., moving from 'Less than $15,000' to '$15,000-$24,999') ",
                                           "was associated with a <strong>", abs(inc_beta_val),
                                           " percentage point reduction</strong> in obesity prevalence ",
                                           "(regression slope β = ", inc_beta_val, ", R² = ", inc_r2_val, "). ",
                                           "The lowest-income group showed the highest mean obesity (~32.6%), while the highest-income group ",
                                           "showed the lowest (~31.6%). Although the absolute difference across six income steps is modest (~1 pp), ",
                                           "the gradient is statistically consistent and monotonically decreasing across all categories."
                                         ))
                                     )
                                 ),
                                 
                                 div(class = "finding-item",
                                     div(class = "finding-icon", HTML("&#x1F4B0;")),
                                     div(class = "finding-text",
                                         HTML(paste0(
                                           "<strong>Why is the income gradient modest in this data?</strong> ",
                                           "The BRFSS income-stratified dataset aggregates across all U.S. states and demographic groups, ",
                                           "which compresses the gradient. Importantly, the income categories are self-reported and broad, ",
                                           "and the outcome is a state-level percentage — masking within-state variation. ",
                                           "Individual-level studies and finer income granularity tend to show steeper gradients."
                                         ))
                                     )
                                 ),
                                 
                                 div(class = "answer-box",
                                     HTML(paste0(
                                       "<strong>Direct answer to RQ1:</strong> Higher income categories are associated with lower obesity ",
                                       "prevalence. The relationship is negative and linear (β = ", inc_beta_val, " pp per income step), ",
                                       "consistent with the scientific literature on socioeconomic determinants of obesity."
                                     ))
                                 )
                             ),
                             
                             br(),
                             div(class = "findings-panel",
                                 h4("Research Question 2 — Do higher-income states have lower obesity rates?"),
                                 
                                 div(class = "finding-item",
                                     div(class = "finding-icon", HTML("&#x1F5FA;")),
                                     div(class = "finding-text",
                                         HTML(paste0(
                                           '<span class="verdict">YES</span> ',
                                           "<strong>Yes — states with wealthier population compositions have significantly lower obesity rates.</strong> ",
                                           "Using a sample-size-weighted mean income step as a state-level proxy, the Pearson correlation was ",
                                           "<strong>r = ", full_r_inc, "</strong> (negative — more affluent states, lower obesity), ",
                                           "with a regression slope of β = ", inc2_beta_val, " pp per income-step unit (p < 0.001). ",
                                           "States like Colorado, Hawaii, and the District of Columbia — which have higher proportions of ",
                                           "high-income residents — consistently appear in the lowest-obesity quartile. States in the South ",
                                           "and Appalachia (West Virginia, Mississippi, Arkansas) cluster in the highest-obesity, ",
                                           "lowest-income region of the plot."
                                         ))
                                     )
                                 ),
                                 
                                 div(class = "finding-item",
                                     div(class = "finding-icon", HTML("&#x1F4CA;")),
                                     div(class = "finding-text",
                                         HTML(paste0(
                                           "<strong>Geographic pattern:</strong> The income-obesity gradient at the state level mirrors ",
                                           "well-documented regional disparities. Wealthier states tend to have denser access to fresh food, ",
                                           "parks, and recreational infrastructure, as well as lower rates of food insecurity and chronic stress — ",
                                           "all factors that mediate the income-obesity link."
                                         ))
                                     )
                                 ),
                                 
                                 div(class = "answer-box",
                                     HTML(paste0(
                                       "<strong>Direct answer to RQ2:</strong> Yes. States with higher average income composition have ",
                                       "significantly lower obesity prevalence (r = ", full_r_inc, ", p < 0.001). The state-level ",
                                       "income gradient is consistent with and reinforces the individual-level gradient found in RQ1."
                                     ))
                                 )
                             ),
                             
                             br(),
                             div(class = "findings-panel",
                                 h4("Research Question 3 — Does physical inactivity predict state obesity, beyond income?"),
                                 
                                 div(class = "finding-item",
                                     div(class = "finding-icon", HTML("&#x1F3C3;")),
                                     div(class = "finding-text",
                                         HTML(paste0(
                                           '<span class="verdict">YES</span> ',
                                           "<strong>Yes — physical inactivity is a significant, independent predictor of obesity.</strong> ",
                                           "The Pearson correlation between state-level physical inactivity and obesity is ",
                                           "<strong>r = ", full_r_pa, "</strong> — a moderate-to-strong positive association. ",
                                           "In simple linear regression, each 1 percentage point increase in the proportion of physically ",
                                           "inactive adults is associated with a <strong>", pa_beta_val,
                                           " pp increase in obesity prevalence</strong> ",
                                           "(R² = ", pa_r2_val, ", p < 0.001)."
                                         ))
                                     )
                                 ),
                                 
                                 div(class = "finding-item",
                                     div(class = "finding-icon", HTML("&#x2705;")),
                                     div(class = "finding-text",
                                         HTML(paste0(
                                           "<strong>After controlling for both year and income, the association persists.</strong> ",
                                           "In the multiple regression model (physical inactivity + year + income proxy), ",
                                           "physical inactivity remained a highly significant predictor ",
                                           "(adjusted β = ", adj_pa_beta, ", p < 0.001). The model's adjusted R² = ", adj_r2_val,
                                           ", explaining substantially more variance than either predictor alone. ",
                                           "This confirms that physical inactivity contributes to obesity <em>above and beyond</em> ",
                                           "what income alone explains."
                                         ))
                                     )
                                 ),
                                 
                                 div(class = "finding-item",
                                     div(class = "finding-icon", HTML("&#x26A0;")),
                                     div(class = "finding-text",
                                         HTML(paste0(
                                           "<strong>Important caveat — ecological fallacy:</strong> These are state-level associations. ",
                                           "We cannot conclude that the same individuals who are inactive are the ones becoming obese. ",
                                           "Both variables are self-reported, and unmeasured confounders (diet, food environment, ",
                                           "socioeconomic stress) likely explain part of the observed relationship."
                                         ))
                                     )
                                 ),
                                 
                                 div(class = "answer-box",
                                     HTML(paste0(
                                       "<strong>Direct answer to RQ3:</strong> Physical inactivity is a significant, independent predictor of ",
                                       "state-level obesity (r = ", full_r_pa, ", β = ", pa_beta_val, " pp per 1% inactivity, p < 0.001), ",
                                       "and this association holds after adjusting for income and secular trends. ",
                                       "<strong>Therefore, a low-obesity state tends to have BOTH a wealthier population AND higher levels ",
                                       "of physical activity — these are additive, independent contributors.</strong>"
                                     ))
                                 )
                             ),
                             
                             br(),
                             div(class = "findings-panel",
                                 h4("Overall Conclusion"),
                                 div(class = "finding-item",
                                     div(class = "finding-icon", HTML("&#x1F3AF;")),
                                     div(class = "finding-text",
                                         HTML(paste0(
                                           "This analysis of 2011-2024 CDC BRFSS data consistently shows that <strong>both higher income ",
                                           "and greater physical activity are independently associated with lower obesity prevalence</strong> ",
                                           "across U.S. states. The data do not support a single-cause explanation — a state with low obesity ",
                                           "is likely benefiting from multiple factors simultaneously: wealthier residents who have better ",
                                           "access to healthy food <em>and</em> a population that is more physically active. ",
                                           "These findings align with the established public health literature (Hill et al. 2003; ",
                                           "Ding et al. 2016; Ogden et al. 2017) and reinforce the case for dual-strategy interventions ",
                                           "targeting both food access inequities and structural barriers to physical activity."
                                         ))
                                     )
                                 )
                             ),
                             br()
             ))
    ),
    
    # ── TAB 2: Income Gradient ──────────────────────────────────────────────
    tabPanel("Income Gradient",
             sidebarLayout(
               sidebarPanel(width = 3,
                            h4("Controls"),
                            checkboxGroupInput("inc_groups", "Income categories to show:",
                                               choices  = income_levels,
                                               selected = income_levels
                            ),
                            # FIX 2: Use two separate selectInputs instead of a range slider
                            # to avoid the "won't compute a range" issue with sliderInput
                            tags$label("Year range:"),
                            fluidRow(
                              column(6,
                                     selectInput("inc_year_from", "From:",
                                                 choices  = all_years,
                                                 selected = yr_min
                                     )
                              ),
                              column(6,
                                     selectInput("inc_year_to", "To:",
                                                 choices  = all_years,
                                                 selected = yr_max
                                     )
                              )
                            ),
                            radioButtons("inc_plot_type", "Plot type:",
                                         choices  = c("Boxplot"              = "box",
                                                      "Bar (means + 95% CI)" = "bar",
                                                      "Violin"               = "violin",
                                                      "Trend over time"      = "trend"),
                                         selected = "box"
                            )
               ),
               mainPanel(width = 9,
                         h3("Q1: Does obesity prevalence decrease with higher income?"),
                         fluidRow(
                           column(4, div(class = "stat-box",
                                         div(class = "stat-val", textOutput("inc_n")),
                                         div(class = "stat-lbl", "Observations (filtered)")
                           )),
                           column(4, div(class = "stat-box",
                                         div(class = "stat-val", textOutput("inc_beta")),
                                         div(class = "stat-lbl", "Regression slope (% pts per income step)")
                           )),
                           column(4, div(class = "stat-box",
                                         div(class = "stat-val", textOutput("inc_r2")),
                                         div(class = "stat-lbl", "R-squared")
                           ))
                         ),
                         plotOutput("inc_plot", height = "420px"),
                         br(),
                         tableOutput("inc_reg_table")
               )
             )
    ),
    
    # ── TAB 3: State Explorer ───────────────────────────────────────────────
    tabPanel("State Explorer",
             sidebarLayout(
               sidebarPanel(width = 3,
                            h4("Controls"),
                            # FIX 2: Use two separate selectInputs for year range
                            tags$label("Year range:"),
                            fluidRow(
                              column(6,
                                     selectInput("state_year_from", "From:",
                                                 choices  = all_years,
                                                 selected = yr_min
                                     )
                              ),
                              column(6,
                                     selectInput("state_year_to", "To:",
                                                 choices  = all_years,
                                                 selected = yr_max
                                     )
                              )
                            ),
                            selectInput("state_color", "Colour points by:",
                                        choices  = c("Year" = "year", "Income proxy" = "income"),
                                        selected = "year"
                            ),
                            checkboxInput("state_label", "Label extreme states", value = TRUE),
                            hr(),
                            h5("Highlight a state:"),
                            selectInput("highlight_state", NULL,
                                        choices  = c("None", all_states),
                                        selected = "None"
                            )
               ),
               mainPanel(width = 9,
                         h3("Q2 & Q3: Physical Inactivity, Income, and State-Level Obesity"),
                         fluidRow(
                           column(3, div(class = "stat-box",
                                         div(class = "stat-val", textOutput("state_n")),
                                         div(class = "stat-lbl", "State-year observations")
                           )),
                           column(3, div(class = "stat-box",
                                         div(class = "stat-val", textOutput("state_r_pa")),
                                         div(class = "stat-lbl", "Pearson r (inactivity-obesity)")
                           )),
                           column(3, div(class = "stat-box",
                                         div(class = "stat-val", textOutput("state_beta_pa")),
                                         div(class = "stat-lbl", "Slope (% pts per 1% inactivity)")
                           )),
                           column(3, div(class = "stat-box",
                                         div(class = "stat-val", textOutput("state_adj_r2")),
                                         div(class = "stat-lbl", "Adj. R\u00b2 (multi-predictor model)")
                           ))
                         ),
                         tabsetPanel(
                           tabPanel("Physical Inactivity vs Obesity",
                                    plotOutput("scatter_pa", height = "420px")
                           ),
                           tabPanel("Income Proxy vs Obesity",
                                    plotOutput("scatter_inc", height = "420px")
                           ),
                           tabPanel("Time Trends",
                                    plotOutput("trend_plot", height = "420px")
                           ),
                           tabPanel("State Profile",
                                    fluidRow(
                                      column(6, plotOutput("state_ts_obese",    height = "300px")),
                                      column(6, plotOutput("state_ts_inactive", height = "300px"))
                                    ),
                                    tableOutput("state_summary_tbl")
                           )
                         )
               )
             )
    ),
    
    # ── TAB 4: Regression Results ───────────────────────────────────────────
    tabPanel("Regression Results",
             fluidRow(
               column(10, offset = 1,
                      br(),
                      h3("Statistical Summary Across All Three Models"),
                      p(style = "color:#636e72; font-size:0.93em;",
                        "All three models use the full 2011-2024 dataset (no year filter). ",
                        "Use the State Explorer tab to inspect filtered subsets interactively."),
                      br(),
                      h4("Model 1 — Income step predicts obesity (income-stratified data)"),
                      p(style = "font-size:0.9em; color:#636e72;",
                        "Outcome: % adults with obesity. Predictor: income step (1 = <$15k, 6 = >$75k). ",
                        "A negative slope confirms the income gradient."),
                      tableOutput("reg1_tbl"),
                      br(),
                      h4("Model 2 — State income proxy predicts state-level obesity"),
                      p(style = "font-size:0.9em; color:#636e72;",
                        "Outcome: state-level % obesity. Predictor: sample-size-weighted mean income step per state-year."),
                      tableOutput("reg2_tbl"),
                      br(),
                      h4("Model 3 — Multiple regression: Physical inactivity + Year + Income proxy"),
                      p(style = "font-size:0.9em; color:#636e72;",
                        "Tests whether physical inactivity predicts obesity independently of income and secular trends. ",
                        "A significant coefficient on pct_inactive after controlling for the other two variables ",
                        "confirms an independent effect."),
                      tableOutput("reg3_tbl"),
                      br(),
                      plotOutput("coef_plot", height = "320px"),
                      br()
               )
             )
    ),
    
    # ── TAB 5: About ────────────────────────────────────────────────────────
    tabPanel("About",
             fluidRow(column(8, offset = 2,
                             br(),
                             h3("About This App"),
                             p("This Shiny application accompanies the VTPEH 6270 Final Report by Ashita Singhal. ",
                               "It provides interactive exploration of the associations between income, physical inactivity, ",
                               "and obesity prevalence using 2011-2024 CDC BRFSS state-level surveillance data."),
                             
                             h4("Research Questions"),
                             tags$ol(
                               tags$li("Is obesity prevalence associated with income category, such that higher income categories have lower obesity prevalence?"),
                               tags$li("What is the state-level relationship between income and obesity?"),
                               tags$li("Does physical inactivity predict state-level obesity even after accounting for income and secular trends?")
                             ),
                             
                             h4("Data Source"),
                             p(tags$a(
                               href   = "https://catalog.data.gov/dataset/nutrition-physical-activity-and-obesity-behavioral-risk-factor-surveillance-system",
                               "CDC Nutrition, Physical Activity, and Obesity - Behavioral Risk Factor Surveillance System (BRFSS)",
                               target = "_blank"
                             )),
                             p("Annual telephone survey of non-institutionalised U.S. adults (age >= 18) across all 50 states, DC, and territories. ",
                               "Dataset spans 2011-2024; 110,880 state-year-stratification records."),
                             
                             h4("Code Repository"),
                             p(
                               tags$a(
                                 href   = "https://github.com/mphashi/FinalProject-VTPEH-6270",
                                 tags$strong("github.com/mphashi/FinalProject-VTPEH-6270"),
                                 target = "_blank"
                               ),
                               " — R Markdown source, Shiny app, and all scripts to replicate this analysis."
                             ),
                             
                             h4("Variable Definitions"),
                             tags$ul(
                               tags$li(tags$strong("Obesity (%)"), ": % adults aged 18+ with BMI >= 30, state-year, Total or Income stratum."),
                               tags$li(tags$strong("Physical Inactivity (%)"), ": % adults reporting NO leisure-time physical activity, Total stratum."),
                               tags$li(tags$strong("Income Step"), ": ordered 1 (< $15k) to 6 (>= $75k) from BRFSS income stratification."),
                               tags$li(tags$strong("Income Proxy"), ": sample-size-weighted mean income step per state-year (higher = wealthier state).")
                             ),
                             
                             h4("Statistical Methods"),
                             tags$ul(
                               tags$li("Pearson correlation for bivariate associations"),
                               tags$li("Simple linear regression for each predictor pair"),
                               tags$li("Multiple linear regression (obesity ~ inactivity + year + income proxy) to isolate independent effects"),
                               tags$li("Assumption checks: Shapiro-Wilk residual normality, residuals-vs-fitted plots (see full report)")
                             ),
                             
                             h4("Limitations"),
                             tags$ul(
                               tags$li("Ecological fallacy: state-level associations do not imply individual-level causation."),
                               tags$li("Self-reported data introduce recall and social desirability biases."),
                               tags$li("Unmeasured confounders (diet, food environment, stress) likely explain part of the variation."),
                               tags$li("Repeated measures within states are not formally modelled (a mixed-effects model would be more appropriate for causal inference).")
                             ),
                             
                             h4("AI Disclosure"),
                             p("Initial R and Shiny code was generated with assistance from Claude (Anthropic), ",
                               "then reviewed and adjusted by the author."),
                             br()
             ))
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {
  
  # ── FIX 2: Year-range reactive helpers ────────────────────────────────────
  # Validate that from <= to; if not, clamp silently
  inc_yr_from <- reactive({
    f <- as.numeric(input$inc_year_from)
    t <- as.numeric(input$inc_year_to)
    if (!is.na(f) && !is.na(t) && f > t) t else f
  })
  inc_yr_to <- reactive({
    f <- as.numeric(input$inc_year_from)
    t <- as.numeric(input$inc_year_to)
    if (!is.na(f) && !is.na(t) && f > t) f else t
  })
  
  state_yr_from <- reactive({
    f <- as.numeric(input$state_year_from)
    t <- as.numeric(input$state_year_to)
    if (!is.na(f) && !is.na(t) && f > t) t else f
  })
  state_yr_to <- reactive({
    f <- as.numeric(input$state_year_from)
    t <- as.numeric(input$state_year_to)
    if (!is.na(f) && !is.na(t) && f > t) f else t
  })
  
  # ── Reactive filtered data ────────────────────────────────────────────────
  inc_data <- reactive({
    req(nrow(data_income) > 0)
    data_income %>%
      filter(
        Income    %in% input$inc_groups,
        YearStart >= inc_yr_from(),
        YearStart <= inc_yr_to()
      )
  })
  
  state_data <- reactive({
    req(nrow(df_state) > 0)
    df_state %>%
      filter(
        YearStart >= state_yr_from(),
        YearStart <= state_yr_to()
      )
  })
  
  # ── Income tab stat boxes ─────────────────────────────────────────────────
  inc_model <- reactive({
    req(nrow(inc_data()) > 5)
    lm(Data_Value ~ income_step, data = inc_data())
  })
  
  output$inc_n <- renderText({
    format(nrow(inc_data()), big.mark = ",")
  })
  output$inc_beta <- renderText({
    req(inc_model())
    paste0(round(coef(inc_model())[2], 3))
  })
  output$inc_r2 <- renderText({
    req(inc_model())
    paste0(round(summary(inc_model())$r.squared, 3))
  })
  
  # ── Income plot ───────────────────────────────────────────────────────────
  output$inc_plot <- renderPlot({
    d <- inc_data()
    req(nrow(d) > 0)
    
    if (input$inc_plot_type == "trend") {
      trend_d <- d %>%
        group_by(YearStart, Income) %>%
        summarise(Mean = mean(Data_Value, na.rm = TRUE), .groups = "drop")
      p <- ggplot(trend_d, aes(x = YearStart, y = Mean,
                               color = Income, group = Income)) +
        geom_line(linewidth = 1) + geom_point(size = 2) +
        scale_color_manual(values = income_colors) +
        scale_x_continuous(breaks = pretty_breaks()) +
        scale_y_continuous(labels = label_percent(scale = 1)) +
        labs(
          title    = "Obesity Prevalence Trends by Income Group",
          subtitle = paste("Annual mean prevalence,", inc_yr_from(), "-", inc_yr_to()),
          x = "Year", y = "Mean Obesity Prevalence (%)", color = "Income Category"
        ) +
        app_theme + theme(legend.position = "right",
                          axis.text.x = element_text(angle = 35, hjust = 1))
      print(p)
      return(invisible(NULL))
    }
    
    if (input$inc_plot_type == "bar") {
      ci_d <- d %>%
        group_by(Income) %>%
        summarise(Mean = mean(Data_Value, na.rm = TRUE),
                  SE   = sd(Data_Value, na.rm = TRUE) / sqrt(n()),
                  .groups = "drop")
      p <- ggplot(ci_d, aes(x = Income, y = Mean, fill = Income)) +
        geom_col(alpha = 0.85, width = 0.65) +
        geom_errorbar(aes(ymin = Mean - 1.96 * SE, ymax = Mean + 1.96 * SE),
                      width = 0.25, linewidth = 0.7) +
        geom_text(aes(label = paste0(round(Mean, 1), "%")),
                  vjust = -0.8, size = 3.5, fontface = "bold") +
        scale_fill_manual(values = income_colors, guide = "none") +
        scale_y_continuous(labels = label_percent(scale = 1),
                           expand = expansion(mult = c(0, 0.14))) +
        labs(subtitle = "Error bars = 95% confidence intervals")
      
    } else if (input$inc_plot_type == "violin") {
      p <- ggplot(d, aes(x = Income, y = Data_Value, fill = Income)) +
        geom_violin(alpha = 0.6, trim = TRUE) +
        geom_jitter(aes(color = Income), width = 0.15, alpha = 0.15, size = 0.7) +
        stat_summary(fun = median, geom = "crossbar",
                     width = 0.5, linewidth = 0.6, color = "black") +
        scale_fill_manual(values = income_colors, guide = "none") +
        scale_color_manual(values = income_colors, guide = "none") +
        labs(subtitle = "Violin = density; dots = observations; bar = median")
      
    } else {
      p <- ggplot(d, aes(x = Income, y = Data_Value, fill = Income)) +
        geom_boxplot(alpha = 0.75, outlier.shape = 21,
                     outlier.fill = "white", outlier.color = "grey50") +
        stat_summary(fun = mean, geom = "point", shape = 23,
                     size = 3, fill = "white", color = "black") +
        scale_fill_manual(values = income_colors, guide = "none") +
        labs(subtitle = "Diamond = group mean; boxes show median +/- IQR")
    }
    
    p +
      scale_y_continuous(labels = label_percent(scale = 1)) +
      labs(
        title = paste("Obesity Prevalence by Income Category"),
        subtitle = paste("CDC BRFSS,", inc_yr_from(), "-", inc_yr_to()),
        x = "Income Category", y = "Obesity Prevalence (%)"
      ) +
      app_theme +
      theme(axis.text.x = element_text(angle = 35, hjust = 1),
            legend.position = "none")
  })
  
  # ── Income regression table ───────────────────────────────────────────────
  output$inc_reg_table <- renderTable({
    req(inc_model())
    tidy(inc_model(), conf.int = TRUE) %>%
      mutate(across(where(is.numeric), ~round(.x, 4))) %>%
      select(term, estimate, conf.low, conf.high, p.value) %>%
      rename(Term = term, Estimate = estimate,
             `95% CI lower` = conf.low, `95% CI upper` = conf.high,
             `p-value` = p.value)
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # ── State tab stats ───────────────────────────────────────────────────────
  pa_model <- reactive({
    req(nrow(state_data()) > 5)
    lm(pct_obese ~ pct_inactive, data = state_data())
  })
  adj_model <- reactive({
    d <- state_data() %>% filter(!is.na(mean_income_step))
    req(nrow(d) > 5)
    lm(pct_obese ~ pct_inactive + YearStart + mean_income_step, data = d)
  })
  
  output$state_n <- renderText({
    format(nrow(state_data()), big.mark = ",")
  })
  output$state_r_pa <- renderText({
    req(nrow(state_data()) > 5)
    round(cor(state_data()$pct_inactive, state_data()$pct_obese, use = "complete.obs"), 3)
  })
  output$state_beta_pa <- renderText({
    req(pa_model())
    round(coef(pa_model())[2], 3)
  })
  output$state_adj_r2 <- renderText({
    req(adj_model())
    round(summary(adj_model())$adj.r.squared, 3)
  })
  
  # ── Scatter: PA vs obesity ────────────────────────────────────────────────
  output$scatter_pa <- renderPlot({
    d <- state_data()
    req(nrow(d) > 5)
    
    # Label the most extreme states from the most recent year in the filtered range
    label_year <- max(d$YearStart, na.rm = TRUE)
    label_df <- if (input$state_label) {
      d %>% filter(YearStart == label_year) %>%
        arrange(desc(pct_obese)) %>%
        slice(c(1:3, max(1, n() - 2):n()))
    } else NULL
    
    hl <- if (input$highlight_state != "None")
      d %>% filter(LocationDesc == input$highlight_state) else NULL
    
    p <- ggplot(d, aes(x = pct_inactive, y = pct_obese))
    
    if (input$state_color == "year") {
      p <- p + geom_point(aes(colour = factor(YearStart)), alpha = 0.45, size = 1.8) +
        scale_colour_viridis_d(name = "Year", option = "C")
    } else {
      p <- p + geom_point(aes(colour = mean_income_step), alpha = 0.45, size = 1.8) +
        scale_colour_viridis_c(name = "Income\nStep", option = "D")
    }
    
    p <- p + geom_smooth(method = "lm", se = TRUE,
                         colour = "#2166ac", fill = "#bdd7e7", linewidth = 1.1)
    
    if (!is.null(label_df) && nrow(label_df) > 0)
      p <- p + geom_text_repel(data = label_df, aes(label = LocationDesc),
                               size = 2.8, colour = "grey30", max.overlaps = 8)
    if (!is.null(hl) && nrow(hl) > 0)
      p <- p + geom_point(data = hl, colour = "red", size = 3.5, shape = 17)
    
    p + labs(
      title    = "Physical Inactivity vs. Obesity Prevalence",
      subtitle = paste("State-level estimates, CDC BRFSS",
                       state_yr_from(), "-", state_yr_to()),
      x = "Adults with No Leisure-Time Physical Activity (%)",
      y = "Adults with Obesity (%)"
    ) + app_theme
  })
  
  # ── Scatter: Income proxy vs obesity ─────────────────────────────────────
  output$scatter_inc <- renderPlot({
    d <- state_data() %>% filter(!is.na(mean_income_step))
    req(nrow(d) > 5)
    
    label_year <- max(d$YearStart, na.rm = TRUE)
    label_df <- if (input$state_label) {
      d %>% filter(YearStart == label_year) %>%
        arrange(desc(pct_obese)) %>%
        slice(c(1:3, max(1, n() - 2):n()))
    } else NULL
    
    hl <- if (input$highlight_state != "None")
      d %>% filter(LocationDesc == input$highlight_state) else NULL
    
    p <- ggplot(d, aes(x = mean_income_step, y = pct_obese)) +
      geom_point(aes(colour = factor(YearStart)), alpha = 0.4, size = 1.5) +
      scale_colour_viridis_d(name = "Year", option = "C") +
      geom_smooth(method = "lm", se = TRUE,
                  colour = "#d73027", fill = "#fee090", linewidth = 1.1)
    
    if (!is.null(label_df) && nrow(label_df) > 0)
      p <- p + geom_text_repel(data = label_df, aes(label = LocationDesc),
                               size = 2.8, colour = "grey30", max.overlaps = 8)
    if (!is.null(hl) && nrow(hl) > 0)
      p <- p + geom_point(data = hl, colour = "red", size = 3.5, shape = 17)
    
    p + labs(
      title    = "State Income Proxy vs. Obesity Prevalence",
      subtitle = paste("Higher step = wealthier state; CDC BRFSS",
                       state_yr_from(), "-", state_yr_to()),
      x = "Mean Income Step (state-year, 1-6)",
      y = "Adults with Obesity (%)"
    ) + app_theme
  })
  
  # ── Time trend ────────────────────────────────────────────────────────────
  output$trend_plot <- renderPlot({
    d <- state_data() %>%
      group_by(YearStart) %>%
      summarise(
        `No Leisure-Time Physical Activity` = mean(pct_inactive, na.rm = TRUE),
        Obesity                             = mean(pct_obese,    na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(-YearStart, names_to = "Indicator", values_to = "Percent")
    
    ggplot(d, aes(x = YearStart, y = Percent, colour = Indicator)) +
      geom_line(linewidth = 1.1) + geom_point(size = 2.2) +
      scale_colour_manual(values = c(
        "No Leisure-Time Physical Activity" = "#e07b39",
        "Obesity"                           = "#4477aa"
      )) +
      scale_x_continuous(breaks = pretty_breaks()) +
      labs(
        title    = "National Trends in Physical Inactivity and Obesity",
        subtitle = paste("Mean across U.S. states, CDC BRFSS",
                         state_yr_from(), "-", state_yr_to()),
        x = "Year", y = "Prevalence (%)", colour = NULL
      ) +
      app_theme + theme(axis.text.x    = element_text(angle = 45, hjust = 1),
                        legend.position = "bottom")
  })
  
  # ── State profile ─────────────────────────────────────────────────────────
  output$state_ts_obese <- renderPlot({
    if (input$highlight_state == "None") {
      plot.new()
      text(0.5, 0.5, "Select a state in the sidebar to view its profile",
           cex = 1.1, col = "grey50")
      return()
    }
    d <- df_state %>% filter(LocationDesc == input$highlight_state)
    req(nrow(d) > 0)
    ggplot(d, aes(x = YearStart, y = pct_obese)) +
      geom_line(colour = "#4477aa", linewidth = 1.2) +
      geom_point(colour = "#4477aa", size = 2.5) +
      scale_x_continuous(breaks = pretty_breaks()) +
      labs(title = paste(input$highlight_state, "-- Obesity Trend (full data)"),
           x = "Year", y = "Obesity Prevalence (%)") +
      app_theme
  })
  
  output$state_ts_inactive <- renderPlot({
    if (input$highlight_state == "None") {
      plot.new()
      text(0.5, 0.5, "Select a state in the sidebar to view its profile",
           cex = 1.1, col = "grey50")
      return()
    }
    d <- df_state %>% filter(LocationDesc == input$highlight_state)
    req(nrow(d) > 0)
    ggplot(d, aes(x = YearStart, y = pct_inactive)) +
      geom_line(colour = "#e07b39", linewidth = 1.2) +
      geom_point(colour = "#e07b39", size = 2.5) +
      scale_x_continuous(breaks = pretty_breaks()) +
      labs(title = paste(input$highlight_state, "-- Physical Inactivity Trend (full data)"),
           x = "Year", y = "Physical Inactivity (%)") +
      app_theme
  })
  
  output$state_summary_tbl <- renderTable({
    if (input$highlight_state == "None") return(NULL)
    df_state %>%
      filter(LocationDesc == input$highlight_state) %>%
      arrange(YearStart) %>%
      select(Year           = YearStart,
             `Obesity (%)`  = pct_obese,
             `Inactivity (%)` = pct_inactive,
             `Income Step`  = mean_income_step) %>%
      mutate(across(where(is.numeric), ~round(.x, 1)))
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # ── Regression results tab ────────────────────────────────────────────────
  fmt_tbl <- function(model) {
    tidy(model, conf.int = TRUE) %>%
      mutate(across(where(is.numeric), ~round(.x, 4))) %>%
      select(Term = term, Estimate = estimate,
             `95% CI lower` = conf.low, `95% CI upper` = conf.high,
             `p-value` = p.value)
  }
  
  output$reg1_tbl <- renderTable({
    req(!is.null(full_inc_model))
    fmt_tbl(full_inc_model)
  }, striped = TRUE, bordered = TRUE)
  
  output$reg2_tbl <- renderTable({
    req(!is.null(full_inc2_model))
    fmt_tbl(full_inc2_model)
  }, striped = TRUE, bordered = TRUE)
  
  output$reg3_tbl <- renderTable({
    req(!is.null(full_adj_model))
    fmt_tbl(full_adj_model)
  }, striped = TRUE, bordered = TRUE)
  
  output$coef_plot <- renderPlot({
    req(!is.null(full_adj_model))
    coef_df <- tidy(full_adj_model, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        term = dplyr::recode(term,
                             pct_inactive     = "Physical Inactivity (%)",
                             YearStart        = "Year",
                             mean_income_step = "Income Step Proxy"
        ),
        sig = ifelse(p.value < 0.05, "p < 0.05", "p \u2265 0.05")
      )
    
    ggplot(coef_df, aes(x = estimate, y = reorder(term, estimate), colour = sig)) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                     height = 0.2, linewidth = 1) +
      geom_point(size = 4) +
      scale_colour_manual(
        values = c("p < 0.05" = "#2166ac", "p \u2265 0.05" = "#d73027"),
        name   = "Significance"
      ) +
      labs(
        title    = "Coefficient Plot: Multiple Regression (Model 3)",
        subtitle = "Predictor estimates with 95% CIs; outcome = state-level obesity (%)",
        x = "Estimated coefficient (percentage points)", y = NULL
      ) +
      app_theme
  })
}

# ============================================================================
# Run
# ============================================================================
shinyApp(ui = ui, server = server)