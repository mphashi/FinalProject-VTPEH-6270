## ============================================================
## VTPEH 6270 -- Nature-Style Figures Script
## Author : Ashita Singhal
## Purpose: Generate all publication-quality figures (Nature style)
##          and save to ./figures/ for Git upload
## ============================================================

## ── 0. Libraries ────────────────────────────────────────────
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(forcats)
library(ggrepel)

## ── 1. Nature-Style Theme ───────────────────────────────────
# Nature journals use: Helvetica/Arial, ~7pt base, minimal grid,
# clean white background, no right/top axis lines.

nature_theme <- function(base_size = 7, base_family = "Helvetica") {
  theme_classic(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Axes
      axis.line        = element_line(colour = "black", linewidth = 0.4),
      axis.ticks       = element_line(colour = "black", linewidth = 0.3),
      axis.ticks.length = unit(2, "pt"),
      axis.text        = element_text(size = rel(1), colour = "black"),
      axis.title       = element_text(size = rel(1.1), colour = "black"),
      
      # Panel
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border     = element_blank(),
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      
      # Legend
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key        = element_rect(fill = "white", colour = NA),
      legend.key.size   = unit(0.35, "cm"),
      legend.text       = element_text(size = rel(0.9)),
      legend.title      = element_text(size = rel(1), face = "bold"),
      legend.position   = "bottom",
      
      # Strip (facets)
      strip.background = element_rect(fill = "grey95", colour = "grey70", linewidth = 0.3),
      strip.text       = element_text(size = rel(1), face = "bold"),
      
      # Titles
      plot.title    = element_text(size = rel(1.3), face = "bold",
                                   hjust = 0, margin = margin(b = 3)),
      plot.subtitle = element_text(size = rel(1), colour = "grey40",
                                   hjust = 0, margin = margin(b = 4)),
      plot.caption  = element_text(size = rel(0.85), colour = "grey50",
                                   hjust = 0, margin = margin(t = 4)),
      plot.margin   = margin(8, 8, 6, 8)
    )
}

## ── Nature colour palette (colourblind-friendly) ────────────
nat_pal <- c(
  "#E64B35",  # red
  "#4DBBD5",  # cyan
  "#00A087",  # teal
  "#3C5488",  # navy
  "#F39B7F",  # salmon
  "#8491B4"   # slate
)

income_colors <- c(
  "Less than $15,000"  = "#D73027",
  "$15,000 - $24,999"  = "#F46D43",
  "$25,000 - $34,999"  = "#FDAE61",
  "$35,000 - $49,999"  = "#ABD9E9",
  "$50,000 - $74,999"  = "#4393C3",
  "$75,000 or greater" = "#2166AC"
)

## ── 2. Output Directory ─────────────────────────────────────
if (!dir.exists("figures")) dir.create("figures")

## ── 3. Load & Clean Data ────────────────────────────────────
# Update this path if running from a different working directory
DATA_PATH <- "Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv"

data_obesity <- read.csv(DATA_PATH, encoding = "latin1", stringsAsFactors = FALSE)

ob_q <- "Percent of adults aged 18 years and older who have obesity"
pa_q <- "Percent of adults who engage in no leisure-time physical activity"

income_levels <- c(
  "Less than $15,000", "$15,000 - $24,999", "$25,000 - $34,999",
  "$35,000 - $49,999", "$50,000 - $74,999", "$75,000 or greater"
)

## Income-stratified obesity (Q1)
data_income <- data_obesity %>%
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

## State-level totals (Q2 & Q3)
ob_total <- data_obesity %>%
  filter(Question == ob_q, StratificationCategory1 == "Total") %>%
  select(YearStart, LocationDesc, pct_obese = Data_Value) %>%
  mutate(pct_obese = as.numeric(pct_obese), YearStart = as.numeric(YearStart))

pa_total <- data_obesity %>%
  filter(Question == pa_q, StratificationCategory1 == "Total") %>%
  select(YearStart, LocationDesc, pct_inactive = Data_Value) %>%
  mutate(pct_inactive = as.numeric(pct_inactive), YearStart = as.numeric(YearStart))

state_income_proxy <- data_income %>%
  group_by(LocationDesc, YearStart) %>%
  summarise(
    mean_income_step = weighted.mean(
      income_step,
      w = ifelse(is.na(Sample_Size), 1, Sample_Size),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

df_state <- ob_total %>%
  inner_join(pa_total,          by = c("YearStart", "LocationDesc")) %>%
  left_join(state_income_proxy, by = c("YearStart", "LocationDesc")) %>%
  drop_na(pct_obese, pct_inactive)

df_state_nona <- df_state %>% filter(!is.na(mean_income_step))

## ── 4. Helper: save figure ──────────────────────────────────
save_fig <- function(plot, filename, width = 89, height = 70, units = "mm") {
  # Nature single-column = 89 mm; double-column = 183 mm
  path <- file.path("figures", filename)
  ggsave(path, plot = plot, width = width, height = height,
         units = units, bg = "white")
  message("Saved: ", path)
}

## ============================================================
## FIGURE 1 -- Boxplot: Obesity by Income Category
## ============================================================
fig1 <- ggplot(data_income, aes(x = Income, y = Data_Value, fill = Income)) +
  geom_boxplot(
    alpha = 0.80, linewidth = 0.35,
    outlier.shape = 21, outlier.size = 0.6,
    outlier.fill = "white", outlier.color = "grey50", outlier.stroke = 0.25
  ) +
  stat_summary(
    fun = mean, geom = "point",
    shape = 23, size = 1.8, fill = "white", colour = "black", stroke = 0.5
  ) +
  scale_fill_manual(values = income_colors, guide = "none") +
  scale_x_discrete(labels = function(x) gsub(" - ", "\n", gsub("Less than ", "<", x))) +
  scale_y_continuous(
    labels = label_number(suffix = "%"),
    limits = c(10, 50), breaks = seq(10, 50, 10)
  ) +
  labs(
    title    = "Fig. 1 | Obesity prevalence by income category",
    subtitle = "CDC BRFSS 2011–2024; diamond = group mean",
    x        = "Annual household income",
    y        = "Obesity prevalence (%)"
  ) +
  nature_theme()

save_fig(fig1, "Fig1_obesity_by_income_boxplot.pdf",  width = 89,  height = 75)

## ============================================================
## FIGURE 2 -- Bar chart: Mean obesity ± 95% CI by income
## ============================================================
inc_ci <- data_income %>%
  group_by(Income) %>%
  summarise(
    Mean = mean(Data_Value, na.rm = TRUE),
    SE   = sd(Data_Value,   na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

fig2 <- ggplot(inc_ci, aes(x = Income, y = Mean, fill = Income)) +
  geom_col(alpha = 0.85, width = 0.65, colour = "black", linewidth = 0.25) +
  geom_errorbar(
    aes(ymin = Mean - 1.96 * SE, ymax = Mean + 1.96 * SE),
    width = 0.25, linewidth = 0.4
  ) +
  geom_text(
    aes(label = paste0(round(Mean, 1), "%"), y = Mean + 1.96 * SE + 0.5),
    vjust = 0, size = 2, fontface = "bold"
  ) +
  scale_fill_manual(values = income_colors, guide = "none") +
  scale_x_discrete(labels = function(x) gsub(" - ", "\n", gsub("Less than ", "<", x))) +
  scale_y_continuous(
    labels = label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title    = "Fig. 2 | Mean obesity prevalence by income category",
    subtitle = "Error bars = 95% confidence intervals; CDC BRFSS 2011–2024",
    x        = "Annual household income",
    y        = "Mean obesity prevalence (%)"
  ) +
  nature_theme()

save_fig(fig2, "Fig2_mean_obesity_by_income_bar.pdf", width = 89, height = 70)

## ============================================================
## FIGURE 3 -- Line: Obesity trends by income over time
## ============================================================
data_income_time <- data_income %>%
  group_by(YearStart, Income) %>%
  summarise(Mean_Obesity = mean(Data_Value, na.rm = TRUE), .groups = "drop")

# Label only last year to avoid clutter
label_time <- data_income_time %>% filter(YearStart == max(YearStart))

fig3 <- ggplot(data_income_time,
               aes(x = YearStart, y = Mean_Obesity, colour = Income, group = Income)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.0, stroke = 0.3) +
  geom_text_repel(
    data = label_time,
    aes(label = gsub("Less than ", "<", Income)),
    size = 1.8, direction = "y", hjust = 0,
    nudge_x = 0.3, segment.size = 0.2, max.overlaps = 10
  ) +
  scale_colour_manual(values = income_colors, guide = "none") +
  scale_y_continuous(labels = label_number(suffix = "%"), limits = c(15, 45)) +
  scale_x_continuous(breaks = seq(2011, 2024, 2), expand = expansion(add = c(0, 3))) +
  labs(
    title    = "Fig. 3 | Obesity prevalence trends by income, 2011–2024",
    subtitle = "Annual mean prevalence per income group; CDC BRFSS",
    x        = "Year",
    y        = "Mean obesity prevalence (%)"
  ) +
  nature_theme()

save_fig(fig3, "Fig3_obesity_trends_by_income.pdf", width = 140, height = 80)

## ============================================================
## FIGURE 4 -- Violin + jitter: Full distribution by income
## ============================================================
fig4 <- ggplot(data_income, aes(x = Income, y = Data_Value, fill = Income)) +
  geom_violin(alpha = 0.55, trim = TRUE, linewidth = 0.3) +
  geom_jitter(aes(colour = Income), width = 0.12, alpha = 0.12,
              size = 0.35, stroke = 0) +
  stat_summary(
    fun = median, geom = "crossbar",
    width = 0.45, linewidth = 0.5, colour = "black"
  ) +
  scale_fill_manual(values  = income_colors, guide = "none") +
  scale_colour_manual(values = income_colors, guide = "none") +
  scale_x_discrete(labels = function(x) gsub(" - ", "\n", gsub("Less than ", "<", x))) +
  scale_y_continuous(labels = label_number(suffix = "%"), limits = c(10, 50)) +
  labs(
    title    = "Fig. 4 | Full distribution of obesity by income category",
    subtitle = "Violin = density; dots = observations; crossbar = median; CDC BRFSS 2011–2024",
    x        = "Annual household income",
    y        = "Obesity prevalence (%)"
  ) +
  nature_theme()

save_fig(fig4, "Fig4_obesity_violin_by_income.pdf", width = 89, height = 80)

## ============================================================
## FIGURE 5 -- Scatter: State income proxy vs. obesity
## ============================================================
label_pts5 <- df_state_nona %>%
  filter(YearStart == 2022) %>%
  arrange(desc(pct_obese)) %>%
  slice(c(1:4, (n() - 3):n()))

fig5 <- ggplot(df_state_nona, aes(x = mean_income_step, y = pct_obese)) +
  geom_point(aes(colour = YearStart), alpha = 0.35, size = 0.9, stroke = 0) +
  geom_smooth(method = "lm", se = TRUE,
              colour = nat_pal[4], fill = adjustcolor(nat_pal[4], 0.15),
              linewidth = 0.8) +
  geom_text_repel(
    data = label_pts5, aes(label = LocationDesc),
    size = 1.8, colour = "grey25",
    segment.size = 0.2, max.overlaps = 10
  ) +
  scale_colour_viridis_c(name = "Year", option = "C",
                         breaks = c(2011, 2016, 2021, 2024)) +
  scale_x_continuous(breaks = 1:6,
                     labels = c("1\n(<$15k)", "2", "3", "4", "5", "6\n(≥$75k)")) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title    = "Fig. 5 | State-level income vs. obesity prevalence",
    subtitle = "Income step: 1 = <$15 k, 6 = ≥$75 k; CDC BRFSS 2011–2024",
    x        = "Mean income step (state-year composite)",
    y        = "Obesity prevalence (%)"
  ) +
  nature_theme() +
  theme(legend.position = "right",
        legend.key.height = unit(0.5, "cm"),
        legend.key.width  = unit(0.25, "cm"))

save_fig(fig5, "Fig5_state_income_vs_obesity.pdf", width = 120, height = 80)

## ============================================================
## FIGURE 6 -- Scatter: Physical inactivity vs. obesity
## ============================================================
label_df6 <- df_state %>%
  filter(YearStart == 2023) %>%
  arrange(desc(pct_obese)) %>%
  slice(c(1:4, (n() - 3):n()))

fig6 <- ggplot(df_state, aes(x = pct_inactive, y = pct_obese)) +
  geom_point(aes(colour = YearStart), alpha = 0.35, size = 0.9, stroke = 0) +
  geom_smooth(method = "lm", se = TRUE,
              colour = nat_pal[1], fill = adjustcolor(nat_pal[1], 0.12),
              linewidth = 0.8) +
  geom_text_repel(
    data = label_df6, aes(label = LocationDesc),
    size = 1.8, colour = "grey25",
    segment.size = 0.2, max.overlaps = 10
  ) +
  scale_colour_viridis_c(name = "Year", option = "C",
                         breaks = c(2011, 2016, 2021, 2024)) +
  scale_x_continuous(labels = label_number(suffix = "%")) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title    = "Fig. 6 | Physical inactivity vs. obesity prevalence",
    subtitle = "State-level estimates; CDC BRFSS 2011–2024",
    x        = "Adults with no leisure-time physical activity (%)",
    y        = "Adults with obesity (%)"
  ) +
  nature_theme() +
  theme(legend.position = "right",
        legend.key.height = unit(0.5, "cm"),
        legend.key.width  = unit(0.25, "cm"))

save_fig(fig6, "Fig6_inactivity_vs_obesity.pdf", width = 120, height = 80)

## ============================================================
## FIGURE 7 -- Dual-line: National trends, inactivity & obesity
## ============================================================
trend_df <- df_state %>%
  group_by(YearStart) %>%
  summarise(
    `No leisure-time physical activity` = mean(pct_inactive, na.rm = TRUE),
    `Obesity`                           = mean(pct_obese,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-YearStart, names_to = "Indicator", values_to = "Percent")

fig7 <- ggplot(trend_df, aes(x = YearStart, y = Percent,
                             colour = Indicator, group = Indicator)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5, stroke = 0.4) +
  scale_colour_manual(
    values = c(
      "No leisure-time physical activity" = nat_pal[1],
      "Obesity"                           = nat_pal[4]
    ),
    name = NULL
  ) +
  scale_x_continuous(breaks = 2011:2024) +
  scale_y_continuous(
    labels = label_number(suffix = "%"),
    limits = c(22, 36), breaks = seq(22, 36, 2)
  ) +
  labs(
    title    = "Fig. 7 | National trends in physical inactivity and obesity",
    subtitle = "Mean across U.S. states; CDC BRFSS 2011–2024",
    x        = "Year",
    y        = "Prevalence (%)"
  ) +
  nature_theme() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.spacing.x = unit(0.3, "cm")
  )

save_fig(fig7, "Fig7_national_trends_inactivity_obesity.pdf", width = 140, height = 80)

## ============================================================
## FIGURE 8 (BONUS) -- Multi-panel summary (Figs 5 & 6 combined)
## Useful as a single publication panel for the main text
## ============================================================
library(patchwork)

fig8 <- (fig5 + theme(legend.position = "none")) +
  (fig6 + theme(legend.position = "right")) +
  plot_annotation(
    title = "Fig. 8 | Income and physical inactivity independently predict state-level obesity",
    subtitle = "Left: income composite; right: leisure-time inactivity. Lines = OLS fit ± 95% CI; CDC BRFSS 2011–2024.",
    theme = nature_theme() + theme(plot.title = element_text(size = 8, face = "bold"),
                                   plot.subtitle = element_text(size = 6.5, colour = "grey40"))
  )

save_fig(fig8, "Fig8_combined_panel.pdf", width = 183, height = 85)

## ── Done ────────────────────────────────────────────────────
message("\n✓ All figures saved to ./figures/")
message("Files ready to stage: git add figures/ && git commit -m 'Add Nature-style figures'")