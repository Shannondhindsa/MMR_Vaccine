


# =============================================================================
# MMR Vaccination Coverage Analysis
# Author: Shannon Dhindsa
# Course:  EPH505
# Purpose: Calculate MMR vaccination coverage by community in Seattle and age group
#          as of December 31, 2024, and visualize gaps below the 95% target.
# =============================================================================


# -----------------------------------------------------------------------------
# 1. SETUP
# -----------------------------------------------------------------------------

# Clear environment and close any open graphics
rm(list = ls())
graphics.off()

# Load packages
library(tidyverse)   # dplyr, ggplot2, tidyr, stringr, etc.
library(lubridate)   # date handling
library(janitor)     # clean_names()
library(scales)      # percent formatting for plots

# Set working directory
setwd("/Users/shannondhindsa/Downloads/EPH505/Labs/code")

# Define the analysis "as of" date — used for age calculation
as_of_date <- as_date("2024-12-31")


# -----------------------------------------------------------------------------
# 2. LOAD DATA
# -----------------------------------------------------------------------------

mmr_raw        <- read.csv("Dataset B.csv")
population_raw <- read.csv("Population Data.csv")


# -----------------------------------------------------------------------------
# 3. CLEAN POPULATION DATA (denominator)
# -----------------------------------------------------------------------------
# Keep only December 2024 records for the two age groups of interest.

pop_denom <- population_raw %>%
  clean_names() %>%
  mutate(
    age_group        = as.character(age_group),
    month            = as.character(month),
    year             = as.integer(year)
  ) %>%
  filter(
    year      == 2024,
    month     == "December",
    age_group %in% c("2-6", "7-11")
  ) %>%
  select(community, age_group, total_population)


# -----------------------------------------------------------------------------
# 4. CLEAN MMR DATA (numerator)
# -----------------------------------------------------------------------------
# Keep only valid MMR doses, then assign each child to an age group
# based on their age as of Dec 31, 2024.

mmr_clean <- mmr_raw %>%
  clean_names() %>%
  mutate(
    dob       = as_date(dob),
    vacc_date = as_date(vacc_date),
    dose      = str_to_lower(dose),
    status    = str_to_lower(status),
    community = as.character(community)
  ) %>%
  filter(
    agent  == "MMR",
    status == "valid"
  ) %>%
  mutate(
    age_years = floor(time_length(interval(dob, as_of_date), "years")),
    age_group = case_when(
      age_years >= 2 & age_years <= 6  ~ "2-6",
      age_years >= 7 & age_years <= 11 ~ "7-11",
      TRUE                             ~ NA_character_
    )
  ) %>%
  filter(!is.na(age_group))


# -----------------------------------------------------------------------------
# 5. BUILD NUMERATOR: one row per child, then count by community + age group
# -----------------------------------------------------------------------------
# Assumption: a child is "fully vaccinated" once they have received dose 2.

num <- mmr_clean %>%
  # Collapse to one row per child
  group_by(id, community, age_group) %>%
  summarise(
    has_dose1 = any(dose == "dose1"),
    has_dose2 = any(dose == "dose2"),
    fully_vax = any(dose == "dose2"),
    .groups   = "drop"
  ) %>%
  # Count children per community / age group
  summarise(
    dose1_n     = sum(has_dose1),
    dose2_n     = sum(has_dose2),
    fully_vax_n = sum(fully_vax),
    .by         = c(community, age_group)
  )


# -----------------------------------------------------------------------------
# 6. JOIN NUMERATOR + DENOMINATOR → COVERAGE
# -----------------------------------------------------------------------------

coverage <- pop_denom %>%
  left_join(num, by = c("community", "age_group")) %>%
  mutate(
    # Replace NAs (communities with zero vaccinated kids) with 0
    across(c(dose1_n, dose2_n, fully_vax_n), ~ replace_na(.x, 0L)),
    dose1_cov = dose1_n     / total_population,
    dose2_cov = dose2_n     / total_population,
    fully_cov = fully_vax_n / total_population
  )


# -----------------------------------------------------------------------------
# 7. KPI: overall coverage by age group (communities collapsed)
# -----------------------------------------------------------------------------

kpi <- coverage %>%
  summarise(
    total_pop       = sum(total_population),
    fully_vax_total = sum(fully_vax_n),
    fully_cov       = fully_vax_total / total_pop,
    .by             = age_group
  )

print(kpi)


# -----------------------------------------------------------------------------
# 8. HEATMAP: coverage by community × age group
# -----------------------------------------------------------------------------
# Tiles below the 95% target are outlined in black.

heatmap_data <- coverage %>%
  select(community, age_group, fully_cov) %>%
  mutate(
    age_group = factor(age_group, levels = c("2-6", "7-11")),
    community = factor(community)
  )

ggplot(heatmap_data, aes(x = community, y = age_group, fill = fully_cov)) +
  geom_tile(aes(color = fully_cov < 0.95), linewidth = 0.7) +
  geom_text(aes(label = percent(fully_cov, accuracy = 1)), size = 3) +
  scale_fill_gradient(
    low    = "#f4cccc",
    high   = "#38761d",
    limits = c(0, 1),
    labels = percent_format(accuracy = 1),
    name   = "Coverage"
  ) +
  scale_color_manual(
    values = c("FALSE" = "white", "TRUE" = "black"),
    guide  = "none"
  ) +
  labs(
    title    = "MMR Fully Vaccinated Coverage by Community and Age Group",
    subtitle = "Black outline indicates coverage below the 95% target",
    x        = "Community",
    y        = "Age group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid  = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

# -----------------------------------------------------------------------------
# 9. EXPORT OUTPUTS
# -----------------------------------------------------------------------------

# Save the heatmap
ggsave(
  filename = "mmr_coverage_heatmap.png",
  width    = 10,
  height   = 5,
  dpi      = 300
)

# Save the KPI summary
write.csv(kpi, "kpi_summary.csv", row.names = FALSE)

# Save the full coverage table
write.csv(coverage, "coverage_by_community.csv", row.names = FALSE)