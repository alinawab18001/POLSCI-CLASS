
# Q1 

library(rvest)
library(dplyr)
library(stringr)
library(tidyverse)

url <- "https://en.wikipedia.org/wiki/COVID-19_pandemic_death_rates_by_country"

covid_page <- read_html(url)

covid_raw <- covid_page %>%
  html_element("table.wikitable") %>%
  html_table()

names(covid_raw)
head(covid_raw)


# Q2 While cleaning data, I've:

# Kept country, total deaths, total cases
# Removed commas, and footnote markers ([1], [a], etc.)
# Converted to numeric
# Dropped rows where deaths is 0, cases is 0 or NA, and deaths > cases
# Added a new column: death per case = deaths / cases


covid_clean <- covid_raw %>%
  
  transmute(
    country    = .[[1]],
    deaths_raw = .[[3]],
    cases_raw  = .[[4]]
  ) %>%
  
  mutate(
    country    = str_remove(country, "\\[.*?\\]"),
    deaths_raw = str_remove(deaths_raw, "\\[.*?\\]"),
    cases_raw  = str_remove(cases_raw, "\\[.*?\\]")
  ) %>%
  
  mutate(
    deaths_raw = gsub(",", "", deaths_raw),
    cases_raw  = gsub(",", "", cases_raw)
  ) %>%
  
  mutate(
    deaths_raw = ifelse(deaths_raw == "" | str_detect(deaths_raw, "[^0-9]"),
                        NA_character_, deaths_raw),
    cases_raw  = ifelse(cases_raw  == "" | str_detect(cases_raw,  "[^0-9]"),
                        NA_character_, cases_raw)
  ) %>%
  
  mutate(
    deaths = as.numeric(deaths_raw),
    cases  = as.numeric(cases_raw)
  ) %>%
  
  filter(
    !is.na(deaths),
    !is.na(cases),
    deaths > 0,
    cases  > 0,
    deaths <= cases
  ) %>%
  
  mutate(
    death_per_case = deaths / cases
  )

head(covid_clean, 20)


range(covid_clean$death_per_case, na.rm = TRUE)
any(covid_clean$death_per_case > 1)

View(covid_clean)     
head(covid_clean, 20) 


# Q3 Scatterplot


library(ggplot2)

ggplot(covid_clean, aes(x = cases, y = deaths)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linewidth = 1) +
  labs(
    title = "COVID-19: Total Cases vs Total Deaths by Country",
    x = "Total COVID-19 Cases",
    y = "Total COVID-19 Deaths"
  ) +
  theme_minimal(base_size = 13)


# Q4 Histogram

covid_clean %>%
  ggplot(aes(x = death_per_case)) +
  geom_histogram(bins = 30, fill = "dodgerblue", color = "white") +
  labs(
    title = "Distribution of Deaths per Case Ratio",
    x = "Deaths per Case",
    y = "Number of Countries"
  ) +
  theme_minimal(base_size = 13)


# Q5 Barplot


covid_clean %>%
  filter(death_per_case > 0.03) %>%       # keep only high-fatality countries
  ggplot(aes(x = reorder(country, death_per_case),
             y = death_per_case)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    title = "Countries with Deaths per Case Ratio Greater Than 0.03",
    x = "Country",
    y = "Deaths per Case Ratio"
  ) +
  theme_minimal(base_size = 13)

# Q6 Violin plot

historical_spending <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/historical_spending.csv"
)

glimpse(historical_spending)

# Violin plot of per-person Valentine's spending
ggplot(historical_spending, aes(x = "Per-Person Spending", y = PerPerson)) +
  geom_violin(fill = "plum", alpha = 0.6, color = "black") +
  labs(
    title = "Distribution of Valentine's Day Spending per Person",
    x     = "",
    y     = "Spending per Person (USD)"
  ) +
  theme_minimal(base_size = 13)

# Q7 & Q8
category_cols <- setdiff(
  names(historical_spending),
  c("Year", "PercentCelebrating", "PerPerson")
)

spend_long <- historical_spending %>%
  pivot_longer(
    cols      = all_of(category_cols),
    names_to  = "category",
    values_to = "amount"
  )

head(spend_long)

# Q7 – Stacked bar chart

latest_year <- max(spend_long$Year, na.rm = TRUE)

spend_latest <- spend_long %>%
  filter(Year == latest_year)

ggplot(spend_latest,
       aes(x = factor(Year), y = amount, fill = category)) +
  geom_col() +
  labs(
    title = paste("Valentine's Day Spending by Category in", latest_year),
    x     = "Year",
    y     = "Spending (USD)",
    fill  = "Category"
  ) +
  theme_minimal(base_size = 13)

# Q8 – Over-time line chart

ggplot(spend_long,
       aes(x = Year, y = amount, color = category)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Valentine's Spending Over Time by Category",
    x     = "Year",
    y     = "Spending (USD)",
    color = "Category"
  ) +
  theme_minimal(base_size = 13)


# Q9–Q10: Portfolio dataset

df <- read.csv("Most Wickets in Test Cricket .csv", check.names = FALSE)

df <- df %>%
  rename(
    wickets          = Wickets,
    economy_rate     = `Economy Rate`,
    strike_rate      = `Strike Rate`,
    five_wicket_haul = `5 Wicket Haul`,
    ten_wicket_haul  = `10 Wicket Haul`
  )

# Q9 – Relationship between Career Length and Total Wickets

df <- df %>%
  mutate(
    start_year = as.numeric(substr(Span, 1, 4)),
    end_year   = as.numeric(substr(Span, 6, 9)),
    career_length = end_year - start_year + 1
  )

df_span <- df %>%
  filter(!is.na(career_length),
         career_length > 0,
         !is.na(wickets))

# Scatter + smooth line: wickets vs career length
ggplot(df_span, aes(x = career_length, y = wickets)) +
  geom_point(color = "darkred", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1) +
  labs(
    title = "Relationship between Career Length and Total Wickets",
    x     = "Career Length (Years)",
    y     = "Total Wickets"
  ) +
  theme_minimal(base_size = 13)

# Q10 – Pie Chart of Countries by Number of 5-Wicket Hauls

country_5w <- df %>%
  group_by(Country) %>%
  summarise(
    total_5w = sum(five_wicket_haul, na.rm = TRUE)
  ) %>%
  filter(total_5w > 0)

ggplot(country_5w, aes(x = "", y = total_5w, fill = Country)) +
  geom_col(color = "white") +
  coord_polar("y") +
  labs(
    title = "Share of 5-Wicket Hauls by Country",
    x     = NULL,
    y     = NULL,
    fill  = "Country"
  ) +
  theme_void() +
  theme(legend.position = "right")
