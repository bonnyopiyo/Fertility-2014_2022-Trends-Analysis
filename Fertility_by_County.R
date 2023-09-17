library(readr)
library(dplyr)
library(tidyr)
library(gt)
library(gtExtras)
library(tidyverse)
library(data.table)
fertility_data <- fread("C:/KDHS/data/fertility_data_by_county_2014_2022.csv")
names(fertility_data)
df <- fertility_data |>
  setNames(c("County", "2014_TFR", "2014_PREG", "2014_CHILD", "2022_TFR", "2022_PREG",
             "2022_CHILD")) |> 
  rowwise() |> 
  mutate(change_TFR = list(c(`2014_TFR`, `2022_TFR`)),
         change_PREG = list(c(`2014_PREG`, `2022_PREG`)),
         change_CHILD = list(c(`2014_CHILD`, `2022_CHILD`))
  ) |> 
  select(County, `2014_TFR`, `2022_TFR`, change_TFR, 
         `2014_PREG`, `2022_PREG`, change_PREG,
         `2014_CHILD`, `2022_CHILD`, change_CHILD)
s <- "black"
fv <- "black"
rl <- "black"
rh <- "black"
t <- "black"

c_p <- c(s, fv, rl, rh, t)

table <- df |> 
  as_tibble() |> 
  gt() |> 
  gt_plt_sparkline(change_TFR, label = F, fig_dim = c(4, 8), palette = c_p) |> 
  gt_plt_sparkline(change_PREG, label = F, fig_dim = c(4, 8), palette = c_p) |> 
  gt_plt_sparkline(change_CHILD, label = F, fig_dim = c(4, 8), palette = c_p) |> 
  tab_header(
    title = "Fertility Levels & Determinats of Fertility (2014 vs. 2022)",
    subtitle = "Trends in Fertility Indicators"
  ) |> 
  tab_spanner(
    label = "Total Fertility Rate",
    columns = `2014_TFR` : `2022_TFR`
  )|> 
  tab_spanner(
    label = "Percentage of Women aged (15-49) who are currently pregnant",
    columns = `2014_PREG` : `2022_PREG`
  )|> 
  tab_spanner(
    label = "Mean number of children ever born to women aged 40-49",
    columns = `2014_CHILD` : `2022_CHILD`
  ) |> 
  cols_label(
    County = "County Name",
    `2014_TFR` = "2014",
    `2022_TFR` = "2022",
    change_TFR = "Trend",
    `2014_PREG` = "2014",
    `2022_PREG` = "2022",
    change_PREG = "Trend",
    `2014_CHILD` = "2014",
    `2022_CHILD` = "2022",
    change_CHILD = "Trend"
    
  ) |> 
  cols_align("left") |> 
  cols_width(
    County ~ px(130),
    `2014_TFR` ~ px(50),
    `2022_TFR` ~ px(50),
    change_TFR ~ px(50),
    `2014_PREG` ~ px(50),
    `2022_PREG` ~ px(50),
    change_PREG ~ px(50),
    `2014_CHILD` ~ px(50),
    `2022_CHILD` ~ px(50),
    change_CHILD ~ px(50)
  )

custom_colors <- c("red3", "green4")

table <- table |> 
  data_color(
    columns = c(
      `2014_TFR`,
      `2022_TFR`,
      `2014_PREG`,
      `2022_PREG`,
      `2014_CHILD`,
      `2022_CHILD`
    ),
    palette = custom_colors,
    direction = "column"
  ) |> 
  opt_table_lines(extent = "all") |> 
  tab_options(
    heading.title.font.size = 20,
    table.font.size = 10,
    heading.subtitle.font.size = 12,
    column_labels.font.weight = "bold",
  )

print(table)