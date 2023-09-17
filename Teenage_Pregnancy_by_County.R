teenage_pregancy_2014_22 <- fread("C:/KDHS/data/teenage_pregancy_2014_22.csv")
colnames(teenage_pregancy_2014_22)

df <- teenage_pregancy_2014_22 |> 
  setnames(c("County", "2014_had a live birth", "2014_pregnant with first child", "2014_begun child bearing",
             "2022_had a live birth", "2022_pregnancy loss", "2022_currently pregnant", "2022_begun child bearing"))|> 
  rowwise() |> 
  mutate(Change_live_birth = list(c(`2014_had a live birth`,`2022_had a live birth`)),
         Change_P1st_child = list(c(`2014_pregnant with first child`,`2022_currently pregnant`)),
         Change_BCB = list(c(`2014_begun child bearing`,`2022_begun child bearing`))
         ) |> 
  select(`County`, `2014_had a live birth`, `2022_had a live birth`, Change_live_birth,
         `2014_pregnant with first child`, `2022_currently pregnant`, Change_P1st_child,
         `2014_begun child bearing`,`2022_begun child bearing`, Change_BCB
       )

s <- "black"
fv <- "black"
rl <- "black"
rh <- "black"
t <- "black"

c_p <- c(s, fv, rl, rh, t)

table <- df |> 
  as_tibble() |> 
  gt() |> 
  gt_plt_sparkline(Change_live_birth, label = F, fig_dim = c(4, 8), palette = c_p) |> 
  gt_plt_sparkline(Change_P1st_child, label = F, fig_dim = c(4, 8), palette = c_p) |>
  gt_plt_sparkline(Change_BCB, label = F, fig_dim = c(4, 8), palette = c_p) |> 
  tab_header(
    title = "Fertility Trends (2014 vs. 2022)",
    subtitle = "Teenage Pregnancy by County"
  ) |> 
  tab_spanner(
    label = "Had a live birth",
    columns = `2014_had a live birth`:`2022_had a live birth`
  )|> 
  tab_spanner(
    label = "Currently Pregnant",
    columns = `2014_pregnant with first child`: `2022_currently pregnant`
  )|>
  tab_spanner(
    label = "Begun Child Bearing",
    columns = `2014_begun child bearing`:`2022_begun child bearing`
  ) |> 
  cols_label(
    `County` = "County Name", 
    `2014_had a live birth`= "2014", 
    `2022_had a live birth`= "2022", 
    Change_live_birth = "Trend",
    `2014_pregnant with first child` = "2014", 
    `2022_currently pregnant` = "2022", 
    Change_P1st_child = "Trend",
    `2014_begun child bearing` = "2014",
    `2022_begun child bearing` = "2022", 
    Change_BCB = "Trend"
    ) |>
    cols_width(
      `County` ~ px(80),
      everything() ~ px(60)
    )
  
  # Set custom colors for sparklines
  custom_colors <- c("red3", "green4")
  
  table <- table |> 
    data_color(
      columns = c(
        `2014_had a live birth`, 
        `2022_had a live birth`, 
        `2014_pregnant with first child`, 
        `2022_currently pregnant`, 
        `2014_begun child bearing`,
        `2022_begun child bearing`
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
  # Print the table
  print(table)