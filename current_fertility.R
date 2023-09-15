current_fertility_2014_22 <- fread("C:/KDHS/data/current_fertility.csv")
colnames(current_fertility_2014_22)
head(current_fertility_2014_22)

current_fertility_2014_22 <- current_fertility_2014_22 %>%
  mutate(
    Change_Urban = `2022_Urban` - `2014_Urban`,
    Change_Rural = `2022_Rural` - `2014_Rural`
  )

s <- "black"
fv <- "black"
rl <- "black"
rh <- "black"
t <- "black"

c_p <- c(s, fv, rl, rh, t)

# Reorder the columns as specified
current_fertility_2014_22 <- current_fertility_2014_22 %>%
  select(
    `Age group`,
    `2014_Urban`,
    `2022_Urban`,
    Change_Urban,
    `2014_Rural`,
    `2022_Rural`,
    Change_Rural
  )

# Create a gt table
table <- current_fertility_2014_22 %>%
  gt() %>%
  tab_header(
    title = "Fertility Trends (2014 vs. 2022)",
    subtitle = "Comparison of Urban and Rural Trends by Age Group"
  ) %>%
  cols_label(
    `2014_Urban` = "Urban 2014",
    `2022_Urban` = "Urban 2022",
    Change_Urban = "Trend",
    `2014_Rural` = "Rural 2014",
    `2022_Rural` = "Rural 2022",
    Change_Rural = "Trend"
  ) %>%
  cols_width(
    `Age group` ~ px(80),
    everything() ~ px(60)
  )

custom_colors <- c("red3", "green4")

table <- table |> 
  data_color(
    columns = c(
      `2014_Urban`,
      `2022_Urban`,
      `2014_Rural`,
      `2022_Rural`,
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
