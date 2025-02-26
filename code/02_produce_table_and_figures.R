# Load libraries ----------------------------------------------------------

library(tidyverse)
library(gt)
library(readxl)
library(writexl)
library(gtExtras)
library(ggplot2)
library(ggrepel)
library(scales)
library(cowplot)

# Load and prepare data ---------------------------------------------------

# World bank country list by income classification
wb_country_list <- read_excel("data/OGHIST.xlsx", sheet = "Country_cat",
                              n_max = 219)

# Retain "Low" and "Low-Medium" in 2020
llm_2020 <- wb_country_list |> 
  filter(FY20 %in% c("L", "LM"))

# Read the files output by GEE
pa_pop <- "data/Population_GEE_output" |> # Where GEE output is stored
  list.files(pattern = "\\.csv$", full.names = TRUE) |> 
  map_dfr(read_csv, col_types = cols(shapeGroup = col_character()),
          show_col_types = FALSE) |> 
  select(-.geo, -`system:index`)

# Aggregate by country if split by ADM1 and make %
pa_pop <- pa_pop |> 
  rename(ISO3 = shapeGroup) |> 
  group_by(ISO3) |> 
  summarise(
    country_area_km2 = first(country_area_km2),
    across(starts_with("pa_area"), \(x) sum(x, na.rm = TRUE)),
    across(starts_with("pop"), \(x) sum(x, na.rm = TRUE))
  ) |> 
  mutate(
    across(starts_with("pa_area"), ~ .x / country_area_km2 * 100, .names = "{.col}_pct"),
    across(starts_with("pop2000"), ~ .x / pop2000_total * 100, .names = "{.col}_pct"),
    across(starts_with("pop2020"), ~ .x / pop2020_total * 100, .names = "{.col}_pct")
  ) %>%
  filter(ISO3 %in% llm_2020$ISO3) %>%
  left_join(select(llm_2020, ISO3, country = COUNTRY), by = "ISO3")

# Rename for clarity
pa_pop_summary <- pa_pop |> 
  select(country, ISO3,
         pa_land_2000 = pa_area_2000_km2_pct,
         pa_land_2020 = pa_area_2020_km2_pct,
         pa_pop_2000 = pop2000_in_pa_pct,
         pa_pop_2020 = pop2020_in_pa_pct,
         pa10_land_2000 = pa_area10km_2000_km2_pct,
         pa10_land_2020 = pa_area10km_2020_km2_pct,
         pa10_pop_2000 = pop2000_in_pa10_pct,
         pa10_pop_2020 = pop2020_in_pa10_pct) |> 
  arrange(country)

# Compute "Total" (all countries)
pa_pop_total <- pa_pop %>%
  summarise(
    country = "Total",
    country_area_km2 = sum(country_area_km2, na.rm = TRUE),
    across(starts_with("pa_area"), sum, na.rm = TRUE),
    across(starts_with("pop"), sum, na.rm = TRUE)
  ) %>%
  mutate(
    across(starts_with("pa_area"), ~ .x / country_area_km2 * 100, .names = "{.col}_pct"),
    across(starts_with("pop2000"), ~ .x / pop2000_total * 100, .names = "{.col}_pct"),
    across(starts_with("pop2020"), ~ .x / pop2020_total * 100, .names = "{.col}_pct")
  ) %>%
  select(
    country,
    pa_land_2000 = pa_area_2000_km2_pct,
    pa_land_2020 = pa_area_2020_km2_pct,
    pa_pop_2000 = pop2000_in_pa_pct,
    pa_pop_2020 = pop2020_in_pa_pct,
    pa10_land_2000 = pa_area10km_2000_km2_pct,
    pa10_land_2020 = pa_area10km_2020_km2_pct,
    pa10_pop_2000 = pop2000_in_pa10_pct,
    pa10_pop_2020 = pop2020_in_pa10_pct
  )

# Compute "Total without India"
pa_pop_total_no_india <- pa_pop %>%
  filter(ISO3 != "IND") %>% # Exclude India
  summarise(
    country = "Total without India",
    country_area_km2 = sum(country_area_km2, na.rm = TRUE),
    across(starts_with("pa_area"), sum, na.rm = TRUE),
    across(starts_with("pop"), sum, na.rm = TRUE)
  ) %>%
  mutate(
    across(starts_with("pa_area"), ~ .x / country_area_km2 * 100, .names = "{.col}_pct"),
    across(starts_with("pop2000"), ~ .x / pop2000_total * 100, .names = "{.col}_pct"),
    across(starts_with("pop2020"), ~ .x / pop2020_total * 100, .names = "{.col}_pct")
  ) %>%
  select(
    country,
    pa_land_2000 = pa_area_2000_km2_pct,
    pa_land_2020 = pa_area_2020_km2_pct,
    pa_pop_2000 = pop2000_in_pa_pct,
    pa_pop_2020 = pop2020_in_pa_pct,
    pa10_land_2000 = pa_area10km_2000_km2_pct,
    pa10_land_2020 = pa_area10km_2020_km2_pct,
    pa10_pop_2000 = pop2000_in_pa10_pct,
    pa10_pop_2020 = pop2020_in_pa10_pct
  )

# Bind both totals to main dataset
pa_pop_summary_with_total <- bind_rows(pa_pop_total, pa_pop_total_no_india, pa_pop_summary)

#Save for in-text citation
save(pa_pop, pa_pop_total, pa_pop_total_no_india, 
     file = "results/pa_pop_total.rds")

# Produce Table 1 --------------------------------------------------------

# Generate the gt table with correct structure
pa_pop_summary_with_total %>%
  select(-ISO3) %>%
  gt() %>%
  tab_header(
    title = "Table 1: PA Coverage and Population Proximity (2000-2020)",
    subtitle = "Percentage of land and population within and near protected areas"
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 1
  ) %>%
  # First-level spanners (Land and Population)
  tab_spanner(label = "Land", 
              columns = c(pa_land_2000, pa_land_2020), 
              id = "land_pa") %>%
  tab_spanner(label = "Population", 
              columns = c(pa_pop_2000, pa_pop_2020), 
              id = "pop_pa") %>%
  tab_spanner(label = "Land", 
              columns = c(pa10_land_2000, pa10_land_2020), 
              id = "land_10km") %>%
  tab_spanner(label = "Population", 
              columns = c(pa10_pop_2000, pa10_pop_2020), 
              id = "pop_10km") %>%
  # Second-level spanners (% within PAs and % within 10km of PAs)
  tab_spanner(label = "% within PAs", spanners = c("land_pa", "pop_pa")) %>%
  tab_spanner(label = "% within 10km of PAs", spanners = c("land_10km", "pop_10km")) %>%
  cols_label(
    country = "Country",
    pa_land_2000 = "2000", pa_land_2020 = "2020",
    pa_pop_2000 = "2000", pa_pop_2020 = "2020",
    pa10_land_2000 = "2000", pa10_land_2020 = "2020",
    pa10_pop_2000 = "2000", pa10_pop_2020 = "2020"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_options(
    table.font.size = px(11),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(12),
    data_row.padding = px(0),
    column_labels.padding = px(0)
  ) %>%
  tab_source_note(
    source_note = "Source: Analysis based on WDPA & WorldPop data (2000-2020)"
  ) %>%
  
  # **Thin vertical lines for all columns**
  tab_style(
    style = cell_borders(sides = "right", color = "gray80", weight = px(0.5)),
    locations = cells_body(columns = everything())
  ) %>%
  
  # **Thick vertical line between "% within PAs" and "% within 10km of PAs"**
  tab_style(
    style = cell_borders(sides = "right", color = "black", weight = px(3)),
    locations = cells_body(columns = c(pa_pop_2020)) # Border after last column in "% within PAs"
  ) %>%
  
  # **Thick vertical line in headers (for clarity)**
  tab_style(
    style = cell_borders(sides = "right", color = "black", weight = px(3)),
    locations = cells_column_labels(columns = c(pa_pop_2020))
  ) %>%
  
  # **Medium vertical lineq"**
  tab_style(
    style = cell_borders(sides = "right", color = "gray40", weight = px(3)),
    locations = cells_body(columns = c(pa_land_2020, pa10_land_2020))
  ) %>%
  tab_style(
    style = list(cell_fill(color = "lightgray"), cell_text(weight = "bold")),
    locations = cells_body(rows = country %in% c("Total", "Total without India"))
  ) -> table_data

# Save in different formats
gtsave(table_data, "results/table1.html")
gtsave(table_data, "results/table1.tex")
gtsave(table_data, "results/table1.docx")
write_rds(table_data, "results/table1.rds")

# Produce Figure 1 ---------------------------------------------------------

# % of Population Near PAs
ggplot(pa_pop_summary, aes(x = reorder(country, pa10_pop_2020))) +
  geom_point(aes(y = pa10_pop_2000), color = "gray", size = 3) +
  geom_point(aes(y = pa10_pop_2020), color = "steelblue", size = 3) +
  geom_segment(aes(y = pa10_pop_2000, yend = pa10_pop_2020, xend = country, 
                   color = pa10_pop_2020 > pa10_pop_2000), linewidth = 1.2) +
  scale_color_manual(values = c("red", "steelblue"), 
                     labels = c("Decrease", "Increase")) +
  coord_flip() +
  labs(
    title = "Figure 1: Change in % Population Near PAs (2000-2020)",
    x = NULL,
    y = "% of Population",
    color = "Trend"
  ) +
  theme_minimal() -> figure1

ggsave("results/figure1.png", plot = figure1, width = 8, height = 9, ,
       unit = "in", dpi = 300)

# Produce Figure 2 ---------------------------------------------------------

# Prepare a note with the country names
country_note <- llm_2020 %>%
  select(ISO3, country = COUNTRY) %>%
  arrange(ISO3) %>%
  mutate(text = paste0(ISO3, ": ", country)) %>%
  pull(text) %>%
  paste(collapse = ", ")

# Define zoom parameters
x_zoom1 = 1 # x limit
y_zoom1 = 1 # y limit
col_zoom1 = "purple" # zoom box color

# Compute trend and filter to avoid inflated growth ratios
pa_pop_summary2 <- pa_pop_summary |> 
  mutate(var_land = (pa_land_2020 - pa_land_2000) / pa_land_2000,
         var_pop = (pa10_pop_2020 - pa10_pop_2000) / pa10_pop_2000) |> 
  filter(pa_land_2000 > 1)


# Compute the number of countries in the subsample
num_countries <- nrow(pa_pop_summary2)



# Create a dummy dataframe for the legend entry
legend_line <- data.frame(x = c(0, 1), y = c(0, 1), type = "Equal PA & Population Growth")

# Create plot
my_plot1 <- pa_pop_summary2 |> 
  ggplot(aes(x = var_land, y = var_pop, label = ISO3)) +
  geom_point(color = "blue", alpha = 0.7) +
  annotate("rect", xmin = 0, xmax = x_zoom1, ymin = 0, ymax = y_zoom1, 
           color = col_zoom1, fill = NA, linewidth = 1) +
  geom_text_repel(size = 3) +  # Add country labels
  geom_abline(slope = 1, intercept = 0, color = "green") +  
  geom_line(data = legend_line, aes(x = x, y = y, linetype = type), 
            color = "green", inherit.aes = FALSE) +  # Ensure it does not inherit global aes()
  scale_linetype_manual(name = NULL, values = c("Equal PA & Population Growth" = "solid")) + 
  labs(
    title = "Evolution of PA Land Coverage and Nearby Population Share (2000-2020)",
    x = "Growth in Land Share Covered by PAs", 
    y = "Growth in Population Share Within 10km of PAs",
    subtitle = paste0("Subsample of ", nrow(pa_pop_summary2), 
                      " countries classified as Low- or Low-Middle Income in 2020, ",
                      "with at least\n1% PA coverage in 2000")  
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, size = 10, face = "italic"),
    legend.position = "right"
  )

# Create zoom by modifying plot
my_plot_zoom1 <- my_plot1 +
  scale_x_continuous(breaks = seq(0, x_zoom1, by = 1), limits = c(0, x_zoom1)) +  
  scale_y_continuous(breaks = seq(0, y_zoom1, by = 1), limits = c(0, y_zoom1)) +
  labs(title = NULL, subtitle = NULL, x = " ", y = " ",
       caption = str_wrap(paste("Country codes:", country_note), 
                          width = 140)) +   # Remove title and labels
  theme(
    plot.caption = element_text(size = 8, hjust = 0)  # Adjust caption size & alignment
  )
# Remove legend from plot
my_plot1 <- my_plot1 + theme(legend.position = "none")

# Arrange using cowplot
final_plot1 <- plot_grid(
  my_plot1,
  my_plot_zoom1,  # Zoomed plot (2/3) and legend (1/3)
  nrow = 2,
  rel_heights = c(1,1)  # Main plot takes more height
)

# Display the final arranged plot
save_plot("results/figure2.png", plot = final_plot1, base_width = 8, 
          base_height = 9, unit = "in", dpi = 300)

# Produce Figure 3 --------------------------------------------------------

library(ggplot2)
library(ggrepel)
library(scales)
library(cowplot)

# Define zoom parameters
x_zoom2 = 3 # x limit
y_zoom2 = 5 # y limit
col_zoom2 = "purple" # zoom box color


# Scatter Plot: PA Coverage vs Population Proximity
pa_pop_summary_with_total2 <- pa_pop_summary_with_total |> 
  left_join(select(pa_pop, country, pop2020_total), by = "country")

# Determine the max values for dynamic axis limits
max_x <- max(pa_pop_summary_with_total2$pa_land_2020, na.rm = TRUE)
max_y <- max(pa_pop_summary_with_total2$pa10_pop_2020, na.rm = TRUE)

# 1. Create my_plot with the legend
my_plot2 <- ggplot(pa_pop_summary_with_total2, aes(x = pa_land_2020, 
                                                   y = pa10_pop_2020, 
                                                   size = pop2020_total / 1e6)) +
  annotate("rect", xmin = 0, xmax = x_zoom2, ymin = 0, ymax = y_zoom2, 
           color = col_zoom2, fill = NA, linewidth = 1) +
  geom_point(alpha = 0.7, color = "blue") +
  geom_text_repel(aes(label = ISO3), size = 3) +  
  scale_size_continuous(
    breaks = c(10, 100, 200),
    labels = c("10", "100", "200"),
    guide = guide_legend(title = "Total Population (millions)", 
                         title.position = "top")
  ) +
  scale_x_continuous(breaks = seq(0, max_x, by = 10), limits = c(0, max_x)) +  
  scale_y_continuous(breaks = seq(0, max_y, by = 10), limits = c(0, max_y)) +  
  labs(
    title = "Figure 3: PA Coverage vs Population Near PAs (2020)",
    x = "% of Country Covered by PAs", 
    y = "% of Population Near PAs"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80", size = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = "right"  # Keep legend for extraction
  )

# 4. Create my_plot_zoom by modifying my_plot
my_plot_zoom2 <- my_plot2 +
  scale_x_continuous(breaks = seq(0, x_zoom2, by = 1), limits = c(0, x_zoom2)) +  
  scale_y_continuous(breaks = seq(0, y_zoom2, by = 1), limits = c(0, y_zoom2)) +
  labs(title = NULL, x = " ", y = " ",
       caption = str_wrap(paste("Country codes:", country_note), 
                          width = 140))  +
  theme(
    plot.caption = element_text(size = 8, hjust = 0)  # Adjust caption size & alignment
  )

# 3. Remove legend from my_plot
my_plot2 <- my_plot2 + theme(legend.position = "none")

# 5. Arrange using cowplot
final_plot2 <- plot_grid(
  my_plot2,
  my_plot_zoom2,  # Zoomed plot (2/3) and legend (1/3)
  nrow = 2,
  rel_heights = c(1, 1)  # Main plot takes more height
)

# Display the final arranged plot
save_plot("results/figure3.png", plot = final_plot2, base_width = 8, 
          base_height = 9, unit = "in", dpi = 300)


# Estimations from Naidoo 2019 --------------------------------------------

naidoo_tb <- read_csv("data/Naidoo2019-TableS1.csv", 
                      col_types = cols(`Survey Years` = col_character())) |> 
  mutate(perc_households = (Households_10km_PA / Households_n) * 100)

perc_hh_10km_pa <- naidoo_tb |> 
  summarise(mean_perc_hh = mean(perc_households))

# perc_hh_10km_pa
# # A tibble: 1 × 1
# mean_perc_hh
# <dbl>
#   1        0.222

test <- naidoo_tb |> 
  inner_join(pa_pop |> 
              mutate(country = case_when(
                country == "Eswatini" ~ "Swaziland",
                .default = country)),
            by = c("Country" = "country")) |> 
  select(Country, `Survey Years`, Survey_households_n = Households_n, 
         Survey_perc_hh_10kmPA = perc_households,
         pop2000_in_pa10_pct, pop2020_in_pa10_pct) |> 
  mutate(consistent = (Survey_perc_hh_10kmPA > pop2000_in_pa10_pct) & 
           (Survey_perc_hh_10kmPA < pop2020_in_pa10_pct))
