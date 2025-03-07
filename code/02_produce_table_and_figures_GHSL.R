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
library(stddiff)
# Load and prepare data ---------------------------------------------------

# World bank country list by income classification
wb_country_list <- read_excel("data/OGHIST.xlsx", sheet = "Country_cat",
                              n_max = 219)

# Retain "Low" and "Low-Medium" in 2020
llm_2020 <- wb_country_list |> 
  filter(FY20 %in% c("L", "LM")) |> 
  filter(ISO3 != "TLS", ISO3 != "SSD") # countries that did not exist in 2000


# Function to read and process GEE output
process_gee_data <- function(folder_path, pop_source) {
  list.files(folder_path, pattern = "\\.csv$", full.names = TRUE) |> 
    map_dfr(read_csv, col_types = cols(shapeGroup = col_character()), show_col_types = FALSE) |> 
    select(-.geo, -`system:index`) |> 
    rename(ISO3 = shapeGroup) |> 
    group_by(ISO3) |> 
    summarise(
      country_area_km2 = first(country_area_km2),
      across(starts_with("pa_area"), \(x) sum(x, na.rm = TRUE)),
      across(starts_with("pop"), \(x) sum(x, na.rm = TRUE))
    ) |> 
    mutate(
      across(starts_with("pa_area"), ~ .x / country_area_km2 * 100, .names = "{.col}_pct"),
      across(starts_with("pop2000"), ~ .x / pop2000_total * 100, .names = paste0("{.col}_", pop_source, "_pct")),
      across(starts_with("pop2020"), ~ .x / pop2020_total * 100, .names = paste0("{.col}_", pop_source, "_pct"))
    ) %>%
    filter(ISO3 %in% llm_2020$ISO3) %>%
    left_join(select(llm_2020, ISO3, country = COUNTRY), by = "ISO3")
}

# Read and process both datasets
pa_pop_worldpop <- process_gee_data("data/Output_GEE_Worldpop", "worldpop")
pa_pop_ghsl <- process_gee_data("data/Output_GEE_GHSL", "ghsl") 

# Merge without duplicating `pa_area` fields
pa_pop_combined <- full_join(pa_pop_worldpop, pa_pop_ghsl, 
                             by = c("ISO3", "country"),
                             suffix = c("_worldpop", "_ghsl")) %>%
  # areas are the same btw wordlpop and ghsl, we rename worlpop
  rename(country_area_km2 = country_area_km2_worldpop,
         pa_area_2000_km2 = pa_area_2000_km2_worldpop,
         pa_area_2020_km2 = pa_area_2020_km2_worldpop,
         pa_area10km_2000_km2 = pa_area10km_2000_km2_worldpop,
         pa_area10km_2020_km2 = pa_area10km_2020_km2_worldpop,
         pa_area_2000_km2_pct = pa_area_2000_km2_pct_worldpop,
         pa_area_2020_km2_pct = pa_area_2020_km2_pct_worldpop,
         pa_area10km_2000_km2_pct = pa_area10km_2000_km2_pct_worldpop,
         pa_area10km_2020_km2_pct = pa_area10km_2020_km2_pct_worldpop) %>%
  # and remove the ghsl
  select(-country_area_km2_ghsl, -pa_area_2000_km2_ghsl, -pa_area_2020_km2_ghsl,
         -pa_area10km_2000_km2_ghsl, pa_area10km_2020_km2_ghsl)

# Compute "Total" (all countries)
compute_totals <- function(data, label) {
  data %>%
    summarise(
      country = label,
      country_area_km2 = sum(country_area_km2, na.rm = TRUE),
      
      # Compute total PA areas in km2 (not percentages)
      pa_area_2000_km2 = sum(pa_area_2000_km2, na.rm = TRUE),
      pa_area_2020_km2 = sum(pa_area_2020_km2, na.rm = TRUE),
      pa_area10km_2000_km2 = sum(pa_area10km_2000_km2, na.rm = TRUE),
      pa_area10km_2020_km2 = sum(pa_area10km_2020_km2, na.rm = TRUE),
      
      # Compute total population counts
      pop2000_total_worldpop = sum(pop2000_total_worldpop, na.rm = TRUE),
      pop2020_total_worldpop = sum(pop2020_total_worldpop, na.rm = TRUE),
      pop2000_total_ghsl = sum(pop2000_total_ghsl, na.rm = TRUE),
      pop2020_total_ghsl = sum(pop2020_total_ghsl, na.rm = TRUE),
      
      # Compute total populations in PAs
      pop2000_in_pa_worldpop = sum(pop2000_in_pa_worldpop, na.rm = TRUE),
      pop2020_in_pa_worldpop = sum(pop2020_in_pa_worldpop, na.rm = TRUE),
      pop2000_in_pa_ghsl = sum(pop2000_in_pa_ghsl, na.rm = TRUE),
      pop2020_in_pa_ghsl = sum(pop2020_in_pa_ghsl, na.rm = TRUE),
      
      # Compute total populations in 10km buffers
      pop2000_in_pa10_worldpop = sum(pop2000_in_pa10_worldpop, na.rm = TRUE),
      pop2020_in_pa10_worldpop = sum(pop2020_in_pa10_worldpop, na.rm = TRUE),
      pop2000_in_pa10_ghsl = sum(pop2000_in_pa10_ghsl, na.rm = TRUE),
      pop2020_in_pa10_ghsl = sum(pop2020_in_pa10_ghsl, na.rm = TRUE)
    ) %>%
    mutate(
      # Compute percentages properly using total areas and population counts
      pa_area_2000_km2_pct = pa_area_2000_km2 / country_area_km2 * 100,
      pa_area_2020_km2_pct = pa_area_2020_km2 / country_area_km2 * 100,
      pa_area10km_2000_km2_pct = pa_area10km_2000_km2 / country_area_km2 * 100,
      pa_area10km_2020_km2_pct = pa_area10km_2020_km2 / country_area_km2 * 100,
      
      pop2000_in_pa_worldpop_pct = pop2000_in_pa_worldpop / pop2000_total_worldpop * 100,
      pop2020_in_pa_worldpop_pct = pop2020_in_pa_worldpop / pop2020_total_worldpop * 100,
      pop2000_in_pa_ghsl_pct = pop2000_in_pa_ghsl / pop2000_total_ghsl * 100,
      pop2020_in_pa_ghsl_pct = pop2020_in_pa_ghsl / pop2020_total_ghsl * 100,
      
      pop2000_in_pa10_worldpop_pct = pop2000_in_pa10_worldpop / pop2000_total_worldpop * 100,
      pop2020_in_pa10_worldpop_pct = pop2020_in_pa10_worldpop / pop2020_total_worldpop * 100,
      pop2000_in_pa10_ghsl_pct = pop2000_in_pa10_ghsl / pop2000_total_ghsl * 100,
      pop2020_in_pa10_ghsl_pct = pop2020_in_pa10_ghsl / pop2020_total_ghsl * 100
    )
}

# Compute "Total" for all countries
pa_pop_total <- compute_totals(pa_pop_combined, "Total")

# Compute "Total without India"
pa_pop_total_no_india <- pa_pop_combined %>%
  filter(ISO3 != "IND") %>% 
  compute_totals("Total without India")

# Combine totals with main dataset
pa_pop_summary_with_total <- bind_rows(pa_pop_total, 
                                       pa_pop_total_no_india, 
                                       pa_pop_combined) %>%
  select(country, ISO3,
         pa_area_2000_km2_pct, pa_area_2020_km2_pct,
         pa_area10km_2000_km2_pct, pa_area10km_2020_km2_pct,
         pop2000_in_pa_ghsl_pct, pop2020_in_pa_ghsl_pct,
         pop2000_in_pa10_ghsl_pct, pop2020_in_pa10_ghsl_pct,
         pop2000_in_pa_worldpop_pct, pop2020_in_pa_worldpop_pct,
         pop2000_in_pa10_worldpop_pct, pop2020_in_pa10_worldpop_pct)


# Save intermediary result for manuscript citation ------------------------


#Save for in-text citation
save(pa_pop_worldpop, pa_pop_ghsl, pa_pop_total, pa_pop_total_no_india, 
     file = "results/pa_pop_total.rds")

# Produce Table 1 --------------------------------------------------------


# Generate Table 1 with expanded structure
pa_pop_summary_with_total %>%
  select(-ISO3) %>%
  gt() %>%
  tab_header(
    title = "Table 1: PA Coverage and Population Proximity (2000-2020)",
    subtitle = "Percentage of land and population within and near protected areas"
  ) %>%
  fmt_number(columns = where(is.numeric), decimals = 1) %>%
  # Level 3 headers: 2000 | 2020
  cols_label(
    country = "Country",
    pa_area_2000_km2_pct = "2000", pa_area_2020_km2_pct = "2020",
    pop2000_in_pa_worldpop_pct = "2000", pop2020_in_pa_worldpop_pct = "2020",
    pop2000_in_pa_ghsl_pct = "2000", pop2020_in_pa_ghsl_pct = "2020",
    pa_area10km_2000_km2_pct = "2000", pa_area10km_2020_km2_pct = "2020",
    pop2000_in_pa10_worldpop_pct = "2000", pop2020_in_pa10_worldpop_pct = "2020",
    pop2000_in_pa10_ghsl_pct = "2000", pop2020_in_pa10_ghsl_pct = "2020"
  ) %>%
  # Level 2 headers: Land (WDPA), Population (WorldPop), Population (GHSL)
  tab_spanner(label = "within PAs", 
              columns = c(pa_area_2000_km2_pct, pa_area_2020_km2_pct),
              id = "land_pa") %>%
  tab_spanner(label = "within PAs", 
              columns = c(pop2000_in_pa_worldpop_pct, pop2020_in_pa_worldpop_pct),
              id = "worldpop_pa") %>%
  tab_spanner(label = "within PAs", 
              columns = c(pop2000_in_pa_ghsl_pct, pop2020_in_pa_ghsl_pct),
              id = "ghsl_pa") %>%
  tab_spanner(label = "10km of PAs", 
              columns = c(pa_area10km_2000_km2_pct, pa_area10km_2020_km2_pct),
              id = "land_pa10") %>%
  tab_spanner(label = "10km of PAs", 
              columns = c(pop2000_in_pa10_worldpop_pct, pop2020_in_pa10_worldpop_pct),
              id = "worldpop_pa10") %>%
  tab_spanner(label = "10km of PAs", 
              columns = c(pop2000_in_pa10_ghsl_pct, pop2020_in_pa10_ghsl_pct),
              id = "ghsl_pa10") %>%
  # Level 3 headers: Land (WDPA), Population (WorldPop), Population (GHSL)
  tab_spanner(label = "% land within (WDPA)", 
              spanners = c("land_pa", "land_pa10")) %>%
  tab_spanner(label = "% population (WorldPop)", 
              spanners = c("worldpop_pa", "worldpop_pa10")) %>%
  tab_spanner(label = "% population (GHSL)", 
              spanners = c("ghsl_pa", "ghsl_pa10")) %>%
  # Level 2 headers: % within PAs | % within 10km of PAs
  cols_align(align = "center", columns = everything()) %>%
  tab_options(
    table.font.size = px(11),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(12),
    data_row.padding = px(0),
    column_labels.padding = px(0)
  ) %>%
  tab_source_note(source_note = "Source: Analysis based on WDPA, WorldPop & GHSL data (2000-2020)") %>%
  
  # **Thin vertical lines for all columns**
  tab_style(
    style = cell_borders(sides = "right", color = "gray80", weight = px(0.5)),
    locations = cells_body(columns = everything())
  ) %>%
  
  # **Thick vertical line between "% within PAs" and "% within 10km of PAs"**
  tab_style(
    style = cell_borders(sides = "right", color = "black", weight = px(2)),
    locations = cells_body(columns = c(country, pa_area10km_2020_km2_pct,
                                       pop2020_in_pa10_ghsl_pct))
  ) %>%
  
  tab_style(
    style = list(cell_fill(color = "lightgray"), cell_text(weight = "bold")),
    locations = cells_body(rows = country %in% c("Total", "Total without India"))
  ) -> table_data
table_data
# Save table in multiple formats
gtsave(table_data, "results/table1.html")
gtsave(table_data, "results/table1.tex")
gtsave(table_data, "results/table1.docx")
write_rds(table_data, "results/table1.rds")

# Produce Table 2 ---------------------------------------------------------

compute_smd <- function(x1, x2) {
  mean_diff <- abs(mean(x1, na.rm = TRUE) - mean(x2, na.rm = TRUE))
  pooled_sd <- sqrt((var(x1, na.rm = TRUE) + var(x2, na.rm = TRUE)) / 2)
  smd <- mean_diff / pooled_sd
  return(smd)
}

# Compute SMDs for 2000 and 2020
smd_results <- tibble(
  Metric = c("Population within PAs", "Population within 10km of PAs"),
  `2000` = c(
    compute_smd(pa_pop_combined$pop2000_in_pa_worldpop, pa_pop_combined$pop2000_in_pa_ghsl),
    compute_smd(pa_pop_combined$pop2000_in_pa10_worldpop, pa_pop_combined$pop2000_in_pa10_ghsl)
  ),
  `2020` = c(
    compute_smd(pa_pop_combined$pop2020_in_pa_worldpop, pa_pop_combined$pop2020_in_pa_ghsl),
    compute_smd(pa_pop_combined$pop2020_in_pa10_worldpop, pa_pop_combined$pop2020_in_pa10_ghsl)
  )
)

# Create the gt table
smd_table <- smd_results %>%
  gt() %>%
  tab_header(
    title = "Table 2: Standard Mean Differences Between GHSL and Worldpop Estimates"
  ) %>%
  fmt_number(columns = everything(), decimals = 2) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(
    table.font.size = px(11),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(12),
    data_row.padding = px(2),
    column_labels.padding = px(2)
  ) %>%
  cols_label(Metric = "") %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_column_labels(columns = everything()), 
      cells_body(rows = everything(), columns = Metric) 
    )
  )
smd_table

# Save the table
gtsave(smd_table, "results/table2.html")
gtsave(smd_table, "results/table2.tex")
gtsave(smd_table, "results/table2.docx")
write_rds(smd_table, "results/table2.rds")


# Produce Table S1 ----------------------------------------------------------

# Compute differences between Worldpop and GHSL

pa_pop_diff <- pa_pop_combined %>%
  select(ISO3, country,
         pop2000_in_pa_worldpop_pct, pop2020_in_pa_worldpop_pct,
         pop2000_in_pa10_worldpop_pct, pop2020_in_pa10_worldpop_pct,
         pop2000_in_pa_ghsl_pct, pop2020_in_pa_ghsl_pct,
         pop2000_in_pa10_ghsl_pct, pop2020_in_pa10_ghsl_pct)  %>%
  pivot_longer(
    cols = -c(ISO3, country),
    names_to = c("year", "metric", "source"),
    names_pattern = "pop(\\d{4})_in_(pa10|pa)_(worldpop|ghsl)"
  ) %>%
  mutate(
    year = as.integer(year)
  ) %>%
  pivot_wider(
    names_from = source,
    values_from = value
  ) %>%
  mutate(abs_diff = abs(worldpop - ghsl),
         diff_perc = (abs_diff / ghsl))

pa_area_diff <- pa_pop_ghsl %>%
  select(ISO3, pa_area_2000_km2_pct, pa_area_2020_km2_pct) %>%
  pivot_longer(
    cols = -ISO3,
    names_to = c("year"),
    names_pattern = "pa_area_(\\d{4})_km2_pct",
    values_to = "pa_area_pct"
  ) %>%
  mutate(year = as.numeric(year))

pa_pop_diff <- pa_pop_diff %>%
  left_join(pa_area_diff, by = c("ISO3", "year"))

major_discrepancies <- pa_pop_diff %>%
  filter(abs_diff > 5 & diff_perc > 0.1) %>%
  arrange(desc(abs_diff))

# length(unique(major_discepancies$ISO3))

table_s1 <- major_discrepancies %>%
  mutate(
    worldpop = worldpop / 100,
    ghsl = ghsl / 100,
    metric = case_when(
      metric == "pa" ~ "Within PAs",
      metric == "pa10" ~ "Within 10km of PAs"
    )
  ) %>%
  select(country, year, metric, worldpop, ghsl, abs_diff, diff_perc) %>%
  gt() %>%
  # Table title and subtitle
  tab_header(
    title = md("**Table S1: Largest Differences Between GHSL and WorldPop Estimates**"),
    subtitle = md("*Absolute difference of more then 5 percentage points (pp) and relative difference of more than 10%*")
  ) %>%
  # Adjust column labels to be on two lines
  cols_label(
    country = "Country",
    year = "Year",
    metric = "Area",
    worldpop = md("**WorldPop**<br>**Estimate**"),
    ghsl = md("**GHSL**<br>**Estimate**"),
    abs_diff = md("**Absolute**<br>**Difference**"),
    diff_perc = md("**Relative**<br>**Difference**")
  ) %>%
  # Bold column headers
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  # Format numeric values: percentages for estimates and relative differences
  fmt_percent(
    columns = c(worldpop, ghsl, diff_perc),
    decimals = 1
  ) %>%
  # Format absolute difference with "pp" suffix
  fmt_number(
    columns = abs_diff,
    decimals = 1,
    suffixing = FALSE
  ) %>%
  text_transform(
    locations = cells_body(columns = abs_diff),
    fn = function(x) paste0(x, " pp")
  ) %>%
  # Align all columns to the center
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  # Adjust table appearance
  tab_options(
    table.font.size = px(11),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(12),
    data_row.padding = px(4),  # Increase padding for better spacing
    column_labels.padding = px(6)  # Increase padding for two-line headers
  )

gtsave(table_s1, "results/table_s1.html")
gtsave(table_s1, "results/table_s1.tex")
gtsave(table_s1, "results/table_s1.docx")
write_rds(table_s1, "results/table_s1.rds")

# Produce Figure 1 ---------------------------------------------------------

# % of Population Near PAs
ggplot(pa_pop_combined, aes(x = reorder(country, pop2020_in_pa10_ghsl))) +
  geom_point(aes(y = pop2000_in_pa10_ghsl), color = "gray", size = 3) +
  geom_point(aes(y = pop2020_in_pa10_ghsl), color = "steelblue", size = 3) +
  geom_segment(aes(y = pop2000_in_pa10_ghsl, 
                   yend = pop2020_in_pa10_ghsl, 
                   xend = country, 
                   color = pop2020_in_pa10_ghsl > pop2000_in_pa10_ghsl), 
               linewidth = 1.2) +
  scale_color_manual(values = c("red", "steelblue"), 
                     labels = c("Decrease", "Increase")) +
  coord_flip() +
  labs(
    title = "Figure 1: Change in % Population Near PAs (GHSL 2000-2020)",
    x = NULL,
    y = "% of Population",
    color = "Trend"
  ) +
  theme_minimal() -> figure1
figure1
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
pa_pop_summary2 <- pa_pop_worldpop |> 
  mutate(var_land = (pa_area_2020_km2_pct - pa_area_2000_km2_pct) / 
           pa_area_2000_km2_pct,
         var_pop = (pop2020_in_pa10_worldpop_pct - pop2000_in_pa10_worldpop_pct) / 
           pop2000_in_pa10_worldpop_pct) |> 
  filter(pa_area_2000_km2_pct > 1)


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
    title = "Figure 2: Evolution of PA Land Coverage and Nearby Population Share (GHSL 2000-2020)",
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
# pa_pop_summary_with_total2 <- pa_pop_summary_with_total |> 
#   left_join(select(pa_pop, country, pop2020_total), by = "country")

# Determine the max values for dynamic axis limits
max_x <- max(pa_pop_worldpop$pa_area_2020_km2_pct, na.rm = TRUE)
max_y <- max(pa_pop_worldpop$pop2020_in_pa10_worldpop_pct, na.rm = TRUE)

# 1. Create my_plot with the legend
my_plot2 <- ggplot(pa_pop_worldpop, aes(x = pa_area_2020_km2_pct, 
                                                   y = pop2020_in_pa10_worldpop_pct, 
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


# Produce Figure 4 --------------------------------------------------------



# Create the lollipop plot
pa_pop_diff_plot <- pa_pop_diff %>%
  filter(!is.na(worldpop) & !is.na(ghsl)) %>%
  ggplot(aes(y = pa_area_pct)) +
  # Connecting line (kept blue for clarity)
  geom_segment(aes(x = ghsl, xend = worldpop, yend = pa_area_pct), 
               color = "blue", linewidth = 1) +
  # GHSL black dot with legend
  geom_point(aes(x = ghsl, color = "GHSL"), size = 2) +
  # WorldPop blue dot with legend
  geom_point(aes(x = worldpop, color = "WorldPop"), size = 2) +
  # Facet by year (horizontal) and metric (vertical) with custom labels
  facet_grid(rows = vars(metric), cols = vars(year),
             labeller = labeller(metric = c("pa" = "Within PAs", "pa10" = "Within 10km of PAs"))) +
  # Define colors in the legend
  scale_color_manual(
    name = "Population Source",
    values = c("GHSL" = "black", "WorldPop" = "blue")
  ) +
  # Labels and theme
  labs(
    title = "Figure 4: Comparison of Population Estimates from GHSL and WorldPop",
    x = "Population Estimate (% of National Population)",
    y = "PA Coverage (% of National Territory)"
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

save_plot("results/figure4.png", plot = pa_pop_diff_plot, base_width = 8, 
          base_height = 9, unit = "in", dpi = 300)



# Figures from Naidoo 2019 ------------------------------------------------

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

