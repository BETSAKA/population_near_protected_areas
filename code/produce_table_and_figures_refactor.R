# Table 1 - Aggregate population exposure in 2020 ----------------------------

t1_s1 <- make_india_diagnostics(s1) |> mutate(period = "2000 (confirmed)")
t1_s2 <- make_india_diagnostics(s2) |> mutate(period = "2020 (confirmed)")

t1_data <- bind_rows(t1_s1, t1_s2) |>
  mutate(
    area_all_k   = area_strict_k + area_nonstrict_k + area_unknowncat_k,
    pop_in_all_m  = pop_strict + pop_nonstrict + pop_unknowncat,
    pop_10k_all_m = pop_strict10 + pop_nonstrict10 + pop_unknowncat10,
    pct_in_all    = pop_in_all_m  / nat_pop * 100,
    pct_10k_all   = pop_10k_all_m / nat_pop * 100
  )

# Pivot to wide: metrics as rows, groups as columns
t1_wide <- t1_data |>
  filter(period == "2020 (confirmed)") |>
  select(group, area_all_k, pop_in_all_m, pct_in_all, pop_10k_all_m, pct_10k_all, nat_pop) |>
  pivot_longer(-group, names_to = "metric", values_to = "value") |>
  pivot_wider(names_from = group, values_from = value) |>
  mutate(metric = factor(metric, levels = c(
    "nat_pop", "area_all_k",
    "pop_in_all_m", "pct_in_all",
    "pop_10k_all_m", "pct_10k_all"
  ))) |>
  arrange(metric) |>
  mutate(metric_label = c(
    "Total population (millions)",
    "PA area (×1,000 km²)",
    "Population inside PAs (millions)",
    "Population inside PAs (% of national)",
    "Population inside or within 10 km (millions)",
    "Population inside or within 10 km (% of national)"
  ))

table_1 <- t1_wide |>
  select(metric_label, `All LMICs`, `All LMICs excl. India`, India) |>
  gt() |>
  tab_header(
    title    = "Table 1: Population exposure to protected areas in 2020",
    subtitle = "Confirmed protected areas (STATUS_YR 1–2020), GHSL population"
  ) |>
  cols_label(
    metric_label          = "",
    `All LMICs`           = "All LMICs",
    `All LMICs excl. India` = "Excl. India",
    India                 = "India"
  ) |>
  fmt_number(
    columns = c(`All LMICs`, `All LMICs excl. India`, India),
    rows    = metric_label %in% c(
      "Total population (millions)",
      "PA area (×1,000 km²)",
      "Population inside PAs (millions)",
      "Population inside or within 10 km (millions)"
    ),
    decimals = 1
  ) |>
  fmt_number(
    columns = c(`All LMICs`, `All LMICs excl. India`, India),
    rows    = metric_label %in% c(
      "Population inside PAs (% of national)",
      "Population inside or within 10 km (% of national)"
    ),
    decimals = 1,
    pattern  = "{x}%"
  ) |>
  tab_row_group(
    label = "National context",
    rows  = metric_label %in% c("Total population (millions)", "PA area (×1,000 km²)")
  ) |>
  tab_row_group(
    label = "Inside protected areas",
    rows  = metric_label %in% c(
      "Population inside PAs (millions)",
      "Population inside PAs (% of national)"
    )
  ) |>
  tab_row_group(
    label = "Inside or within 10 km",
    rows  = metric_label %in% c(
      "Population inside or within 10 km (millions)",
      "Population inside or within 10 km (% of national)"
    )
  ) |>
  tab_footnote("Source: GHSL 2023, WDPA May 2021. Population matched to year 2020.") |>
  tab_options(table.width = pct(100))

table_1

gtsave(table_1, "results/table_1.html")
gtsave(table_1, "results/table_1.tex")
gtsave(table_1, "results/table_1.docx")
saveRDS(table_1, "results/table_1.rds")
        area_strict_nat = area_strict,
        area_nonstrict_nat = area_nonstrict,
        area_unknown_nat = area_unknown
      ),
    by = "iso3"
  ) |>
  left_join(llm_2020, by = c("iso3" = "ISO3"))

# Build the three scenario datasets (GHSL as default) ----------------------

# Scenario 1: Confirmed PAs by 2000, population 2000
s1 <- analysis |>
  filter(scenario == "Confirmed_2000", source == "GHSL") |>
  mutate(nat_pop = pop_total, label = "Confirmed by 2000")

# Scenario 2: Confirmed PAs by 2020, population 2020
s2 <- analysis |>
  filter(scenario == "Confirmed_2020", source == "GHSL") |>
  mutate(nat_pop = pop_total, label = "Confirmed by 2020")

# Scenario 3: Confirmed by 2020 + Unknown year, population 2020
# We need to add Unknown_Year populations/areas to Confirmed_2020
s3_confirmed <- analysis |>
  filter(scenario == "Confirmed_2020", source == "GHSL") |>
  select(
    iso3,
    country,
    pop_total,
    starts_with("pop_"),
    starts_with("area_")
  )

s3_unknown <- analysis |>
  filter(scenario == "Unknown_Year", source == "GHSL") |>
  select(
    iso3,
    starts_with("pop_"),
    matches("^area_(strict|nonstrict|unknowncat)")
  ) |>
  rename_with(~ paste0(.x, "_unk"), -iso3)

s3 <- s3_confirmed |>
  left_join(s3_unknown, by = "iso3") |>
  mutate(
    pop_strict = pop_strict + replace_na(pop_strict_unk, 0),
    pop_strict10 = pop_strict10 + replace_na(pop_strict10_unk, 0),
    pop_nonstrict = pop_nonstrict + replace_na(pop_nonstrict_unk, 0),
    pop_nonstrict10 = pop_nonstrict10 + replace_na(pop_nonstrict10_unk, 0),
    pop_unknowncat = pop_unknowncat + replace_na(pop_unknowncat_unk, 0),
    pop_unknowncat10 = pop_unknowncat10 + replace_na(pop_unknowncat10_unk, 0),
    pop_inside_all = pop_strict + pop_nonstrict + pop_unknowncat,
    pop_10km_all = pop_strict10 + pop_nonstrict10 + pop_unknowncat10,
    area_strict = area_strict + replace_na(area_strict_unk, 0),
    area_strict10 = area_strict10 + replace_na(area_strict10_unk, 0),
    area_nonstrict = area_nonstrict + replace_na(area_nonstrict_unk, 0),
    area_nonstrict10 = area_nonstrict10 + replace_na(area_nonstrict10_unk, 0),
    area_unknowncat = area_unknowncat + replace_na(area_unknowncat_unk, 0),
    area_unknowncat10 = area_unknowncat10 +
      replace_na(area_unknowncat10_unk, 0),
    area_inside_all = area_strict + area_nonstrict + area_unknowncat,
    area_10km_all = area_strict10 + area_nonstrict10 + area_unknowncat10,
    nat_pop = pop_total,
    label = "All PAs in 2020 (incl. unknown year)"
  ) |>
  select(-ends_with("_unk"))

# Helper: aggregate to global totals (with India diagnostics) ---------------

compute_global <- function(df, group_label) {
  df |>
    summarize(
      across(
        c(
          pop_strict,
          pop_strict10,
          pop_nonstrict,
          pop_nonstrict10,
          pop_unknowncat,
          pop_unknowncat10,
          pop_inside_all,
          pop_10km_all,
          pop_total,
          nat_pop,
          area_strict,
          area_strict10,
          area_nonstrict,
          area_nonstrict10,
          area_unknowncat,
          area_unknowncat10,
          area_inside_all,
          area_10km_all
        ),
        \(x) sum(x, na.rm = TRUE)
      )
    ) |>
    mutate(group = group_label)
}

make_india_diagnostics <- function(df) {
  all_countries <- compute_global(df, "All LMICs")
  excl_india <- df |>
    filter(iso3 != "IND") |>
    compute_global("All LMICs excl. India")
  india_only <- df |> filter(iso3 == "IND") |> compute_global("India")
  bind_rows(all_countries, excl_india, india_only)
}

# Intermediate objects (analysis, s1, s2, s3, national_totals, llm_2020)
# are saved at the end of this script together with t1_data, table_2_data,
# fig3_change, and other objects needed by the manuscript's inline R code.

# Table 1 - Population magnitudes by PA category ---------------------------
t1_s1 <- make_india_diagnostics(s1) |> mutate(period = "2000 (confirmed)")
t1_s2 <- make_india_diagnostics(s2) |> mutate(period = "2020 (confirmed)")

t1_data <- bind_rows(t1_s1, t1_s2) |>
  transmute(
    period,
    group,
    # PA area (thousands km²)
    area_strict_k = area_strict / 1e3,
    area_nonstrict_k = area_nonstrict / 1e3,
    area_unknowncat_k = area_unknowncat / 1e3,
    area_all_k = area_inside_all / 1e3,
    # Pop inside PAs (millions)
    pop_in_strict_m = pop_strict / 1e6,
    pop_in_nonstrict_m = pop_nonstrict / 1e6,
    pop_in_unknowncat_m = pop_unknowncat / 1e6,
    pop_in_all_m = pop_inside_all / 1e6,
    # Pop within 10km (millions) - buffer only (exclusive of inside)
    pop_10k_strict_m = pop_strict10 / 1e6,
    pop_10k_nonstrict_m = pop_nonstrict10 / 1e6,
    pop_10k_unknowncat_m = pop_unknowncat10 / 1e6,
    pop_10k_all_m = pop_10km_all / 1e6,
    # Total population (millions)
    nat_pop_m = nat_pop / 1e6,
    # Shares
    pct_in_all = pop_inside_all / nat_pop * 100,
    pct_10k_all = (pop_inside_all + pop_10km_all) / nat_pop * 100
  )

table_1 <- t1_data |>
  select(
    period,
    group,
    area_strict_k,
    area_nonstrict_k,
    area_unknowncat_k,
    area_all_k,
    pop_in_strict_m,
    pop_in_nonstrict_m,
    pop_in_unknowncat_m,
    pop_in_all_m,
    pop_10k_strict_m,
    pop_10k_nonstrict_m,
    pop_10k_unknowncat_m,
    pop_10k_all_m,
    nat_pop_m,
    pct_in_all,
    pct_10k_all
  ) |>
  gt(groupname_col = "period") |>
  tab_header(
    title = "Table 1: Population magnitudes by PA category",
    subtitle = "Low- and lower-middle-income countries, GHSL estimates"
  ) |>
  cols_label(
    group = "",
    area_strict_k = "Strict",
    area_nonstrict_k = "Non-strict",
    area_unknowncat_k = "Unknown cat.",
    area_all_k = "All",
    pop_in_strict_m = "Strict",
    pop_in_nonstrict_m = "Non-strict",
    pop_in_unknowncat_m = "Unknown cat.",
    pop_in_all_m = "All",
    pop_10k_strict_m = "Strict",
    pop_10k_nonstrict_m = "Non-strict",
    pop_10k_unknowncat_m = "Unknown cat.",
    pop_10k_all_m = "All",
    nat_pop_m = "Total pop. (M)",
    pct_in_all = "Inside PAs",
    pct_10k_all = "Within 10 km"
  ) |>
  tab_spanner(
    label = md("**PA area (×1000 km²)**"),
    columns = c(area_strict_k, area_nonstrict_k, area_unknowncat_k, area_all_k)
  ) |>
  tab_spanner(
    label = md("**Pop. inside PAs (millions)**"),
    columns = c(
      pop_in_strict_m,
      pop_in_nonstrict_m,
      pop_in_unknowncat_m,
      pop_in_all_m
    )
  ) |>
  tab_spanner(
    label = md("**Pop. within 10 km buffer (millions)**"),
    columns = c(
      pop_10k_strict_m,
      pop_10k_nonstrict_m,
      pop_10k_unknowncat_m,
      pop_10k_all_m
    )
  ) |>
  tab_spanner(
    label = md("**% of total pop.**"),
    columns = c(pct_in_all, pct_10k_all)
  ) |>
  fmt_number(
    columns = c(starts_with("area_"), starts_with("pop_"), nat_pop_m),
    decimals = 1
  ) |>
  fmt_number(columns = c(pct_in_all, pct_10k_all), decimals = 1) |>
  cols_align(align = "center", columns = -group) |>
  tab_options(
    table.font.size = px(11),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(12),
    data_row.padding = px(2)
  ) |>
  tab_source_note(
    "Source: WDPA May 2021, GHSL. IUCN categories: Strict = Ia-III, Non-strict = IV-VI."
  )

table_1

gtsave(table_1, "results/table_1.html")
gtsave(table_1, "results/table_1.tex")
gtsave(table_1, "results/table_1.docx")
saveRDS(table_1, "results/table_1.rds")


# Table 2 - Evaluation design: implied population magnitudes ---------------
s2_global <- make_india_diagnostics(s2) |>
  filter(group == "All LMICs")

table_2_data <- tibble(
  design = c(
    "In-out comparison (inside PAs only)",
    "Inside PAs or within 10 km buffer",
    "Strict PAs only (IUCN Ia-III): inside or within 10 km",
    "Non-strict PAs only (IUCN IV-VI): inside or within 10 km",
    "Unknown category PAs: inside or within 10 km"
  ),
  pop_millions = c(
    s2_global$pop_inside_all / 1e6,
    (s2_global$pop_inside_all + s2_global$pop_10km_all) / 1e6,
    (s2_global$pop_strict + s2_global$pop_strict10) / 1e6,
    (s2_global$pop_nonstrict + s2_global$pop_nonstrict10) / 1e6,
    (s2_global$pop_unknowncat + s2_global$pop_unknowncat10) / 1e6
  ),
  pct_total = c(
    s2_global$pop_inside_all / s2_global$nat_pop * 100,
    (s2_global$pop_inside_all + s2_global$pop_10km_all) /
      s2_global$nat_pop *
      100,
    (s2_global$pop_strict + s2_global$pop_strict10) / s2_global$nat_pop * 100,
    (s2_global$pop_nonstrict + s2_global$pop_nonstrict10) /
      s2_global$nat_pop *
      100,
    (s2_global$pop_unknowncat + s2_global$pop_unknowncat10) /
      s2_global$nat_pop *
      100
  )
)

table_2 <- table_2_data |>
  gt() |>
  tab_header(
    title = "Table 2: Implied population magnitudes by evaluation design",
    subtitle = "Confirmed PAs by 2020, all LMICs, GHSL population estimates"
  ) |>
  cols_label(
    design = "Evaluation design (exposure definition)",
    pop_millions = "Population (millions)",
    pct_total = "% of LMIC population"
  ) |>
  fmt_number(columns = pop_millions, decimals = 1) |>
  fmt_number(columns = pct_total, decimals = 1) |>
  cols_align(align = "center", columns = -design) |>
  tab_options(
    table.font.size = px(11),
    heading.title.font.size = px(14),
    data_row.padding = px(4)
  ) |>
  tab_source_note(
    "Source: WDPA May 2021, GHSL 2020. Buffer populations are exclusive of inside-PA populations."
  )

table_2

gtsave(table_2, "results/table_2.html")
gtsave(table_2, "results/table_2.docx")
gtsave(table_2, "results/table_2.tex")
saveRDS(table_2, "results/table_2.rds")

# Figure 1 - Treemap: India's leverage on global aggregates ----------------

fig1_data <- s2 |>
  transmute(
    iso3,
    country,
    nat_pop_m = nat_pop / 1e6,
    pct_exposed = (pop_inside_all + pop_10km_all) / nat_pop * 100,
    is_india = if_else(iso3 == "IND", "India", "Other LMICs"),
    label_colour = if_else(pct_exposed > 50, "black", "white")
  )

figure_1 <- fig1_data |>
  ggplot(aes(
    area = nat_pop_m,
    fill = pct_exposed,
    label = iso3,
    subgroup = is_india
  )) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "black", size = 3) +
  geom_treemap_text(
    aes(colour = label_colour),
    place = "centre",
    size = 8,
    min.size = 3
  ) +
  scale_colour_identity() +
  scale_fill_viridis_c(
    name = "% pop. within\n10 km of PAs",
    option = "C"
  ) +
  labs(
    title = "Figure 1: India's influence on LMIC aggregates",
    subtitle = "Tile area = total population (GHSL 2020). Color = % within 10 km of confirmed PAs."
  ) +
  theme_minimal() +
  theme(legend.position = "right")

figure_1

ggsave("results/figure_1.png", figure_1, width = 10, height = 7, dpi = 300)


# Figure 2 - Stacked bars: population exposure by PA category --------------
fig2_data <- bind_rows(
  make_india_diagnostics(s1) |> mutate(year = 2000),
  make_india_diagnostics(s2) |> mutate(year = 2020)
) |>
  # Pivot categories long
  select(
    group,
    year,
    pop_strict,
    pop_nonstrict,
    pop_unknowncat,
    pop_strict10,
    pop_nonstrict10,
    pop_unknowncat10
  ) |>
  pivot_longer(
    cols = -c(group, year),
    names_to = "variable",
    values_to = "pop"
  ) |>
  mutate(
    perimeter = if_else(
      str_detect(variable, "10"),
      "Within 10 km buffer",
      "Inside PAs"
    ),
    category = case_when(
      str_detect(variable, "nonstrict") ~ "Non-strict (IV-VI)",
      str_detect(variable, "unknowncat") ~ "Unknown category",
      str_detect(variable, "strict") ~ "Strict (Ia-III)"
    ),
    pop_m = pop / 1e6,
    year = factor(year),
    group = factor(
      group,
      levels = c("All LMICs", "All LMICs excl. India", "India")
    )
  )

figure_2 <- fig2_data |>
  ggplot(aes(x = year, y = pop_m, fill = category)) +
  geom_col(position = "stack", width = 0.7) +
  facet_grid(perimeter ~ group, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Strict (Ia-III)" = "#1b9e77",
      "Non-strict (IV-VI)" = "#d95f02",
      "Unknown category" = "#7570b3"
    ),
    name = "PA category"
  ) +
  labs(
    title = "Figure 2: Population exposure by PA category (2000-2020)",
    subtitle = "GHSL estimates (PAs with available creation year only)",
    x = NULL,
    y = "Population (millions)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

figure_2

ggsave("results/figure_2.png", figure_2, width = 10, height = 7, dpi = 300)


# Figure 3 - Net change in PA-adjacent population by category --------------

fig3_s1 <- make_india_diagnostics(s1) |> mutate(year = 2000)
fig3_s2 <- make_india_diagnostics(s2) |> mutate(year = 2020)

fig3_data <- bind_rows(fig3_s1, fig3_s2) |>
  filter(group == "All LMICs") |>
  select(
    year,
    pop_strict,
    pop_strict10,
    pop_nonstrict,
    pop_nonstrict10,
    pop_unknowncat,
    pop_unknowncat10
  ) |>
  pivot_longer(-year, names_to = "variable", values_to = "pop") |>
  mutate(
    perimeter = if_else(
      str_detect(variable, "10"),
      "Within 10 km buffer",
      "Inside PAs"
    ),
    category = case_when(
      str_detect(variable, "strict") &
        !str_detect(variable, "non") ~ "Strict (Ia-III)",
      str_detect(variable, "nonstrict") ~ "Non-strict (IV-VI)",
      str_detect(variable, "unknowncat") ~ "Unknown category"
    ),
    pop_m = pop / 1e6,
    year = factor(year)
  )

# Compute net change (2020 - 2000) for each cell
fig3_change <- fig3_data |>
  select(perimeter, category, year, pop_m) |>
  pivot_wider(names_from = year, values_from = pop_m) |>
  mutate(change = `2020` - `2000`)

figure_3 <- fig3_change |>
  ggplot(aes(x = category, y = change, fill = category)) +
  geom_col(width = 0.6) +
  facet_wrap(~perimeter) +
  scale_fill_manual(
    values = c(
      "Strict (Ia-III)" = "#1b9e77",
      "Non-strict (IV-VI)" = "#d95f02",
      "Unknown category" = "#7570b3"
    ),
    guide = "none"
  ) +
  geom_text(aes(label = sprintf("+%.1f M", change)), vjust = -0.3, size = 3.5) +
  labs(
    title = "Figure 3: Net change in PA-adjacent population (2000-2020)",
    subtitle = "All LMICs, GHSL. PAs with available creation year only.",
    x = NULL,
    y = "Change in population (millions)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

figure_3

ggsave("results/figure_3.png", figure_3, width = 8, height = 5, dpi = 300)

# Figure 4 - STATUS_YR uncertainty -------------------
# pct_exposed can exceed 100% in small countries because 10 km buffers
# from adjacent ADM1 units overlap at national level; capped at 100% for display
fig4_data <- bind_rows(
  s2 |>
    transmute(
      iso3,
      country,
      pop_exposed = pop_inside_all + pop_10km_all,
      pop_exposed_m = pop_exposed / 1e6,
      nat_pop,
      scenario = "Confirmed only"
    ),
  s3 |>
    transmute(
      iso3,
      country,
      pop_exposed = pop_inside_all + pop_10km_all,
      pop_exposed_m = pop_exposed / 1e6,
      nat_pop,
      scenario = "Including unknown year"
    )
) |>
  mutate(
    pct_exposed_raw = pop_exposed / nat_pop * 100,
    pct_exposed = pmin(pct_exposed_raw, 100) # cap at 100% for display
  )

# Pivot to compute the gap
fig4_wide <- fig4_data |>
  select(iso3, country, scenario, pct_exposed) |>
  pivot_wider(names_from = scenario, values_from = pct_exposed) |>
  mutate(gap = `Including unknown year` - `Confirmed only`) |>
  arrange(desc(gap))

# Show top 30 countries by gap to keep figure readable
fig4_top <- fig4_wide |>
  slice_max(gap, n = 30)

figure_4 <- fig4_top |>
  ggplot(aes(y = reorder(country, gap))) +
  geom_segment(
    aes(
      x = `Confirmed only`,
      xend = `Including unknown year`,
      yend = reorder(country, gap)
    ),
    color = "grey60",
    linewidth = 0.5
  ) +
  geom_point(aes(x = `Confirmed only`, color = "Confirmed by 2020"), size = 2) +
  geom_point(
    aes(x = `Including unknown year`, color = "Incl. unknown year"),
    size = 2
  ) +
  scale_color_manual(
    name = "Scenario",
    values = c(
      "Confirmed by 2020" = "#1b9e77",
      "Incl. unknown year" = "#d95f02"
    )
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    labels = scales::percent_format(scale = 1)
  ) +
  labs(
    title = "Figure 4: Effect of including PAs with unknown designation year",
    subtitle = "% of national population inside PAs or within 10 km (GHSL 2020)\nTop 30 countries by gap",
    x = "% of national population",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 7)
  )

figure_4

ggsave("results/figure_4.png", figure_4, width = 8, height = 10, dpi = 300)


# Figure 5 - Country lollipop (% pop near PAs, 2000-2020) ---------------

figs5_data <- bind_rows(
  s1 |>
    transmute(
      iso3,
      country,
      pct = (pop_inside_all + pop_10km_all) / nat_pop * 100,
      year = 2000
    ),
  s2 |>
    transmute(
      iso3,
      country,
      pct = (pop_inside_all + pop_10km_all) / nat_pop * 100,
      year = 2020
    )
) |>
  pivot_wider(names_from = year, values_from = pct, names_prefix = "y") |>
  mutate(increased = y2020 >= y2000)

figure_5 <- figs5_data |>
  ggplot(aes(y = reorder(country, y2020))) +
  geom_segment(
    aes(
      x = y2000,
      xend = y2020,
      yend = reorder(country, y2020),
      color = increased
    ),
    linewidth = 0.8
  ) +
  geom_point(aes(x = y2000), color = "grey50", size = 2) +
  geom_point(aes(x = y2020), color = "steelblue", size = 2) +
  scale_color_manual(
    values = c("TRUE" = "steelblue", "FALSE" = "red"),
    labels = c("Decrease", "Increase"),
    name = "Trend"
  ) +
  labs(
    title = "Figure 5: Change in % population near PAs (2000-2020)",
    subtitle = "GHSL estimates, confirmed PAs only. Grey = 2000, blue = 2020.",
    x = "% of national population inside PAs or within 10 km",
    y = NULL
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

figure_5

ggsave("results/figure_5.png", figure_5, width = 8, height = 12, dpi = 300)


# Figure 6 - Robustness: GHSL vs WorldPop ----------------------------------
fig6_data <- analysis |>
  filter(scenario == "Confirmed_2020") |>
  mutate(
    pct_inside = pop_inside_all / pop_total * 100,
    pct_10km = (pop_inside_all + pop_10km_all) / pop_total * 100
  ) |>
  select(iso3, country, source, pct_inside, pct_10km) |>
  pivot_longer(
    cols = c(pct_inside, pct_10km),
    names_to = "perimeter",
    values_to = "pct"
  ) |>
  pivot_wider(names_from = source, values_from = pct) |>
  mutate(
    perimeter = if_else(
      perimeter == "pct_inside",
      "Inside PAs",
      "Inside PAs or within 10 km"
    )
  )

figure_6 <- fig6_data |>
  ggplot(aes(x = GHSL, y = WP)) +
  geom_abline(slope = 1, intercept = 0, color = "grey50", linetype = "dashed") +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_text_repel(aes(label = iso3), size = 2.5, max.overlaps = 15) +
  facet_wrap(~perimeter, scales = "free") +
  labs(
    title = "Figure 6: GHSL vs WorldPop population estimates (2020)",
    subtitle = "% of national population. Dashed line = 1:1 agreement.",
    x = "GHSL estimate (%)",
    y = "WorldPop estimate (%)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

figure_6

ggsave("results/figure_6.png", figure_6, width = 10, height = 5, dpi = 300)

# Table S1 - Full country-level detail -------------------------------------

ts1_data <- bind_rows(
  s1 |>
    transmute(
      iso3,
      country,
      year = 2000,
      nat_pop,
      area_strict,
      area_nonstrict,
      area_unknowncat,
      area_inside_all,
      pop_inside_all,
      pop_10km_all,
      pct_inside = pop_inside_all / nat_pop * 100,
      pct_10km = (pop_inside_all + pop_10km_all) / nat_pop * 100
    ),
  s2 |>
    transmute(
      iso3,
      country,
      year = 2020,
      nat_pop,
      area_strict,
      area_nonstrict,
      area_unknowncat,
      area_inside_all,
      pop_inside_all,
      pop_10km_all,
      pct_inside = pop_inside_all / nat_pop * 100,
      pct_10km = (pop_inside_all + pop_10km_all) / nat_pop * 100
    )
) |>
  select(
    country,
    iso3,
    year,
    area_strict,
    area_nonstrict,
    area_unknowncat,
    area_inside_all,
    pop_inside_m = pop_inside_all,
    pop_10km_m = pop_10km_all,
    pct_inside,
    pct_10km
  ) |>
  mutate(
    pop_inside_m = pop_inside_m / 1e6,
    pop_10km_m = pop_10km_m / 1e6
  ) |>
  pivot_wider(
    names_from = year,
    values_from = c(
      area_strict,
      area_nonstrict,
      area_unknowncat,
      area_inside_all,
      pop_inside_m,
      pop_10km_m,
      pct_inside,
      pct_10km
    ),
    names_sep = "_"
  ) |>
  arrange(country)

table_s1 <- ts1_data |>
  select(
    country,
    area_inside_all_2000,
    area_inside_all_2020,
    pct_inside_2000,
    pct_inside_2020,
    pct_10km_2000,
    pct_10km_2020
  ) |>
  gt() |>
  tab_header(
    title = "Table S1: PA coverage and population proximity by country (2000-2020)",
    subtitle = "Confirmed PAs only, GHSL estimates"
  ) |>
  cols_label(
    country = "Country",
    area_inside_all_2000 = "2000",
    area_inside_all_2020 = "2020",
    pct_inside_2000 = "2000",
    pct_inside_2020 = "2020",
    pct_10km_2000 = "2000",
    pct_10km_2020 = "2020"
  ) |>
  tab_spanner(
    label = md("**PA area (km²)**"),
    columns = c(area_inside_all_2000, area_inside_all_2020)
  ) |>
  tab_spanner(
    label = md("**% pop. inside PAs**"),
    columns = c(pct_inside_2000, pct_inside_2020)
  ) |>
  tab_spanner(
    label = md("**% pop. inside or within 10 km**"),
    columns = c(pct_10km_2000, pct_10km_2020)
  ) |>
  fmt_number(
    columns = c(area_inside_all_2000, area_inside_all_2020),
    decimals = 0,
    use_seps = TRUE
  ) |>
  fmt_number(
    columns = c(pct_inside_2000, pct_inside_2020, pct_10km_2000, pct_10km_2020),
    decimals = 1
  ) |>
  cols_align(align = "center", columns = -country) |>
  tab_options(
    table.font.size = px(10),
    heading.title.font.size = px(13),
    data_row.padding = px(1)
  ) |>
  tab_source_note("Source: WDPA May 2021, GHSL.")

table_s1

gtsave(table_s1, "results/table_s1.html")
saveRDS(table_s1, "results/table_s1.rds")


# Table S2 - Largest GHSL/WorldPop discrepancies ---------------------------
ts2_data <- analysis |>
  filter(scenario == "Confirmed_2020") |>
  mutate(
    pct_10km = (pop_inside_all + pop_10km_all) / pop_total * 100,
    pct_inside = pop_inside_all / pop_total * 100
  ) |>
  select(iso3, country, source, pct_inside, pct_10km) |>
  pivot_longer(
    cols = c(pct_inside, pct_10km),
    names_to = "perimeter",
    values_to = "value"
  ) |>
  pivot_wider(names_from = source, values_from = value) |>
  mutate(
    abs_diff = abs(WP - GHSL),
    rel_diff = round(abs_diff / GHSL, 2),
    perimeter = if_else(
      perimeter == "pct_inside",
      "Inside PAs",
      "Inside or within 10 km"
    )
  ) |>
  filter(abs_diff > 5, rel_diff > 0.1) |>
  arrange(desc(abs_diff))

table_s2 <- ts2_data |>
  select(country, perimeter, GHSL, WP, abs_diff, rel_diff) |>
  gt() |>
  tab_header(
    title = "Table S2: Largest differences between GHSL and WorldPop estimates",
    subtitle = "Confirmed PAs by 2020. Absolute difference > 5 pp and relative difference > 10%"
  ) |>
  cols_label(
    country = "Country",
    perimeter = "Perimeter",
    GHSL = "GHSL (%)",
    WP = "WorldPop (%)",
    abs_diff = "Abs. diff. (pp)",
    rel_diff = "Rel. diff."
  ) |>
  fmt_number(columns = c(GHSL, WP, abs_diff), decimals = 1) |>
  fmt_percent(columns = rel_diff, decimals = 0) |>
  cols_align(align = "center", columns = -country) |>
  tab_options(
    table.font.size = px(11),
    data_row.padding = px(3)
  ) |>
  tab_source_note("Source: WDPA May 2021, GHSL & WorldPop 2020.")

table_s2

gtsave(table_s2, "results/table_s2.html")
saveRDS(table_s2, "results/table_s2.rds")

# Save all objects needed for manuscript inline citations -------------------
save(
  analysis,
  s1,
  s2,
  s3,
  national_totals,
  llm_2020,
  t1_data,
  table_2_data,
  fig3_change,
  file = "results/pa_pop_refactored.rds"
)
