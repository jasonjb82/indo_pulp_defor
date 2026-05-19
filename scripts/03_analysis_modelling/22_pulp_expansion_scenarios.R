#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Robert Heilmayr
# Project: Indonesia pulp deforestation
# Date: 2-25-26
# Purpose: Pulp expansion scenarios and figures
# Inputs: pulp_predictions.csv produced by 21_pulp_expansion_model.R
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# load packages --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(sf)
library(terra)
library(tmap)
library(cols4all)
library(gt)
library(rnaturalearth)
library(patchwork)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# load data --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wdir <- "remote/"

# Predicted expansion probabilities from 21_pulp_expansion_model.R
# (lat/lon pre-joined; no need to reload p2_df)
pred_df <- read_csv(
  paste0(wdir, "01_data/02_out/tables/pulp_predictions.csv")
)

# Province boundaries for maps
kab_sf  <- read_sf(paste0(wdir, "01_data/01_in/big/idn_kabupaten_big.shp"))
prov_sf <- kab_sf %>%
  group_by(prov, prov_code) %>%
  summarise(.groups = "drop")

# Parameters from MAI analysis
mai_df <- read_csv(paste0(wdir, "/01_data/04_results/key_parameters.csv"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# build raster template and 2022-2027 probability raster --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pred2027_sf   <- pred_df %>%
  drop_na(lon, lat) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
pred2027_vect <- vect(pred2027_sf)
rast_template <- rast(pred2027_vect, resolution = 0.1)
pred2027_rast <- rasterize(pred2027_vect, rast_template, field = ".pred_pulp", fun = mean)
names(pred2027_rast) <- "pred_pulp_2027"

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# estimate needed new pulp plantation area for scenarios --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new_wood_demand <- 34.27 # Million m3 needed. Should be pulled / calculated from existing data
prior_plantations <- 3050000 # Hectares in 2021. Should be pulled from existing data
n_years <- 7

mai_2021 <- mai_df$dmai_2021
mai_rate <- c("lb" = mai_df$yield_growth - mai_df$yield_growth_ci,
              "central" = mai_df$yield_growth,
              "ub" = mai_df$yield_growth + mai_df$yield_growth_ci)
mai_2028 <- (1 + mai_rate)^n_years * mai_2021
extra_production <- prior_plantations * (mai_2028 - mai_2021) / 1000000

additional_area <- (new_wood_demand - extra_production) / mai_2028
add_area_ci <- additional_area['central'] - additional_area['lb']

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# pulp expansion scenarios --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Predicted area of pulp expansion in hectares
exp_area_1 <- additional_area['ub'] * 1000000
exp_area_2 <- additional_area['central'] * 1000000
exp_area_3 <- additional_area['lb'] * 1000000


# All pixels in pred_df have pulp_start == 0 (enforced by filter in
# pred2027_input), so the full set is eligible for expansion

# --- 1. Derive island and land type for each candidate pixel ---
scenario_df <- pred_df %>%
  mutate(
    island = case_when(
      str_sub(as.character(kab_code), 1, 1) == "1" ~ "Sumatra",
      str_sub(as.character(kab_code), 1, 1) == "6" ~ "Kalimantan"
    ),
    land_type = case_when(
      forest_start == 1 & peat == 1 ~ "Forest on peat",
      forest_start == 1 & peat == 0 ~ "Forest off peat",
      forest_start == 0 & peat == 1 ~ "Non-forest on peat",
      TRUE                           ~ "Other"
    )
  ) %>%
  filter(!is.na(island))  # keep Sumatra + Kalimantan only

# --- 2. Helper functions ---
# Select top-probability pixels up to the target expansion area
select_scenario <- function(df, exp_area_ha) {
  n_px <- exp_area_ha / 100  # 1 pixel = 1 km² = 100 ha
  n_px <- round(n_px)
  df %>%
    slice_max(.pred_pulp, n = n_px, with_ties = FALSE) %>%
    mutate(area_ha = 100)
}

# Summarise selected pixels by island x land type, with island subtotals
summarise_scenario <- function(scenario_df) {
  by_type <- scenario_df %>%
    group_by(island, land_type) %>%
    summarise(area_ha = sum(area_ha), .groups = "drop")

  bind_rows(
    by_type,
    by_type %>%
      group_by(island) %>%
      summarise(area_ha = sum(area_ha), .groups = "drop") %>%
      mutate(land_type = "Island total")
  ) %>%
    arrange(island, land_type == "Island total", land_type)
}

# --- 3. Run all three scenarios ---
scenario1_df <- select_scenario(scenario_df, exp_area_1)
scenario2_df <- select_scenario(scenario_df, exp_area_2)
scenario3_df <- select_scenario(scenario_df, exp_area_3)

# --- 4. Build combined table with one column per scenario ---
expansion_table <- summarise_scenario(scenario1_df) %>%
  rename(scenario_1_ha = area_ha) %>%
  full_join(summarise_scenario(scenario2_df) %>% rename(scenario_2_ha = area_ha),
            by = c("island", "land_type")) %>%
  full_join(summarise_scenario(scenario3_df) %>% rename(scenario_3_ha = area_ha),
            by = c("island", "land_type")) %>%
  mutate(across(c(scenario_1_ha, scenario_2_ha, scenario_3_ha), ~ replace_na(.x, 0)))

expansion_table %>%
  gt(groupname_col = "island", rowname_col = "land_type") %>%
  cols_label(
    scenario_1_ha = sprintf("S1: %.1f%% annual growth", mai_rate["lb"]      * 100),
    scenario_2_ha = sprintf("S2: %.1f%% annual growth", mai_rate["central"] * 100),
    scenario_3_ha = sprintf("S3: %.1f%% annual growth", mai_rate["ub"]      * 100)
  ) %>%
  fmt_number(columns = c(scenario_1_ha, scenario_2_ha, scenario_3_ha), decimals = 0)

# --- Reorganised table: land type as rows, islands as columns, S2 (S1–S3) cells ---
fmt_cell <- function(s2, s3, s1) {
  fmt <- function(x) formatC(x, format = "d", big.mark = ",")
  pad <- function(x, w) strrep(" ", pmax(w - nchar(fmt(x)), 0L))
  paste0(pad(s2, 3L), fmt(s2),
         " (", pad(s3, 3L), fmt(s3),
         "–", pad(s1, 5L), fmt(s1), ")")
}

land_type_levels <- c("Forest on peat", "Forest off peat", "Non-forest on peat", "Other")

reorg_base <- expansion_table %>%
  filter(land_type != "Island total") %>%
  mutate(
    across(c(scenario_1_ha, scenario_2_ha, scenario_3_ha), ~ round(.x / 1000)),
    land_type = factor(land_type, levels = land_type_levels)
  ) %>%
  arrange(land_type)

# Total column: sum across islands for each land type
reorg_totals_col <- reorg_base %>%
  group_by(land_type) %>%
  summarise(across(c(scenario_1_ha, scenario_2_ha, scenario_3_ha), sum), .groups = "drop") %>%
  mutate(Total = fmt_cell(scenario_2_ha, scenario_1_ha, scenario_3_ha)) %>%
  select(land_type, Total)

# Total row: sum across land types for each island + overall total
reorg_totals_row <- reorg_base %>%
  group_by(island) %>%
  summarise(across(c(scenario_1_ha, scenario_2_ha, scenario_3_ha), sum), .groups = "drop") %>%
  mutate(cell = fmt_cell(scenario_2_ha, scenario_1_ha, scenario_3_ha),
         land_type = "Total") %>%
  select(land_type, island, cell) %>%
  pivot_wider(names_from = island, values_from = cell, values_fill = paste0(strrep(" ", 2L), "0 (", strrep(" ", 2L), "0–", strrep(" ", 4L), "0)")) %>%
  left_join(
    reorg_base %>%
      summarise(across(c(scenario_1_ha, scenario_2_ha, scenario_3_ha), sum)) %>%
      mutate(Total = fmt_cell(scenario_2_ha, scenario_1_ha, scenario_3_ha)) %>%
      select(Total),
    by = character()
  )

expansion_gt <- reorg_base %>%
  mutate(cell = fmt_cell(scenario_2_ha, scenario_1_ha, scenario_3_ha)) %>%
  select(land_type, island, cell) %>%
  pivot_wider(names_from = island, values_from = cell, values_fill = paste0(strrep(" ", 2L), "0 (", strrep(" ", 2L), "0–", strrep(" ", 4L), "0)")) %>%
  left_join(reorg_totals_col, by = "land_type") %>%
  bind_rows(reorg_totals_row) %>%
  gt(rowname_col = "land_type") %>%
  tab_header(
    title    = "Projected pulpwood plantation expansion (thousand ha)",
    # subtitle = "Central estimate (low–high), thousand ha; S1 = high growth, S2 = central growth, S3 = low growth"
  ) %>%
  cols_label(
    Kalimantan = "Kalimantan",
    Sumatra   = "Sumatra",
    Total      = "Total"
  ) %>%
  tab_options(table.width = pct(100)) %>%
  tab_style(
    style     = cell_text(align = "center"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style     = cell_text(font = "Courier New", size = px(13), color = "black"),
    locations = cells_body()
  )

expansion_gt

# --- 5. Rasterise scenario expansion pixels ---
# lat/lon carried through from pred_df; no separate coords join needed
rasterise_scenario <- function(scenario_df, rast_template) {
  scenario_df %>%
    drop_na(lon, lat) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    vect() %>%
    rasterize(rast_template, field = "area_ha", fun = sum)
}

scenario1_rast <- rasterise_scenario(scenario1_df, rast_template)
scenario2_rast <- rasterise_scenario(scenario2_df, rast_template)
scenario3_rast <- rasterise_scenario(scenario3_df, rast_template)
names(scenario1_rast) <- "scenario1_expansion"
names(scenario2_rast) <- "scenario2_expansion"
names(scenario3_rast) <- "scenario3_expansion"

# --- 6. Scenario map: 2022-2027 probability base + expansion layers ---
tmap_mode("view")

scenario_map <- tm_shape(pred2027_rast) +
  tm_raster(
    col       = "pred_pulp_2027",
    palette   = "brewer.yl_or_rd",
    col_alpha = 0.8,
    title     = "Predicted P(pulp expansion, 2022-2027)"
  ) +
tm_shape(scenario1_rast, group = "Scenario 1: No productivity growth") +
  tm_raster(
    col       = "scenario1_expansion",
    palette   = "brewer.greens",
    col_alpha = 0.8,
    title     = "Low productivity-growth expansion (ha)"
  ) +
tm_shape(scenario2_rast, group = "Scenario 2: 3.0% annual growth") +
  tm_raster(
    col       = "scenario2_expansion",
    palette   = "brewer.greens",
    col_alpha = 0.8,
    title     = "Moderate productivity-growth expansion (ha)"
  ) +
tm_shape(scenario3_rast, group = "Scenario 3: 5.9% annual growth") +
  tm_raster(
    col       = "scenario3_expansion",
    palette   = "brewer.greens",
    col_alpha = 0.8,
    title     = "High productivity-growth expansion (ha)"
  ) +
tm_shape(prov_sf) +
  tm_borders(col = "grey40", lwd = 1) +
tm_title("Pulp expansion scenarios, 2022-2027")

scenario_map
htmlwidgets::saveWidget(
  tmap_leaflet(scenario_map),
  file          = paste0(wdir, "01_data/04_results/figures/pulp_expansion_scenarios.html"),
  selfcontained = TRUE
)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# publication map: scenario 2 expansion (central estimate) --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Convert scenario 2 raster to data frame (expansion ha per 0.1° cell)
scenario2_plot_df <- as.data.frame(scenario2_rast, xy = TRUE) %>%
  rename(expansion_ha = scenario2_expansion) %>%
  drop_na()

# Transform to WGS84 and clip to Sumatra + Kalimantan
# (st_transform ensures geom_sf and geom_tile share the same CRS)
map_bbox  <- st_bbox(c(xmin = 93, xmax = 121, ymin = -8, ymax = 8), crs = 4326)
prov_wgs84 <- st_transform(prov_sf, 4326)
prov_clip  <- st_crop(prov_wgs84, map_bbox)

# Neighboring country land (Malaysia, Brunei, PNG, etc.) for context
neighbors <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(sovereignt != "Indonesia") %>%
  st_transform(4326) %>%
  st_crop(map_bbox)

pub_map <- ggplot() +
  # Ocean background — drawn first so land sits on top
  annotate("rect", xmin = 93, xmax = 121, ymin = -8, ymax = 8,
           fill = "grey99") +
  # Neighboring countries (lighter grey — context only)
  geom_sf(data = neighbors, fill = "grey80", colour = "grey60", linewidth = 0.2) +
  # Indonesia provinces
  geom_sf(data = prov_clip, fill = "grey92", colour = NA) +
  # Expansion hot spots (scenario 2); geom_tile avoids coord_sf alignment issues
  geom_tile(data = scenario2_plot_df, aes(x = x, y = y, fill = expansion_ha)) +
  scale_fill_gradientn(colours = c("#F0E442", "#E69F00", "#D55E00"),
                       name    = "Projected pulpwood\nplantation expansion\n(ha)",
                       labels  = scales::comma) +
  # Province borders on top
  geom_sf(data = prov_clip, fill = NA, colour = "grey50", linewidth = 0.2) +
  # Island labels
  # Island labels
  annotate("text", x = 102.5, y = -4,
           label = "Sumatra", hjust = 1, vjust = 1,
           size = 3.5, fontface = "italic", colour = "grey20") +
  annotate("text", x = 114, y = -4,
           label = "Kalimantan", hjust = 0.5, vjust = 1,
           size = 3.5, fontface = "italic", colour = "grey20") +
  coord_sf(xlim = c(93, 121), ylim = c(-8, 8), expand = FALSE, crs = 4326) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 11) +
  theme(
    panel.background = element_rect(fill = "grey97", colour = NA),
    panel.grid       = element_line(colour = "white", linewidth = 0.3),
    legend.position        = "inside",
    legend.position.inside = c(0.02, 0.05),
    legend.justification   = c(0, 0),
    legend.background      = element_rect(fill = alpha("white", 0.7), colour = NA),
    axis.text              = element_text(size = 8)
  )

pub_map
ggsave(paste0(wdir, "01_data/04_results/figures/fig_expansion_map.png"),
       pub_map, width = 9, height = 5, dpi = 300)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# combined two-panel figure: (a) map + (b) expansion table --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# as_gtable() produces fixed-width columns; convert to proportional npc units
# so the table stretches to fill whatever width patchwork allocates to it
gt_grob <- as_gtable(expansion_gt)
w <- as.numeric(gt_grob$widths)
gt_grob$widths <- unit(w / sum(w), "npc")

combined_fig <- (pub_map + theme(plot.margin = margin(4, 4, 2, 4, "pt"))) /
  (wrap_elements(full = gt_grob) + theme(plot.margin = margin(0, 4, 4, 4, "pt"))) +
  plot_annotation(tag_levels = "A") +
  plot_layout(heights = c(1.8, 1))

combined_fig
ggsave(paste0(wdir, "01_data/04_results/figures/fig_expansion_combined.png"),
       combined_fig, width = 7.5, height = 8, dpi = 300)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# stats for paper --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# productivity growth (with CI)
cat(sprintf(
  "Productivity growth: %.1f%% per year (%.1f%%-%.1f%%)\n",
  mai_rate["central"] * 100,
  mai_rate["lb"]      * 100,
  mai_rate["ub"]      * 100
))

# Final MAI in 2028
cat(sprintf(
  "MAI in 2028: %.2f m3/ha/yr (%.2f-%.2f m3/ha/yr)\n",
  mai_2028["central"],
  mai_2028["lb"],
  mai_2028["ub"]
))

# Quantity of wood produced through productivity growth on existing plantations
cat(sprintf(
  "Additional amount of production: %.1f million m3 (%.1f -%.1f)\n",
  extra_production["central"],
  extra_production["lb"],
  extra_production["ub"]
))

# Percent of new wood demand met through productivity growth on existing plantations
cat(sprintf(
  "Share of demand met by productivity growth: %.1f%% (%.1f%%-%.1f%%)\n",
  extra_production["central"] / new_wood_demand * 100,
  extra_production["lb"]      / new_wood_demand * 100,
  extra_production["ub"]      / new_wood_demand * 100
))


# total area of pulp expansion (with CI)
# additional_area in millions of ha; scenario_1=high growth=least area, scenario_3=low growth=most area
cat(sprintf(
  "Total pulp expansion: %s ha (%s-%s ha)\n",
  formatC(round(additional_area["central"] * 1e6), format = "d", big.mark = ","),
  formatC(round(additional_area["ub"]      * 1e6), format = "d", big.mark = ","),
  formatC(round(additional_area["lb"]      * 1e6), format = "d", big.mark = ",")
))

# total area of deforestation (with CI)
defor_stats <- expansion_table %>%
  filter(land_type %in% c("Forest on peat", "Forest off peat")) %>%
  summarise(across(c(scenario_1_ha, scenario_2_ha, scenario_3_ha), sum))

cat(sprintf(
  "Deforestation: %s ha (%s-%s ha)\n",
  formatC(defor_stats$scenario_2_ha, format = "d", big.mark = ","),
  formatC(defor_stats$scenario_1_ha, format = "d", big.mark = ","),
  formatC(defor_stats$scenario_3_ha, format = "d", big.mark = ",")
))

# total area of peatland conversion (with CI)
peat_stats <- expansion_table %>%
  filter(land_type %in% c("Forest on peat", "Non-forest on peat")) %>%
  summarise(across(c(scenario_1_ha, scenario_2_ha, scenario_3_ha), sum))

cat(sprintf(
  "Peatland conversion: %s ha (%s-%s ha)\n",
  formatC(peat_stats$scenario_2_ha, format = "d", big.mark = ","),
  formatC(peat_stats$scenario_1_ha, format = "d", big.mark = ","),
  formatC(peat_stats$scenario_3_ha, format = "d", big.mark = ",")
))
