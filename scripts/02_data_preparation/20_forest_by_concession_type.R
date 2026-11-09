## ---------------------------------------------------------
##
## Project: Indonesia pulp deforestation
##
## Purpose of script: Summarise forest area by concession type and island
## (SI Table SM12) from the GEE export of
## scripts/01_data_extraction/other_extraction/extract_forest_by_concession_type.js
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2026-11-09
##
## ---------------------------------------------------------
##
## Notes: remaining forest 2022 = gfc_ttm 100 (dryland), 400 (mangrove) and
## 600 (peat swamp), mangroves included as in R/05_paper_stats.R. Forest 2000 =
## all 1xx, 4xx and 6xx codes.
##
## ---------------------------------------------------------

library(tidyverse)

wdir <- "remote"

## read data -------------------------------------------------

forest <- list.files(
  paste0(wdir, "/01_data/02_out/tables"),
  pattern = "^forest_by_concession_type_.*[.]csv$",
  full.names = TRUE
) %>%
  map_dfr(read_csv, show_col_types = FALSE)

island_tab <- tibble(
  island_code = 1:7,
  island = c("Other", "Kalimantan", "Maluku", "Papua", "Sulawesi", "Sumatra", "Other")
)

forest <- forest %>%
  left_join(island_tab, by = "island_code") %>%
  mutate(
    forest_type = case_when(
      gfc_ttm %/% 100 == 1 ~ "Dryland",
      gfc_ttm %/% 100 == 4 ~ "Mangrove",
      gfc_ttm %/% 100 == 6 ~ "Peat swamp"
    ),
    remaining_2022 = gfc_ttm %in% c(100, 400, 600)
  ) %>%
  filter(!is.na(forest_type))

## SM12: remaining forest 2022 by concession type and island --

island_order <- c("Kalimantan", "Maluku", "Papua", "Sulawesi", "Sumatra", "Other")

sm12_long <- forest %>%
  filter(remaining_2022, layer != "indonesia") %>%
  group_by(layer, island) %>%
  summarise(area_ha = sum(area_ha), .groups = "drop")

sm12 <- sm12_long %>%
  bind_rows(
    sm12_long %>%
      group_by(layer) %>%
      summarise(area_ha = sum(area_ha), island = "Indonesia Total")
  ) %>%
  mutate(
    island = factor(island, levels = c(island_order, "Indonesia Total")),
    area_ha = round(area_ha)
  ) %>%
  pivot_wider(names_from = layer, values_from = area_ha) %>%
  arrange(island)

print(sm12, width = Inf)

# remaining forest by type, for checking against the pipeline
# (hti_remaining_forest_2022.csv: 2,964,002 ha in HTI concessions)
forest %>%
  filter(remaining_2022) %>%
  group_by(layer, forest_type) %>%
  summarise(area_ha = round(sum(area_ha)), .groups = "drop") %>%
  pivot_wider(names_from = forest_type, values_from = area_ha) %>%
  print(width = Inf)

## SM1: forest area in 2000, all of Indonesia ---------------

sm1_check <- forest %>%
  filter(layer == "indonesia") %>%
  group_by(forest_type) %>%
  summarise(area_ha = round(sum(area_ha))) %>%
  bind_rows(summarise(., forest_type = "Total", area_ha = sum(area_ha)))

print(sm1_check)

## SM12 as published ------------------------------------------
# Existing HTI from the pipeline (1 ha samples), so the total matches the
# 2.96 Mha in the main text; the GEE hti_pulp_2024 layer agrees within 0.05%.
# Proposed or new allocation for PBPH = Arahan 2022 (without HL) plus PBPH
# applications (Peta_permohonan_PBPH), overlaps counted once. Existing
# selective logging = IUPHHK Hutan Alam without HPT.

hti_pipeline <- read_csv(
  paste0(wdir, "/01_data/02_out/tables/hti_remaining_forest_2022.csv"),
  show_col_types = FALSE
) %>%
  mutate(island = case_when(
    island == "Sumatera" ~ "Sumatra",
    island == "Balinusa" ~ "Other",
    TRUE ~ island
  )) %>%
  group_by(island) %>%
  summarise(existing_hti = sum(area_ha))

sm12_final <- sm12_long %>%
  filter(layer %in% c("arahan_permohonan", "ha_without_hpt")) %>%
  pivot_wider(names_from = layer, values_from = area_ha) %>%
  select(island, proposed_pbph = arahan_permohonan, selective_logging = ha_without_hpt) %>%
  full_join(hti_pipeline, by = "island") %>%
  select(island, existing_hti, proposed_pbph, selective_logging) %>%
  mutate(across(-island, ~ replace_na(.x, 0)))

sm12_final <- sm12_final %>%
  bind_rows(summarise(sm12_final, island = "Indonesia Total", across(-island, sum))) %>%
  mutate(
    island = factor(island, levels = c(island_order, "Indonesia Total")),
    across(-island, round)
  ) %>%
  arrange(island)

print(sm12_final, width = Inf)

## export ------------------------------------------------------

write_excel_csv(sm12, paste0(wdir, "/01_data/02_out/tables/sm12_forest_by_concession_type_all_layers.csv"))
write_excel_csv(sm12_final, paste0(wdir, "/01_data/02_out/tables/sm12_forest_by_concession_type.csv"))
