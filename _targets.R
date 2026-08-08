## ---------------------------------------------------------
## Project: Indonesia Pulp Deforestation Pipeline
## Purpose: Master targets workflow configuration file
## ---------------------------------------------------------

library(targets)
library(sf)
library(tidyfast)
library(ggbreak)
library(svglite)

# =========================================================================
# 1. PIPELINE OPTIONS & REQUIRED PACKAGES
# =========================================================================
tar_option_set(
  packages = c(
    "tidyverse",
    "sf",
    "data.table",
    "dtplyr",
    "readxl",
    "janitor",
    "lubridate",
    "scales",
    "tidyfast",
    "patchwork",
    "ggbreak",
    "ggrepel",
    "showtext",
    "sysfonts",
    "svglite"
  ),
  garbage_collection = TRUE,
  format = "rds"
)

# =========================================================================
# 2. SOURCE FUNCTION SCRIPTS
# =========================================================================
tar_source()
# =========================================================================
# 3. TARGET PIPELINE DEFINITION
# =========================================================================
list(
  # -----------------------------------------------------------------------
  # A. FILE TRACKING (ZENODO DOWNLOAD LOCATION)
  # -----------------------------------------------------------------------

  tar_target(
    zenodo_data_check,
    download_zenodo_data(
      zenodo_record_id = "21542417",
      output_dir = "data/01_data_replication"
    ),
    format = "file"
  ),

  tar_target(
    kab_file,
    file.path(zenodo_data_check, "01_in/big/idn_kabupaten_big.shp"),
    format = "file"
  ),
  tar_target(
    hti_file,
    file.path(
      zenodo_data_check,
      "01_in/klhk/IUPHHK_HTI_TRASE_20230314_proj.shp"
    ),
    format = "file"
  ),
  tar_target(
    policy_tl_file,
    file.path(zenodo_data_check, "01_in/tables/policy_timeline_cats_rev1.csv"),
    format = "file"
  ),
  tar_target(
    pulp_for_id_file,
    file.path(
      zenodo_data_check,
      "02_out/gee/gaveau/pulp_annual_defor_forest_id.csv"
    ),
    format = "file"
  ),
  tar_target(
    pulp_nonfor_id_file,
    file.path(
      zenodo_data_check,
      "02_out/gee/gaveau/pulp_annual_defor_non-forest_id.csv"
    ),
    format = "file"
  ),
  tar_target(
    timber_for_pulp_file,
    file.path(zenodo_data_check, "01_in/obidzinski_dermawan/plot_data.csv"),
    format = "file"
  ),
  tar_target(
    pulp_prices_file,
    file.path(zenodo_data_check, "01_in/tables/WPU0911_FRED.csv"),
    format = "file"
  ),
  tar_target(
    pulp_production_file,
    file.path(zenodo_data_check, "01_in/tables/annual_pulp_shr_prod.xlsx"),
    format = "file"
  ),
  tar_target(
    hti_conv_timing_file,
    file.path(
      zenodo_data_check,
      "02_out/tables/hti_grps_deforestation_timing.csv"
    ),
    format = "file"
  ),
  tar_target(
    hti_annual_lc_file,
    file.path(zenodo_data_check, "02_out/tables/hti_land_use_change_areas.csv"),
    format = "file"
  ),
  tar_target(
    lic_dates_hti_file,
    file.path(zenodo_data_check, "01_in/wwi/HTI_LICENSE_DATES.csv"),
    format = "file"
  ),
  tar_target(
    samples_hti_file,
    file.path(zenodo_data_check, "02_out/samples/samples_hti_id.csv"),
    format = "file"
  ),
  tar_target(
    ann_pulp_tbl_file,
    file.path(
      zenodo_data_check,
      "02_out/tables/pulp_expansion_areas_2001_2022.csv"
    ),
    format = "file"
  ),
  tar_target(
    hti_nonhti_conv_file,
    file.path(
      zenodo_data_check,
      "02_out/tables/idn_pulp_conversion_hti_nonhti_treemap.csv"
    ),
    format = "file"
  ),
  tar_target(
    samples_landuse_ttm_file,
    file.path(zenodo_data_check, "02_out/tables/samples_landuse_ttm.csv"),
    format = "file"
  ),
  tar_target(
    samples_gfc_ttm_file,
    file.path(zenodo_data_check, "02_out/tables/samples_gfc_ttm.csv"),
    format = "file"
  ),
  tar_target(
    rs_acc_file,
    file.path(zenodo_data_check, "04_results/rs_accuracy_paper_stats.csv"),
    format = "file"
  ),
  tar_target(
    id_annual_exp_file,
    file.path(
      zenodo_data_check,
      "02_out/tables/id_annual_expansion_stats_ttm.csv"
    ),
    format = "file"
  ),
  tar_target(
    pw_annual_area_file,
    file.path(zenodo_data_check, "02_out/gee/pulp_annual_area_id.csv"),
    format = "file"
  ),
  tar_target(
    pulp_soil_file,
    file.path(
      zenodo_data_check,
      "02_out/gee/gaveau/idn_pulp_annual_expansion_peat_mineral_soils.csv"
    ),
    format = "file"
  ),
  tar_target(
    kali_exp_file,
    file.path(
      zenodo_data_check,
      "02_out/tables/kali_annual_pulp_exp_stats_ttm.csv"
    ),
    format = "file"
  ),
  tar_target(
    groups_reclass_file,
    file.path(
      zenodo_data_check,
      "01_in/tables/ALIGNED_NAMES_GROUP_HTI_reclassed.csv"
    ),
    format = "file"
  ),
  tar_target(
    scenario_stats_file,
    file.path(zenodo_data_check, "04_results/scenario_stats.csv"),
    format = "file"
  ),
  tar_target(
    mai_file,
    file.path(zenodo_data_check, "04_results/key_parameters.csv"),
    format = "file"
  ),
  tar_target(
    ws_2015_2022_file,
    file.path(zenodo_data_check, "02_out/tables/ws_merge_clean_2015_2022.csv"),
    format = "file"
  ),
  tar_target(
    cap_df_file,
    file.path(zenodo_data_check, "01_in/wwi/MILLS_EXPORTERS_20200405.xlsx"),
    format = "file"
  ),

  # -----------------------------------------------------------------------
  # B. RAW DATA INGESTION & DATA CLEANING
  # -----------------------------------------------------------------------
  tar_target(kab, read_kab_data(kab_file)),
  tar_target(hti, read_hti_data(hti_file)),
  tar_target(policy_tl, read_csv(policy_tl_file, show_col_types = FALSE)),
  tar_target(
    pulp_for_id,
    read_csv(pulp_for_id_file, show_col_types = FALSE) %>%
      select(-`system:index`, -.geo)
  ),
  tar_target(
    pulp_nonfor_id,
    read_csv(pulp_nonfor_id_file, show_col_types = FALSE) %>%
      select(-`system:index`, -.geo)
  ),
  tar_target(timber_for_pulp, read_ws_data(timber_for_pulp_file)),
  tar_target(pulp_prices, read_csv(pulp_prices_file, show_col_types = FALSE)),
  tar_target(pulp_production, read_cap_df(pulp_production_file)),
  tar_target(
    hti_conv_timing,
    read_csv(hti_conv_timing_file, show_col_types = FALSE)
  ),
  tar_target(
    hti_annual_lc,
    read_csv(hti_annual_lc_file, show_col_types = FALSE)
  ),
  tar_target(
    lic_dates_hti,
    read_csv(
      lic_dates_hti_file,
      col_types = cols(license_date = col_date("%m/%d/%Y"))
    )
  ),
  tar_target(samples_hti, read_csv(samples_hti_file, show_col_types = FALSE)),
  tar_target(ann_pulp_tbl, read_csv(ann_pulp_tbl_file, show_col_types = FALSE)),
  tar_target(hti_nonhti_conv, read_hti_nonhti_conv(hti_nonhti_conv_file)),
  tar_target(
    samples_landuse_ttm,
    read_csv(samples_landuse_ttm_file, show_col_types = FALSE)
  ),
  tar_target(
    samples_gfc_ttm,
    read_csv(samples_gfc_ttm_file, show_col_types = FALSE)
  ),
  tar_target(rs_acc_df, read_csv(rs_acc_file, show_col_types = FALSE)),
  tar_target(
    id_annual_exp_stats,
    read_csv(id_annual_exp_file, show_col_types = FALSE)
  ),
  tar_target(
    pw_annual_area_id,
    read_csv(pw_annual_area_file, show_col_types = FALSE)
  ),
  tar_target(
    pulp_ttm_soil_type,
    read_csv(pulp_soil_file, show_col_types = FALSE)
  ),
  tar_target(
    kali_annual_pulp_exp_stats,
    read_csv(kali_exp_file, show_col_types = FALSE)
  ),
  tar_target(
    groups_reclass_hti,
    read_csv(groups_reclass_file, show_col_types = FALSE)
  ),
  tar_target(
    scenario_stats,
    read_csv(scenario_stats_file, show_col_types = FALSE)
  ),
  tar_target(mai_df, read_csv(mai_file, show_col_types = FALSE)),
  tar_target(ws_2015_2022, read_csv(ws_2015_2022_file, show_col_types = FALSE)),
  tar_target(cap_df, read_cap_df(cap_df_file)),

  # -----------------------------------------------------------------------
  # C. SCRIPT 1: FIGURE 1 (SUMMARY TRENDS)
  # -----------------------------------------------------------------------
  tar_target(islands_df, prep_island_mapping(kab)),
  tar_target(
    id_pulp_conv_for,
    clean_pulp_conversion(pulp_for_id, islands_df, "forest")
  ),
  tar_target(
    id_pulp_conv_nonfor,
    clean_pulp_conversion(pulp_nonfor_id, islands_df, "non-forest")
  ),
  tar_target(pulp_prices_clean, clean_pulp_prices(pulp_prices)),
  tar_target(
    defor_price_comb,
    prep_defor_price_comb(
      id_pulp_conv_for,
      id_pulp_conv_nonfor,
      pulp_prices_clean
    )
  ),
  tar_target(
    pulp_prod_ratio_merged,
    prep_wood_supply_data(timber_for_pulp, pulp_production)
  ),
  tar_target(tl_df, prep_timeline_data(policy_tl)),

  # Panels & Composite Export
  tar_target(fig1_panel_a, plot_panel_a(defor_price_comb)),
  tar_target(fig1_panel_b, plot_panel_b(pulp_prod_ratio_merged)),
  tar_target(fig1_panel_c, plot_panel_c(tl_df)),
  tar_target(
    fig1_summary,
    create_fig1_summary(fig1_panel_a, fig1_panel_b, fig1_panel_c)
  ),
  tar_target(
    fig1_files,
    save_fig1(
      fig1_summary,
      "data/01_data_replication/04_results/figures/f1_summary_figure.png",
      "data/01_data_replication/04_results/figures/f1_summary_figure.svg"
    ),
    format = "file"
  ),

  # -----------------------------------------------------------------------
  # D. SCRIPT 2: FIGURE 2 (DEFORESTATION TIMING BY SUPPLIER)
  # -----------------------------------------------------------------------
  tar_target(
    freq_tab_fig2,
    prep_hti_defor_timing(hti_conv_timing)
  ),
  tar_target(
    fig2_png,
    save_fig2(
      freq_tab_fig2,
      "data/01_data_replication/04_results/figures/f2_supplier_groups_defor_class_plot.png"
    ),
    format = "file"
  ),

  # -----------------------------------------------------------------------
  # E. SCRIPT 3: SI CONCESSION ANNUAL LAND COVER CHANGE FIGURES
  # -----------------------------------------------------------------------
  tar_target(
    concession_plots_saved,
    render_and_save_all_concessions(
      hti_annual_lc,
      "data/01_data_replication/04_results/figures/concessions/"
    ),
    format = "file"
  ),

  # -----------------------------------------------------------------------
  # F. SCRIPT 4: SI TABLE 2 (MAPPED PULP EXPANSION TABLE)
  # -----------------------------------------------------------------------
  tar_target(hti_concession_names, clean_hti_concession_names(hti)),
  tar_target(hti_dates_clean, clean_hti_license_dates(lic_dates_hti)),
  tar_target(
    samples_df,
    prep_samples_df(
      samples_gfc_ttm,
      samples_hti,
      samples_landuse_ttm,
      hti_dates_clean,
      hti_concession_names
    )
  ),
  tar_target(hti_pulp_conv, get_hti_pulp_conversion(samples_df)),
  tar_target(hti_pulp_conv_all, calc_hti_pulp_expansion_all(hti_pulp_conv)),
  tar_target(
    hti_pulp_conv_license,
    calc_hti_pulp_expansion_post_license(hti_pulp_conv)
  ),
  tar_target(
    hti_pulp_driven_defor,
    calc_hti_pulp_driven_defor(hti_nonhti_conv)
  ),
  tar_target(
    si_table_2_df,
    prep_si_table_2(
      ann_pulp_tbl,
      hti_pulp_driven_defor,
      hti_pulp_conv_all,
      hti_pulp_conv_license
    )
  ),
  tar_target(
    si_table_2_csv,
    save_si_table_2(
      si_table_2_df,
      "data/01_data_replication/02_out/tables/pulp_expansion_areas_all_2001_2022.csv"
    ),
    format = "file"
  ),

  # -----------------------------------------------------------------------
  # G. SCRIPT 5: MANUSCRIPT PAPER STATISTICS (SUMMARY TEXT REPORT)
  # -----------------------------------------------------------------------
  tar_target(
    paper_stats,
    calc_paper_stats(
      rs_acc_df = rs_acc_df,
      id_annual_exp_stats = id_annual_exp_stats,
      pw_annual_area_id = pw_annual_area_id,
      pulp_ttm_soil_type = pulp_ttm_soil_type,
      ws_2015_2022 = ws_2015_2022,
      kali_annual_pulp_exp_stats = kali_annual_pulp_exp_stats,
      hti_nonhti_conv = hti_nonhti_conv,
      groups_reclass_hti = groups_reclass_hti,
      cap_df = cap_df,
      scenario_stats = scenario_stats,
      mai_df = mai_df
    )
  ),
  tar_target(
    paper_stats_txt,
    save_paper_stats(
      paper_stats,
      "data/01_data_replication/02_out/tables/paper_text_snippets.txt"
    ),
    format = "file"
  )
)
