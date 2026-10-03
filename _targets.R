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
  # Tracks the small .zenodo_record marker rather than the whole data folder,
  # so editing one input file only invalidates the targets that use it. The
  # download runs when the marker is missing (data deleted) or the record ID
  # below changes. To move to a new Zenodo version, change zenodo_record_id.
  tar_target(
    zenodo_marker,
    file.path(
      download_zenodo_data(
        zenodo_record_id = "21542417",
        output_dir = "data/01_data_replication"
      ),
      ".zenodo_record"
    ),
    format = "file"
  ),
  tar_target(zenodo_data_check, dirname(zenodo_marker)),

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
    id_annual_exp_file,
    file.path(
      zenodo_data_check,
      "02_out/tables/id_annual_expansion_stats_ttm.csv"
    ),
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
  tar_target(ann_pulp_tbl, calc_annual_pulp_expansion(id_annual_exp_stats)),
  tar_target(hti_nonhti_conv, read_hti_nonhti_conv(hti_nonhti_conv_file)),
  tar_target(
    samples_landuse_ttm,
    read_csv(samples_landuse_ttm_file, show_col_types = FALSE)
  ),
  tar_target(
    samples_gfc_ttm,
    read_csv(samples_gfc_ttm_file, show_col_types = FALSE)
  ),
  tar_target(
    id_annual_exp_stats,
    read_csv(id_annual_exp_file, show_col_types = FALSE)
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
      "outputs/figures/f1_summary_figure.png",
      "outputs/figures/f1_summary_figure.svg"
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
      "outputs/figures/f2_supplier_groups_defor_class_plot.png"
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
      "outputs/figures/concessions/"
    ),
    format = "file"
  ),

  # -----------------------------------------------------------------------
  # E2. SCRIPT 6: SI SECTION 9 CONCESSION ATLAS (GENERATED PDF APPENDIX)
  # -----------------------------------------------------------------------
  # Tracking the template as a file target means editing the layout rebuilds
  # only the PDF (about two seconds), not the 305 tiles.
  tar_target(
    atlas_template_file,
    "typst/concession_atlas.typ",
    format = "file"
  ),
  tar_target(
    atlas_meta,
    build_atlas_metadata(
      hti_annual_lc,
      hti_conv_timing,
      groups_reclass_hti,
      hti
    )
  ),
  tar_target(
    concession_tile_pngs,
    render_and_save_concession_tiles(
      hti_annual_lc,
      atlas_meta,
      "outputs/figures/concession_tiles"
    ),
    format = "file"
  ),
  tar_target(
    atlas_data_typ,
    write_atlas_data_typ(
      concession_tile_pngs,
      atlas_meta,
      "outputs/atlas/atlas_data.typ"
    ),
    format = "file"
  ),
  # sm_pages = 0 numbers the atlas from 1. The merge script recompiles with the
  # exported SI's real page count so folios continue that document's numbering.
  # The PDF embeds the tile images, but atlas_data_typ only lists their paths,
  # so naming concession_tile_pngs here makes the PDF rebuild when a tile
  # changes even if the paths do not.
  tar_target(
    concession_atlas_pdf,
    {
      concession_tile_pngs
      compile_concession_atlas(
        atlas_template_file,
        atlas_data_typ,
        "outputs/atlas/concession_atlas.pdf",
        sm_pages = 0L
      )
    },
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
      "outputs/tables/pulp_expansion_areas_all_2001_2022.csv"
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
      "outputs/text/paper_text_snippets.txt"
    ),
    format = "file"
  ),

  # -----------------------------------------------------------------------
  # H. ANALYSIS 01: RS ACCURACY ASSESSMENT
  # -----------------------------------------------------------------------
  # Computes rs_acc_df from the validation sample

  tar_target(
    validation_xlsx_file,
    file.path(
      zenodo_data_check,
      "01_in/gaveau/Validation_11classes_land-cover-change-map_v2.xlsx"
    ),
    format = "file"
  ),
  tar_target(rs_acc_results, run_rs_accuracy(validation_xlsx_file)),
  tar_target(rs_acc_df, rs_acc_results$paper_stats),
  tar_target(
    si_table3_csv,
    save_csv_table(
      rs_acc_results$si_table3,
      "outputs/tables/si_table3_class_descriptions.csv"
    ),
    format = "file"
  ),
  tar_target(
    si_table4_csv,
    save_csv_table(
      rs_acc_results$si_table4,
      "outputs/tables/si_table4_change_map_accuracy.csv"
    ),
    format = "file"
  ),
  tar_target(
    si_table5_csv,
    save_csv_table(
      rs_acc_results$si_table5,
      "outputs/tables/si_table5_static_map_accuracy.csv"
    ),
    format = "file"
  ),
  tar_target(
    rs_acc_diagnostics_txt,
    save_text_lines(
      rs_acc_results$diagnostics,
      "outputs/text/rs_accuracy_diagnostics.txt"
    ),
    format = "file"
  ),

  # -----------------------------------------------------------------------
  # I. ANALYSIS 02: DMAI AND PRODUCTIVITY TRENDS
  # -----------------------------------------------------------------------
  # Computes key parameters (mai_df) and concession DMAI from the harvest
  # record rather than reading 04_results/key_parameters.csv from Zenodo.
  tar_target(
    harvest_file,
    file.path(zenodo_data_check, "02_out/tables/hti_harvest_yr.csv"),
    format = "file"
  ),
  tar_target(mai_results, run_calc_mai(harvest_file, ws_2015_2022)),
  tar_target(mai_df, mai_results$key_parameters),
  # .docx output needs pandoc on the PATH
  tar_target(
    si_table6_docx,
    save_mai_table(mai_results, "outputs/tables/si_table6_yield_growth.docx"),
    format = "file"
  ),
  tar_target(
    si_section3_txt,
    save_text_lines(
      mai_results$si_text,
      "outputs/text/si_section3_statements.txt"
    ),
    format = "file"
  ),

  # -----------------------------------------------------------------------
  # J. ANALYSIS 03: DEFORESTATION ELASTICITY (SI SECTIONS 4.2 AND 5)
  # -----------------------------------------------------------------------
  # pulp_prices_annual_2001_2024.csv is derived from licensed price data by
  # scripts/02_data_preparation/19_prep_pulp_prices.R, outside the pipeline.
  tar_target(
    defor_long_file,
    file.path(
      zenodo_data_check,
      "02_out/tables/tbl_long_pulp_clearing_gfc_forest.csv"
    ),
    format = "file"
  ),
  tar_target(
    grid_admin_file,
    file.path(zenodo_data_check, "02_out/tables/grid_10km_adm_prov_kab_kec.csv"),
    format = "file"
  ),
  tar_target(
    pulp_prices_annual_file,
    file.path(
      zenodo_data_check,
      "02_out/tables/pulp_prices_annual_2001_2024.csv"
    ),
    format = "file"
  ),
  tar_target(
    gaez_hti_file,
    file.path(zenodo_data_check, "02_out/tables/gaez_hti_areas.csv"),
    format = "file"
  ),
  tar_target(
    gaez_grid_file,
    file.path(zenodo_data_check, "02_out/tables/gaez_grid_share.csv"),
    format = "file"
  ),
  tar_target(
    mill_prod_file,
    file.path(zenodo_data_check, "01_in/wwi/MILL_PRODUCTION_2015_2024.xlsx"),
    format = "file"
  ),
  tar_target(
    elast_results,
    run_defor_elasticity(
      defor_long_csv = defor_long_file,
      grid_admin_csv = grid_admin_file,
      pulp_prices_csv = pulp_prices_annual_file,
      gaez_hti_csv = gaez_hti_file,
      gaez_grid_csv = gaez_grid_file,
      hti_mai = mai_results$hti_mai,
      cap_df = cap_df,
      mill_prod_xlsx = mill_prod_file
    )
  ),
  tar_target(
    si_table8_csv,
    save_csv_table(
      elast_results$si_table8,
      "outputs/tables/si_table8_aez_productivity.csv",
      writer = "readr"
    ),
    format = "file"
  ),
  tar_target(
    si_fig3_png,
    save_si_fig3(elast_results, "outputs/figures/SI_f3_elasticity.png"),
    format = "file"
  ),
  tar_target(
    si_sections4_5_txt,
    save_text_lines(
      elast_results$si_text,
      "outputs/text/si_sections4_5_statements.txt"
    ),
    format = "file"
  ),
  tar_target(
    elast_diagnostics_txt,
    save_text_lines(
      elast_results$diagnostics,
      "outputs/text/defor_elasticity_diagnostics.txt"
    ),
    format = "file"
  ),
  # SI Tables 9 and 10; .docx output needs pandoc on the PATH
  tar_target(
    si_tables9_10_docx,
    save_defor_elast_tables(
      elast_results,
      main_path = "outputs/tables/si_table9_defor_elasticity.docx",
      robust_path = "outputs/tables/si_table10_defor_elasticity_robustness.docx"
    ),
    format = "file"
  ),

  # -----------------------------------------------------------------------
  # K. ANALYSIS 04: SPATIAL MODEL OF PULP EXPANSION (SI SECTION 8)
  # -----------------------------------------------------------------------
  # Predictions come from the authors' saved model (rf_final_fit.rds), which
  # reproduces the published predictions exactly. Re-estimation (rf_results,
  # ~15 min) is version-sensitive: tuning is a near-tie between mtry 13 and
  # 22, so other package versions can select a different mtry. SI Figure 4
  # and the SI Section 8 statements below come from the re-estimation.
  tar_target(
    rf_vars_2017_file,
    file.path(zenodo_data_check, "02_out/tables/pulp_exp_model_var_1km_2017.csv"),
    format = "file"
  ),
  tar_target(
    rf_vars_2022_file,
    file.path(zenodo_data_check, "02_out/tables/pulp_exp_model_var_1km_2022.csv"),
    format = "file"
  ),
  tar_target(
    rf_final_fit_file,
    file.path(zenodo_data_check, "02_out/models/rf_final_fit.rds"),
    format = "file"
  ),
  tar_target(
    pulp_predictions,
    predict_pulp_expansion(rf_final_fit_file, rf_vars_2022_file)
  ),
  tar_target(
    pulp_predictions_csv,
    save_csv_table(
      pulp_predictions,
      "outputs/tables/pulp_predictions.csv",
      writer = "readr"
    ),
    format = "file"
  ),
  tar_target(
    rf_results,
    run_pulp_expansion_model(rf_vars_2017_file, rf_vars_2022_file)
  ),
  tar_target(
    si_fig4_png,
    save_si_fig4(rf_results, "outputs/figures/SI_f4_auc.png"),
    format = "file"
  ),
  tar_target(
    si_section8_txt,
    save_text_lines(rf_results$si_text, "outputs/text/si_section8_statements.txt"),
    format = "file"
  ),
  tar_target(
    rf_diagnostics_txt,
    save_text_lines(
      rf_results$diagnostics,
      "outputs/text/rf_model_diagnostics.txt"
    ),
    format = "file"
  ),

  # -----------------------------------------------------------------------
  # L. ANALYSIS 05: PULP EXPANSION SCENARIOS (FIGURE 3, SI 4.3 AND 8.4)
  # -----------------------------------------------------------------------
  # Computes scenario_stats from the predictions rather than reading
  # 04_results/scenario_stats.csv from Zenodo.
  tar_target(
    scenario_results,
    run_pulp_expansion_scenarios(
      pulp_predictions = pulp_predictions,
      mai_df = mai_df,
      rs_acc_df = rs_acc_df,
      cap_df = cap_df,
      ws_2015_2022 = ws_2015_2022,
      kab = kab
    )
  ),
  tar_target(scenario_stats, scenario_results$scenario_stats),
  tar_target(
    scenario_stats_csv,
    save_csv_table(
      scenario_stats,
      "outputs/tables/scenario_stats.csv",
      writer = "readr"
    ),
    format = "file"
  ),
  tar_target(
    fig3_png,
    save_fig3(scenario_results, "outputs/figures/f3_expansion_combined.png"),
    format = "file"
  ),
  tar_target(
    si_sections4_8_txt,
    save_text_lines(
      scenario_results$si_text,
      "outputs/text/si_sections4_8_statements.txt"
    ),
    format = "file"
  ),
  tar_target(
    scenario_diagnostics_txt,
    save_text_lines(
      scenario_results$diagnostics,
      "outputs/text/scenario_diagnostics.txt"
    ),
    format = "file"
  ),

  # -----------------------------------------------------------------------
  # M. CHECK PAPER STATS AGAINST THE MANUSCRIPT
  # -----------------------------------------------------------------------
  # manuscript_values.csv holds each number as the manuscript prints it.
  # Update it whenever the manuscript text changes.
  tar_target(
    manuscript_values_file,
    "manuscript/manuscript_values.csv",
    format = "file"
  ),
  tar_target(
    manuscript_check,
    check_manuscript_values(paper_stats, manuscript_values_file)
  ),
  tar_target(
    manuscript_check_csv,
    save_manuscript_check(
      manuscript_check,
      "outputs/text/manuscript_check.csv"
    ),
    format = "file"
  )
)
