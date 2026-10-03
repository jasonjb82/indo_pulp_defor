# Packages the pipeline needs at run time that renv's code scan cannot see.
# renv reads this file when snapshotting; nothing sources it (it is outside
# R/, so tar_source() ignores it).

# Random forest engine behind parsnip::set_engine("ranger"), used to fit and
# to load the saved model (R/analysis/04_pulp_expansion_model.R)
library(ranger)

# Used by modelsummary to write the .docx SI Tables 6, 9 and 10 (alongside
# the pandoc program itself, which must be on the PATH)
library(pandoc)

# Map data loaded by rnaturalearth::ne_countries(scale = "medium") for the
# neighbouring-country outlines in Figure 3
# (R/analysis/05_pulp_expansion_scenarios.R)
library(rnaturalearthdata)
