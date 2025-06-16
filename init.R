#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Jason Benedict
# Project: Brazil ILUC
# Date: 1/29/21
# Purpose: Creates a symlink from code directory to a directory storing project data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# NOTE: Must be run with administrator permission

# Define the path to your local code directory
code_dir <- '/Users/rheilmayr/dev/indonesia/indo_pulp_defor/'
dir.exists(code_dir)

# Define the path to your local google drive Treeconomics\\Data directory 
data_dir <- '/Users/rheilmayr/Dropbox/collaborations/indonesia/indo_pulp_defor/'
dir.exists(data_dir)

library(R.utils)
createLink(paste0(code_dir, 'remote'), data_dir, overwrite = FALSE)
