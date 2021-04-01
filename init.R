#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Jason Benedict
# Project: Brazil ILUC
# Date: 1/29/21
# Purpose: Creates a symlink from code directory to a directory storing project data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# NOTE: Must be run with administrator permission

# Define the path to your local code directory
code_dir <- 'D:\\dev\\indonesia\\indo_pulp_defor\\'

# Define the path to your local google drive Treeconomics\\Data directory 
data_dir <- 'D:\\cloud\\Dropbox\\collaborations\\indonesia\\indo_pulp_defor\\'

library(R.utils)
createLink(paste0(code_dir, 'remote\\'), data_dir, overwrite = FALSE)
