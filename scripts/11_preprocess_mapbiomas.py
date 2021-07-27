# -*- coding: utf-8 -*-
"""
Created on Tue Jul 27 11:45:47 2021

@author: Jason
"""

import arcpy
import dirfind
from arcpy import env
import os
from pathlib import Path 
import time
import pandas as pd

# Set workspace environment
input_folder = 'D:/General_Data/MapBiomas'
output_wks = 'D:\z_Temp\indo_pulp_defor\mapbiomas_idn.gdb'
env.workspace = 'D:\z_Temp\indo_pulp_defor\mapbiomas_idn.gdb'
env.overwriteOutput=True

######################################
### Copy shapefiles to geodatabase
######################################

list_shps=[]
walk = arcpy.da.Walk(input_folder,datatype="FeatureClass")

for dir_path, dir_names, file_names in walk:
    for filename in file_names:
        list_shps.append(os.path.join(dir_path,filename)) # FULL path to each shp
     
for shapefile in list_shps:
    # Determine the new output feature class path and name
    file_name = os.path.splitext(os.path.basename(shapefile))[0] 
    outFC = os.path.join(output_wks, file_name.strip(".shp"))
    arcpy.CopyFeatures_management(shapefile, outFC)

###################################
#### Union fcs ####################
###################################    

fcList = arcpy.ListFeatureClasses()
arcpy.Union_analysis(fcList,"mapbiomas_union")
