# -*- coding: utf-8 -*-
"""
Created on Tue Apr  6 19:30:33 2021

@author: Jason
"""

import os,arcpy
from arcpy import env

# Set workspace environment
data_dir = os.getcwd() + "/remote/01_data/01_in/"
output_dir = os.getcwd() + "/remote/01_data/02_out/samples/data.gdb"
env.workspace = os.getcwd() + "/remote/01_data/02_out/samples/data.gdb"
env.overwriteOutput=True

# define location of input datasets
idn_proj = data_dir + "bps/idn_adm0_bps_proj.shp"
hti_proj = data_dir + "klhk/IUPHHK_HT_proj.shp"

# =============================================================================
# Draw fishnet sample covering indonesia
# =============================================================================
sample_interval = 100 # x,y distance between samples (m)
origin_x = arcpy.Describe(idn_proj).extent.XMin
origin_y = arcpy.Describe(idn_proj).extent.YMin
yax_y = origin_y + 1

origin_coord = str(origin_x) + ' ' + str(origin_y)
y_axis_coord = str(origin_x) + ' ' + str(yax_y)
arcpy.CreateFishnet_management(out_feature_class = 'fishnet_idn', 
                               origin_coord = origin_coord,
                               y_axis_coord = y_axis_coord, 
                               cell_width = sample_interval, 
                               cell_height = sample_interval, 
                               number_rows = '0', 
                               number_columns = '0',
                               template = idn_proj,
                               labels='NO_LABELS',
                               geometry_type='POLYLINE')

# =============================================================================
# Clip to hti concessions boundary
# =============================================================================
sample_clipped_path = output_dir + 'fishnet_clip_hti'
sample_clipped = arcpy.Intersect_analysis(in_features = [output_dir + '/fishnet_idn', hti_proj],
                        out_feature_class = sample_clipped_path)

# =============================================================================
# Intersect to create sample points from fishnet line intersects
# =============================================================================
intersections= output_dir + "/fishnet_pts"
arcpy.Intersect_analysis(in_features=output_dir + '/fishnet_clip_hti', out_feature_class=intersections, output_type="POINT")

# =============================================================================
# Extract sample points that are completely within concessions
# =============================================================================
selected_samples = arcpy.SelectLayerByLocation_management(intersections,'COMPLETELY_WITHIN',hti_proj)
arcpy.FeatureClassToFeatureClass_conversion(selected_samples,output_dir,"fishnet_pts_hti")

# =============================================================================
# Dissolve to delete overlapping points from intersect 
# =============================================================================
arcpy.Dissolve_management(output_dir + '/fishnet_pts_hti',output_dir + '/samples_hti', "", "","SINGLE_PART", "")

# =============================================================================
# Add sample ID (sid)
# =============================================================================
samples = output_dir + '/samples_hti'
arcpy.AddField_management(in_table = samples, field_name = 'sid', field_type = 'LONG')
arcpy.CalculateField_management(samples, 'sid', '!OBJECTID!')

# =============================================================================
# Export feature class to shapefile
# =============================================================================
outLocation = os.getcwd() + "/remote/01_data/02_out/samples"
inFeatures = arcpy.ListFeatureClasses('samples*') 
arcpy.FeatureClassToShapefile_conversion(inFeatures, outLocation)
