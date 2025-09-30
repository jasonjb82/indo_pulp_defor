# -*- coding: utf-8 -*-
"""
Created on Wed Jun 10 14:21:41 2020

# Notes: 

@author: Jason
"""
import os,arcpy
from arcpy import env
from collections import Counter
import pandas as pd
import numpy as np

# Set workspace environment

data_dir = os.getcwd() + "/remote/01_data/01_in/"
env.workspace = os.getcwd() + "/remote/data//01_in/"
env.overwriteOutput=True

###############################################################################
# Prep data layers
###############################################################################

arcpy.MakeFeatureLayer_management(data_dir + "gaveau/data.gdb/ioppitp_expansion_2024", "ioppitp_2024")
arcpy.MakeFeatureLayer_management(data_dir + "klhk/IUPHHK_HTI_TRASE_20230314_proj.shp", "hti")

# Reproject
# Define paths to your input and target layers
ioppitp_reproj = data_dir + "gaveau/data.gdb/ioppitp_expansion_2024_reproj"

try:
    # Get the spatial reference of the target layer
    target_desc = arcpy.Describe("hti")
    target_sr = target_desc.spatialReference

    # Reproject the input feature class to the target's coordinate system
    arcpy.management.Project("ioppitp_2024", ioppitp_reproj, target_sr)

except arcpy.ExecuteError:
    print(arcpy.GetMessages(2))
except Exception as e:
    print(f"An error occurred: {e}")

###############################################################################
# Union layers
###############################################################################

arcpy.MakeFeatureLayer_management(data_dir + "gaveau/data.gdb/ioppitp_expansion_2024_reproj", "ioppitp_reproj")

inFeatures = [["ioppitp_reproj", 1], ["hti", 2]]
union_result = data_dir + "gaveau/data.gdb/ioppitp_hti_union"
arcpy.Union_analysis(inFeatures,union_result)

###############################################################################
# Union layers
###############################################################################

# Set the name of the field to calculate
field_name = "Area_Ha"

# Construct the expression to calculate area in hectares
expression = "!shape.geodesicArea@hectares!"

# Set the expression type to Python 3
expression_type = "Python3"

# Execute the Calculate Field tool
arcpy.management.CalculateField(union_result, field_name, expression, expression_type)

# =============================================================================
# # Delete non-required columns
# =============================================================================
fields = [f.name for f in arcpy.ListFields(union_result)]
keep_fields = ['OBJECTID','FID','Shape','ID','year','gridcod','CmpDrvn','prv_cvr','cnvrsn_','Distrct','Provinc','Area_Ha','Shape_Area','Shape_Length']
result = list((Counter(fields)-Counter(keep_fields)).elements())
arcpy.DeleteField_management(union_result,result)

# =============================================================================
# # Delete non-required rows
# =============================================================================

# Define the field to check for nulls
field_to_check = "prv_cvr"  # Replace with the name of the field to check

# Create an UpdateCursor
# The UpdateCursor allows you to read and modify rows in a table or feature class
with arcpy.da.UpdateCursor(union_result, [field_to_check]) as cursor:
    for row in cursor:
        # Check if the field value is None (null) or an empty string
        # You might need to adjust this condition based on how "no value" is represented in your data
        if row[0] is None or (isinstance(row[0], str) and row[0].strip() == ""):
            cursor.deleteRow()

print("Rows with no value in '{}' field have been removed.".format(field_to_check))
