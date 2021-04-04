# -*- coding: utf-8 -*-
"""
Created on Fri Apr  2 08:06:39 2021

@author: Jason
"""

import arcpy, os

###############################################################################
######## Generating sample ID's within HTI concessions ########################
###############################################################################

def CreateFishnetsForFeats(in_polys, out_loc, cell_x=0, cell_y=0):

    '''
    in_polys: input polygon feature class
    out_loc: folder location for new file gdb containing fishnet feature class
    cell_x: cell width
    cell_y: cell height
    '''

    # Set output directroy
    output_dir = os.getcwd() + "/remote/01_data/02_out/samples/"  
    arcpy.env.overwriteOutput = True
    # spatial reference
    arcpy.env.outputCoordinateSystem = arcpy.Describe(in_polys).spatialReference
    # Loop thru rows of input polygons
    with arcpy.da.SearchCursor(polys, ['SHAPE@', 'OID@','ID']) as rows:
        for row in rows:
            ext = row[0].extent
            st = '%f %f' %(ext.XMin, ext.YMin)
            orien = '%f %f' %(ext.XMin, ext.YMax)
            if cell_y == 0:
                n_rows = 1
                cell_y = ext.height
            else:
                n_rows = int((ext.height - (ext.height % cell_y)) / cell_y) + 1
            if cell_x == 0:
                n_cols = 1
                cell_x = ext.width
            else:
                n_cols = int((ext.width - (ext.width % cell_x)) / cell_x) + 1

            # create fishnet
            out = os.path.join(output_dir, 'fish_{0}'.format(row[2]))
            arcpy.CreateFishnet_management(out, st, orien, cell_x,
                                           cell_y, n_rows, n_cols,
                                           labels='LABELS')
            where='"ID"' + '=' + '\'' + str(row[2]) + '\''
            hti_selected = arcpy.SelectLayerByAttribute_management(in_polys,"NEW_SELECTION",where)
            selected_samples = arcpy.SelectLayerByLocation_management(output_dir + 'fish_{0}'.format(row[2]) + "_label.shp",'COMPLETELY_WITHIN',hti_selected)
            arcpy.FeatureClassToFeatureClass_conversion(selected_samples, output_dir,'samples_{0}'.format(row[2]) + ".shp")
            arcpy.Delete_management(selected_samples)

    # set workspace to output_dir
    arcpy.env.workspace = output_dir
    samples = arcpy.ListFeatureClasses('samples_*')
    targ = samples[0]
    for i, sample in enumerate(samples):
        # add field for original polygon ID
        #fid0 = sample.split('_')[1]
        #fid = fid0.split('.')[0]
        #arcpy.AddField_management(sample, 'HT_ID', 'TEXT')
        #with arcpy.da.UpdateCursor(sample, ['HT_ID']) as rows:
        #    for row in rows:
        #        row[0] = fid
        #        rows.updateRow(row)
        # append fishnets into one feature class
        if i > 0:
            arcpy.Append_management([sample], targ, 'NO_TEST')
            
            arcpy.Delete_management(sample)
            print ('Appended: {0}'.format(sample))
    # deleting unused files
    fish = arcpy.ListFeatureClasses('fish_*')
    arcpy.Delete_management(fish)
    # rename file
    append_shp = arcpy.ListFeatureClasses('samples_*')
    arcpy.Rename_management(append_shp[0],"samples_hti.shp")  
    # adding sample ids
    arcpy.AddField_management(in_table = output_dir + 'samples_hti.shp', field_name = 'sid', field_type = 'LONG')
    arcpy.CalculateField_management(output_dir + 'samples_hti.shp', 'sid', "!FID!", "PYTHON")   
    arcpy.DeleteField_management(output_dir + 'samples_hti.shp',['Id'])
    print ('Done')

    return
if __name__ == '__main__':
    data_dir = os.getcwd() + "/remote/01_data/01_in/"
    polys = data_dir + "klhk/IUPHHK_HT_proj.shp"
    loc = os.getcwd() + "/remote/01_data/02_out/samples/" 
    CreateFishnetsForFeats(polys, loc, 100, 100)