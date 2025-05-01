# -*- coding: utf-8 -*-
"""
Created on Mon Jun 21 12:53:18 2021

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
dropbox_dir = dirfind.guess_dropbox_dir()
path = dropbox_dir + "indo_pulp_defor/01_data/01_in/ucsb/transport_cost/"
input_gdb = path + "odmatrix.gdb"
env.workspace = path + "odmatrix.gdb"
env.overwriteOutput = True

# Specify island for create OD matrix analysis
# Options: kalimantan, sumatera, sulawesi, papua and jawa
island = "sumatera"

# Set inputs (mills, ports, etc)
origins = os.path.join(input_gdb, "centroids")
destinations = os.path.join(input_gdb, "mills")
network = os.path.join(input_gdb, island, island + "_ND")

# Get field names for origin and destination points
orig_field_names = [f.name for f in arcpy.ListFields(origins)]
print(orig_field_names)
dest_field_names = [f.name for f in arcpy.ListFields(destinations)]
print(dest_field_names)


def calcDistances(origins, network, destinations, outLoc):
    """
    Uses a transportation network and list of origins and destinations points to
    find the total distance from origins to destinations. Exports the names of
    origin-destination and distance in meters as a csv file

    Parameters
    ----------
    network: road network data converted to a network datasets

    origins: feature class of origins as points

    destinations: feature class of origins as points

    outLoc: location of output feature layer (in gdb)

    Returns
    -------
    csv: CSV file with names of OD links and distances

    ODCostMatrix: feature layer of OD cost matrix with OD lines, origins
                  destinations, etc

    """

    outNALayerName = "distMatrix"
    outLayerFile = outNALayerName

    # Make OD Cost Matrix Layer
    stime = time.time()
    print("Creating OD layer...")
    outNALayer = arcpy.MakeODCostMatrixLayer_na(
        network,
        outNALayerName,
        "Length",
        "",
        "",
        "",
        "ALLOW_UTURNS",
        "",
        "NO_HIERARCHY",
        "",
        "STRAIGHT_LINES",
        "",
    )

    outNALayer = outNALayer.getOutput(0)

    # Get the names of all the sublayers within the OD cost matrix layer.
    subLayerNames = arcpy.na.GetNAClassNames(outNALayer)

    # Stores the layer names that we will use later
    origLayerName = subLayerNames["Origins"]
    destLayerName = subLayerNames["Destinations"]
    linesLayerName = subLayerNames["ODLines"]

    # Adjust field names according to origin and destination codes
    oriField = "Name " + orig_field_names[2] + " #"
    destField = "Name " + dest_field_names[2] + " #"

    # oriField = "Name trase_id #"
    # destField = "Name ID #"
    # Set search criteria and query
    # search_criteria = island + " " + "SHAPE" + ";" + island + "_ND_Junctions NONE"
    # search_query = island + " #" + ";" + island + "_ND_Junctions #"

    # Add origins
    print("Loading Origins...")
    arcpy.AddLocations_na(
        outNALayer,
        origLayerName,
        origins,
        oriField,
        "30 Kilometers",
        "",
        "",
        # search_criteria,
        "MATCH_TO_CLOSEST",
        "APPEND",
        "NO_SNAP",
        "5 Meters",
        "INCLUDE",
        "",
        # search_query,
    )

    # Add destinations
    print("Loading Destinations...")
    arcpy.AddLocations_na(
        outNALayer,
        destLayerName,
        destinations,
        destField,
        "30 Kilometers",
        "",
        "",
        # search_criteria,
        "MATCH_TO_CLOSEST",
        "APPEND",
        "NO_SNAP",
        "5 Meters",
        "INCLUDE",
        "",
        # search_query,
    )

    print("Solving...")
    # Solve od cost matrix
    arcpy.Solve_na(outNALayer, "SKIP", "TERMINATE", "")
    print("Finished in %.0f" % (time.time() - stime))
    print("Solved! Saving...")
    # fc = outLoc+distanceMatrix
    # arcpy.SaveToLayerFile_management(distanceMatrix,fc,"RELATIVE")
    arcpy.SaveToLayerFile_management(outNALayer, outLayerFile, "RELATIVE")

    # Extract lines layer, export to CSV
    fields = ["Name", "Total_Length"]
    for lyr in outNALayer.listLayers():
        if lyr.name == linesLayerName:
            print("Saving lines...")
            with open(outFile, "w") as f:
                f.write(",".join(fields) + "\n")  # csv headers
                with arcpy.da.SearchCursor(lyr, fields) as cursor:
                    for row in cursor:
                        f.write(",".join([str(r) for r in row]) + "\n")

    df = pd.read_csv(outFile, index_col=None, header=0)
    df = df.drop_duplicates(keep="first")
    df.to_csv(outFile, index=False)

    print("Script completed successfully")

    # Deleteing in memory outNAlayer
    arcpy.Delete_management(outNALayer)


outFile = (
     dropbox_dir
     + "indo_pulp_defor/01_data/02_out/tables/"
    #"D:/trase/palm/ucsb/logistics/transportation/outputs/"
    + island
    + "_"
    + Path(origins).name
    + "_"
    + Path(destinations).name
    + "_odmatrix.csv"
)  # csv file output and location
calcDistances(origins, network, destinations, input_gdb)
