# -*- coding: utf-8 -*-
"""
Created on Wed Jun  5 14:40:32 2019

@author: Jason
"""

from osgeo import gdal
import dirfind
from skimage.graph import route_through_array
import numpy as np
from geopy.distance import geodesic
import matplotlib.pyplot as plt
import pandas as pd
import boto3
import io
from itertools import product
import time
import math

# Set working directory
dropbox_dir = dirfind.guess_dropbox_dir()
working_dir = dropbox_dir + "indo_pulp_defor/01_data/01_in/ucsb/transport_cost/"


# Function to calculate start and end time of function
def time_me(*arg):
    if len(arg) != 0:
        elapsedTime = time.time() - arg[0]
        # print(elapsedTime);
        hours = math.floor(elapsedTime / (60 * 60))
        elapsedTime = elapsedTime - hours * (60 * 60)
        minutes = math.floor(elapsedTime / 60)
        elapsedTime = elapsedTime - minutes * (60)
        seconds = math.floor(elapsedTime)
        elapsedTime = elapsedTime - seconds
        ms = elapsedTime * 1000
        if hours != 0:
            print("%d hours %d minutes %d seconds" % (hours, minutes, seconds))
        elif minutes != 0:
            print("%d minutes %d seconds" % (minutes, seconds))
        else:
            print("%d seconds %f ms" % (seconds, ms))
    else:
        # print ('does not exist. here you go.');
        return time.time()


# Transforming a raster map to array datatype.
raster = gdal.Open(working_dir + "cost_layer_rc_005.tif")
band = raster.GetRasterBand(1)
mapArray = band.ReadAsArray()

# Get geotransform information and declare some variables for later use
geotransform = raster.GetGeoTransform()
originX = geotransform[0]
originY = geotransform[3]
pixelWidth = geotransform[1]
pixelHeight = geotransform[5]

ports_kali = pd.read_csv(working_dir + "ports_kalimantan.csv")
ports_suma = pd.read_csv(working_dir + "ports_sumatera.csv")


# Visualize the map base on the array, if you want, not neccessary.
plt.imshow(mapArray)
plt.gray()
plt.axis([5400, 6500, 2010, 1600])
plt.show()


# Transform the coordinates to the exact position in the array.
def coord2pixelOffset(x, y):
    xOffset = int((x - originX) / pixelWidth)
    yOffset = int((y - originY) / pixelHeight)
    return xOffset, yOffset


# Create a path which travels through the cost map.
def createPath(costSurfaceArray, startCoord, stopCoord):
    # coordinates to array index
    startCoordX = startCoord[0]
    startCoordY = startCoord[1]
    startIndexX, startIndexY = coord2pixelOffset(startCoordX, startCoordY)

    stopCoordX = stopCoord[0]
    stopCoordY = stopCoord[1]
    stopIndexX, stopIndexY = coord2pixelOffset(stopCoordX, stopCoordY)

    # create path
    indices, weight = route_through_array(
        costSurfaceArray,
        (startIndexY, startIndexX),
        (stopIndexY, stopIndexX),
        geometric=True,
        fully_connected=True,
    )
    indices = np.array(indices).T
    indices = indices.astype(float)
    indices[1] = indices[1] * pixelWidth + originX
    indices[0] = indices[0] * pixelHeight + originY
    return indices


# Calculate the distance starts from the first pair of points to the last.
def calculateDistance(pathIndices):
    distance = 0
    for i in range(0, (len(pathIndices[0]) - 1)):
        # Get distance in km
        distance += geodesic(
            (pathIndices[1, i], pathIndices[0, i]),
            (pathIndices[1, i + 1], pathIndices[0, i + 1]),
        ).km
    return distance


def distanceCalculator(startCoord, stopCoord):
    pathIndices = createPath(mapArray, startCoord, stopCoord)
    distance = calculateDistance(pathIndices)
    print(distance)
    return distance


# Test on single port
ports_kali.id[1]
ports_suma.id[1]
startCoord = (ports_kali.latitude[1], ports_kali.longitude[1])
stopCoord = (ports_suma.latitude[1], ports_suma.longitude[1])

time_var = time_me()
# get a variable with the current timestamp
distanceCalculator(startCoord, stopCoord)
time_me(time_var)
# print the time difference

# Randomly sample 10pc of ports
#ports_df = ports_df.sample(frac=0.10)

# Set up data frames for start and end points
ref_df = ports_kali
comp_df = ports_suma

# Create a data frame of port-port distance by iterating over row pairs in the Cartesian product
time_var = time_me()
merge_df = pd.DataFrame(
    [
        {
            "port_end": r.id,
            "port_start": c.id,
            "distance": distanceCalculator(
                (r.latitude, r.longitude), (c.latitude, c.longitude)
            ),
        }
        for r, c in product(ref_df.itertuples(), comp_df.itertuples())
    ]
)
time_me(time_var)

# Export to csv
merge_df.to_csv(working_dir + "idn_kali_suma_ports_od.csv", index=False)
