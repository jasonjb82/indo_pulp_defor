# -*- coding: utf-8 -*-
"""
Created on Thu Apr  5 14:45:00 2018

@author: rheil
"""

import os

def check_possibilities(possibilities):
    """
    Parameters
    ----------
    possibilities : list
        Strings specifying possibly directories to check for existence.
        
    Returns
    -------
    p: string
        Path with a match        
    """

    for p in possibilities:
        if os.path.exists(p):
            return p

    return None
    
    
def guess_dropbox_dir():
    """
    Looks for the folder where dropbox data lives.
    """
    possibilities = ['C:/Users/rheil/Dropbox/rspo/', # Robert's old dropbox
                     'C:/Users/Jason/Dropbox/', # Jason's dropbox
                     '/rheil/Dropbox/rspo/RSPO_data/',
                     'D:/cloud/Dropbox/rspo/',
                     'D:/Dropbox/']
    return check_possibilities(possibilities)

#def guess_google_dir():
#    """
#    Looks for the folder where dropbox data lives.
#    """
#    possibilities = ['D:/webStorage/googleDrive/rspo/', # Robert's old computer
#                     'C:/Users/rheil/Google Drive/rspo/'] # Robert's new computer
#    return check_possibilities(possibilities)    