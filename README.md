# CVD_JACC
# Author: Megan Lindstrom 
# Date: Oct 27, 2022
# Description: This code creates the figures and table for the JACC Almanac 2022. 
#              It can be launched from an array script, or if running for a 
#              single region use the regionname/locid variables (currently 
#              set to pull from the launch script). The code has three sections
#              The first calls in libraries and general arguments. Following is
#              where all data is pulled in for each figure/table. The last 
#              section creates all of the figures, saving them to a specified
#              output location. If trying to create a single figure and not run
#              the entire code it is suggested to still run the entire data step,
#              as there are shared calls between figures/table.

# Data needed: GBD results for cardiovascular disease and GBD shapefile
