""" 
Gravity syntax styles for Pygments. 
"""

from setuptools import setup

entry_points = """ 
[pygments.styles]
simple = styles.simple:SimpleStyle
"""

setup( 
    name         = 'gravitystyles', 
    version      = '0.1', 
    description  = __doc__, 
    author       = "Gravity", 
    packages     = ['styles'], 
    entry_points = entry_points
) 
