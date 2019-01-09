import numpy as np
from scipy.interpolate import griddata
from scipy.interpolate import LinearNDInterpolator

# put the available x,y,z data as a numpy array
points = np.array([
        [ 27.827,  18.53 , -30.417], [ 24.002,  17.759, -24.782],
        [ 22.145,  13.687, -33.282], [ 17.627,  18.224, -25.197],
        [ 29.018,  18.841, -38.761], [ 24.834,  20.538, -33.012],
        [ 26.232,  22.327, -27.735], [ 23.017,  23.037, -29.23 ],
        [ 28.761,  21.565, -31.586], [ 26.263,  23.686, -32.766]])
# and put the moisture corresponding data values in a separate array:
values = np.array([0.205,  0.197,  0.204,  0.197,  0.212,  
                   0.208,  0.204,  0.205, 0.211,  0.215])

# Finally, put the desired point/points you want to interpolate over
request = np.array([[25, 20, -30], [27, 20, -32]])

#Method 1, using griddata

print griddata(points, values, request)
# OUTPUT: array([ 0.20448536, 0.20782028])

#Method 2, using LinearNDInterpolator

# First, define an interpolator function
linInter= LinearNDInterpolator(points, values)

# Then, apply the function to one or more points
print linInter(np.array([[25, 20, -30]]))
#print linInter(xi)
# OUTPUT: [0.20448536  0.20782028]
# I think you may use it with python map or pandas.apply as well
