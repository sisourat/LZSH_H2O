import sys
import numpy as np
from scipy.interpolate import griddata
from scipy.interpolate import LinearNDInterpolator
import math as math

ista = int(sys.argv[2])+2

xyz = []
e = []
for l in open(sys.argv[1]):
    w = l.split()
    xyz.append([float(w[0]),float(w[1]),float(w[2])])
    e.append([float(w[ista])])

# put the available x,y,z data as a numpy array
points = np.array(xyz)
values = np.array(e)

#print points
#print
#print values

# Finally, put the desired point/points you want to interpolate over
#request = np.array([[1.7, 1.7, 1.99], [1.7, 1.7, 1.0]])
#request = np.array([[1.7, 1.7, 45], [1.7, 1.7, 46]])

athe = np.linspace(43.5,174,50)
ar1 = np.linspace(1.6,4.95,30)
ar2 = np.linspace(1.6,4.95,30)

request = []
for r1 in ar1:
 for r2 in ar2:
  for t in athe:
   request.append([r1, r2, t])

#print request
#sys.exit()

#Method 1, using griddata
einterp = griddata(points, values, request)

fxyz = open('xyz','w')
for i in range(len(request)):
    print >> fxyz, ' '.join(map(str,request[i]))
    if(math.isnan(einterp[i])):
         print 0.0
    else:
         print ' '.join(map(str,einterp[i]))

#Method 2, using LinearNDInterpolator

# First, define an interpolator function
#linInter= LinearNDInterpolator(points, values)

# Then, apply the function to one or more points
#print linInter(request)
