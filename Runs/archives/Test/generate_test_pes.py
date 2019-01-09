import numpy as np
import sys


r1 = np.linspace(0.2,20.0,20)
r2 = np.linspace(0.2,20.0,20)
the = np.linspace(0,190.0,10)

print len(r1), len(r2), len(the), 2
for ir1 in r1:
 for ir2 in r2:
  for it in the:
#      print ir1, ir2, it, 1.0/ir1+0.1*(ir2-3.0)**2+0.1*(it-26.0)**2, 1.0
      print ir1, ir2, it, 0.1*(ir1-3.0)**2+0.1*(ir2-3.0)**2, 1.0 #,+0.1*(it-26.0)**2, 1.0
