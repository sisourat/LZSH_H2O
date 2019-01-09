import sys
import subprocess

ifin = 10
istart = 0

f = open(sys.argv[1],"r")
f.readline()

l = f.readline()
d = l.split()
ninit = int(d[1])
print 'ninit=',ninit

l = f.readline()
d = l.split()
natom = int(d[1])
print 'natom=',natom

i=0
while i<ifin:
        l = f.readline()
        d = l.split()
        if(len(d)>0 and d[0]=='Index'):
                i+=1
                if(i>istart):
                        fgeom = open('init'+str(i),'w')

                l = f.readline()
                for j in range(natom):
                        l = f.readline()
                        d = l.split()
                        if(i>istart):
                                print >> fgeom, '  '.join(d[0:10])

                if(i>istart):
                        fgeom.close()

