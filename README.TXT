# Nicolas SISOURAT Dec. 2018
# The code simulates the photodynamics of H2O
# using the Landau-Zener Surface Hopping Method  J. Chem. Phys. 142, 104307 (2015))
#


# INSTALLATION
# The installation should be straigthforward with gfortran
# just do to src and type "make"
# it should generate main.exe
#


# INPUT FILES
# The main input file defines the timestep (in au), the value (in au) for the numerical derivative of the potential energy (to get the energy gradient)
# the total simulation time (in fs), the initial state populated by photoexcitation, and the filenames containing 1) the potential energy surfaces and 
# 2) the initial positions and velocities
#
# for example
# ! namelist data file
&input
 dt=20.0
 dr=0.005
 fpot='pes.txt'
 fxyz='xyz.txt'
 tf=100.0
 fsta=1
/

# potential energy surfaces are in pes.txt
# and the surfaces are assumed to be on regular grid and the file should have the following format
# number of r1 points, number of r2 points, number of theta points, number of states
# r1 (in au), r2 (in au), theta(in degree), energies (in au)
#

# The initiali Cartesian coordinates (in au) are in xyz.txt (generally taken from Wigner sampling)
# massO, xO, yO, zO, vxO, vyO, vzO
# massH1, xH1, yH1, zH1, vxH1, vyH1, vzH1
# massH2, xH2, yH2, zH2, vxH2, vyH2, vzH2
#
