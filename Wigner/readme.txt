# to generate 1000 initial conditions from Wigner sampling (using MOLPRO freq output)
python /home/nico/Workspace/SHARC/sharc/bin/wigner.py --MOLPRO -n 1000 MOLPRO.out

MOLPRO
  ***,HeH2 potential
  basis=avdz

  set,charge=1
  geomtyp=xyz
  geometry={
  3
  C,    -1.7131057716,  -0.4266961107,   0.1412129046
  H,    -1.6636126962,   0.1353416745,   1.0768111434
  H,    -2.4904872032,  -0.8220319057,  -0.5168660173 }

  hf
  frequencies

