#!/bin/bash

ntraj=3
itraj=1

until [ $itraj -gt $ntraj ]; do
#        echo Value of count is: $itraj
	sed -e 's/xyz.txt/..\/Wigner\/init'$itraj'/g' input_temp > input
	/home/nico/Results/Yigeng/Propag_LZSH_3D/src/main.exe input > log$itraj
	cp fort.200 Traj$itraj
        let itraj=itraj+1
done 
