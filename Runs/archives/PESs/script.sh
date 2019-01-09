

for i in {1..9}
 do
   python interp_nonregular.py A_double_prime.txt $i > e$i
 done  

 paste xyz e1 e2 e3 e4 e5 e6 e7 e8 e9 > A_double_prime.interp
