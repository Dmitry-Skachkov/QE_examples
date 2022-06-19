# [Quantum Espresso](https://www.quantum-espresso.org/) usage

* [Parallel calculation](#qe-parallel-calculation-on-2-nodes)    
* [Band structure calculation](#band-structure-calculation-in-qe)
    

## QE parallel calculation on 2 nodes

QE has several levels of parallelization. The parallelization scheme can be manipulated by 5 input parameters in calling pw.x program:    

> mpirun -np $PPN pw.x -ni 1 -nk 2 -nt 2 -nd 1 -nb 2 -input scf.in   

For detailed description see [here](https://www.quantum-espresso.org/Doc/user_guide/node20.html)   

The default values for ni, nk, nt, nd, nb are equal to 1.     

Here are the test results for calculation time running QE on 2 nodes with 48 cores:   

```
 ni  nk   nt   nd   nb   t_calc.
 1    1    1    1    1    2m32s
 1    1    1    4    1    3m13s
 1    1    1   16    1    5m32s
 1    2    2   16    2    5m59s
 1    2    1   16    2    3m41s
 1    2    2    1    8    2m34s
 1    4    1    4    4    1m45s
 1    2    1    4    2    1m40s
 1    2    1    4    4    1m38s
 1    2    2    1    4    1m28s
 1    2    1    1    2    1m19s
 1    2    2    1    2    1m15s      <-- optimized
```
The optimized running job script for 2 nodes is [job_QE_2x48](https://github.com/Dmitry-Skachkov/Yambo_examples/blob/main/Example_04/job_QE_2x48)    
Please note that optimized parameters depend on particular compiled version of QE (including scalapack or not) and particular system. Please also note, that for nscf calculation the optimized parameters will be different than for scf.    
For larger number of nodes, probably, the optimized parameters for parallelization will involve hybrid MPI/OpenMP scheme.     


[Go to top](#quantum-espresso-usage)    

## Band structure calculation in QE     

In order to use automatic generation for k-points pathway you need to use [ibrav](https://www.quantum-espresso.org/Doc/INPUT_PW.html#idm218) parameter of QE input /= 0. 

For notations of high-symmetry points used in QE see [pdf](Example_02/A.Dal_Corso__Brillouin_zones.pdf)   

For description of different types of Brillouin zones see [Computational Materials Science, 49, 299 (2010)](https://doi.org/10.1016/j.commatsci.2010.05.010).   
To generate the pathway you can use [on-line tool](https://aflow.org/aflow-online/).  

There are available other on-line tools to generate k-points pathway, for example, [here](https://www.materialscloud.org/work/tools/seekpath), with **different** notations for high-symmetry points. In QE there are implementation of two different schemes for plotting pathway.    

[Go to top](#quantum-espresso-usage)    


