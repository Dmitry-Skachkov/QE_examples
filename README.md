# [Quantum Espresso](https://www.quantum-espresso.org/) usage

* [Compilation QE 7.3.1](#compilation-qe-731-on-carpenter-cluster)
* [Ground state calculation](#qe-ground-state-calculation)
* [Parallel calculation](#qe-parallel-calculation-on-2-nodes)    
* [Band structure calculation](#band-structure-calculation-in-qe)
* [Fat band plot](#fat-band-plot)


## Compilation QE 7.3.1 on Carpenter cluster

> wget https://www.quantum-espresso.org/rdm-download/488/v7-3-1/b1efb691fb214393cd1c1a762d82ea3d/qe-7.3.1-ReleasePack.tar.gz

Load modules
> module load PrgEnv-intel/8.4.0

> module list

```
1) craype-x86-rome
2) cray-pals/1.2.12
3) cray-dsmml/0.2.2
4) cray-mpich/8.1.26
5) cray-libsci/23.05.1.4
6) craype/2.7.21
7) craype-network-ofi
8) perftools-base/23.05.0
9) intel/2023.0.0
10) libfabric/1.15.2.0
11) xpmem/2.5.2-2.4_3.58__gd0f7936.shasta
12) PrgEnv-intel/8.4.0
```        

> ./configure

> make all


## QE ground state calculation

Submit job with ground state calculation

> sbatch [job_QE](https://github.com/Dmitry-Skachkov/QE_examples/blob/main/Example_01/job_QE)     

[Go to top](#quantum-espresso-usage)       


## QE parallel calculation on 2 nodes

QE has several levels of parallelization. The parallelization scheme can be manipulated by 5 input parameters in calling pw.x program:    

> mpirun -np 96 pw.x -ni 1 -nk 2 -nt 2 -nd 1 -nb 2 -input scf.in   

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

In order to calculate band structure in QE, you need to run 'scf' calculation to calculate the ground state first, and then to run 'bands' calculation in order to calculate the band structure on the desired k-points pathway.

For description of different types of Brillouin zones see [Computational Materials Science, 49, 299 (2010)](https://doi.org/10.1016/j.commatsci.2010.05.010).   
To generate the pathway you can use [AFlow on-line tool](https://aflow.org/aflow-online/).  

In order to use automatic generation for k-points pathway in QE you need to use non-zero [ibrav](https://www.quantum-espresso.org/Doc/INPUT_PW.html#idm218) parameter in QE input. 

For notations of high-symmetry points used in QE see [Notes by Andrea Dal Corso](Example_02/A.Dal_Corso__Brillouin_zones.pdf)   

In input file for 'bands' calculation you can set the pathway by using notations for high-symmetry points:   
```
K_POINTS crystal_b
6                           # total number of high-symmetry points
gG   20                     # point Gamma with 20 intermediate points to next high-symmetry point 
K    10
M    20
Y1   10
L2   10
gS1   1
```

For Greek names use first letter g:     
gG - &Gamma;     
gS - &Sigma;   

*Note1*. There are available other on-line tools to generate k-points pathway, for example, [MaterialsCloud](https://www.materialscloud.org/work/tools/seekpath), with **different** notations for high-symmetry points. In QE there are implemented two different schemes for plotting pathway.    

*Note2*. Both on-line tools, AFlow and MaterialsCloud, transform initial system to new symmetrized system with different coordinate system. 

[Go to top](#quantum-espresso-usage)    

## Fat band plot  

Calculate band structure

> sbatch [job_QE_band](Example_03/job_QE_band)

Calculate k-resolved PDOS

> sbatch [job_QE_band2](Example_03/job_QE_band2)

Make data file for bands

> qe_band_plot 1 51 -0.7604 0.000

Make data file with fat bands

> sbatch [job_plot](Example_03/job_plot)

The results is [Fat_bands.pdf](Example_03/Fat_bands.pdf)

[Go to top](#quantum-espresso-usage)   

