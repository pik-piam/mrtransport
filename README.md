# Input data generation for the EDGE-Transport model

R package **mredgetransport**, version **0.0.1.9004**

[![CRAN status](https://www.r-pkg.org/badges/version/mredgetransport)](https://cran.r-project.org/package=mredgetransport)  [![R build status](https://github.com/pik-piam/mrtransport/workflows/check/badge.svg)](https://github.com/pik-piam/mrtransport/actions) [![codecov](https://codecov.io/gh/pik-piam/mrtransport/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrtransport) 

## Purpose and Functionality

The mrtransport package contains data preprocessing for the
    EDGE-Transport model.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mredgetransport")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Johanna Hoppe <johanna.hoppe@pik-potsdam.de>.

## Citation

To cite package **mredgetransport** in publications use:

Hoppe J, Dirnaichner A (2023). _mredgetransport: Input data generation for the EDGE-Transport model_. R package version 0.0.1.9004, <https://github.com/pik-piam/mrtransport>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mredgetransport: Input data generation for the EDGE-Transport model},
  author = {Johanna Hoppe and Alois Dirnaichner},
  year = {2023},
  note = {R package version 0.0.1.9004},
  url = {https://github.com/pik-piam/mrtransport},
}
```
