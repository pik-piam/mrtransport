# What the Package Does (One Line, Title Case)

R package **mredgetransport**, version **0.0.1.9001**

[![CRAN status](https://www.r-pkg.org/badges/version/mredgetransport)](https://cran.r-project.org/package=mredgetransport)  [![R build status](https://github.com/johannah-pik/mredgetransport/workflows/check/badge.svg)](https://github.com/johannah-pik/mredgetransport/actions) [![codecov](https://codecov.io/gh/johannah-pik/mredgetransport/branch/master/graph/badge.svg)](https://app.codecov.io/gh/johannah-pik/mredgetransport) 

## Purpose and Functionality

What the package does (one paragraph).


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

In case of questions / problems please contact First Last <first.last@example.com>.

## Citation

To cite package **mredgetransport** in publications use:

Last F (2023). _mredgetransport: What the Package Does (One Line, Title Case)_. R package version 0.0.1.9001.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mredgetransport: What the Package Does (One Line, Title Case)},
  author = {First Last},
  year = {2023},
  note = {R package version 0.0.1.9001},
}
```
