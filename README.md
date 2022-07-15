# pmx6

The dev branch is being actively developed. See below for installation instructions.

## Install (in development)

To install (and verify that installation worked) run the code below in R.

```
library(devtools)
devtools::install_github("pseastham-certara/pmx6")
library(pmx6)
testfunc()
```

The development branch is what is actively being developed. Eventually, this branch will be merged with main. 
To install this dev branch, use the following code:

```
library(devtools)
devtools::install_github("pseastham-certara/pmx6", ref="dev")
library(pmx6)
testfunc()
```

To uninstall one of the above, use the following:

```
remove.packages("pmx6")
```
