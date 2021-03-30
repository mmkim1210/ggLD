# ggLD
R package to visualize a correlation matrix (e.g. LD matrix) using ggplot2 and the diagnoal on the x-axis

## Installation
```r
devtools::install_github("mmkim1210/ggLD")
```

## Example
```r
# Load relevant packages
library(tidyverse)
library(ggLD)

# ggplot2
ggLD(data = abs(cor(data.frame(a = rnorm(20), 
                               b = rnorm(20), 
                               c = rnorm(20),
                               d = rnorm(20),
                               e = rnorm(20)))))
```
<img src="man/figures/README-example-1.png" width="40%" />
