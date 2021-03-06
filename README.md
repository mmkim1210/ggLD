# ggLD
An R package to visualize a correlation matrix (e.g. LD matrix) using ggplot2 and the diagonal on the x-axis. The package as it is can visualize more than 1,000 features (e.g. SNPs) fast. 

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
df <- abs(cor(data.frame(a = rnorm(20), 
                         b = rnorm(20), 
                         c = rnorm(20),
                         d = rnorm(20),
                         e = rnorm(20))))
ggLD(data = df)
```
<img src="man/figures/README-example-1.png" width="40%" />

```r
# Visualize with the right (square) aspect ratio
n <- dim(df)[1]
ggLD(data = df) + 
    theme(legend.position = "none") + 
    theme(aspect.ratio = (1 / n  + (n - 1) / (2 * n)))
```


The following is a real example of SNP data from chromosome 7 for ~3,000 SNPs. 
<img src="man/figures/README-example-2.png" width="85%" />
