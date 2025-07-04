# dycorr : Dyadic Data Correlation

To install:

```{r}
devtools::install_github("florisvanvugt/dycorr", upgrade_dependencies = FALSE)
``` 


# Overview

```{r}
dycor::ax.ay(data,ax,ay,bx,by) # within-individual correlation (controlling for dyadic dependence in data)
dycor::ax.by(data,ax,ay,bx,by) # within-dyad correlation
```

Will follow.



# Building

```{r}
devtools::document(pkg=".")
devtools::check(pkg=".")
```
