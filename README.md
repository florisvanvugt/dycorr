# dycorr : Dyadic Data Correlation

To install:

```{r}
devtools::install_github("florisvanvugt/dycorr", upgrade_dependencies = FALSE)
``` 


# Overview

Supply a data frame (`data`) and indicate the column name corresponding to the scores of a person A on a variable X (`ax`), person A's score on Y (`ay`), their dyadic partner B's score on X (`bx`) and on Y (`by`):

```{r}
dycor::ax.ay(data,ax,ay,bx,by) # within-individual correlation (controlling for dyadic dependence in data)
dycor::ax.by(data,ax,ay,bx,by) # within-dyad correlation (correlate person A's X score against their partner B's Y score)
```


## Usage

Let's imagine you have dyadic data where dyad members are not distinguishable (e.g. roommates as opposed to husband/wife).

The functions in this package allow you to calculate within-individual correlations controlling for dyadic dependencies, such as for instance, if you want to know whether a person's empathy (X) predicts how shy they feel towards their roommate (Y). You might think, let's just run a regular correlation using the data from all individuals. However, the significance (p value) for a regular correlation assumes that all data points are independent. That assumption is not always plausible in dyadic data, where scores of one member from a dyad is often more similar to the scores of another member, for a varity of reasons. Hence, the data is not independent. Griffin & Gonzales (1995) propose a way to adjust the way significance is calculated. It essentially involves calculating an adjusted sample size (N*) that lies somewhere the number of dyads (N) and the number of individuals (2N).

```{r}

require('tidyr')

empshy <- tibble::tribble(
~id,~dyad,~empathy,~shyness,
1,1,30,20,
2,1,40,25,
3,2,15,17,
4,2,10,12,
5,3,25,18,
6,3,28,18
)

## You can run a naive correlation test that assumes the individual data are all independent:
cor.test(empshy$empathy, empshy$shyness)
## We will see this p value is actually an overestimation of the significance
## (the p value is smaller than it should be).

```

Before you can use this data with this package though, you need to add columns that correspond, for each person, to the empathy and shyness (X and Y) of their dyadic partner. There are many ways to do that but here is one:

```{r}

dyads <- empshy %>%
    select(idA=id, dyad) %>%
    merge(empshy %>% select(idB=id, dyad)) %>%
    filter(idA!=idB)

dyads

crossed <- 
    dyads %>%
    merge(empshy %>% rename(idA=id, empathyA=empathy, shynessA=shyness ) ) %>%
    merge(empshy %>% rename(idB=id, empathyB=empathy, shynessB=shyness ) ) %>%
    tibble()

crossed

```

Now we can run the within-individual correlation:

```{r}

crossed %>% dycorr::ax.ay('empathyA', 'shynessA', 'empathyB', 'shynessB')

```

Note that the correlation coefficient value is the same but the
p value is not. That is because the effective sample size (N.star) is
 smaller than the number of individuals (6).

Similarly:

```{r}
dycor::ax.ay(data,ax,ay,bx,by) # within-individual correlation (controlling for dyadic dependence in data)
dycor::ax.by(data,ax,ay,bx,by) # within-dyad correlation (between dyadic partner's A and B)
```


# Building

```{r}
devtools::document(pkg=".")
devtools::check(pkg=".")
```
