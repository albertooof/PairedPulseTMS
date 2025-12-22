
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PairedPulseTMS <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

<!-- badges: end -->

This package includes a set of functions to analyse raw paired-pulse TMS
data and normalise them

## Installation

You can install the development version of PairedPulseTMS from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("albertooof/PairedPulseTMS")
```

You can also install the development version of PairedPulseTMS from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("albertooof/PairedPulseTMS")
```

## Types of Dataframe expected as input

![](man/figures/TypesOfDataframe.png)

## Types of normalisation

![](man/figures/TypesOfNormalisation.PNG)

## Example

Basic examples which show you how to run the code and what output to
expect:

``` r

library(PairedPulseTMS)

data_wide <- data.frame(id = 999, t(rnorm(10, mean = 1, sd = 0.1)))
NORMALISE_PP_to_SP(data_wide, format = "Wide", number_of_triggers = 10 , normalise = "C")
#>                      ID TS_alone1 CS_TS_1 TS_alone2  CS_TS_2 TS_alone3  CS_TS_3
#> combined[indices, ] 999         1       1  1.449482 1.449482  1.070172 1.070172
#>                     TS_alone4  CS_TS_4 TS_alone5 CS_TS_5
#> combined[indices, ]  1.371998 1.371998   1.04447 1.04447
```

``` r

data_long <- data.frame(value = c(999, rnorm(10, mean = 1, sd = 0.1)))
NORMALISE_PP_to_SP(data_long, format = "Long", number_of_triggers = 10 , normalise = "A")
#>        ID TS_alone1  CS_TS_1 TS_alone2  CS_TS_2 TS_alone3  CS_TS_3 TS_alone4
#> value 999         1 1.192838         1 1.028868         1 1.337118         1
#>        CS_TS_4 TS_alone5  CS_TS_5
#> value 1.036081         1 1.136345
```

``` r

ID <- LETTERS[1:5]
dataframe_wide <- data.frame(ID = ID, matrix(stats::rnorm(5 * 16, mean = 1, sd = 0.1), nrow = 5, ncol = 16) )
NORMALISE_PP_to_SP_forDataframe(data = dataframe_wide, format = "Wide", number_of_triggers = 16 , normalise = 3)
#>   ID TS_alone1 CS_TS_1 TS_alone2   CS_TS_2 TS_alone3   CS_TS_3 TS_alone4
#> 1  A         1       1 0.8352156 0.8352156 0.6720380 0.6720380 0.6262157
#> 2  B         1       1 1.3195233 1.3195233 1.1918426 1.1918426 1.0216590
#> 3  C         1       1 1.1658587 1.1658587 0.9563315 0.9563315 1.0140510
#> 4  D         1       1 1.1894307 1.1894307 1.2637724 1.2637724 1.1639374
#> 5  E         1       1 0.8993478 0.8993478 0.8703721 0.8703721 0.8764973
#>     CS_TS_4 TS_alone5   CS_TS_5 TS_alone6   CS_TS_6 TS_alone7   CS_TS_7
#> 1 0.6262157 0.8307642 0.8307642 0.7463783 0.7463783 0.6572433 0.6572433
#> 2 1.0216590 1.1381886 1.1381886 1.2569309 1.2569309 0.9633065 0.9633065
#> 3 1.0140510 1.0638478 1.0638478 0.9655759 0.9655759 1.0689028 1.0689028
#> 4 1.1639374 1.1443736 1.1443736 1.0172555 1.0172555 1.3891045 1.3891045
#> 5 0.8764973 0.9452128 0.9452128 0.8845529 0.8845529 1.0751223 1.0751223
#>   TS_alone8   CS_TS_8
#> 1 0.7151704 0.7151704
#> 2 1.3516258 1.3516258
#> 3 0.8811992 0.8811992
#> 4 1.2408277 1.2408277
#> 5 0.8270450 0.8270450
```

``` r

dataframe_long <- as.data.frame(t(dataframe_wide))
NORMALISE_PP_to_SP_forDataframe(data = dataframe_long, format = "Long", number_of_triggers = 16 , normalise = 4)
#>   ID TS_alone1     CS_TS_1 TS_alone2     CS_TS_2 TS_alone3     CS_TS_3
#> 1  A         0  0.29472331         0  0.11465799         0 -0.10271706
#> 2  B         0 -0.12797844         0  0.14929207         0  0.04752215
#> 3  C         0 -0.09060481         0  0.06285322         0 -0.13525546
#> 4  D         0 -0.14058720         0  0.03288772         0  0.09351402
#> 5  E         0  0.06836166         0 -0.03772369         0 -0.07047276
#>   TS_alone4     CS_TS_4 TS_alone5      CS_TS_5 TS_alone6      CS_TS_6 TS_alone7
#> 1         0 -0.17333677         0  0.109314046         0  0.002200696         0
#> 2         0 -0.10655082         0  0.001459617         0  0.100694538         0
#> 3         0 -0.07665196         0 -0.028712465         0 -0.125635342         0
#> 4         0  0.01122140         0 -0.005729712         0 -0.123478796         0
#> 5         0 -0.06345958         0  0.012016604         0 -0.054311247         0
#>       CS_TS_7 TS_alone8     CS_TS_8
#> 1 -0.12497775         0 -0.04051101
#> 2 -0.16536196         0  0.17332983
#> 3 -0.02397202         0 -0.21707637
#> 4  0.18807214         0  0.07519147
#> 5  0.14079606         0 -0.12153439
```

``` r

ID <- LETTERS[1:5]
dataframe_wide <- data.frame(ID = ID, matrix(stats::rnorm(5 * 16, mean = 1, sd = 0.5), nrow = 5, ncol = 16) )
data <- NORMALISE_PP_to_SP_forDataframe(data = dataframe_wide, format = "Wide", number_of_triggers = 16 , normalise = 5)
plots <- plot_data_for_Dataframes(data, format = "Wide", col_line = "grey", col_dots = "grey", error_measure = "se")
```

``` r

plot1 <- plots$plot1
plot1 + ggplot2::geom_hline(yintercept = 1, col = "red")
```

![](man/figures/plot1.PNG)

``` r

plots$plot2
```

![](man/figures/plot2.PNG)

``` r

plots$plot3
```

![](man/figures/plot3.PNG)

The plotting function can also take dataframes with only one ID

``` r

data_wide <- data[1, ]
plots <- plot_data_for_Dataframes(data_wide, format = "Wide", col_line = "grey", col_dots = "grey", error_measure = "se")
#> Warning in qt(conf.interval/2 + 0.5, datac$N - 1): NaNs produced
#> Warning in qt(conf.interval/2 + 0.5, datac$N - 1): NaNs produced
#> Warning in qt(conf.interval/2 + 0.5, datac$N - 1): NaNs produced
```

``` r

plots$plot3
```

![](man/figures/plot4.PNG)
