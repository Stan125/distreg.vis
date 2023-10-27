<!-- README.md is generated from README.Rmd. Please edit that file -->

# distreg.vis

[![CRAN
Version](https://www.r-pkg.org/badges/version/distreg.vis)](https://cran.r-project.org/package=distreg.vis)
[![](https://cranlogs.r-pkg.org/badges/distreg.vis)](https://cran.r-project.org/package=distreg.vis)
![R CMD
CHECK](https://github.com/Stan125/distreg.vis/actions/workflows/check-standard.yaml/badge.svg)

### What is `distreg.vis`?

`distreg.vis` is built upon the R package `bamlss` ([CRAN
Link](https://cran.r-project.org/web/packages/bamlss/index.html)) and
`gamlss` ([CRAN
Link](https://cran.r-project.org/web/packages/gamlss/index.html)), which
fit distributional regression models. `distreg.vis` requires a fitted
`bamlss` or `gamlss` object to achieve two goals:

1.  See and compare the expected distribution for chosen sets of
    covariates
2.  View the direct relationship between moments of the response
    distribution and a chosen explanatory variable, given a set of
    covariates.

The implementation is an interactive application using the Shiny
framework.

### Install

`distreg.vis` is now on CRAN. Run the following code to install the
package and all its extensions:

``` r
install.packages("distreg.vis")
```

Or, if you would like to have the newest version, install the GitHub
variant:

``` r
devtools::install_github("Stan125/distreg.vis")
```

### Supported distributions

At the moment, the following distributions are fully supported:

``` r
dists_char <- dists %>%
  filter(moment_funs) %>%
  dplyr::select(dist_name, class)

# GAMLSS Families
dists_char[dists_char$class == "gamlss", "dist_name"]
#>  [1] "BE"       "BEINF"    "BNB"      "DEL"      "EGB2"     "exGAUS"  
#>  [7] "EXP"      "GA"       "GB2"      "GEOM"     "GEOMo"    "GG"      
#> [13] "GIG"      "GPO"      "GT"       "GU"       "IG"       "IGAMMA"  
#> [19] "JSU"      "JSUo"     "LG"       "LO"       "LOGNO"    "NBF"     
#> [25] "NBI"      "NBII"     "NO"       "NO2"      "NOF"      "PARETO2" 
#> [31] "PARETO2o" "PE"       "PE2"      "PIG"      "PO"       "RG"      
#> [37] "SHASHo"   "SICHEL"   "SN1"      "SN2"      "SST"      "ST2"     
#> [43] "ST3"      "ST3C"     "ST4"      "ST5"      "TF"       "TF2"     
#> [49] "WEI"      "WEI2"     "WEI3"     "ZAGA"     "ZALG"     "ZANBI"   
#> [55] "ZAP"      "ZAPIG"    "ZASICHEL" "ZAZIPF"   "ZIBNB"    "ZINBI"   
#> [61] "ZIP"      "ZIP2"     "ZIPF"     "ZIPIG"    "ZISICHEL"

# BAMLSS Families
dists_char[dists_char$class == "bamlss", "dist_name"]
#>  [1] "beta"        "binomial"    "cnorm"       "gamma"       "gaussian"   
#>  [6] "gaussian2"   "glogis"      "gpareto"     "lognormal"   "multinomial"
#> [11] "poisson"
```

The rest of the distributions are mostly supported, but only in
displaying their predicted distributions, and not their moments.

### Show-Case

Let’s show an example using the `Wage` dataset, taken from the ISLR R
package. It depicts the wage of 3000 male workers in the Mid-Atlantic
region of the US, related to a couple of socio-economic variables.

First, let’s fit a bamlss using the log normal distribution (Wages
cannot be negative):

``` r
library(gamlss)
Wage <- ISLR::Wage
wage_model <- gamlss(wage ~ ps(age) + race + year + education,
                     ~ ps(age) + race + year + education,
                     data = Wage, family = LOGNO())
```

Both parameters of the modeled wage distribution are now modeled as
being dependent on the socio-economic variables.

#### Starting the application

The app can be started as an RStudio Add-In or running
`distreg.vis::vis()`. Afterwards, select the fitted model like so:
<p align="center">
<img src="images/01_start.gif"/>
</p>

After the model was selected, you can head to the “Scenarios” tab.

#### Expected Distribution

At the Scenarios tab, you can choose specific observations of
explanatory variables for which to predict the distribution. In this
case, we select socioeconomic variables and get the expected income
distribution for this combination:

<p align="center">
<img src="images/02_expected_dist.gif"/>
</p>

The real power of `distreg.vis` is visible when we add multiple
“scenarios” for comparing different covariate combinations:

<p align="center">
<img src="images/03_more_scenarios.gif"/>
</p>

#### Influence of Covariates

Furthermore, we can easily view the “influence” of a covariate on the
expected income distribution. For this, head to the “Properties” tab:

<p align="center">
<img src="images/04_influence_plot.gif"/>
</p>

#### Obtaining Code

All plots which were created can be reproduced with the code that
appears when you press “Obtain Code”:
<p align="center">
<img src="images/05_obtain_code.gif"/>
</p>
