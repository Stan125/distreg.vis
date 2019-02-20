<!-- README.md is generated from README.Rmd. Please edit that file -->

distreg.vis
===========

[![CRAN
Version](https://www.r-pkg.org/badges/version/distreg.vis)](https://cran.r-project.org/package=distreg.vis)
[![](https://cranlogs.r-pkg.org/badges/distreg.vis)](https://cran.r-project.org/package=distreg.vis)
[![Build
Status](https://api.travis-ci.org/Stan125/distreg.vis.svg?branch=master)](https://travis-ci.org/Stan125/distreg.vis)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/Stan125/distreg.vis?branch=master&svg=true)](https://ci.appveyor.com/project/Stan125/distreg.vis)

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
dists %>%
  filter(moment_funs) %>%
  dplyr::select(dist_name, class) %>%
  as.data.frame() %>%
  t()
#>           [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]    
#> dist_name "BE"     "BEo"    "BNB"    "DEL"    "EGB2"   "exGAUS" "EXP"   
#> class     "gamlss" "gamlss" "gamlss" "gamlss" "gamlss" "gamlss" "gamlss"
#>           [,8]     [,9]     [,10]    [,11]    [,12]    [,13]    [,14]   
#> dist_name "GA"     "GB2"    "GEOM"   "GEOMo"  "GG"     "GIG"    "GPO"   
#> class     "gamlss" "gamlss" "gamlss" "gamlss" "gamlss" "gamlss" "gamlss"
#>           [,15]    [,16]    [,17]    [,18]    [,19]    [,20]    [,21]   
#> dist_name "GT"     "GU"     "IG"     "IGAMMA" "JSU"    "JSUo"   "LG"    
#> class     "gamlss" "gamlss" "gamlss" "gamlss" "gamlss" "gamlss" "gamlss"
#>           [,22]    [,23]    [,24]    [,25]    [,26]    [,27]    [,28]   
#> dist_name "LO"     "LOGNO"  "NBF"    "NBI"    "NBII"   "NO"     "NO2"   
#> class     "gamlss" "gamlss" "gamlss" "gamlss" "gamlss" "gamlss" "gamlss"
#>           [,29]    [,30]     [,31]      [,32]    [,33]    [,34]   
#> dist_name "NOF"    "PARETO2" "PARETO2o" "PE"     "PE2"    "PIG"   
#> class     "gamlss" "gamlss"  "gamlss"   "gamlss" "gamlss" "gamlss"
#>           [,35]    [,36]    [,37]    [,38]    [,39]    [,40]    [,41]   
#> dist_name "PO"     "RG"     "SHASHo" "SICHEL" "SN1"    "SN2"    "SST"   
#> class     "gamlss" "gamlss" "gamlss" "gamlss" "gamlss" "gamlss" "gamlss"
#>           [,42]    [,43]    [,44]    [,45]    [,46]    [,47]    [,48]   
#> dist_name "ST2"    "ST3"    "ST3C"   "ST4"    "ST5"    "TF"     "TF2"   
#> class     "gamlss" "gamlss" "gamlss" "gamlss" "gamlss" "gamlss" "gamlss"
#>           [,49]    [,50]    [,51]    [,52]    [,53]    [,54]    [,55]   
#> dist_name "WEI"    "WEI2"   "WEI3"   "ZAGA"   "ZALG"   "ZANBI"  "ZAP"   
#> class     "gamlss" "gamlss" "gamlss" "gamlss" "gamlss" "gamlss" "gamlss"
#>           [,56]    [,57]      [,58]    [,59]    [,60]    [,61]    [,62]   
#> dist_name "ZAPIG"  "ZASICHEL" "ZAZIPF" "ZIBNB"  "ZINBI"  "ZIP"    "ZIP2"  
#> class     "gamlss" "gamlss"   "gamlss" "gamlss" "gamlss" "gamlss" "gamlss"
#>           [,63]    [,64]    [,65]      [,66]    [,67]      [,68]   
#> dist_name "ZIPF"   "ZIPIG"  "ZISICHEL" "beta"   "binomial" "cnorm" 
#> class     "gamlss" "gamlss" "gamlss"   "bamlss" "bamlss"   "bamlss"
#>           [,69]    [,70]      [,71]       [,72]    [,73]     [,74]        
#> dist_name "gamma"  "gaussian" "gaussian2" "glogis" "gpareto" "multinomial"
#> class     "bamlss" "bamlss"   "bamlss"    "bamlss" "bamlss"  "bamlss"     
#>           [,75]    
#> dist_name "poisson"
#> class     "bamlss"
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
