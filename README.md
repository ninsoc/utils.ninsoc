
<!-- README.md is generated from README.Rmd. Please edit that file -->

# utils.ninsoc

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of utils.ninsoc is to provide several utilities functions.

## Installation

You can install utils.ninsoc with:

``` r
install.packages(r"(C:\Libraries\utils.ninsoc)", repos = NULL, type = "source")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# Show data.frame variable's type and name
utils.ninsoc::df_variables(mtcars)
#> # A tibble: 11 × 2
#>    var   type  
#>    <chr> <chr> 
#>  1 mpg   double
#>  2 cyl   double
#>  3 disp  double
#>  4 hp    double
#>  5 drat  double
#>  6 wt    double
#>  7 qsec  double
#>  8 vs    double
#>  9 am    double
#> 10 gear  double
#> 11 carb  double

# Compress data.frame
mtcars_compressed = utils.ninsoc::compress_data(mtcars)

# Show compressed data.frame variable's type and name
utils.ninsoc::df_variables(mtcars_compressed)
#> # A tibble: 11 × 2
#>    var   type   
#>    <chr> <chr>  
#>  1 mpg   double 
#>  2 cyl   integer
#>  3 disp  double 
#>  4 hp    integer
#>  5 drat  double 
#>  6 wt    double 
#>  7 qsec  double 
#>  8 vs    integer
#>  9 am    integer
#> 10 gear  integer
#> 11 carb  integer
```
