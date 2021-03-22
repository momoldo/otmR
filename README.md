
<!-- README.md is generated from README.Rmd. Please edit that file -->

# otmR

<!-- badges: start -->

[![R-CMD-check](https://github.com/momoldo/otmR/workflows/R-CMD-check/badge.svg)](https://github.com/momoldo/otmR/actions)
[![Codecov test
coverage](https://codecov.io/gh/momoldo/otmR/branch/main/graph/badge.svg)](https://codecov.io/gh/momoldo/otmR?branch=main)
<!-- badges: end -->

The goal of otmR is to …

## Installation

You can install the released version of otmR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("otmR") # not available!
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("momoldo/otmR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tidyverse)
#> ─ Attaching packages ──────────────────── tidyverse 1.3.0 ─
#> ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
#> ✓ tibble  3.1.0     ✓ dplyr   1.0.5
#> ✓ tidyr   1.1.3     ✓ stringr 1.4.0
#> ✓ readr   1.4.0     ✓ forcats 0.5.1
#> ─ Conflicts ───────────────────── tidyverse_conflicts() ─
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(otmR)
#> Loading required package: e1071
#> Loading required package: kableExtra
#> 
#> Attaching package: 'kableExtra'
#> The following object is masked from 'package:dplyr':
#> 
#>     group_rows
#> Loading required package: readxl
## basic example code
iris %>% # data.frame
  select(Sepal.Length:Petal.Width) %>% # variable selected
  otBasicStats() %>% # compute statistics
  otPrint() # print result
```

<table class=" lightable-classic" style="font-family: &quot;Arial Narrow&quot;, &quot;Source Sans Pro&quot;, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Result of BasicStats
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Size
</th>
<th style="text-align:right;">
Mean
</th>
<th style="text-align:right;">
Median
</th>
<th style="text-align:right;">
SD
</th>
<th style="text-align:right;">
Skewness
</th>
<th style="text-align:right;">
Kurtosis
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Sepal.Length
</td>
<td style="text-align:right;">
150
</td>
<td style="text-align:right;">
5.843
</td>
<td style="text-align:right;">
5.80
</td>
<td style="text-align:right;">
0.828
</td>
<td style="text-align:right;">
0.315
</td>
<td style="text-align:right;">
-0.552
</td>
</tr>
<tr>
<td style="text-align:left;">
Sepal.Width
</td>
<td style="text-align:right;">
150
</td>
<td style="text-align:right;">
3.057
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
0.436
</td>
<td style="text-align:right;">
0.319
</td>
<td style="text-align:right;">
0.228
</td>
</tr>
<tr>
<td style="text-align:left;">
Petal.Length
</td>
<td style="text-align:right;">
150
</td>
<td style="text-align:right;">
3.758
</td>
<td style="text-align:right;">
4.35
</td>
<td style="text-align:right;">
1.765
</td>
<td style="text-align:right;">
-0.275
</td>
<td style="text-align:right;">
-1.402
</td>
</tr>
<tr>
<td style="text-align:left;">
Petal.Width
</td>
<td style="text-align:right;">
150
</td>
<td style="text-align:right;">
1.199
</td>
<td style="text-align:right;">
1.30
</td>
<td style="text-align:right;">
0.762
</td>
<td style="text-align:right;">
-0.103
</td>
<td style="text-align:right;">
-1.341
</td>
</tr>
</tbody>
</table>
