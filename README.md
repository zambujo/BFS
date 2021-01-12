
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/BFS)](https://CRAN.R-project.org/package=BFS)
[![Grand
total](https://cranlogs.r-pkg.org/badges/grand-total/BFS)](https://cran.r-project.org/package=BFS)
[![pipeline
status](https://gitlab.com/lgnbhl/BFS/badges/master/pipeline.svg)](https://gitlab.com/lgnbhl/BFS/pipelines)
[![R build
status](https://github.com/lgnbhl/BFS/workflows/R-CMD-check/badge.svg)](https://github.com/lgnbhl/BFS/actions)
<!-- badges: end -->

# BFS <img src="man/figures/logo.png" align="right" />

> Search and download data from the Swiss Federal Statistical Office

The `BFS` package allows to search and download public data from the
[Swiss Federal Statistical Office
(BFS)](https://www.bfs.admin.ch/bfs/en/home/statistics/catalogues-databases/data.html)
in a dynamic and reproducible way.

:warning::warning::warning: **the bfs\_get\_data() function is sometimes
not working on Windows** :warning::warning::warning:

## Installation

``` r
# Install the released version from CRAN
install.packages("BFS")
```

To get a bug fix, or use a feature from the development version, you can
install BFS from GitHub.

``` r
# install from Github
devtools::install_github("lgnbhl/BFS")
```

## Usage

``` r
library(BFS)
```

### Download metadata

To search and download data from the Swiss Federal Statistical Office,
you first need to retrieve information about the available public data
sets. The function `bfs_get_metadata()` returns a data frame/tibble
containing the titles, publication dates, observation periods, data
sources, website URLs and download URLs of all the BFS data sets
available in a given language. You can get the metadata in German (“de”,
by default), French (“fr”), Italian (“it”) or English (“en”). Note that
Italian and English metadata give access to less data sets.

``` r
meta_en <- bfs_get_metadata(language = "en")

head(meta_en)
```

    ## # A tibble: 6 x 6
    ##   title         observation_period  published  source  url_bfs       url_px     
    ##   <chr>         <chr>               <chr>      <chr>   <chr>         <chr>      
    ## 1 Retail Trade~ 1.1.2000-30.11.2020 07.01.2021 Federa~ https://www.~ https://ww~
    ## 2 Retail Trade~ 1.1.2000-30.11.2020 07.01.2021 Federa~ https://www.~ https://ww~
    ## 3 New registra~ 1.1.2019-30.11.201~ 14.12.2020 Federa~ https://www.~ https://ww~
    ## 4 Environmenta~ 2000-2019           08.12.2020 Federa~ https://www.~ https://ww~
    ## 5 Hotel accomm~ 1.1.2005-31.10.2020 07.12.2020 Federa~ https://www.~ https://ww~
    ## 6 Hotel accomm~ 1.1.2005-31.10.2020 07.12.2020 Federa~ https://www.~ https://ww~

Using the development version on Github, you can also get the catalog
data by language. The new function `bfs_get_catalog()` get the metadata
based on the new RSS feed provided by the Swiss Federal Statistical
Office. Note that the number of datasets available may differ from the
output of the `bfs_get_metadata()`.

``` r
catalog_en <- bfs_get_catalog(language = "en")

head(catalog_en) 
```

    ## # A tibble: 6 x 5
    ##   title            language published           url_bfs           url_px        
    ##   <chr>            <chr>    <dttm>              <chr>             <chr>         
    ## 1 Retail Trade Tu~ en       2021-01-07 08:30:00 https://www.bfs.~ https://www.b~
    ## 2 Retail Trade Tu~ en       2021-01-07 08:30:00 https://www.bfs.~ https://www.b~
    ## 3 New registratio~ en       2020-12-14 08:30:00 https://www.bfs.~ https://www.b~
    ## 4 Environmental g~ en       2020-12-08 08:30:00 https://www.bfs.~ https://www.b~
    ## 5 Hotel accommoda~ en       2020-12-07 08:30:00 https://www.bfs.~ https://www.b~
    ## 6 Hotel accommoda~ en       2020-12-07 08:30:00 https://www.bfs.~ https://www.b~

### Search for data

To search for a specific data set title in the BFS metadata, you can use
the `bfs_search()` function. This function leverages the R base function
`grepl()` but calls the `data` argument first to allow the use of the
pipe operator `%>%`.

``` r
library(magrittr)

meta_en_uni <- bfs_get_metadata("en") %>%
  bfs_search("university students")
```

### Download data set

To download a BFS data set, add the related URL link from the `url_px`
column of the downloaded metadata as an argument to the `bfs_get_data
set()` function. You can choose the language (German, French, Italian or
English if any) in which the data set is downloaded with the `language`
argument.

``` r
df_uni <- bfs_get_dataset(url_px = meta_en_uni$url_px[1], language = "en")
```

You can access additional information about the downloaded data set
using the R base `attributes()` function.

``` r
attributes(df_uni) %>%
  str()
```

In case the function fails to download the data set, you can have a look
at its related BFS webpage using the `url_bfs` link.

``` r
browseURL(meta_en_uni$url_bfs[1]) # open webpage
```

Sometimes the PC-Axis file of the data set doesn’t exist. You should
then use the “STAT-TAB - interactive table” service provided by BFS to
download manually the data set.

### Data caching

Data caching is handled using the [pins](https://pins.rstudio.com/) R
package. To open the folder containing all the BFS data sets you already
downloaded, you can use the `bfs_open_dir()` function.

``` r
bfs_open_dir()
```

If a data set has already been downloaded during the day, the functions
`bfs_get_metadata()` and `bfs_get_data set()` retrieve the already
downloaded data sets from your local pins caching folder. Caching speeds
up code and reduces BFS server requests. However, if a given data set
has not been downloaded during the day, the functions download it again
to be sure that you have the lastest data available. You can also force
the download using the `force` argument.

## Other information

A [blog article](https://felixluginbuhl.com/blog/2019/11/07/swiss-data)
showing a concret example about how to use the BFS package.

Alternative R package: [pxweb](https://github.com/rOpenGov/pxweb).

This package is in no way officially related to or endorsed by the Swiss
Federal Statistical Office (BFS).
