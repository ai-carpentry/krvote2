
# krvote2

<!-- badges: start -->
<!-- badges: end -->

krvote2 패키지는 대한민국 중앙선거관리위원회에서 제공하는 선거 관련
데이터를 조회하고 분석하는 기능을 제공합니다.

## 설치 방법

GitHub에서 krvote2의 개발 버전을 설치할 수 있습니다:

``` r
# install.packages("devtools")
devtools::install_github("ai-carpentry/krvote2")
```

## 사용 예제

선거코드 데이터를 조회하는 기본적인 예제입니다:

``` r
library(krvote2)
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.4
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.5.0     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.2     ✔ tidyr     1.3.1
#> ✔ purrr     1.0.2     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ purrr::%||%()   masks base::%||%()
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(httr)
library(jsonlite)
#> 
#> Attaching package: 'jsonlite'
#> 
#> The following object is masked from 'package:purrr':
#> 
#>     flatten
```

``` r

getCommonSgCodeList(serviceKey = Sys.getenv("DATA_GO_DECODE_KEY"))
#>    num     sgId                 sgName sgTypecode sgVotedate
#> 1    1 19921218      제14대 대통령선거          0   19921218
#> 2    2 19921218             대통령선거          1   19921218
#> 3    3 19960411    제15대 국회의원선거          0   19960411
#> 4    4 19960411           국회의원선거          2   19960411
#> 5    5 19971218      제15대 대통령선거          0   19971218
#> 6    6 19971218             대통령선거          1   19971218
#> 7    7 20000413    제16대 국회의원선거          0   20000413
#> 8    8 20000413           국회의원선거          2   20000413
#> 9    9 20000413   비례대표국회의원선거          7   20000413
#> 10  10 20020613 제3회 전국동시지방선거          0   20020613
```
