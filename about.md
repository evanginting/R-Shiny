
#### Data

The data contain three different datasets, which are attendance, games, and standings. The attendance dataset shows the total attendance of NFL game per week and per team. It is spread over multiple years from 2000 until 2019. The games dataset shows information about weekly games, which provide facts about who's winning the game and the game performances. Finally, the standings dataset cover general information about how many wins, points, and ranks  of every team in multiple seasons. The data can be accessed from [here](https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-02-04).


#### Purpose

The purpose of the app is to:

1. give the preliminary information about NFL to the new follower,
2. show the yearly attendance and break it down into team level, and 
3. show that NFL is a competitive sports (no dominant team), which shows that play at home does not guarantee a win. It will supported by the comparison of home and away win of every team. 

The data showed obvious signs of bimodality (both in the marginal and joint distribution). The app used interactive components to see how the choices in plot elements (e.g. number of bins and plot types) effect the visibility of these features. The last part used an interactive approach for a user to decide the model parameters that fit the data. 

#### Creator

Evan Ginting (egin0003@student.monash.edu)

<details>
<summary>Session information</summary>

```
## ─ Session info ────────────────────────────────────────────────────────────────────────────────────────────
##  setting  value
##  version  R version 4.3.1 (2023-06-16 ucrt)
##  os       Windows 11 x64 (build 22621)
##  system   x86_64, mingw32
##  ui       RStudio
##  language (EN)
##  collate  English_Australia.utf8
##  ctype    English_Australia.utf8
##  tz       Australia/Sydney
##  date     2023-08-25
##  rstudio  2023.06.1+524 Mountain Hydrangea (desktop)
##  pandoc   NA
## 
## ─ Packages ────────────────────────────────────────────────────────────────────────────────────────────────
##  package     * version date (UTC) lib source
##  bit           4.0.5   2022-11-15 [1] CRAN (R 4.3.1)
##  bit64         4.0.5   2020-08-30 [1] CRAN (R 4.3.1)
##  bslib         0.5.0   2023-06-09 [1] CRAN (R 4.3.1)
##  cachem        1.0.8   2023-05-01 [1] CRAN (R 4.3.1)
##  cli           3.6.1   2023-03-23 [1] CRAN (R 4.3.1)
##  colorspace    2.1-0   2023-01-23 [1] CRAN (R 4.3.1)
##  crayon        1.5.2   2022-09-29 [1] CRAN (R 4.3.1)
##  curl          5.0.1   2023-06-07 [1] CRAN (R 4.3.1)
##  digest        0.6.33  2023-07-07 [1] CRAN (R 4.3.1)
##  dplyr       * 1.1.2   2023-04-20 [1] CRAN (R 4.3.1)
##  ellipsis      0.3.2   2021-04-29 [1] CRAN (R 4.3.1)
##  evaluate      0.21    2023-05-05 [1] CRAN (R 4.3.1)
##  fansi         1.0.4   2023-01-22 [1] CRAN (R 4.3.1)
##  fastmap       1.1.1   2023-02-24 [1] CRAN (R 4.3.1)
##  fontawesome   0.5.1   2023-04-18 [1] CRAN (R 4.3.1)
##  forcats     * 1.0.0   2023-01-29 [1] CRAN (R 4.3.1)
##  generics      0.1.3   2022-07-05 [1] CRAN (R 4.3.1)
##  ggplot2     * 3.4.2   2023-04-03 [1] CRAN (R 4.3.1)
##  glue          1.6.2   2022-02-24 [1] CRAN (R 4.3.1)
##  gtable        0.3.3   2023-03-21 [1] CRAN (R 4.3.1)
##  hms           1.1.3   2023-03-21 [1] CRAN (R 4.3.1)
##  htmltools     0.5.5   2023-03-23 [1] CRAN (R 4.3.1)
##  httpuv        1.6.11  2023-05-11 [1] CRAN (R 4.3.1)
##  jquerylib     0.1.4   2021-04-26 [1] CRAN (R 4.3.1)
##  jsonlite      1.8.7   2023-06-29 [1] CRAN (R 4.3.1)
##  knitr         1.43    2023-05-25 [1] CRAN (R 4.3.1)
##  later         1.3.1   2023-05-02 [1] CRAN (R 4.3.1)
##  lifecycle     1.0.3   2022-10-07 [1] CRAN (R 4.3.1)
##  lubridate   * 1.9.2   2023-02-10 [1] CRAN (R 4.3.1)
##  magrittr      2.0.3   2022-03-30 [1] CRAN (R 4.3.1)
##  markdown      1.7     2023-05-16 [1] CRAN (R 4.3.1)
##  memoise       2.0.1   2021-11-26 [1] CRAN (R 4.3.1)
##  mime          0.12    2021-09-28 [1] CRAN (R 4.3.0)
##  munsell       0.5.0   2018-06-12 [1] CRAN (R 4.3.1)
##  pillar        1.9.0   2023-03-22 [1] CRAN (R 4.3.1)
##  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.3.1)
##  promises      1.2.0.1 2021-02-11 [1] CRAN (R 4.3.1)
##  purrr       * 1.0.1   2023-01-10 [1] CRAN (R 4.3.1)
##  R6            2.5.1   2021-08-19 [1] CRAN (R 4.3.1)
##  Rcpp          1.0.11  2023-07-06 [1] CRAN (R 4.3.1)
##  readr       * 2.1.4   2023-02-10 [1] CRAN (R 4.3.1)
##  rlang         1.1.1   2023-04-28 [1] CRAN (R 4.3.1)
##  rsconnect     1.0.1   2023-07-20 [1] CRAN (R 4.3.1)
##  rstudioapi    0.15.0  2023-07-07 [1] CRAN (R 4.3.1)
##  sass          0.4.7   2023-07-15 [1] CRAN (R 4.3.1)
##  scales      * 1.2.1   2022-08-20 [1] CRAN (R 4.3.1)
##  sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.3.1)
##  shiny       * 1.7.4.1 2023-07-06 [1] CRAN (R 4.3.1)
##  shinythemes * 1.2.0   2021-01-25 [1] CRAN (R 4.3.1)
##  stringi       1.7.12  2023-01-11 [1] CRAN (R 4.3.0)
##  stringr     * 1.5.0   2022-12-02 [1] CRAN (R 4.3.1)
##  tibble      * 3.2.1   2023-03-20 [1] CRAN (R 4.3.1)
##  tidyr       * 1.3.0   2023-01-24 [1] CRAN (R 4.3.1)
##  tidyselect    1.2.0   2022-10-10 [1] CRAN (R 4.3.1)
##  tidyverse   * 2.0.0   2023-02-22 [1] CRAN (R 4.3.1)
##  timechange    0.2.0   2023-01-11 [1] CRAN (R 4.3.1)
##  tzdb          0.4.0   2023-05-12 [1] CRAN (R 4.3.1)
##  utf8          1.2.3   2023-01-31 [1] CRAN (R 4.3.1)
##  vctrs         0.6.3   2023-06-14 [1] CRAN (R 4.3.1)
##  vroom         1.6.3   2023-04-28 [1] CRAN (R 4.3.1)
##  withr         2.5.0   2022-03-03 [1] CRAN (R 4.3.1)
##  xfun          0.39    2023-04-20 [1] CRAN (R 4.3.1)
##  xtable        1.8-4   2019-04-21 [1] CRAN (R 4.3.1)
## 
##  [1] C:/Users/evang/AppData/Local/R/win-library/4.3
##  [2] C:/Program Files/R/R-4.3.1/library
## 
## ───────────────────────────────────────────────────────────────────────────────────────────────────────────
```
</details>
