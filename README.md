# Activity 10
Dominic Sitto
Invalid Date

## dependencies

Dataset: tortillas[^1]

``` r
library(ggplot2)

library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ✔ purrr     1.0.2     ✔ tidyr     1.3.1
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(Stat2Data)
library(dplyr)
library(ggplot2)
library(dcData)

tacoData <- read_csv("tortilla_data.csv")
```

    Rows: 14055 Columns: 5
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (3): state, city, storeType
    dbl (2): year, price

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# should have named data set tortillaData instead...
```

## Question 1

``` r
myFunction <- function(x){
  y <- (cos(4*x) + tan(0.5*x))^4
  return(y)
}

ggplot() +
  stat_function(
    fun = myFunction,
    xlim = c(0, 1),
    n = 1000
  ) +
  theme_bw() +
  labs(
    x = "x",
    y = "h(x)"
  )
```

![](Activity10.markdown_github_files/figure-markdown_github/unnamed-chunk-2-1.png)

## Question 2

``` r
data(BabyNames)

dominicSet <- BabyNames %>%
    filter(name == "Dominic") %>%
    filter(sex == "M") %>%
    select(year, count, name)

deniseSet <- BabyNames %>%
    filter(name == "Denise") %>%
    filter(sex == "F") %>%
    select(year, count, name)

louSet <- BabyNames %>%
    filter(name == "Lou") %>%
    filter(sex == "M") %>%
    select(year, count, name)

totalSet <- dominicSet %>%
    bind_rows(deniseSet) %>%
    bind_rows(louSet)

ggplot(totalSet, aes(x = year, y = count)) + 
  geom_line(aes(color = name)) + 
  scale_color_manual(values = c("darkgoldenRod1", "red", "blue"))
```

![](Activity10.markdown_github_files/figure-markdown_github/unnamed-chunk-3-1.png)

## Question 3

### (a)

``` r
avgPrice2007 <- tacoData %>%
  filter(year == 2007) %>%
  summarize(average_price = mean(price))
# Takes average price only from 2017
print(avgPrice2007)
```

    # A tibble: 1 × 1
      average_price
              <dbl>
    1          7.68

### (b)

``` r
avgPrice2024 <- tacoData %>%
  filter(year == 2024) %>%
  summarize(average_price = mean(price))
# Takes average price only from 2024
print(avgPrice2024)
```

    # A tibble: 1 × 1
      average_price
              <dbl>
    1          20.5

### (c)

``` r
print(((avgPrice2024 - avgPrice2007) / avgPrice2007) * 100)
```

      average_price
    1      166.4113

``` r
# 100% * (Top Value - Smaller Value) / Original Value
```

### (d)

``` r
avgPrices <- tacoData %>%
  group_by(year) %>%
  summarize(average_price = mean(price))
# Only take the average from every year insetad of the entire set

ggplot(avgPrices, aes(x = year, y = average_price)) + 
  geom_line()
```

![](Activity10.markdown_github_files/figure-markdown_github/unnamed-chunk-7-1.png)

### (e)

``` r
library(simplermarkdown)

tortillaData2016 <- tacoData %>%
  filter(year == 2016) %>%
  group_by(state) %>%
  select(state, price) %>%
  summarize(
      count = n(),
      min = min(price), 
      firstQuartile = quantile(price, probs = 0.25), 
      median = median(price), 
      thirdQuartile = quantile(price, probs = 0.75), 
      max = max(price),
      mad = mad(price),
      mean = mean(price),
      std = sd(price)
  )
tortillaData2016 <- as.data.frame(tortillaData2016)

cat("Prices Statistics in Different States (2016)" ,md_table(tortillaData2016))
```

| state | count | min | firstQuartile | median | thirdQuartile | max | mad | mean | std |
|------------|-----|-----|-----------|------|-----------|-----|-------|-------|-------|
| BajaCalifornia | 100 | 9.63 | 10.30 | 10.90 | 17.57 | 17.71 | 1.630860 | 13.85330 | 3.636626 |
| Chihuahua | 100 | 11.06 | 11.36 | 11.36 | 15.00 | 15.00 | 0.222390 | 13.14060 | 1.832002 |
| Coahuila | 99 | 9.77 | 10.17 | 12.60 | 13.20 | 13.40 | 1.186080 | 11.72283 | 1.482322 |
| NuevoLeon | 101 | 10.03 | 10.20 | 14.00 | 14.05 | 14.29 | 0.429954 | 12.12871 | 1.953983 |
| Sonora | 201 | 9.93 | 10.13 | 10.30 | 18.00 | 19.00 | 0.548562 | 13.53731 | 3.714320 |
| Tamaulipas | 199 | 10.50 | 11.30 | 11.50 | 15.00 | 16.00 | 1.482600 | 13.30266 | 2.165758 |

Prices Statistics in Different States (2016)

``` r
ggplot(data=tacoData, mapping=aes(x=state, y=price))+
  geom_boxplot() + ggtitle("Prices of Tortillas in Different Mexican States (2016)")
```

![](Activity10.markdown_github_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
# I am unsure how to add a caption with GGPlot2
print("Coahuila had the cheapest tortillas while Sonora had the more expensive tortillas")
```

\[1\] “Coahuila had the cheapest tortillas while Sonora had the more
expensive tortillas”

### (f)

``` r
nuevoData <- tacoData %>%
  filter(state == "NuevoLeon")

ggplot(nuevoData, aes(x=year, y=price, group=storeType, color=storeType)) +
  geom_line()+ ggtitle("Prices of Tortillas in differnet types of stores in the state of Nuevo León")
```

![](Activity10.markdown_github_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
# Grouping by type in the ggplot is a very simple way to present the two different lines on the single plot
print("Throughout every year the Mom and Pop stores consistently were sold tortillas for more money than Big Retail stores, although the quality / size of these tortilals is not accounted for.")
```

    [1] "Throughout every year the Mom and Pop stores consistently were sold tortillas for more money than Big Retail stores, although the quality / size of these tortilals is not accounted for."

### (g)

<figure>
<img src="tortillaimage.jpeg" alt="Tortilla" />
<figcaption aria-hidden="true">Tortilla</figcaption>
</figure>

#### Citation: Gallagher, Joanne. “Easy Soft Flour Tortillas.” Inspired Taste - Easy Recipes for Home Cooks, 17 Aug. 2024, www.inspiredtaste.net/48394/flour-tortillas/.

### (h)

[^1]: Rick Chavelas via Kaggle at
    https://www.kaggle.com/datasets/richave/tortilla-prices-in-mexico
