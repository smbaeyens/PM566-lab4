Lab 4
================
Sylvia Baeyens
9/17/2021

Objective= to examine the association between weekly average dew pt temp
& wind speed in 4 regions of the US

# 1. Read in the Data

``` r
#download file ONLY IF it has not been downloaded previously
if (!file.exists("../met_all.gz")) {
  download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz", "../met_all.gz", method="libcurl", timeout = 60)}
met <- data.table::fread("../met_all.gz")
```

# 2. Prepare the data

``` r
#Remove temperatures less than -17C
met = met[temp>-17]
#Make sure there are no missing data in the key variables coded as 9999, 999, etc
met[,table(is.na(temp))]
```

    ## 
    ##   FALSE 
    ## 2317204

``` r
met[,table(is.na(rh))]
```

    ## 
    ##   FALSE    TRUE 
    ## 2310917    6287

``` r
met[,table(is.na(wind.sp))]
```

    ## 
    ##   FALSE    TRUE 
    ## 2285461   31743

``` r
met[,table(is.na(vis.dist))]
```

    ## 
    ##   FALSE    TRUE 
    ## 2283538   33666

``` r
met[,table(is.na(dew.point))]
```

    ## 
    ##   FALSE    TRUE 
    ## 2310917    6287

``` r
met[,table(is.na(lat))]
```

    ## 
    ##   FALSE 
    ## 2317204

``` r
met[,table(is.na(lon))]
```

    ## 
    ##   FALSE 
    ## 2317204

``` r
met[,table(is.na(elev))]
```

    ## 
    ##   FALSE 
    ## 2317204

``` r
#met[elev == 9999.0, elev := na]

#Generate a date variable using the functions as.Date() (hint: You will need the following to create a date paste(year, month, day, sep = "-")).
met[, ymd := as.Date(paste(year, month, day, sep = "-"))]
#Using the data.table::week function, keep the observations of the first week of the month.
met[, table(week(ymd))]
```

    ## 
    ##     31     32     33     34     35 
    ## 297259 521600 527922 523847 446576

``` r
met = met[week(ymd) == 31]
#Compute the mean by station of the variables temp, rh, wind.sp, vis.dist, dew.point, lat, lon, and elev.
met_avg = met[,.(
  temp = mean(temp,na.rm = TRUE),
  rh = mean(rh,na.rm = TRUE),
  wind.sp = mean(wind.sp,na.rm = TRUE),
  vis.dist = mean(vis.dist,na.rm = TRUE),
  dew.point = mean(dew.point,na.rm = TRUE),
  lat = mean(lat,na.rm = TRUE),
  lon = mean(lon,na.rm = TRUE),
  elev = mean(elev,na.rm = TRUE)
),by = "USAFID"]
#Create a region variable for NW, SW, NE, SE based on lon = -98.00 and lat = 39.71 degrees
met_avg[lat>= 39.71 & lon<= -98,region:="Northwest"]
met_avg[lat< 39.71 & lon<= -98,region:="Southwest"]
met_avg[lat>= 39.71 & lon> -98,region:="Northeast"]
met_avg[lat< 39.71 & lon> -98,region:="Southeast"]

met_avg[,table(region, useNA="always")]
```

    ## region
    ## Northeast Northwest Southeast Southwest      <NA> 
    ##       484       146       649       296         0

``` r
#Create a categorical variable for elevation as in the lecture slides
met_avg[, elev_cat:=fifelse(elev>252,"high","low")]
```

# 3. Using geom\_violin to examine wind speed & dew pt temp

``` r
ggplot(met_avg, mapping = aes(y= wind.sp, x=1)) +
  geom_violin() +
  facet_grid(~region)
```

    ## Warning: Removed 15 rows containing non-finite values (stat_ydensity).

![](lab04_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
ggplot(met_avg, mapping = aes(y= dew.point, x=1)) +
  geom_violin() +
  facet_grid(~region)
```

![](lab04_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

\#4.

\#5.

\#6. Using stat\_summary to examine mean dew point and wind speed

``` r
ggplot(met_avg[!is.na(wind.sp) & !is.na(dew.point)], 
       mapping=aes(x=region, y= wind.sp)) +
    stat_summary(fun.data = "mean_sdl") +
    stat_summary(fun.data = "mean_sdl", geom= "errorbar")
```

![](lab04_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

\#7.

``` r
temp.pal = colorNumeric(c('darkgreen','goldenrod','brown'), domain = met_avg$rh)
```
