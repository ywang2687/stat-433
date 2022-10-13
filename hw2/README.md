stat 433 hw2
================
2022-10-08

``` r
library(nycflights13)
library(ggplot2)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(tidyverse)
```

    ## ── Attaching packages
    ## ───────────────────────────────────────
    ## tidyverse 1.3.2 ──

    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ✔ purrr   0.3.4      
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ lubridate::as.difftime() masks base::as.difftime()
    ## ✖ lubridate::date()        masks base::date()
    ## ✖ dplyr::filter()          masks stats::filter()
    ## ✖ lubridate::intersect()   masks base::intersect()
    ## ✖ dplyr::lag()             masks stats::lag()
    ## ✖ lubridate::setdiff()     masks base::setdiff()
    ## ✖ lubridate::union()       masks base::union()

To investigate the time have people may avoid flights delay, I intend to
simply group data by hour without consideration of other potential
factors. I use total delay time, which is created by adding dep_delay
and arr_delay together, to determine if the flight is delayed or not. I
choose proportion of delay rather than number of flights delayed because
proportion is a better representation if number of observations for each
group is not fixed. Then I take carrier, season and airport into account
to check the trend. Generally, the proportion of delay flights increases
as hour of the day increases for all carriers, seasons, and airports.

``` r
flight3 = flights %>%
  mutate(date = paste(year,month,day),date=ymd(date),delay_total = dep_delay+arr_delay)%>%
  group_by(hour)%>%
  summarise(n = n(), n_dep_delay =sum(dep_delay>0,na.rm = TRUE),n_arr_delay =sum(arr_delay>0,na.rm = TRUE),n_total_delay =sum(delay_total>0,na.rm = TRUE),n_cancelled = sum(is.na(delay_total)), prop_delay =n_total_delay/n)%>%
  filter(n>100)%>%
  arrange(prop_delay)
flight3
```

    ## # A tibble: 19 × 7
    ##     hour     n n_dep_delay n_arr_delay n_total_delay n_cancelled prop_delay
    ##    <dbl> <int>       <int>       <int>         <int>       <int>      <dbl>
    ##  1     7 22821        4963        6113          5815         346      0.255
    ##  2     6 25951        5430        7484          6943         504      0.268
    ##  3     5  1953         489         563           542          13      0.278
    ##  4     8 27242        6790        8797          8365         508      0.307
    ##  5     9 20312        5392        6745          6537         381      0.322
    ##  6    10 16708        4942        5794          5639         338      0.338
    ##  7    11 16033        5034        5467          5457         344      0.340
    ##  8    12 18181        6408        6759          6737         437      0.371
    ##  9    13 19956        8183        8050          8313         499      0.417
    ## 10    14 21706        9257        9001          9387         684      0.432
    ## 11    16 23002       10699       10149         10656         957      0.463
    ## 12    22  2639        1184        1257          1254          81      0.475
    ## 13    15 23888       11364       10941         11421         806      0.478
    ## 14    18 21783       10636       10232         10662         711      0.489
    ## 15    19 21441       10839       10025         10601         934      0.494
    ## 16    17 24426       12132       11582         12114         759      0.496
    ## 17    23  1061         461         542           529          19      0.499
    ## 18    20 16739        8633        7996          8428         678      0.503
    ## 19    21 10933        5596        5507          5659         430      0.518

I dropped 1am because it doesn’t have sufficient sample size(it only
contains 1 observation), this can mislead our interpretation. We can see
7 am has the smallest proportion of delay flights in this summary table.

``` r
flight3%>%
  ggplot(aes(x=hour, y = prop_delay))+
  geom_line()+
  geom_point()
```

![](stat-433-hw2_files/figure-gfm/unnamed-chunk-4-1.png)<!-- --> **The
simple line plot proves that at 7am we can expect the smallest
proportion of delay without consideration of other factors. There’s also
a increasing trend of proportion of day as hour increases.**

**In general, people should fly at 7 if they wish to avoid delay since
the proportion of total delay is the smallest. This is a very
generalized conclusion, since I simply group all flights by hour and
then calculate the proportion of delay flights. This conclusion doesn’t
account for any potential confounding factors, including but not limited
to weather, destination, airline, etc. In next section, I intend to
check the grouped data by airline.**

# Carrier

``` r
flight4  = flights%>%
  mutate(date = paste(year,month,day),date=ymd(date),delay_total = dep_delay+arr_delay)%>%
  group_by(carrier,hour)%>%
  summarise(n = n(), n_dep_delay =sum(dep_delay>0,na.rm = TRUE),n_arr_delay =sum(arr_delay>0,na.rm = TRUE),n_total_delay =sum(delay_total>0,na.rm = TRUE),n_cancelled = sum(is.na(delay_total)), prop_delay =n_total_delay/n)%>%
  filter(n>10)%>%
  arrange(prop_delay)
```

    ## `summarise()` has grouped output by 'carrier'. You can override using the
    ## `.groups` argument.

``` r
flight4
```

    ## # A tibble: 190 × 8
    ## # Groups:   carrier [16]
    ##    carrier  hour     n n_dep_delay n_arr_delay n_total_delay n_cancelled prop_…¹
    ##    <chr>   <dbl> <int>       <int>       <int>         <int>       <int>   <dbl>
    ##  1 AS          7   349          74          59            54           4   0.155
    ##  2 OO         18    24           4           6             4           3   0.167
    ##  3 US          5   341          33          74            57           1   0.167
    ##  4 9E          7   628         134         128           122          22   0.194
    ##  5 US          9   948          97         245           185          36   0.195
    ##  6 AA          7  3348         526         748           656          61   0.196
    ##  7 9E         10   136          22          29            27           6   0.199
    ##  8 DL          7  3787         472         877           766          40   0.202
    ##  9 YV         19    19           6           4             4           0   0.211
    ## 10 US          7   912         106         240           194          19   0.213
    ## # … with 180 more rows, and abbreviated variable name ¹​prop_delay

From the initial summary table, I can see that people fly at 7 with AS
carrier can avoid the delay.

# plot of prop_delay vs hour by carriers

``` r
flight4%>%
  ggplot(aes(x=hour, y = prop_delay))+
  geom_line()+
  facet_wrap(~carrier)+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?

![](stat-433-hw2_files/figure-gfm/unnamed-chunk-6-1.png)<!-- --> The
plot shows that AS at 7 am has the smallest proportion of delay
flights.From the plot, we also see AS, HA,OO, and YV have a few
observations. In general, the proportion of delay increases as hour
increases with all carriers. For popular carriers, US tend to have
smaller proportion of delay flights. However, the result my be
confounded since some carriers only operate a few flights to a few
destination. The delay status may be affected by the destination since
popular destination may have larger probability of delayed flights. For
further investigation, I plan to choose three most popular destination
respectively to see what time and which airline people should fly if
they want to avoid delay.

``` r
dest= flights%>%
  group_by(dest)%>%
  summarise(n = n())%>%
  slice_max(n,n = 3)
dest
```

    ## # A tibble: 3 × 2
    ##   dest      n
    ##   <chr> <int>
    ## 1 ORD   17283
    ## 2 ATL   17215
    ## 3 LAX   16174

We can see ORD, ATL and LAX are the most three popular destination. To
investigate the delay status, I’ll only consider carriers that operate
flights from NY to those destinations.

**dataframe only for flights going to ORD,ATL,and LAX**

``` r
dest1= flights%>%
  filter(dest %in% c("ORD","ATL","LAX"))%>%
  mutate(date = paste(year,month,day),date=ymd(date),delay_total = dep_delay+arr_delay)%>%
  group_by(carrier,hour,dest)%>%
  summarise(n = n(), n_dep_delay =sum(dep_delay>0,na.rm = TRUE),n_arr_delay =sum(arr_delay>0,na.rm = TRUE),n_total_delay =sum(delay_total>0,na.rm = TRUE),n_cancelled = sum(is.na(delay_total)), prop_delay =n_total_delay/n)%>%
  filter(n>50)%>%
  arrange(prop_delay)
```

    ## `summarise()` has grouped output by 'carrier', 'hour'. You can override using
    ## the `.groups` argument.

``` r
dest1
```

    ## # A tibble: 150 × 9
    ## # Groups:   carrier, hour [103]
    ##    carrier  hour dest      n n_dep_delay n_arr_delay n_total_d…¹ n_can…² prop_…³
    ##    <chr>   <dbl> <chr> <int>       <int>       <int>       <int>   <int>   <dbl>
    ##  1 UA          8 ORD     188          27          32          29       2   0.154
    ##  2 AA          6 ORD     643          74         106         100      20   0.156
    ##  3 AA          7 ORD     614          75         115         107      12   0.174
    ##  4 AA          9 ORD     483          81          99          89      14   0.184
    ##  5 UA         11 ORD     110          22          24          21       1   0.191
    ##  6 AA          8 LAX      86          15          18          18       0   0.209
    ##  7 AA         13 LAX     364          81          83          78       4   0.214
    ##  8 AA         12 LAX     363          69          93          78       1   0.215
    ##  9 UA          6 ORD     625         178         127         138      19   0.221
    ## 10 B6          7 LAX     348          67          79          78       4   0.224
    ## # … with 140 more rows, and abbreviated variable names ¹​n_total_delay,
    ## #   ²​n_cancelled, ³​prop_delay

We can see flying with UA to ORD at 8am have the smallest delay
proportion. And the five smallest proportion of delay flights are all
going to ORD.

**by dest**

``` r
LAX = dest1%>%
  filter(dest =="LAX")
LAX
```

    ## # A tibble: 51 × 9
    ## # Groups:   carrier, hour [51]
    ##    carrier  hour dest      n n_dep_delay n_arr_delay n_total_d…¹ n_can…² prop_…³
    ##    <chr>   <dbl> <chr> <int>       <int>       <int>       <int>   <int>   <dbl>
    ##  1 AA          8 LAX      86          15          18          18       0   0.209
    ##  2 AA         13 LAX     364          81          83          78       4   0.214
    ##  3 AA         12 LAX     363          69          93          78       1   0.215
    ##  4 B6          7 LAX     348          67          79          78       4   0.224
    ##  5 AA          9 LAX     364          86          98          85       3   0.234
    ##  6 AA          7 LAX     256          60          56          60       4   0.234
    ##  7 DL          9 LAX     354          54          82          83       3   0.234
    ##  8 DL         12 LAX     348          57          98          83       1   0.239
    ##  9 UA         11 LAX     523         131         128         125       5   0.239
    ## 10 AA         10 LAX     337          52          88          82       3   0.243
    ## # … with 41 more rows, and abbreviated variable names ¹​n_total_delay,
    ## #   ²​n_cancelled, ³​prop_delay

Flyting to LAX at 8 has the smallest proportion of delay.

``` r
ATL = dest1%>%
  filter(dest =="ATL")
ATL
```

    ## # A tibble: 48 × 9
    ## # Groups:   carrier, hour [48]
    ##    carrier  hour dest      n n_dep_delay n_arr_delay n_total_d…¹ n_can…² prop_…³
    ##    <chr>   <dbl> <chr> <int>       <int>       <int>       <int>   <int>   <dbl>
    ##  1 DL          9 ATL     524          61         143         122       7   0.233
    ##  2 DL          7 ATL     930         116         269         226      11   0.243
    ##  3 DL          6 ATL    1104         144         399         336       9   0.304
    ##  4 DL         11 ATL     668         159         237         214       8   0.320
    ##  5 DL          8 ATL     593         134         204         191       6   0.322
    ##  6 DL         10 ATL     701         115         258         230       4   0.328
    ##  7 DL         13 ATL     767         231         297         265       8   0.346
    ##  8 EV          9 ATL     237          80          82          83       6   0.350
    ##  9 UA         12 ATL      58          26          22          22       1   0.379
    ## 10 MQ          6 ATL     265          28         126         102       6   0.385
    ## # … with 38 more rows, and abbreviated variable names ¹​n_total_delay,
    ## #   ²​n_cancelled, ³​prop_delay

Flying to ALT at 9 with DL has the smallest proportion of delay.

Plots:

``` r
dest1%>%
  ggplot(aes(x=hour, y = prop_delay))+
  geom_line()+
  facet_wrap(~dest)+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

![](stat-433-hw2_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
Unfortunately, I don’t see any general pattern if the data is grouped by
hour and carrier with those three most popular destination.

**mean table**

``` r
m1 = dest1%>%
  group_by(dest,carrier)%>%
  summarise(mean_prop_delay = mean(prop_delay))%>%
  arrange(mean_prop_delay)
```

    ## `summarise()` has grouped output by 'dest'. You can override using the
    ## `.groups` argument.

``` r
m1
```

    ## # A tibble: 17 × 3
    ## # Groups:   dest [3]
    ##    dest  carrier mean_prop_delay
    ##    <chr> <chr>             <dbl>
    ##  1 ORD   AA                0.307
    ##  2 LAX   AA                0.327
    ##  3 LAX   DL                0.363
    ##  4 ORD   UA                0.377
    ##  5 ATL   UA                0.379
    ##  6 ATL   DL                0.397
    ##  7 ORD   9E                0.423
    ##  8 ORD   MQ                0.427
    ##  9 ATL   9E                0.436
    ## 10 ORD   B6                0.443
    ## 11 LAX   VX                0.445
    ## 12 LAX   UA                0.455
    ## 13 LAX   B6                0.464
    ## 14 ATL   MQ                0.489
    ## 15 ATL   EV                0.506
    ## 16 ATL   FL                0.582
    ## 17 ATL   WN                0.643

It suggests that flying to ORD with AA has the smallest mean proprotion
of delay flights.

``` r
m1%>%
  ggplot(aes(x = carrier, y = mean_prop_delay, color=dest))+
  geom_point()
```

![](stat-433-hw2_files/figure-gfm/unnamed-chunk-13-1.png)<!-- --> The
plot shows that carrier AA has the smallest average delay, followed by
carrier DL.

# Section conclusion:

The general trend for proportion of delay versus hour for all carriers
is mostly increasing. However, if I account potential confounding
factor, destination, I can’t see a specific pattern for proportion
versus hour for carriers with those three popular destination.

# Season

``` r
flight5 = flights %>%
  mutate(date = paste(year,month,day),date=ymd(date),delay_total = dep_delay+arr_delay, season = case_when(
    month %in% c(12,1,2) ~ "Winter",
  month %in% c(3,4,5) ~ "Spring",
  month %in% c(6,7,8) ~ "Summer",
  month %in% c(9,10,11) ~ "Fall"))%>%
  group_by(hour,season)%>%
  summarise(n = n(), n_dep_delay =sum(dep_delay>0,na.rm = TRUE),n_arr_delay =sum(arr_delay>0,na.rm = TRUE),n_total_delay =sum(delay_total>0,na.rm = TRUE),n_cancelled = sum(is.na(delay_total)), prop_delay =n_total_delay/n)%>%
  filter(n>10)%>%
  arrange(prop_delay)
```

    ## `summarise()` has grouped output by 'hour'. You can override using the
    ## `.groups` argument.

``` r
flight5
```

    ## # A tibble: 76 × 8
    ## # Groups:   hour [19]
    ##     hour season     n n_dep_delay n_arr_delay n_total_delay n_cancelled prop_d…¹
    ##    <dbl> <chr>  <int>       <int>       <int>         <int>       <int>    <dbl>
    ##  1     5 Fall     455          79          91            79           1    0.174
    ##  2     7 Fall    5754         901        1216          1107          39    0.192
    ##  3     6 Fall    6149         912        1332          1195          71    0.194
    ##  4     9 Fall    4863         926        1224          1111          36    0.228
    ##  5     7 Spring  5677        1198        1431          1327          71    0.234
    ##  6     8 Fall    7293        1376        1841          1707          70    0.234
    ##  7     6 Spring  6716        1299        1739          1584         125    0.236
    ##  8     7 Summer  5972        1331        1519          1479          99    0.248
    ##  9    10 Fall    4403         966        1213          1129          54    0.256
    ## 10    11 Fall    4008        1001        1086          1049          46    0.262
    ## # … with 66 more rows, and abbreviated variable name ¹​prop_delay

We can see fly at 5 when it’s Fall has the smallest proportion of delay.
And just by a general peak, eight of the most eleven smallest proportion
of delay are in fall season.

# plots for prop_delay vs. hour by each season

``` r
flight5%>%
  ggplot(aes(x=hour, y = prop_delay))+
  geom_line()+
  facet_wrap(~season)+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

![](stat-433-hw2_files/figure-gfm/unnamed-chunk-15-1.png)<!-- --> There
are also increasing trends for proportion of delay versus hour for four
seasons. Fall tends to have smaller proportion of delay than other
seasons generally, and summer tends to have larger proportion of delay
than other seasons generally.

# airport

``` r
flight6  = flights%>%
  mutate(date = paste(year,month,day),date=ymd(date),delay_total = dep_delay+arr_delay)%>%
  group_by(origin,hour)%>%
  summarise(n = n(), n_dep_delay =sum(dep_delay>0,na.rm = TRUE),n_arr_delay =sum(arr_delay>0,na.rm = TRUE),n_total_delay =sum(delay_total>0,na.rm = TRUE),n_cancelled = sum(is.na(delay_total)), prop_delay =n_total_delay/n)%>%
  filter(n>100)%>%
  arrange(prop_delay)
```

    ## `summarise()` has grouped output by 'origin'. You can override using the
    ## `.groups` argument.

``` r
flight6
```

    ## # A tibble: 54 × 8
    ## # Groups:   origin [3]
    ##    origin  hour     n n_dep_delay n_arr_delay n_total_delay n_cancelled prop_d…¹
    ##    <chr>  <dbl> <int>       <int>       <int>         <int>       <int>    <dbl>
    ##  1 LGA        7  7137        1090        1793          1579         141    0.221
    ##  2 LGA        6  8558        1413        2359          2059         223    0.241
    ##  3 JFK        7  7026        1452        1909          1791          68    0.255
    ##  4 EWR        5   895         226         238           234           6    0.261
    ##  5 EWR        6 11133        2808        3127          3066         205    0.275
    ##  6 LGA        8  7167        1422        2196          1993         143    0.278
    ##  7 JFK        5   750         168         218           210           5    0.28 
    ##  8 LGA        9  7760        1687        2342          2175         204    0.280
    ##  9 EWR        7  8658        2421        2411          2445         137    0.282
    ## 10 JFK        6  6260        1209        1998          1818          76    0.290
    ## # … with 44 more rows, and abbreviated variable name ¹​prop_delay

The table shows that flying at LGA airport at 7 am can potentially avoid
the delay.

``` r
flight6%>%
  ggplot(aes(x=hour,y = prop_delay,color = origin))+
  geom_line()+
  geom_point()
```

![](stat-433-hw2_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

Generally, there is an increasing trend for proportion of delay versus
hour for all airports. For specific pattern, all airports tend to have
smaller proportion of delay flights in the morning(5 to 10), and the
proportion of delay flights starts to increase after 10 am.  
\#check the mean prop_delay for each airport

``` r
flight6%>%
  group_by(origin)%>%
  summarise(mean_delay= mean(prop_delay))
```

    ## # A tibble: 3 × 2
    ##   origin mean_delay
    ##   <chr>       <dbl>
    ## 1 EWR         0.430
    ## 2 JFK         0.398
    ## 3 LGA         0.377

The mean table also shows that LGA has the smallest proportion of delay.
So flying at LGA may also avoid delay.

# Conclusion

Through my exploration of the dataset, I find that as hour of a day
increases, the proportion of delay also increases. Without consideration
of other factors, flying at 7 can avoid the probability of flights
delay. With consideration of carrier, season, and airport respectively,
I see the general increasing trend for proportion of delay flights
versus hour of the day for all factors. However, if I only pick the most
three popular destination when I investigate the carrier, I fail to get
any significant pattern.
