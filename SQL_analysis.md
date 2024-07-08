Sql Project Water Quality
================

### About the datatset

This dataset was found in the website
<https://catalog.data.gov/dataset/water-quality-data-41c5e/resource/c013c8da-49d3-4898-93a5-f6c0f0e95a0d>.
One important metric used to assess water quality is PH, which is a measure of how acidic/basic something is. 
PH ranges from 0-14, with 7 being considered neutral like drinkable water, 2 being considered acidic such as lemon,
and 12 considered basic such as soap. Fluctuating pH in a stream can be an indicator of increasing pollution, so 
maintaining a stable PH range of about 6.5-8.5 in a natural fresh water source is extremely important.  If freshwater 
falls out of that range it can decrease reproduction, stunt growth, and introduce disease to the organisms that reside there. 

The purpose of using this dataset was to utilize data that is important to me to showcase my ability in understanding and 
using SQL to clean data, aggregate metrics such as averages, and use functions such as wildcards and case whens to find if 
water quality variables like PH are at unsafe levels for this dataset. 


``` r
head(df_water)
```

    ## # A tibble: 6 × 17
    ##   Site_Id Unit_Id Read_Date `Salinity (ppt)` `Dissolved Oxygen (mg/L)`    ph
    ##   <chr>   <chr>   <chr>                <dbl>                     <dbl> <dbl>
    ## 1 Bay     <NA>    1/3/1994               1.3                      11.7   7.3
    ## 2 Bay     <NA>    1/31/1994              1.5                      12     7.4
    ## 3 Bay     <NA>    2/7/1994               1                        10.5   7.2
    ## 4 Bay     <NA>    2/23/1994              1                        10.1   7.4
    ## 5 Bay     <NA>    2/28/1994              1                        12.6   7.2
    ## 6 Bay     <NA>    3/7/1994               1                         9.9   7.1
    ## # ℹ 11 more variables: `Secchi Depth (m)` <dbl>, `Water Depth (m)` <dbl>,
    ## #   `Water Temp (?C)` <dbl>, `Air Temp-Celsius` <dbl>, air_temp <dbl>,
    ## #   `Time (24:00)` <chr>, Field_Tech <chr>, DateVerified <chr>,
    ## #   WhoVerified <chr>, `AirTemp (C)` <dbl>, Year <dbl>

## Question 1: What is the oldest and newest year date entry?

``` r
years <- sqldf("SELECT MIN(Year) as smallest_year, MAX(Year) as largest_year
                 FROM df_water")
```

### Question 1: Analysis

    ##   smallest_year largest_year
    ## 1          1899         2019

## Question 2: What is the year with the highest average air temperature in Fahrenheit?

``` r
average <- sqldf("SELECT Year, AVG(air_temp) as Avg_Temp 
                 FROM df_water
                 GROUP BY Year
                 ORDER BY Avg_Temp Desc
                 LIMIT 1")
```

### Question 2: Analysis

    ##   Year Avg_Temp
    ## 1 1989 72.21875

## Question 3: What data had the safe range for aquatic animal habitat, which consists of PH levels of 6.5 to 8.5?

``` r
ph_levels <- sqldf("SELECT * 
                    FROM df_water
                    WHERE ph >= 6.5 AND ph <= 8.5
                    LIMIT 10")
```

### Question 3: Analysis

    ##    Site_Id Unit_Id Read_Date Salinity (ppt) Dissolved Oxygen (mg/L)  ph
    ## 1      Bay    <NA>  1/3/1994            1.3                    11.7 7.3
    ## 2      Bay    <NA> 1/31/1994            1.5                    12.0 7.4
    ## 3      Bay    <NA>  2/7/1994            1.0                    10.5 7.2
    ## 4      Bay    <NA> 2/23/1994            1.0                    10.1 7.4
    ## 5      Bay    <NA> 2/28/1994            1.0                    12.6 7.2
    ## 6      Bay    <NA>  3/7/1994            1.0                     9.9 7.1
    ## 7      Bay    <NA> 3/14/1994            0.5                    10.4 7.2
    ## 8      Bay    <NA> 3/28/1994            1.0                     9.2 7.1
    ## 9      Bay    <NA>  4/4/1994            1.0                     9.2 7.2
    ## 10     Bay    <NA> 4/11/1994            1.0                     8.6 7.3
    ##    Secchi Depth (m) Water Depth (m) Water Temp (?C) Air Temp-Celsius air_temp
    ## 1              0.40            0.40             5.9              8.0    46.40
    ## 2              0.20            0.35             3.0              2.6    36.68
    ## 3              0.25            0.60             5.9              7.6    45.68
    ## 4              0.35            0.50            10.0              2.7    36.86
    ## 5              0.20            0.40             1.6              0.0    32.00
    ## 6              0.20            0.90             9.7             15.2    59.36
    ## 7              0.25            0.75             9.8             10.1    50.18
    ## 8              0.15            0.95            16.1             22.1    71.78
    ## 9              0.25            0.75            15.0             13.5    56.30
    ## 10             0.20            0.75            15.7             13.0    55.40
    ##    Time (24:00) Field_Tech DateVerified WhoVerified AirTemp (C) Year
    ## 1         11:00       <NA>         <NA>        <NA>         8.0 1994
    ## 2         11:30       <NA>         <NA>        <NA>         2.6 1994
    ## 3          9:45       <NA>         <NA>        <NA>         7.6 1994
    ## 4           N/A       <NA>         <NA>        <NA>         2.7 1994
    ## 5         10:30       <NA>         <NA>        <NA>         0.0 1994
    ## 6         10:00       <NA>         <NA>        <NA>        15.2 1994
    ## 7         11:00       <NA>         <NA>        <NA>        10.1 1994
    ## 8          9:50       <NA>         <NA>        <NA>        22.1 1994
    ## 9         10:00       <NA>         <NA>        <NA>        13.5 1994
    ## 10        10:00       <NA>         <NA>        <NA>        13.0 1994

## Question 4: How many days was PH considered not drinkable water and a PH considered drinkable water?

``` r
case_when_ph <- sqldf("SELECT COUNT(Read_Date) as total_days,
                    CASE
                      WHEN ph >= 6.5 AND ph <= 8.5 THEN 'drinkable'
                      ELSE 'not drinkable'
                    END as ph_drink
                    FROM df_water
                    GROUP BY ph_drink")
```

### Question 4: Analysis

    ##   total_days      ph_drink
    ## 1       2028     drinkable
    ## 2        338 not drinkable

## Question 5: How many days has data been recorded for each site?

``` r
days_recorded <- sqldf("SELECT Site_Id, COUNT(DISTINCT Read_Date) as days_recorded
                        FROM df_water
                        GROUP BY Site_Id")
```

### Question 5: Analysis

    ##   Site_Id days_recorded
    ## 1    <NA>             0
    ## 2       A           432
    ## 3       B           436
    ## 4     Bay           792
    ## 5       C           264
    ## 6       D           438
    ## 7       d             1

## Question 6: How many times was each tech in the field?

``` r
field_tech <- sqldf("SELECT DISTINCT Field_Tech as Field_Tech, COUNT(Field_Tech) as total_times_tech
                        FROM df_water
                        GROUP BY Field_Tech
                        ORDER BY total_times_tech DESC")
```

### Question 6: Analysis

    ##                  Field_Tech total_times_tech
    ## 1              Not Recorded             1225
    ## 2                    S. Poe              358
    ## 3                   Sue Poe              342
    ## 4                   Feldman              172
    ## 5                 Susan Poe               86
    ## 6  J Phillips, Mary Feldman               37
    ## 7   Strader, Pease, Feldman               35
    ## 8            Pease, Strader               31
    ## 9             John Phillips               17
    ## 10                  Strader               15
    ## 11          Strader, S. Poe               11
    ## 12                  sue Poe                1
    ## 13                  Sue poe                1
    ## 14                  Sue POE                1
    ## 15                     <NA>                0

## Question 7: Seems like Susan Poe, John Phillips and Mary Feldman are being counted as different row entries, How can you take into account the different spelling variations for the techs to get a more accurate count representation?

``` r
wild_card <- sqldf("SELECT COUNT(Read_Date) as total_days,
                    CASE
                      WHEN Field_Tech LIKE '%,%' THEN Field_Tech
                      WHEN Field_Tech LIKE '%Poe%' THEN 'Susan Poe'
                      WHEN Field_Tech LIKE '%Feldman%' THEN 'Mary Feldman'
                      WHEN Field_Tech LIKE '%Phillips%' THEN 'John Phillips'
                      ELSE Field_Tech
                    END as field_tech_name
                    
                    FROM df_water
                    
                    GROUP BY field_tech_name
                    ORDER BY total_days DESC")
```

### Question 7: Analysis

    ##    total_days          field_tech_name
    ## 1        1225             Not Recorded
    ## 2         785                Susan Poe
    ## 3         172             Mary Feldman
    ## 4          38                     <NA>
    ## 5          37 J Phillips, Mary Feldman
    ## 6          35  Strader, Pease, Feldman
    ## 7          31           Pease, Strader
    ## 8          17            John Phillips
    ## 9          15                  Strader
    ## 10         11          Strader, S. Poe
