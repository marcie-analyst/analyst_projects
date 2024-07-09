R Studio Payer Type Project
================

### About the datatset

This dataset was found on the website
<https://data.ca.gov/dataset/healthcare-payments-data-hpd-healthcare-measures>.
Created by the Department of Health Care Access and Information, this
dataset investigates health conditions, healthcare utilization, and
demographics of Californians under Covered California. I selected this
dataset because I was interested in seeing the differences between
utilization depending on California counties. I also wanted to
investigate what the main reason for ER visits was and if there were any
descriptive differences through various payer types. Specifically, I
wanted to demonstrate my ability to use R to manipulate data, create
metrics such as weighted means, and to take into account what illnesses
have the highest prevalence in the population.

#### Joining Files

``` r
#Merging files
health_df <- health_data_df %>% 
  left_join(health_descrip, by = "measure_id")
```

#### How the joined dataframe looks for the first few rows

``` r
head(health_df)
```

    ## # A tibble: 6 × 16
    ##   reporting_year age_band assigned_sex_at_birth county_name
    ##            <dbl> <chr>    <chr>                 <chr>      
    ## 1           2018 0-17     Female                Alameda    
    ## 2           2018 0-17     Female                Alameda    
    ## 3           2018 0-17     Female                Alameda    
    ## 4           2018 0-17     Female                Alameda    
    ## 5           2018 0-17     Female                Alameda    
    ## 6           2018 0-17     Female                Alameda    
    ## # ℹ 12 more variables: covered_california_region <chr>, payer_type <chr>,
    ## #   measure_id <dbl>, measure_numerator <dbl>, measure_denominator <dbl>,
    ## #   measure_scaling_factor <dbl>, suppression_ind <chr>, measure_name <chr>,
    ## #   `Measure Category` <chr>, `Measure Description` <chr>,
    ## #   `Numerator Definition` <chr>, `Denominator Definition` <chr>

## Question 1: In 2021, which counties were among the top 15 highest number of Emergency Department visit rates?

``` r
q1 <- health_df %>% 
  filter(reporting_year == "2021" & measure_name == "ED Visit Rate") %>% 
  group_by(county_name, measure_name) %>% 
  summarise(total_ED = as.integer(sum(measure_numerator, na.rm = TRUE))) %>% 
  arrange(desc(total_ED)) %>% 
  ungroup() %>% 
  filter(total_ED > 0) %>% 
  mutate(county_name = fct_reorder(.f = county_name, .x = total_ED)) %>% 
  head(15)
```

![](R_healthcare_analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### Question 1: Analysis

From the plot we can observe that the counties with the highest
Emergency Department visits are those that have the highest population
concentrations. In 2021 Los Angeles County had a population of 9.8
million compared to Solano county which had a population of 451,000. Due
to the large difference in population, this is expected with our
results.

## Question 2: Out of those 15 counties with the higest Emergency Department rates, what was the behavior of their payer type?

``` r
q2 <- health_df %>% 
  filter(reporting_year == "2021" & measure_name == "ED Visit Rate" & county_name %in% q1$county_name) %>% 
  group_by(county_name, payer_type) %>% 
  summarise(total_payer_type = as.integer(sum(measure_numerator, na.rm = TRUE))) %>% 
  arrange(desc(total_payer_type)) %>% 
  ungroup() %>% 
  mutate(county_name = fct_reorder(.f = county_name, .x = total_payer_type)) 
```

![](R_healthcare_analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

### Question 2: Analysis

As we can see from the plot, Medi-Cal was the largest payer type in
every county. With more data than is provided in this dataset, it would
be interesting to look into why state funded Medi-Cal is the most
prevalent among the counties with the highest emergency department
visits. I hypothesize that the general income of California would either
mirror the plot ratios we see, or perhaps with how impacted the medical
system in California is,the Emergency Department visits are higher due
to long wait times to see a general care practitioner with Medi-Cal.

## Question 3: In the year of 2021 which illness has the highest percentage?

``` r
q3 <- health_df %>% 
  filter(reporting_year == "2021" & !(measure_name %in% c("Average Age", "Medical Member Count", "Pharmacy Member Count", "Medi-Cal Enrollment Rate", "Commercial Enrollment Rate", "Medicare Enrollment Rate"))) %>% 
  group_by(measure_name) %>% 
  summarise(total_count = as.integer(sum(measure_numerator, na.rm = TRUE)),
            total_whole = as.integer(sum(measure_denominator, na.rm = TRUE)),
            percentage = round(total_count/total_whole, digits = 4) * 100,
            total_count = comma(total_count),
            total_whole = comma(total_whole))%>% 
  arrange(desc(percentage)) %>% 
  ungroup()
```

    ## # A tibble: 29 × 4
    ##    measure_name                               total_count total_whole percentage
    ##    <chr>                                      <chr>       <chr>            <dbl>
    ##  1 High Blood Pressure (Hypertension) Preval… 4,475,565   31,947,574       14.0 
    ##  2 High Cholesterol (Hyperlipidemia) Prevale… 4,089,177   31,947,574       12.8 
    ##  3 Anxiety Prevalence                         2,572,781   31,947,574        8.05
    ##  4 Depression, Bipolar, or Other Depressive … 2,537,011   31,947,574        7.94
    ##  5 Diabetes Prevalence                        2,431,897   31,947,574        7.61
    ##  6 Obesity Prevalence                         2,366,404   31,947,574        7.41
    ##  7 Rheumatoid Arthritis/Osteoarthritis Preva… 1,890,674   31,947,574        5.92
    ##  8 Asthma Prevalence                          1,359,819   31,947,574        4.26
    ##  9 Anemia Prevalence                          1,345,333   31,947,574        4.21
    ## 10 Chronic Kidney Disease Prevalence          1,040,959   31,947,574        3.26
    ## # ℹ 19 more rows

### Question 3: Analysis

The illness with the highest percentage is High Blood Pressure at 14%.
Second is High Cholesterol at 12.8%, and Anxiety at 8%. These are
interesting outcomes as High Blood Pressure and Anxiety can cause
similar sensations in the body, and can affect each other. It is also
interesting to note out of the top five greatest illnesses, two are
mental illness.

## Question 4: Since the illness with the highest percentage is High Blood Pressure, are the percentages across payer types behaving similarly within each county?

### Standardizing percentages by using the standard deviation of the means from each payor type group

``` r
q4 <- health_df %>% 
  filter(reporting_year == "2021" & measure_name == "High Blood Pressure (Hypertension) Prevalence") %>% 
  group_by(county_name, measure_name, payer_type) %>% 
  summarise(total_count = as.integer(sum(measure_numerator, na.rm = TRUE)),
            total_whole = as.integer(sum(measure_denominator, na.rm = TRUE)),
            percentage = round(total_count/total_whole, digits = 4) * 100) %>% 
  group_by(payer_type) %>% 
  mutate(stand_percent = scale(percentage),
         stand_percent = round(stand_percent, digits = 2)) %>% 
  arrange(desc(stand_percent)) %>% 
  ungroup() %>% 
  select(-measure_name)
```

    ## # A tibble: 174 × 6
    ##    county_name   payer_type total_count total_whole percentage stand_percent[,1]
    ##    <chr>         <chr>            <int>       <int>      <dbl>             <dbl>
    ##  1 San Francisco Medi-Cal         39915      217909       18.3              2.34
    ##  2 Modoc         Medi-Cal           617        3430       18.0              2.21
    ##  3 Imperial      Medi-Cal         17416       97907       17.8              2.13
    ##  4 Del Norte     Commercial        1330        7096       18.7              1.94
    ##  5 Tuolumne      Medi-Cal          2483       14738       16.8              1.76
    ##  6 Tuolumne      Commercial        2527       14847       17.0              1.42
    ##  7 Lake          Medi-Cal          5608       35298       15.9              1.38
    ##  8 Lassen        Commercial        1730       10338       16.7              1.33
    ##  9 Calaveras     Commercial        2213       13292       16.7              1.3 
    ## 10 Modoc         Commercial         236        1424       16.6              1.28
    ## # ℹ 164 more rows

### Question 4: Analysis

Standardization is important because based on percentages, it appears
that San Francisco Medi-Cal has a lower percentage of high blood
pressure cases. But the sizes of each payer type are different within
each group, therefore we could be comparing apples to oranges. To ensure
there is an actual difference between populations, we can compare them
by standardizing the data. For example, when standard deviation is
applied, San Francisco actually has a higher prevalence of high blood
pressure with an SD of 2.34.Meaning that the even though percentage
wise, 18% doesn’t look very high, compared to the mean, San Francisco’s
Medi-cal has greater rates of High Blood Pressure.

## Question 5: What is the overall weighted average percentage of High Blood Pressure by population?

``` r
q5 <- q4 %>% 
  group_by(county_name) %>% 
  summarise(weighted_average = sum((percentage * total_whole), na.rm = TRUE)/sum(total_whole, na.rm = TRUE),
            weighted_average = round(weighted_average, digits = 2)) %>% 
  arrange(desc(weighted_average))
```

    ## # A tibble: 58 × 2
    ##    county_name weighted_average
    ##    <chr>                  <dbl>
    ##  1 Amador                  19.3
    ##  2 Tuolumne                18.5
    ##  3 Imperial                17.4
    ##  4 Modoc                   17.4
    ##  5 Calaveras               17.3
    ##  6 Lake                    17.3
    ##  7 Del Norte               17.0
    ##  8 Mariposa                16.6
    ##  9 Kings                   16.4
    ## 10 Lassen                  16.4
    ## # ℹ 48 more rows

### Question 5: Analysis

Utilizing weighted average helps in understanding the prevalence of this
illness per county by adjusting for the varying numbers of people
covered by the three payer types. Weighted average helps to provide a
more accurate overall percentage that reflects the county as a whole
rather than treating each payer type equally regardless of the
population size. Because of this, we can see that Amador county has
approximately 19.31% of it’s population having High Blood Pressure.
Without the weighted average, these reflections of High Blood Pressure
prevalence in each county shown may not have been as accurate.

## Question 6: In the year 2021 what was the total rate of individuals by payer type?

``` r
q6 <- health_df %>% 
  filter(reporting_year == "2021" & measure_name %in% c("Commercial Enrollment Rate","Medicare Enrollment Rate","Medi-Cal Enrollment Rate")) %>% 
  group_by(payer_type) %>%
  summarise(total_count = as.integer(sum(measure_numerator, na.rm = TRUE)),
            total_count = comma(round(total_count, digits = 2)))
```

    ## # A tibble: 3 × 2
    ##   payer_type total_count
    ##   <chr>      <chr>      
    ## 1 Commercial 14,743,491 
    ## 2 Medi-Cal   14,022,861 
    ## 3 Medicare   3,181,222

### Question 6: Analysis

The rate of individuals per payer type shows that Commercial and
Medi-Cal had the highest utilization during this calendar year. This
makes sense in that Medicare is utilized by elderly people which made up
about %14 of California’s population in 2021, accounting for an expected
lower rate of utilization.
