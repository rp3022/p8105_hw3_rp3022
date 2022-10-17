hw3
================
Rhea Pawar

## Problem 0

## Problem 1

#### Read in the data

``` r
data("instacart")

instacart = 
  instacart %>% 
  as_tibble(instacart)
```

#### Answer questions about the data

This dataset contains 1384617 rows and 15 columns, with each row
resprenting a single product from an instacart order. Variables include
identifiers for user, order, and product; the order in which each
product was added to the cart. There are several order-level variables,
describing the day and time of the order, and number of days since prior
order. Then there are several item-specific variables, describing the
product name (e.g. Yogurt, Avocado), department (e.g. dairy and eggs,
produce), and aisle (e.g. yogurt, fresh fruits), and whether the item
has been ordered by this user in the past. In total, there are 39123
products found in 131209 orders from 131209 distinct users.

Below is a table summarizing the number of items ordered from aisle. In
total, there are 134 aisles, with fresh vegetables and fresh fruits
holding the most items ordered by far.

``` r
instacart %>% 
  count(aisle) %>% 
  arrange(desc(n))
```

    ## # A tibble: 134 × 2
    ##    aisle                              n
    ##    <chr>                          <int>
    ##  1 fresh vegetables              150609
    ##  2 fresh fruits                  150473
    ##  3 packaged vegetables fruits     78493
    ##  4 yogurt                         55240
    ##  5 packaged cheese                41699
    ##  6 water seltzer sparkling water  36617
    ##  7 milk                           32644
    ##  8 chips pretzels                 31269
    ##  9 soy lactosefree                26240
    ## 10 bread                          23635
    ## # … with 124 more rows

Next is a plot that shows the number of items ordered in each aisle.
Here, aisles are ordered by ascending number of items.

``` r
instacart %>% 
  count(aisle) %>% 
  filter(n > 10000) %>% 
  mutate(aisle = fct_reorder(aisle, n)) %>% 
  ggplot(aes(x = aisle, y = n)) + 
  geom_point() + 
  labs(title = "Number of items ordered in each aisle") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
```

<img src="HW3_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

Our next table shows the three most popular items in aisles
`baking ingredients`, `dog food care`, and `packaged vegetables fruits`,
and includes the number of times each item is ordered in your table.

``` r
instacart %>% 
  filter(aisle %in% c("baking ingredients", "dog food care", "packaged vegetables fruits")) %>%
  group_by(aisle) %>% 
  count(product_name) %>% 
  mutate(rank = min_rank(desc(n))) %>% 
  filter(rank < 4) %>% 
  arrange(desc(n)) %>%
  knitr::kable()
```

| aisle                      | product_name                                  |    n | rank |
|:---------------------------|:----------------------------------------------|-----:|-----:|
| packaged vegetables fruits | Organic Baby Spinach                          | 9784 |    1 |
| packaged vegetables fruits | Organic Raspberries                           | 5546 |    2 |
| packaged vegetables fruits | Organic Blueberries                           | 4966 |    3 |
| baking ingredients         | Light Brown Sugar                             |  499 |    1 |
| baking ingredients         | Pure Baking Soda                              |  387 |    2 |
| baking ingredients         | Cane Sugar                                    |  336 |    3 |
| dog food care              | Snack Sticks Chicken & Rice Recipe Dog Treats |   30 |    1 |
| dog food care              | Organix Chicken & Brown Rice Recipe           |   28 |    2 |
| dog food care              | Small Dog Biscuits                            |   26 |    3 |

Finally is a table showing the mean hour of the day at which Pink Lady
Apples and Coffee Ice Cream are ordered on each day of the week. This
table has been formatted in an untidy manner for human readers. Pink
Lady Apples are generally purchased slightly earlier in the day than
Coffee Ice Cream, with the exception of day 5.

``` r
instacart %>%
  filter(product_name %in% c("Pink Lady Apples", "Coffee Ice Cream")) %>%
  group_by(product_name, order_dow) %>%
  summarize(mean_hour = mean(order_hour_of_day)) %>%
  spread(key = order_dow, value = mean_hour) %>%
  knitr::kable(digits = 2)
```

    ## `summarise()` has grouped output by 'product_name'. You can override using the
    ## `.groups` argument.

| product_name     |     0 |     1 |     2 |     3 |     4 |     5 |     6 |
|:-----------------|------:|------:|------:|------:|------:|------:|------:|
| Coffee Ice Cream | 13.77 | 14.32 | 15.38 | 15.32 | 15.22 | 12.26 | 13.83 |
| Pink Lady Apples | 13.44 | 11.36 | 11.70 | 14.25 | 11.55 | 12.78 | 11.94 |

## Problem 2

Tidying the accelerometer data

``` r
acc_df = read_csv("data/accel_data.csv") %>%
  janitor::clean_names() %>% 
  pivot_longer(activity_1:activity_1440,
               names_to = "minute_act",
               names_prefix = "activity_",
               values_to = "physical_activity") %>% 
  mutate(minute_act = as.numeric (minute_act),
         day_type = case_when(day == "MOnday"~"weekday", day == "Tuesday"~"weekday",day == "Wednesday"~"weekday",day == "Thursday"~"weekday",day == "Friday"~"weekday",day == "Saturday"~"weekend", day == "Sunday"~"weekend"))
```

### describing the dataset

|                                                  |        |
|:-------------------------------------------------|:-------|
| Name                                             | acc_df |
| Number of rows                                   | 50400  |
| Number of columns                                | 6      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |        |
| Column type frequency:                           |        |
| character                                        | 2      |
| numeric                                          | 4      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |        |
| Group variables                                  | None   |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| day           |         0 |          1.00 |   6 |   9 |     0 |        7 |          0 |
| day_type      |      7200 |          0.86 |   7 |   7 |     0 |        2 |          0 |

**Variable type: numeric**

| skim_variable     | n_missing | complete_rate |   mean |     sd |  p0 |    p25 |   p50 |     p75 | p100 | hist  |
|:------------------|----------:|--------------:|-------:|-------:|----:|-------:|------:|--------:|-----:|:------|
| week              |         0 |             1 |   3.00 |   1.41 |   1 |   2.00 |   3.0 |    4.00 |    5 | ▇▇▇▇▇ |
| day_id            |         0 |             1 |  18.00 |  10.10 |   1 |   9.00 |  18.0 |   27.00 |   35 | ▇▇▇▇▇ |
| minute_act        |         0 |             1 | 720.50 | 415.70 |   1 | 360.75 | 720.5 | 1080.25 | 1440 | ▇▇▇▇▇ |
| physical_activity |         0 |             1 | 267.04 | 443.16 |   1 |   1.00 |  74.0 |  364.00 | 8982 | ▇▁▁▁▁ |

-   This dataset contains **50400** rows(observations) and **6**
    columns(variables).
-   The dataset has variables **week, day_id, day, minute_act,
    physical_activity, day_type**

Aggregating across minutes to find total activity for each day

``` r
sum_acc = acc_df %>%
  select(everything()) %>%
  group_by (day) %>% 
  summarise(total_activity = sum(physical_activity)) %>% 
  pivot_wider(
    names_from = day,
    values_from = total_activity) %>% 
  select( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday") 

sum_acc %>%   
  knitr::kable()
```

|  Monday | Tuesday | Wednesday | Thursday |  Friday | Saturday |  Sunday |
|--------:|--------:|----------:|---------:|--------:|---------:|--------:|
| 1858699 | 1799238 |   2129772 |  2091151 | 2291711 |  1369237 | 1919213 |

Based on the results it can be seen that the physical activity(in
minutes) was higher on Wednesday and Thursday and **highest on Friday**.
The activity is **least on Saturday**

``` r
plot_acc =acc_df %>% 
  mutate(hour= minute_act/60)
ggplot(plot_acc, aes(x = hour, y = physical_activity, color = day))+
  geom_point(alpha = .5)+geom_line()
```

<img src="HW3_files/figure-gfm/plot accelerometer-1.png" width="90%" />

-   From the graph, it can be seen that the physical activity increases
    everyday after 5:00 am. It remains almost constant throughout the
    day and peaks at 7:00 pm i.e 19:00 hours. Maximum activity can be
    seen from 7:00 pm to 11: 00 pm i.e 19:00 hrs to 23:00. Least
    activity is seen from 12:00 am to 5:00 am most likely because
    individual is sleeping during that time period.
-   The physical activity trend differs by day too. On Sunday maximum
    activity can be observed from 10 am to 12 pm whereas on Fridays
    maximum activity can be observed from 8 pm to 10 pm( 20:00 hrs to
    22:00 hrs)

## Problem 3

``` r
data ("ny_noaa")
ny_noaa = ny_noaa %>% 
  as_tibble(ny_noaa)
```

|                                                  |         |
|:-------------------------------------------------|:--------|
| Name                                             | ny_noaa |
| Number of rows                                   | 2595176 |
| Number of columns                                | 7       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |         |
| Column type frequency:                           |         |
| character                                        | 3       |
| Date                                             | 1       |
| numeric                                          | 3       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |         |
| Group variables                                  | None    |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| id            |         0 |          1.00 |  11 |  11 |     0 |      747 |          0 |
| tmax          |   1134358 |          0.56 |   1 |   4 |     0 |      532 |          0 |
| tmin          |   1134420 |          0.56 |   1 |   4 |     0 |      548 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 1981-01-01 | 2010-12-31 | 1997-01-21 |    10957 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |  mean |     sd |  p0 | p25 | p50 | p75 |  p100 | hist  |
|:--------------|----------:|--------------:|------:|-------:|----:|----:|----:|----:|------:|:------|
| prcp          |    145838 |          0.94 | 29.82 |  78.18 |   0 |   0 |   0 |  23 | 22860 | ▇▁▁▁▁ |
| snow          |    381221 |          0.85 |  4.99 |  27.22 | -13 |   0 |   0 |   0 | 10160 | ▇▁▁▁▁ |
| snwd          |    591786 |          0.77 | 37.31 | 113.54 |   0 |   0 |   0 |   0 |  9195 | ▇▁▁▁▁ |

-   There are 7 rows(observations) and 2595176 columns variables in this
    dataset. Each row refers to a single daily observation from a
    weather station in the US.

-   The dataset has variables id, date, prcp, snow, snwd, tmax, tmin
    corresponding to id, date, precipitation (tenths of mm), snow fall
    (mm), snow depth (mm), maximum and minimum temperature (tenths of
    degrees C), respectively.

-   Large amounts of missing data can be seen for variables tmax, tmin,
    prcp, snow and snwd which can be an issue

    -   The tmax variable is missing 1134358 observations.

    -   The tmin variable has 1134420 missing observations.

    -   The prcp (precipitation) variable has 145838 missing
        observations.

    -   The snow (snowfall) variable has 381221 missing observations.

    -   The snwd (snow depth) variable has 591786 missing observations.

``` r
ny_tidy = ny_noaa %>% 
  janitor::clean_names() %>%
  separate (col = date, into = c("year", "month", "day"), sep = '-', convert = TRUE) %>% 
  mutate(month = month.abb[month],
         tmax = as.numeric(tmax),
         tmin = as.numeric(tmin),
         prcp = prcp/10) %>% 
  select (id, year, month, day, everything()) %>%
  mutate(tmax = tmax/10, tmin = tmin/10)
```

For snowfall, the most commonly observed value is **0**. Since most days
of the year do not experience snow fall, it makes sense for 0 to be the
most commonly observed value.
