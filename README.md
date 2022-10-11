Project 2
================
Shaoyu Wang
2022-10-09

``` r
library(httr)
library(dplyr)
library(jsonlite)
library(tidyverse)
```

``` r
#My API key 90c7af330c5f45a4a1b709068daed452 

myData <- GET("https://api.spoonacular.com/recipes/findByNutrients?minCarbs=10&maxCarbs=50&number=30&apiKey=90c7af330c5f45a4a1b709068daed452")
parsed <- fromJSON(rawToChar(myData$content))
parsed %>% colnames()
```

    ## [1] "id"        "title"     "image"     "imageType" "calories"  "protein"   "fat"       "carbs"

``` r
# search function
search_by_nutr <- function(nutrient, from, to, number){
  base<-"https://api.spoonacular.com/recipes/findByNutrients"
  call1 <- paste(base,"?","min",nutrient,"=",from,"&","max",nutrient,"=",to,"&number=",number,"&apiKey=90c7af330c5f45a4a1b709068daed452",sep="")
  get_nutrients <- GET(call1)
  get_nutrients_text <- content(get_nutrients, "text")
  get_nutrients_json <- fromJSON(get_nutrients_text, flatten = TRUE)
  get_nutrients_tb <- as_tibble(get_nutrients_json) %>%
    select("id","title","calories","protein","fat","carbs")
  return(get_nutrients_tb)
}
```

``` r
# for a certain range calories, to compare protein/carbs/fat with calories, find which one the more related to calories.
cal_data <- search_by_nutr(nutrient="Calories", from=1000, to=2500, number=100)

cal_data$protein <- as.numeric(substr(cal_data$protein,1, nchar(cal_data$protein)-1))
cal_data$fat <- as.numeric(substr(cal_data$fat,1, nchar(cal_data$fat)-1))
cal_data$carbs <- as.numeric(substr(cal_data$carbs,1, nchar(cal_data$carbs)-1))

cal_data
```

    ## # A tibble: 48 × 6
    ##        id title                                 calories protein   fat carbs
    ##     <int> <chr>                                    <int>   <dbl> <dbl> <dbl>
    ##  1  31237 Black Eyed Peas With Ham Hocks            1073      86    61    41
    ##  2 157399 Crispy-Crowned Guacamole Fish Fillets     1026      77    57    63
    ##  3 157426 Stuffed Shells with Beef and Broc         1192      56    72    81
    ##  4 622825 Tortilla Burger Loco Vaca                 1159      54    86    42
    ##  5 631814 $50,000 Burger                            1075      75    69    42
    ##  6 631868 4 Ingredient Chicken Pot Pie              1053      89    35    90
    ##  7 633538 Baked Chicken with Cinnamon Apples        1222      72    85    32
    ##  8 633661 Baked Lasagne                             1215      65    64    97
    ##  9 633754 Baked Ratatouille                         1028      32    69    82
    ## 10 633841 Baked Teriyaki Chicken Drumsticks         1093      99    46    65
    ## # … with 38 more rows

``` r
g_prot_scat <- ggplot(cal_data, aes(x = protein, y = calories))
g_prot_scat + geom_point()
```

![](README_files/figure-gfm/unnamed-chunk-137-1.png)<!-- -->

``` r
g_fat_scat <- ggplot(cal_data, aes(x = fat, y = calories))
g_fat_scat + geom_point()
```

![](README_files/figure-gfm/unnamed-chunk-137-2.png)<!-- -->

``` r
g_carb_scat <- ggplot(cal_data, aes(x = carbs, y = calories))
g_carb_scat + geom_point()
```

![](README_files/figure-gfm/unnamed-chunk-137-3.png)<!-- -->

``` r
g_carb_box <- ggplot(cal_data, aes(x = calories, y = carbs))
g_carb_box + geom_boxplot() +
     geom_point(alpha = 1, size = 2, position = "jitter") +
     labs()
```

![](README_files/figure-gfm/unnamed-chunk-138-1.png)<!-- -->

``` r
g_fat_box <- ggplot(cal_data, aes(x = calories, y = fat))
g_fat_box + geom_boxplot() +
     geom_point(alpha = 1, size = 2, position = "jitter") +
     labs()
```

![](README_files/figure-gfm/unnamed-chunk-138-2.png)<!-- -->

``` r
g_prot_box <- ggplot(cal_data, aes(x = calories, y = protein))
g_prot_box + geom_boxplot() +
     geom_point(alpha = 1, size = 2, position = "jitter") +
     labs()
```

![](README_files/figure-gfm/unnamed-chunk-138-3.png)<!-- -->

``` r
g_carb_hist <- ggplot(cal_data, aes(x = carbs)) 
g_carb_hist + geom_histogram(color = "blue", fill = "lightblue", breaks=c(0,10,20,30,40,50,60,70,80,90,100), size = 1, binwidth = 2)
```

![](README_files/figure-gfm/unnamed-chunk-139-1.png)<!-- -->

``` r
g_fat_hist <- ggplot(cal_data, aes(x = fat)) 
g_fat_hist + geom_histogram(color = "blue", fill = "lightblue", breaks=c(0,10,20,30,40,50,60,70,80,90,100), size = 1, binwidth = 2)
```

![](README_files/figure-gfm/unnamed-chunk-139-2.png)<!-- -->

``` r
g_prot_hist <- ggplot(cal_data, aes(x = protein)) 
g_prot_hist + geom_histogram(color = "blue", fill = "lightblue", breaks=c(0,10,20,30,40,50,60,70,80,90,100), size = 1, binwidth = 2)
```

![](README_files/figure-gfm/unnamed-chunk-139-3.png)<!-- -->

``` r
search_by_2_nutrs <- function(first_nutrient, first_from, first_to, sec_nutrient, sec_from, sec_to, number){
  base<-"https://api.spoonacular.com/recipes/findByNutrients"
  call2 <- paste(base,"?min",first_nutrient,"=",first_from,"&max",first_nutrient,"=", first_to,
                 "&min",sec_nutrient,"=",sec_from,"&max",sec_nutrient,"=", sec_to,
                 "&number=",number,"&apiKey=90c7af330c5f45a4a1b709068daed452",sep="")
  get_2_nutrs <- GET(call2)
  get_2_nutrs_text <- content(get_2_nutrs, "text")
  get_2_nutrs_json <- fromJSON(get_2_nutrs_text, flatten = TRUE)
  get_2_nutrs_tb <- as_tibble(get_2_nutrs_json) %>%
    select("id","title","calories","protein","fat","carbs")
  return(get_2_nutrs_tb)
}
```

``` r
# For people who like fitness, find a certain range calories but high protein
cal_prot_data <- search_by_2_nutrs(first_nutrient="Calories", first_from=1000, first_to=2500,
                                  sec_nutrient="Protein", sec_from=30, sec_to=100, number=100)

cal_prot_data$protein <- as.numeric(substr(cal_prot_data$protein,1, nchar(cal_prot_data$protein)-1))
cal_prot_data$fat <- as.numeric(substr(cal_prot_data$fat,1, nchar(cal_prot_data$fat)-1))
cal_prot_data$carbs <- as.numeric(substr(cal_prot_data$carbs,1, nchar(cal_prot_data$carbs)-1))

cal_prot_data <- cal_prot_data %>% 
  mutate(fat_level = ifelse(cal_prot_data$fat >= 80, "3",
                            ifelse(cal_prot_data$fat >= 50,"2",
                                   ifelse(cal_prot_data$fat >= 0, "1"))),
         carbs_level = ifelse(cal_prot_data$carbs >= 80, "3",
                              ifelse(cal_prot_data$carbs >= 50, "2",
                                     ifelse(cal_prot_data$carbs >= 0, "1"))))

cal_prot_data <- cal_prot_data %>% 
  mutate(fat_level = factor(fat_level,
                            level = c(1,2,3),
                            labels = c("low","medium","high")),
         carbs_level = factor(carbs_level,
                              level = c(1,2,3),
                              labels = c("low","medium","high")))
```

``` r
g_fat_bar <- ggplot(cal_prot_data, aes(x = fat_level))
g_fat_bar + geom_bar(aes(fill = fat_level), position = "dodge") +
  labs()
```

![](README_files/figure-gfm/unnamed-chunk-142-1.png)<!-- -->

``` r
g_carbs_bar <- ggplot(cal_prot_data, aes(x = carbs_level))
g_carbs_bar + geom_bar(aes(fill = carbs_level), position = "dodge") +
  labs()
```

![](README_files/figure-gfm/unnamed-chunk-142-2.png)<!-- -->

``` r
table(cal_prot_data$fat_level)
```

    ## 
    ##    low medium   high 
    ##      2     23      9

``` r
table(cal_prot_data$carbs_level)
```

    ## 
    ##    low medium   high 
    ##     11     11     12

``` r
table(cal_prot_data$fat_level, cal_prot_data$carbs_level)
```

    ##         
    ##          low medium high
    ##   low      0      1    1
    ##   medium   6      6   11
    ##   high     5      4    0
