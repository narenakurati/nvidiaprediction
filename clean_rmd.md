Preliminary Analysis
--------------------

    Sys.setenv(LANG = "en")
    library(car)

    ## Loading required package: carData

    library(ggplot2)
    library(gridExtra)
    library(knitr)
    library(kableExtra)

Let's first bring in the data here. We're looking at all the cards from
the GTX 280 to the current GTX 1080

    xx80 <- read.csv("xx80.csv", sep = ";")
    xx80

    ##     model     launch transistors_millions die_size cuda_cores core_speed
    ## 1  GTX280 2009-01-08                 1400      576        240        648
    ## 2  GTX480 2010-03-26                 3000      520        448        700
    ## 3  GTX580 2010-11-09                 3000      520        512        772
    ## 4  GTX680 2012-03-22                 3540      294       1536       1110
    ## 5  GTX780 2013-03-23                 7080      561       2304       1002
    ## 6  GTX980 2014-09-18                 5200      398       2048       1253
    ## 7 GTX1080 2016-03-27                 7200      314       2560       1800
    ##   shader_speed memory_speed pixel_rate texture_rate memory_size_gb
    ## 1         1476         2484     20.736        51.84            1.0
    ## 2         1401         3696     33.600        42.00            1.5
    ## 3         1544         4008     37.050        49.41            1.5
    ## 4         1006         1110     32.200       128.80            2.0
    ## 5           NA         1502     41.410       160.50            3.0
    ## 6           NA         1753     77.820       155.60            4.0
    ## 7           NA         1376    110.900       277.30            8.0
    ##   bandwidtch bus processing_power_single processing_power_double tdp price
    ## 1    159.000 512                  708.48                      NA 204   649
    ## 2    177.400 384                 1344.96                  168.12 250   499
    ## 3    192.384 384                 1581.10                  197.63 244   499
    ## 4    192.256 256                 3090.43                  128.77 195   499
    ## 5    288.400 384                 3976.70                  165.70 250   649
    ## 6    224.400 256                 4981.00                  155.60 165   549
    ## 7    320.300 256                 8873.00                  277.30 180   599

Our most important metrics that are going to help us here are transistor
density, cuda core count, core speed, and floating point performance.
These tend to be positively correlated with the overall performance of a
graphics card, whereas something like memory speed or bus size really
depends on the nature of the current architecture.

    p1 <- ggplot(xx80, aes(x = as.Date(launch), y = core_speed)) + geom_point() + geom_smooth(se=FALSE) + labs(x = "time", y = "core clock speed")
    p2 <- ggplot(xx80, aes(x = as.Date(launch), y = cuda_cores)) + geom_point() + geom_smooth(se=FALSE) + labs(x = "time", y = "cuda core count")
    p4 <- ggplot(xx80, aes(x = as.Date(launch), y = processing_power_single)) + geom_point() + geom_smooth(se=FALSE) + labs(x = "time", y = "floating point perf")
    grid.arrange(p2,p1,p4, ncol = 3, nrow =2)

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](clean_rmd_files/figure-markdown_strict/unnamed-chunk-3-1.png)

However, one metric is very important for gauging raw computer
performance and that's transistor density. Let's create a vector for
transistor density and rewrite it in terms of transistors per square
inch.

    transistor_density <- xx80$transistors_millions/xx80$die_size
    scaled_transistor_density <- xx80$transistors_millions*1000000/xx80$die_size*0.00155
    kable(data.frame("card" = xx80[,1], "transistor_denisty" = scaled_transistor_density)) %>%  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>% save_kable(file = "cards_transistor_density.html", self_contained = F)

Moore’s Law predicts that the number of transistors per square inch
doubles each 18 months. This may be an interesting metric in predicting
future Nvidia transistor density. Let’s create another vector for what
Moore’s Law predicts the transistor density to be taking the GTX 280’s
transistor density as our starting point.

    #moore's predicted transistor density
    moore_predicted_scaled_transistory_density <- rep(scaled_transistor_density[1], 9) * c(1,2,4,8,16,32,64,128,256)
    moore_predicted_scaled_transistory_density

    ## [1]   3767.361   7534.722  15069.444  30138.889  60277.778 120555.556
    ## [7] 241111.111 482222.222 964444.444

    moore_dates <- c("2009-01-08", "2010-07-08", "2012-01-08", "2012-07-08", "2014-01-08", "2015-07-08", "2017-01-08", "2018-07-08", "2020-01-08")

    kable(data.frame("date" = moore_dates, "transistor_denisty" = moore_predicted_scaled_transistory_density)) %>%  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>% save_kable(file = "cards_transistor_density.html")

Let's see where Nvidia's transistor density is compared to Moore's Law.

    transistor_df <- data.frame("date" = c(as.Date(xx80$launch), as.Date(moore_dates)), "transistors" = c(scaled_transistor_density, moore_predicted_scaled_transistory_density), "type" = c(rep("nvidia", 7), rep("moore", 9))) 
    transistor_df

    ##          date transistors   type
    ## 1  2009-01-08    3767.361 nvidia
    ## 2  2010-03-26    8942.308 nvidia
    ## 3  2010-11-09    8942.308 nvidia
    ## 4  2012-03-22   18663.265 nvidia
    ## 5  2013-03-23   19561.497 nvidia
    ## 6  2014-09-18   20251.256 nvidia
    ## 7  2016-03-27   35541.401 nvidia
    ## 8  2009-01-08    3767.361  moore
    ## 9  2010-07-08    7534.722  moore
    ## 10 2012-01-08   15069.444  moore
    ## 11 2012-07-08   30138.889  moore
    ## 12 2014-01-08   60277.778  moore
    ## 13 2015-07-08  120555.556  moore
    ## 14 2017-01-08  241111.111  moore
    ## 15 2018-07-08  482222.222  moore
    ## 16 2020-01-08  964444.444  moore

    ggplot(transistor_df, aes(x = date, y = transistors, color = type)) + geom_point() + geom_smooth(se=FALSE) + scale_color_manual(values=c("royalblue1", "#76b900")) + xlim(as.Date("2009-01-08"), as.Date("2018-08-02")) + ylim(0, 500000) + labs(x = "time", y = "transistor_density")

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 1 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](clean_rmd_files/figure-markdown_strict/unnamed-chunk-6-1.png)

It looks like Nvidia's progression in transitor density has not kept up
with Moore's Law. This makes sense, as it becomes increasingly more
difficult with each generation to keep up such progress. Now let's
create a linear model regressing past GTX graphics cards' transistor
densities against time. While the specific release date of each graphics
card is definitely important, it's probably better we avoid using them
as factors. Instead we take the response variable as continuous. THis
helps us a lot more with our model's accuracy, especially since, to us,
the release date of each graphics card is more or less arbitrary and in
the control of Nvidia.

    nvidia_transistors <- data.frame("date" = as.Date(xx80$launch), "transistors" = scaled_transistor_density)
    moore_transistors <- data.frame("date" = as.Date(moore_dates), "transistors" = moore_predicted_scaled_transistory_density)

    nvidia_transistor_model <- lm(transistors ~ date, data = nvidia_transistors)
    summary(nvidia_transistor_model)

    ## 
    ## Call:
    ## lm(formula = transistors ~ date, data = nvidia_transistors)
    ## 
    ## Residuals:
    ##       1       2       3       4       5       6       7 
    ##   408.2   807.9 -1655.3  2674.6  -381.3 -5568.7  3714.6 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -1.506e+05  2.259e+04  -6.667 0.001146 ** 
    ## date         1.080e+01  1.458e+00   7.410 0.000704 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3337 on 5 degrees of freedom
    ## Multiple R-squared:  0.9165, Adjusted R-squared:  0.8999 
    ## F-statistic: 54.91 on 1 and 5 DF,  p-value: 0.0007045

A linear fit seems to be pretty good, and we have a significant slope
with an incredibly low p-value. However, this can be made better. Now
while we saw Nvidia's transistor density improvments don't happen at the
level Moore's Law suggests, the growth is certainly more than linear.
Power transform close to 0.5 suggests a square root transformation on y.
Our new model is now more accurate.

    powerTransform(nvidia_transistors[,2])

    ## Estimated transformation parameter 
    ## nvidia_transistors[, 2] 
    ##               0.3664345

    nvidia_transistor_model_2 <- lm(sqrt(transistors) ~ date, data = nvidia_transistors)
    summary(nvidia_transistor_model_2)

    ## 
    ## Call:
    ## lm(formula = sqrt(transistors) ~ date, data = nvidia_transistors)
    ## 
    ## Residuals:
    ##       1       2       3       4       5       6       7 
    ##  -8.391   5.651  -4.223  16.216   3.614 -17.502   4.636 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -5.475e+02  8.147e+01  -6.720  0.00111 ** 
    ## date         4.331e-02  5.258e-03   8.237  0.00043 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 12.03 on 5 degrees of freedom
    ## Multiple R-squared:  0.9314, Adjusted R-squared:  0.9176 
    ## F-statistic: 67.85 on 1 and 5 DF,  p-value: 0.0004297

Now let's use both models to predict transistor densities for an
estimated release GTX 1180 release date of August 1, 2018. We use both
the linear model and the transformed models from before to predict two
transistor densities. We divide each by the GTX 1080's transistor
density to calculate what the GTX 1180's percentage increase in
transistor density will be. We plot our new predicted transistor
densities on our graph. It looks like we can expect anywhere between
15.6% and 37.4% boost in transistor density with the new GTX 1180.

    #predicted performance boost by transistor density through nvidia linear model
    predict(nvidia_transistor_model, newdata = data.frame("date" = as.Date("2018-08-01")), type = "response")/nvidia_transistors[7,2] - 1

    ##         1 
    ## 0.1559908

    #predicted performance boost by transistor density through nvidia transformed model
    predict(nvidia_transistor_model_2, newdata = data.frame("date" = as.Date("2018-08-01")), type = "response")^2/nvidia_transistors[7,2] - 1

    ##         1 
    ## 0.3742506

    ggplot(nvidia_transistors, aes(x = date, y = transistors,)) + geom_point() + geom_smooth(se=FALSE,  color = "#76b900") + xlim(as.Date("2009-01-08"), as.Date("2018-8-02")) + ylim(0, 50000) + labs(x = "time", y = "transistor_density") + geom_point(aes(x=as.Date("2018-08-01"), y=predict(nvidia_transistor_model, newdata = data.frame("date" = as.Date("2018-08-01")), type = "response")), shape = 16, color = "black", size = 2) + geom_point(aes(x=as.Date("2018-08-01"), y=predict(nvidia_transistor_model_2, newdata = data.frame("date" = as.Date("2018-08-01")), type = "response")^2), shape = 16, color = "black", size = 2) + ggtitle("Nvidia GTX cards transistor density over time")

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](clean_rmd_files/figure-markdown_strict/unnamed-chunk-9-1.png)

Bringing in Synthetic benchmarks
--------------------------------

    #3DMark Graphics scores pulled from 3dmark site
    xx80$threedmark_score <- c(NA, 3649, 4952, 7672, 10490, 13898, 21787)

    #Battlefield1 scores pulled from techspot site
    xx80$battlefield1 <- c(NA, NA, NA, 53 , 68 , 97 , 151)

    bench <- data.frame("card" = rep(c(280, 480, 580, 680, 780, 980, 1080),3), "score" = c(xx80$processing_power_single, xx80$threedmark_score, xx80$battlefield1), "category" = c(rep("teraflops", 7), rep("3d_mark_score", 7), rep("battlefield1_fps", 7)), "date" = rep(xx80$launch, 3))

    threedmark_data <- data.frame("date" = as.Date(xx80$launch), "score" = xx80$threedmark_score)
    tflop_data <- data.frame("date" = as.Date(xx80$launch), "score" = xx80$processing_power_single)

    threedmark_model <- lm(score ~ date, threedmark_data)
    summary(threedmark_model)

    ## 
    ## Call:
    ## lm(formula = score ~ date, data = threedmark_data)
    ## 
    ## Residuals:
    ##       2       3       4       5       6       7 
    ##   932.5   445.5  -752.3  -807.8 -1670.8  1852.9 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -1.126e+05  1.223e+04  -9.214 0.000771 ***
    ## date         7.851e+00  7.791e-01  10.077 0.000545 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1459 on 4 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.9621, Adjusted R-squared:  0.9526 
    ## F-statistic: 101.6 on 1 and 4 DF,  p-value: 0.0005455

    tflop_model <- lm(score ~ date, tflop_data)
    summary(tflop_model)

    ## 
    ## Call:
    ## lm(formula = score ~ date, data = tflop_data)
    ## 
    ## Residuals:
    ##        1        2        3        4        5        6        7 
    ##   732.82    88.07  -336.70  -273.83  -448.49 -1021.09  1259.22 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -4.134e+04  5.715e+03  -7.233 0.000788 ***
    ## date         2.899e+00  3.688e-01   7.860 0.000536 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 844.1 on 5 degrees of freedom
    ## Multiple R-squared:  0.9251, Adjusted R-squared:  0.9101 
    ## F-statistic: 61.77 on 1 and 5 DF,  p-value: 0.0005355

    powerTransform(tflop_data[,2])

    ## Estimated transformation parameter 
    ## tflop_data[, 2] 
    ##       0.0868053

    tflop_model2 <- lm(sqrt(score) ~ date, tflop_data)
    summary(tflop_model2)

    ## 
    ## Call:
    ## lm(formula = sqrt(score) ~ date, data = tflop_data)
    ## 
    ## Residuals:
    ##        1        2        3        4        5        6        7 
    ##  1.32744  0.53072 -1.97837  1.59748  0.07994 -5.76263  4.20542 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -3.247e+02  2.334e+01  -13.91 3.45e-05 ***
    ## date         2.456e-02  1.506e-03   16.30 1.58e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.447 on 5 degrees of freedom
    ## Multiple R-squared:  0.9815, Adjusted R-squared:  0.9778 
    ## F-statistic: 265.8 on 1 and 5 DF,  p-value: 1.583e-05

    predict(tflop_model, newdata = data.frame("date" = as.Date("2018-08-01")), type = "response")

    ##        1 
    ## 10097.98

    predict(tflop_model2, newdata = data.frame("date" = as.Date("2018-08-01")), type = "response")^2

    ##        1 
    ## 12328.65

    ggplot(xx80, aes(x = as.Date(launch), y = processing_power_single)) + geom_point() + geom_smooth(se=FALSE, color = "#619CFF")

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](clean_rmd_files/figure-markdown_strict/unnamed-chunk-10-1.png)

    ggplot(xx80, aes(x = as.Date(launch), y = threedmark_score)) + geom_point() + geom_smooth(se=FALSE, color = "#F8766D")

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](clean_rmd_files/figure-markdown_strict/unnamed-chunk-10-2.png)

    ggplot(xx80, aes(x = as.Date(launch), y = battlefield1)) + geom_point() + geom_smooth(se=FALSE, color = "#00BA38")

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](clean_rmd_files/figure-markdown_strict/unnamed-chunk-10-3.png)

    #Looking at percentage increase
    xx80$threedmark_percentage <- xx80$threedmark_score/c(1, xx80$threedmark_score[1:6])
    xx80$processing_power_single_percentage <- xx80$processing_power_single/c(NA, xx80$processing_power_single[1:6])
    xx80$battlefield1_percentage <- xx80$battlefield1/c(1, xx80$battlefield1[1:6])

    bench_percentage <- data.frame("card" = rep(c(280, 480, 580, 680, 780, 980, 1080),3), "percentage_imporvement" = c(xx80$processing_power_single_percentage, xx80$threedmark_percentage, xx80$battlefield1_percentage), "category" = c(rep("teraflops", 7), rep("3d_mark_score", 7), rep("battlefield1_fps", 7)), "date" = rep(xx80$launch, 3))

    bench_percentage

    ##    card percentage_imporvement         category       date
    ## 1   280                     NA        teraflops 2009-01-08
    ## 2   480               1.898374        teraflops 2010-03-26
    ## 3   580               1.175574        teraflops 2010-11-09
    ## 4   680               1.954608        teraflops 2012-03-22
    ## 5   780               1.286779        teraflops 2013-03-23
    ## 6   980               1.252546        teraflops 2014-09-18
    ## 7  1080               1.781369        teraflops 2016-03-27
    ## 8   280                     NA    3d_mark_score 2009-01-08
    ## 9   480                     NA    3d_mark_score 2010-03-26
    ## 10  580               1.357084    3d_mark_score 2010-11-09
    ## 11  680               1.549273    3d_mark_score 2012-03-22
    ## 12  780               1.367310    3d_mark_score 2013-03-23
    ## 13  980               1.324881    3d_mark_score 2014-09-18
    ## 14 1080               1.567636    3d_mark_score 2016-03-27
    ## 15  280                     NA battlefield1_fps 2009-01-08
    ## 16  480                     NA battlefield1_fps 2010-03-26
    ## 17  580                     NA battlefield1_fps 2010-11-09
    ## 18  680                     NA battlefield1_fps 2012-03-22
    ## 19  780               1.283019 battlefield1_fps 2013-03-23
    ## 20  980               1.426471 battlefield1_fps 2014-09-18
    ## 21 1080               1.556701 battlefield1_fps 2016-03-27

    ggplot(bench_percentage, aes(x = as.Date(date), y = percentage_imporvement, color = category)) + geom_point() + geom_smooth(se=FALSE)

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](clean_rmd_files/figure-markdown_strict/unnamed-chunk-10-4.png)

    as.Date(as.character(xx80$launch[2:7])) - as.Date(as.character(xx80$launch[1:6]))

    ## Time differences in days
    ## [1] 442 228 499 366 544 556

    mean(as.Date(as.character(xx80$launch[2:7])) - as.Date(as.character(xx80$launch[1:6])))

    ## Time difference of 439.1667 days

    as.Date(as.character(xx80$launch[7])) - as.Date(as.character("2018-07-05"))

    ## Time difference of -830 days

    830/440

    ## [1] 1.886364

Seeing above, we can separate the releases between BIG and SMALL
performance improvments. GTX 480, 680, and 1080 were BIG performance
improvements. GTX 580, 780, and 980 were SMALL performance improvements.
Given the time since the last release is 1.88 times greater than the
average time in between releases, we can assume we are in for a
significant performance boost with this next generation.

    xx80$processing_power_single_percentage

    ## [1]       NA 1.898374 1.175574 1.954608 1.286779 1.252546 1.781369

    mean(xx80$processing_power_single_percentage[c(2,4,7)])

    ## [1] 1.878117

    xx80$processing_power_single[7] * mean(xx80$processing_power_single_percentage[c(2,4,7)])

    ## [1] 16664.53

Bringing in Game Benchmarks
---------------------------

Won't be looking too much into gaming benchmarks as the people over at
Anandtech, Techspot, and PCPer have this down with great detail.

    #Have already added in Battlefield 1 benchmarks at 1080p
    xx80$battlefield1

    ## [1]  NA  NA  NA  53  68  97 151

    #Tech spot aggregated game performance
    xx80$average_game_perf_1080p <- c(NA, 21, 24, 40, 56, 77, 127)
    xx80$average_game_perf_1600p <- c(NA, 30, 35, 53, 71, 109, 161)

    game_synthetic_scores <- data.frame("date" = as.Date(xx80$launch), "game_perf_1080p" = xx80$average_game_perf_1080p, "game_perf_1600p" = xx80$average_game_perf_1600p, "tflop" = xx80$processing_power_single, "threedmark" = xx80$threedmark_score)

    game_synthetic_1080p_model <- lm(game_perf_1080p ~ tflop, data = game_synthetic_scores)
    summary(game_synthetic_1080p_model)

    ## 
    ## Call:
    ## lm(formula = game_perf_1080p ~ tflop, data = game_synthetic_scores)
    ## 
    ## Residuals:
    ##       2       3       4       5       6       7 
    ##  1.2472  0.8574 -4.8088 -1.5311  5.0523 -0.8169 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 0.4461073  2.7914181    0.16    0.881    
    ## tflop       0.0143549  0.0005924   24.23 1.72e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.673 on 4 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.9932, Adjusted R-squared:  0.9915 
    ## F-statistic: 587.1 on 1 and 4 DF,  p-value: 1.721e-05

    game_synthetic_1600p_model <- lm(game_perf_1600p ~ tflop, data = game_synthetic_scores)
    summary(game_synthetic_1600p_model)

    ## 
    ## Call:
    ## lm(formula = game_perf_1600p ~ tflop, data = game_synthetic_scores)
    ## 
    ## Residuals:
    ##       2       3       4       5       6       7 
    ##  0.6413  1.4079 -7.6504 -5.5389 14.4567 -3.3166 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 5.247182   6.708340   0.782 0.477816    
    ## tflop       0.017927   0.001424  12.592 0.000229 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.826 on 4 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.9754, Adjusted R-squared:  0.9692 
    ## F-statistic: 158.6 on 1 and 4 DF,  p-value: 0.0002289

    predict(game_synthetic_1080p_model, newdata = data.frame("tflop" = 12328.65, type = "response"))

    ##        1 
    ## 177.4223

    177/127

    ## [1] 1.393701

    predict(game_synthetic_1600p_model, newdata = data.frame("tflop" = 12328.65, type = "response"))

    ##        1 
    ## 226.2672

    226/127

    ## [1] 1.779528
