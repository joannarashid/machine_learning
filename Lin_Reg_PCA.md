Lin_Reg_PCA
================
Joanna Rashid
2021-11-26

``` r
library(GGally)
```

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 4.1.2

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

``` r
library(corrplot)
```

    ## corrplot 0.90 loaded

``` r
uscrime <- read.delim("/Users/joannarashid/Documents/Documents - Joanna’s MacBook Pro/School/ISYE6501/hw5-Fall 21/uscrime.txt", header=TRUE)
```

## EDA

``` r
uscrime.cor = cor(uscrime)
corrplot(uscrime.cor, method = "number", type = "full")
```

![](Lin_Reg_PCA_files/figure-gfm/unnamed-chunk-3-1.png)<!-- --> We can
see that we have significant multicollinarity especially with Po1 and
Po2, which makes this data set a good candidate for PCA.

## PCA

``` r
pca <- prcomp(uscrime[,1:15], scale = TRUE)
summary(pca)
```

    ## Importance of components:
    ##                           PC1    PC2    PC3     PC4     PC5     PC6     PC7
    ## Standard deviation     2.4534 1.6739 1.4160 1.07806 0.97893 0.74377 0.56729
    ## Proportion of Variance 0.4013 0.1868 0.1337 0.07748 0.06389 0.03688 0.02145
    ## Cumulative Proportion  0.4013 0.5880 0.7217 0.79920 0.86308 0.89996 0.92142
    ##                            PC8     PC9    PC10    PC11    PC12    PC13   PC14
    ## Standard deviation     0.55444 0.48493 0.44708 0.41915 0.35804 0.26333 0.2418
    ## Proportion of Variance 0.02049 0.01568 0.01333 0.01171 0.00855 0.00462 0.0039
    ## Cumulative Proportion  0.94191 0.95759 0.97091 0.98263 0.99117 0.99579 0.9997
    ##                           PC15
    ## Standard deviation     0.06793
    ## Proportion of Variance 0.00031
    ## Cumulative Proportion  1.00000

``` r
screeplot(pca, type = "lines", col = "red")
```

![](Lin_Reg_PCA_files/figure-gfm/unnamed-chunk-5-1.png)<!-- --> It looks
like there is an elbow at both 4 and 6, suggesting somewhere between 4
and 6 principal components would be suitable for use in the model.

``` r
proportion <- c(summary(pca)$importance[2,])

print("Percent of variance explained by PC 1-4")
```

    ## [1] "Percent of variance explained by PC 1-4"

``` r
sum(proportion[1:4])
```

    ## [1] 0.79919

``` r
print("Percent of variance explained by PC 1-5")
```

    ## [1] "Percent of variance explained by PC 1-5"

``` r
sum(proportion[1:5])
```

    ## [1] 0.86308

``` r
print("Percent of variance explained by PC 1-6")
```

    ## [1] "Percent of variance explained by PC 1-6"

``` r
sum(proportion[1:6])
```

    ## [1] 0.89996

We will select 6 principal components for the model since this explains
nearly 90% of the variance in the data and represents an elbow on the
scree plot.

## Linear Model with Principal Components 1-6

``` r
#selecting components 1-6
select_comps <- pca$x[,1:6]

#new data frame with principal components and response data
new_df <- data.frame(cbind(select_comps, uscrime[,16]))

#liner model with selected components
model <- lm(V7~., data = new_df) 
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = V7 ~ ., data = new_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -377.15 -172.23   25.81  132.10  480.38 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   905.09      35.35  25.604  < 2e-16 ***
    ## PC1            65.22      14.56   4.478 6.14e-05 ***
    ## PC2           -70.08      21.35  -3.283  0.00214 ** 
    ## PC3            25.19      25.23   0.998  0.32409    
    ## PC4            69.45      33.14   2.095  0.04252 *  
    ## PC5          -229.04      36.50  -6.275 1.94e-07 ***
    ## PC6           -60.21      48.04  -1.253  0.21734    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 242.3 on 40 degrees of freedom
    ## Multiple R-squared:  0.6586, Adjusted R-squared:  0.6074 
    ## F-statistic: 12.86 on 6 and 40 DF,  p-value: 4.869e-08

# Reversing PCA

``` r
coefs <- data.frame(model$coefficients[2:7] %*% t(pca$rotation[,1:6]))

coefs
```

    ##          M       So       Ed      Po1      Po2      LF      M.F      Pop
    ## 1 87.83811 43.89972 20.46387 123.1119 118.6478 45.8933 112.6126 25.93763
    ##         NW       U1       U2   Wealth     Ineq     Prob     Time
    ## 1 94.98769 1.819916 29.44592 45.24734 5.724056 -51.7129 36.12882

## Prediction with Test Data Point

``` r
#creating test point for predicted values
test_point <- data.frame(M = 14.0,So = 0,Ed = 10.0, Po1 = 12.0,Po2 = 15.5,
                         LF = 0.640, M.F = 94.0,Pop = 150,NW = 1.1,U1 = 0.120,
                         U2 = 3.6, Wealth = 3200,Ineq = 20.1,Prob = 0.04, Time = 39.0)

#multiplying all of the transformed coefficients by the test poitn data
intercept <- as.numeric(model$coefficients[1])

predicted_value <- intercept +(coefs$M * test_point$M)+
          (coefs$So * test_point$So)+
          (coefs$Ed * test_point$Ed)+
          (coefs$Po1 * test_point$Po1)+
          (coefs$Po2 * test_point$Po2)+
          (coefs$LF * test_point$LF)+
          (coefs$M.F * test_point$M.F)+
          (coefs$Pop * test_point$Pop)+
          (coefs$NW * test_point$NW)+
          (coefs$U1 * test_point$U1)+
          (coefs$U2 * test_point$U2)+
          (coefs$Wealth * test_point$Wealth)+
          (coefs$Ineq * test_point$Ineq)+
          (coefs$Prob * test_point$Prob)+
          (coefs$Time * test_point$Time)
          
print('Model test point prediction')         
```

    ## [1] "Model test point prediction"

``` r
predicted_value
```

    ## [1] 166685.6
