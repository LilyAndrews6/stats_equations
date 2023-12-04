Statistical Equations
================
Lily Andrews
2023-11-28

``` r
# parameters
n <- 10000
maf <- 0.2
b <- 0.3
a <- 0
 
# simulate x and e
x <- rbinom(n, 2, maf)
table(x)
```

    ## x
    ##    0    1    2 
    ## 6432 3170  398

``` r
p <- 
e <- rnorm(n)
 
# simulate y
y <- a + b * x + e
 
# assoc of y ~ x
summary(lm(y ~ x))
```

    ## 
    ## Call:
    ## lm(formula = y ~ x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8242 -0.6843 -0.0119  0.6782  3.5887 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 0.006692   0.012121   0.552    0.581    
    ## x           0.280911   0.017565  15.992   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9919 on 9998 degrees of freedom
    ## Multiple R-squared:  0.02494,    Adjusted R-squared:  0.02485 
    ## F-statistic: 255.8 on 1 and 9998 DF,  p-value: < 2.2e-16

``` r
# recapitulate b
print("recapitulate b")
```

    ## [1] "recapitulate b"

``` r
b_hat <- cov(x, y)/var(x)
b_hat
```

    ## [1] 0.2809113

``` r
# recapitulate b
print("recapitulate b")
```

    ## [1] "recapitulate b"

``` r
b_hat <- sum((x-mean(x))^2*(y-mean(y))^2)/sum((y-mean(y))^2)
b_hat
```

    ## [1] 0.3363805

``` r
# recapitulate a
print("recapitulate b")
```

    ## [1] "recapitulate b"

``` r
a_hat <- mean(y) - b_hat*mean(x)
a_hat
```

    ## [1] -0.0153073

``` r
#recapitulate r
print("recapitulate r")
```

    ## [1] "recapitulate r"

``` r
cov(x, y)/(sd(x)*sd(y))
```

    ## [1] 0.1579331

``` r
# rsq of x,y
print("recapitulate rsq x and y")
```

    ## [1] "recapitulate rsq x and y"

``` r
cor(y, x)^2
```

    ## [1] 0.02494287

``` r
# recapitulate bhat - predicted beta?
print("recapitulate bhat")
```

    ## [1] "recapitulate bhat"

``` r
bhat <- cov(x, y)/var(x)
bhat^2 * maf * (1-maf) *2 / var(y)
```

    ## [1] 0.02502574

``` r
#standard error of x
print("se of x")
```

    ## [1] "se of x"

``` r
se_x <- sd(x)/n
se_x
```

    ## [1] 5.64748e-05

``` r
#standard error of y
print("se of y")
```

    ## [1] "se of y"

``` r
se_y <- sd(y)/n
se_y
```

    ## [1] 0.0001004502

``` r
#recapitulated variance of x from allele frequency
print("recapitulate var(x)")
```

    ## [1] "recapitulate var(x)"

``` r
p <- length(x[x==1])/n
var_x <- 2*p*(1-p)
var_x
```

    ## [1] 0.433022

``` r
## variance of x is also calculated by
print("var(x)")
```

    ## [1] "var(x)"

``` r
sum((x-mean(x))^2)/n
```

    ## [1] 0.3189084

``` r
## variance of y is also calculated by
print("var(y)")
```

    ## [1] "var(y)"

``` r
sum((y-mean(y))^2)/n
```

    ## [1] 1.008923
