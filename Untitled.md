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
    ## 6431 3155  414

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
    ## -3.6324 -0.6865 -0.0062  0.6795  4.2787 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.01305    0.01218  -1.071    0.284    
    ## x            0.30696    0.01755  17.486   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9968 on 9998 degrees of freedom
    ## Multiple R-squared:  0.02967,    Adjusted R-squared:  0.02958 
    ## F-statistic: 305.8 on 1 and 9998 DF,  p-value: < 2.2e-16

``` r
# recapitulate b
print("recapitulate b")
```

    ## [1] "recapitulate b"

``` r
b_hat <- cov(x, y)/var(x)
b_hat
```

    ## [1] 0.3069622

``` r
# recapitulate b
print("recapitulate b")
```

    ## [1] "recapitulate b"

``` r
b_hat <- sum((x-mean(x))^2*(y-mean(y))^2)/sum((y-mean(y))^2)
b_hat
```

    ## [1] 0.3460619

``` r
# recapitulate a
print("recapitulate b")
```

    ## [1] "recapitulate b"

``` r
a_hat <- mean(y) - b_hat*mean(x)
a_hat
```

    ## [1] -0.02861843

``` r
#recapitulate r
print("recapitulate r")
```

    ## [1] "recapitulate r"

``` r
cov(x, y)/(sd(x)*sd(y))
```

    ## [1] 0.1722641

``` r
# rsq of x,y
print("recapitulate rsq x and y")
```

    ## [1] "recapitulate rsq x and y"

``` r
cor(y, x)^2
```

    ## [1] 0.02967493

``` r
# recapitulate bhat - predicted beta?
print("recapitulate bhat")
```

    ## [1] "recapitulate bhat"

``` r
bhat <- cov(x, y)/var(x)
bhat^2 * maf * (1-maf) *2 / var(y)
```

    ## [1] 0.02944586

``` r
#standard error of x
print("se of x")
```

    ## [1] "se of x"

``` r
se_x <- sd(x)/n
se_x
```

    ## [1] 5.678815e-05

``` r
#standard error of y
print("se of y")
```

    ## [1] "se of y"

``` r
se_y <- sd(y)/n
se_y
```

    ## [1] 0.0001011924

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

    ## [1] 0.4319195

``` r
## variance of x is also calculated by
print("var(x)")
```

    ## [1] "var(x)"

``` r
sum((x-mean(x))^2)/n
```

    ## [1] 0.3224571

``` r
## variance of y is also calculated by
print("var(y)")
```

    ## [1] "var(y)"

``` r
sum((y-mean(y))^2)/n
```

    ## [1] 1.023887
