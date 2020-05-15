
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Ridge vs. Individual Linear Base-Learner

## Load compboost

``` r
devtools::load_all( "~/repos/compboost")
#> Loading compboost
```

## Simulate data

``` r
# Simulate Data:
nclasses = 10L
nsim = 50000L

classes = sample(x = LETTERS, size = nclasses)
gmean = sample(nclasses) * rnorm(1)

idx = sample(x = seq_len(nclasses), size = nsim, replace = TRUE)

x = classes[idx]
y = gmean[idx] + rnorm(nsim)

df_cat = data.frame(x = x, y = y)

library(ggplot2)
ggplot(data = df_cat, aes(x = x, y = y, fill = x)) +
  geom_boxplot(alpha = 0.4, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  xlab("Class") +
  ylab("Value") +
  ggtitle("Group Means")
#> Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
#> Returning the palette you asked for with that many colors
```

![](files-ridge-vs-linear/unnamed-chunk-3-1.png)<!-- -->

## Set parameter for compboost

``` r
learning_rate = 0.05
iter_max      = 2000L
```

## Fit linear model for each category in feature (current state of the art)

``` r
cboost_lin = Compboost$new(data = df_cat, target = "y", loss = LossQuadratic$new())
cboost_lin$addBaselearner(feature = "x", id = "category", bl_factory = BaselearnerPolynomial, intercept = FALSE)
cboost_lin$train(iter_max, trace = as.integer(iter_max / 4))
#>    1/2000   risk = 2.9  
#>  500/2000   risk = 0.51  
#> 1000/2000   risk = 0.5  
#> 1500/2000   risk = 0.5  
#> 2000/2000   risk = 0.5  
#> 
#> 
#> Train 2000 iterations in 8 Seconds.
#> Final risk based on the train set: 0.5
```

## Fit ridge regression on categorical feature

``` r
response = ResponseRegr$new("mpg", as.matrix(df_cat$y))

cdata_source = CategoricalData$new(df_cat$x, "x")
bl = BaselearnerCategoricalRidge$new(cdata_source, list(df = 3))

factory_list = BlearnerFactoryList$new()
factory_list$registerFactory(bl)

loss_quadratic = LossQuadratic$new()
optimizer = OptimizerCoordinateDescent$new()

log_iterations = LoggerIteration$new(" iterations", TRUE, iter_max)
logger_list = LoggerList$new()
logger_list$registerLogger(log_iterations)

cboost_ridge = Compboost_internal$new(
  response      = response,
  learning_rate = learning_rate,
  stop_if_all_stopper_fulfilled = FALSE,
  factory_list = factory_list,
  loss         = loss_quadratic,
  logger_list  = logger_list,
  optimizer    = optimizer
)
cboost_ridge$train(trace = as.integer(iter_max / 4))
#>    1/2000   risk = 2.7  
#>  500/2000   risk = 0.5  
#> 1000/2000   risk = 0.5  
#> 1500/2000   risk = 0.5  
#> 2000/2000   risk = 0.5  
#> 
#> 
#> Train 2000 iterations in 4 Seconds.
#> Final risk based on the train set: 0.5
```

|                | Real.Means | Estimate.Ridge | Estimate.Linear |
| -------------- | ---------: | -------------: | --------------: |
| x\_H\_category |     0.7681 |         0.7286 |          0.7287 |
| x\_R\_category |     1.5363 |         1.5294 |          1.5294 |
| x\_Q\_category |     2.3044 |         2.2935 |          2.2935 |
| x\_B\_category |     3.0725 |         3.0844 |          3.0845 |
| x\_D\_category |     3.8406 |         3.8406 |          3.8406 |
| x\_K\_category |     4.6088 |         4.5919 |          4.5919 |
| x\_E\_category |     5.3769 |         5.3832 |          5.3831 |
| x\_Y\_category |     6.1450 |         6.1616 |          6.1615 |
| x\_I\_category |     6.9132 |         6.9237 |          6.9237 |
| x\_C\_category |     7.6813 |         7.6887 |          7.6886 |

## Microbenchmark

| expr   |    min |     lq |   mean | median |     uq |    max | neval |
| :----- | -----: | -----: | -----: | -----: | -----: | -----: | ----: |
| linear | 12.353 | 13.759 | 14.212 | 14.181 | 14.826 | 17.474 |    20 |
| ridge  |  4.227 |  4.588 |  4.761 |  4.804 |  4.917 |  5.117 |    20 |
