Detecting Flu Epidemics via Search Engine Quieries
================
2022-12-17

# About the Study and the Data Set

Flu epidemics are a major concern for public health because they can
lead to respiratory illnesses, hospitalizations, and deaths. According
to a report published in 2012, influenza was the eighth leading cause of
death in the United States in 2011. Each year, between 250,000 and
500,000 people around the world die from influenza-related diseases. The
U.S. Centers for Disease Control and Prevention (CDC) and the European
Influenza Surveillance Scheme (EISS) track influenza activity using data
from virologic tests and clinical visits for Influenza-like Illness
(ILI). However, national and regional data on influenza activity is
usually published with a lag of 1-2 weeks. The Google Flu Trends project
was created to see if it would be possible to get faster reporting on
influenza activity by using data from online searches related to the
flu, which is available almost immediately.

### EDA

The goal is to use Google search logs to estimate influenza-like illness
(ILI) activity. The Centers for Disease Control and Prevention (CDC)
publishes data on its website on a weekly basis showing the percentage
of patient visits to healthcare providers for ILI purposes at the
regional and state levels. Google Trends allows anyone to access weekly
counts for every query searched by users around the world. For each
location, the counts are normalized by dividing the count for each query
in a particular week by the total number of online search queries
submitted in that location during the week, and then adjusting the
values to be between 0 and 1. The FluTrain.csv file contains data from
January 1, 2004 to December 31, 2011 and includes columns for the date
range, the percentage of ILI-related physician visits for that week, and
the fraction of queries related to ILI for that week. To begin analyzing
this data, it should be loaded into a data frame called FluTrain.

``` r
#Loading the dataset
FluTrain = read.csv("FluTrain.csv")
#Figuring at what week is ILI max
subset(FluTrain, ILI == max(ILI))
```

    ##                        Week      ILI Queries
    ## 303 2009-10-18 - 2009-10-24 7.618892       1

First, we can see that the maximum ILI is 7.618 and happened on the week
18/10/2009

``` r
#Now Let's see the skewness of ILI data
hist(FluTrain$ILI)
```

![](Detecting%20Flu%20Epidemics%20via%20Search%20Engine%20Queries_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

The ILI data is skewed to the right, meaning there exist more lower ILI
values than higher ones in our data set.

``` r
plot(FluTrain$Queries, FluTrain$ILI)
```

![](Detecting%20Flu%20Epidemics%20via%20Search%20Engine%20Queries_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
plot(FluTrain$Queries, log(FluTrain$ILI))
```

![](Detecting%20Flu%20Epidemics%20via%20Search%20Engine%20Queries_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

As we can see from the two scatter plots, there’s seem to be a positive
linear relation between the quires and logarithmic scale of the ILI. The
reason we would check the logarithmic is because our ILI data is
naturally skewed.

### Linear Regression Model

``` r
lmodel = lm(log(ILI) ~ Queries, data = FluTrain)
summary(lmodel)
```

    ## 
    ## Call:
    ## lm(formula = log(ILI) ~ Queries, data = FluTrain)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.76003 -0.19696 -0.01657  0.18685  1.06450 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.49934    0.03041  -16.42   <2e-16 ***
    ## Queries      2.96129    0.09312   31.80   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2995 on 415 degrees of freedom
    ## Multiple R-squared:  0.709,  Adjusted R-squared:  0.7083 
    ## F-statistic:  1011 on 1 and 415 DF,  p-value: < 2.2e-16

As we can see, $R^2$ is equal to 0.709, which is relatively good and
suggests that our initial assumption that quires are correlated with the
logarithmic scale of ILI is correct.  
Let’s apply the model to the training test.

``` r
FluTest = read.csv("FluTest.csv")
predTest = exp(predict(lmodel, newdata = FluTest))
```

Here, we applied the exponential to the prediction because we trained
our model on the logarithmic. and we want our predictions to be in the
form of ILI not Log(ILI).

So, Let’s find the RMSE of the model,

``` r
SSE = sum((predTest - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE
```

    ## [1] 0.7490645

So the RMSE of the model is 0.749

### Time Series

The dataset for this analysis contains weekly measurements of the
dependent and independent variables. This type of dataset is called a
“time series” because the observations are taken at consecutive
intervals of time. In this case, the observations are taken on a weekly
basis. In order to improve the statistical model, it may be helpful to
use the value of the dependent variable from earlier weeks to predict
the current value of the dependent variable. For this analysis, this
means that the ILI variable for the current week will be predicted using
values of the ILI variable from previous weeks. To do this, the
observations need to be “lagged,” or delayed, by a certain amount of
time. Because the ILI variable is reported with a 1- or 2-week lag, the
decision maker will only have data available from 2 or more weeks ago.
To account for this lag, a variable called ILILag2 will be created that
contains the ILI value from 2 weeks before the current observation.

``` r
library(zoo)
```

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
ILIlag2 = lag(zoo(FluTrain$ILI), -2, na.pad = TRUE)
#na.pad = TRUE, adds "missing values" or NA's to the start of the data set
FluTrain$ILIlag2 = coredata(ILIlag2)
```

Now lets’s check the missing values in the new variable.

``` r
summary(FluTrain$ILIlag2)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##  0.5341  0.9010  1.2519  1.6754  2.0580  7.6189       2

The two missing values are the ones at the start of the records where we
have no data about the ILI values.  
Now let’s see the relation between ILI and ILIlag2

``` r
plot(FluTrain$ILI, FluTrain$ILIlag2)
```

![](Detecting%20Flu%20Epidemics%20via%20Search%20Engine%20Queries_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
plot(log(FluTrain$ILI), log(FluTrain$ILIlag2))
```

![](Detecting%20Flu%20Epidemics%20via%20Search%20Engine%20Queries_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

As with the previous model, we are going to use the log of the new
variable.

``` r
lmodel_updated = lm(log(ILI) ~ Queries + log(ILIlag2), data = FluTrain)
summary(lmodel_updated)
```

    ## 
    ## Call:
    ## lm(formula = log(ILI) ~ Queries + log(ILIlag2), data = FluTrain)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.52209 -0.11082 -0.01819  0.08143  0.76785 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -0.24064    0.01953  -12.32   <2e-16 ***
    ## Queries       1.25578    0.07910   15.88   <2e-16 ***
    ## log(ILIlag2)  0.65569    0.02251   29.14   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1703 on 412 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.9063, Adjusted R-squared:  0.9059 
    ## F-statistic:  1993 on 2 and 412 DF,  p-value: < 2.2e-16

As we can see, all the variables are of high significance and the $R^2$
value has increased to 0.9063.

### Testing the Updated Model

``` r
ILIlag2 = lag(zoo(FluTest$ILI), -2, na.pad = TRUE)
FluTest$ILIlag2 = coredata(ILIlag2)
summary(FluTest$ILIlag2)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##  0.9018  1.1359  1.3409  1.5188  1.7606  3.6002       2

For this analysis, the training set contains all of the observations
from 2004 to 2011, and the testing set contains all of the observations
from 2012. The training and testing sets are split sequentially, with no
time gap between them. This means that the first observation in the
testing set (FluTest) was recorded one week after the last observation
in the training set (FluTrain). Based on this information, it is
possible to determine how to fill in the missing values for the ILILag2
variable in FluTest. Specifically, the ILILag2 value for the first
observation in FluTest can be calculated using the ILI value from two
weeks before the last observation in FluTrain. Similarly, the ILILag2
value for the second observation in FluTest can be calculated using the
ILI value from two weeks before the second-to-last observation in
FluTrain, and so on.

``` r
nrow(FluTrain)
```

    ## [1] 417

``` r
FluTest$ILIlag2[1] = FluTrain$ILI[416]
FluTest$ILIlag2[2] = FluTrain$ILI[417]
```

``` r
summary(FluTest$ILIlag2)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.9018  1.1535  1.3592  1.5368  1.8554  3.6002

No missing values :D.

``` r
predTest_updated = exp(predict(lmodel_updated, newdata = FluTest))
```

Let’s calculate the RMSE of the model.

``` r
SSE = sum((predTest_updated - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE
```

    ## [1] 0.2942029

As we can see the $RMSE$ has dropped significantly to 0.294, which could
indicate a huge improvement in the model.
