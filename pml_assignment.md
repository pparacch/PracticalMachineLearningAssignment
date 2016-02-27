# Practical Machine Learning Assignment
Pier Lorenzo Paracchini  
27 February 2016  



## Overview

_"Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement <U+2013> a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset)."_

One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. The goal of this assignment is to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants to predict the manner in which they did the exercise. The __outcome__ of the model is the __class__ variable.

_"Class A corresponds to the specified execution of the exercise, while the other 4 classes correspond to common mistakes. Participants were supervised by an experienced weight lifter to make sure the execution complied to the manner they were supposed to simulate. The exercises were performed by six male participants aged between 20-28 years, with little weight lifting experience. We made sure that all participants could easily simulate the mistakes in a safe and controlled manner by using a relatively light dumbbell (1.25kg)." [1]_

## Data

The training and testing datasets for the assignments are available at the following links:

* training, https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv 
* testing,  https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

More information about the dataset can be found at the following link (http://groupware.les.inf.puc-rio.br/har) [1].

### Loading the training dataset

Looking at the content of the file it is possible to see that there missing values "" and "NA". The initial approach is to consider "", "NA" as __NAs__.


```r
data.training <- read.csv("pml-training.csv", stringsAsFactors = FALSE, na.strings=c("", "NA", "NULL"))
```

__Note!__ When loading the data a __simplification__ has been done, "", "NA" and "NULL" have been cosidered as __NA__ (not vailable data).

## Exploratory Analysis

Using the __training__ dataset it is possible to start to explore the available predictors in order to understand the pre-processing steps that need to be performed on the datasets before trying to fit the model.

Number of observations and variables

```r
dim(data.training)
```

```
## [1] 19622   160
```

Summary for some of the variables of the datataset   

```r
#First 15 variables
summary(data.training[,1:15])
```

```
##        X          user_name         raw_timestamp_part_1
##  Min.   :    1   Length:19622       Min.   :1.32e+09    
##  1st Qu.: 4906   Class :character   1st Qu.:1.32e+09    
##  Median : 9812   Mode  :character   Median :1.32e+09    
##  Mean   : 9812                      Mean   :1.32e+09    
##  3rd Qu.:14717                      3rd Qu.:1.32e+09    
##  Max.   :19622                      Max.   :1.32e+09    
##  raw_timestamp_part_2 cvtd_timestamp      new_window          num_window 
##  Min.   :   294       Length:19622       Length:19622       Min.   :  1  
##  1st Qu.:252912       Class :character   Class :character   1st Qu.:222  
##  Median :496380       Mode  :character   Mode  :character   Median :424  
##  Mean   :500656                                             Mean   :431  
##  3rd Qu.:751891                                             3rd Qu.:644  
##  Max.   :998801                                             Max.   :864  
##    roll_belt       pitch_belt        yaw_belt      total_accel_belt
##  Min.   :-28.9   Min.   :-55.80   Min.   :-180.0   Min.   : 0.0    
##  1st Qu.:  1.1   1st Qu.:  1.76   1st Qu.: -88.3   1st Qu.: 3.0    
##  Median :113.0   Median :  5.28   Median : -13.0   Median :17.0    
##  Mean   : 64.4   Mean   :  0.31   Mean   : -11.2   Mean   :11.3    
##  3rd Qu.:123.0   3rd Qu.: 14.90   3rd Qu.:  12.9   3rd Qu.:18.0    
##  Max.   :162.0   Max.   : 60.30   Max.   : 179.0   Max.   :29.0    
##  kurtosis_roll_belt kurtosis_picth_belt kurtosis_yaw_belt 
##  Length:19622       Length:19622        Length:19622      
##  Class :character   Class :character    Class :character  
##  Mode  :character   Mode  :character    Mode  :character  
##                                                           
##                                                           
##                                                           
##  skewness_roll_belt
##  Length:19622      
##  Class :character  
##  Mode  :character  
##                    
##                    
## 
```

```r
#Last 5 variables
summary(data.training[,155:160])
```

```
##  accel_forearm_y accel_forearm_z  magnet_forearm_x magnet_forearm_y
##  Min.   :-632    Min.   :-446.0   Min.   :-1280    Min.   :-896    
##  1st Qu.:  57    1st Qu.:-182.0   1st Qu.: -616    1st Qu.:   2    
##  Median : 201    Median : -39.0   Median : -378    Median : 591    
##  Mean   : 164    Mean   : -55.3   Mean   : -313    Mean   : 380    
##  3rd Qu.: 312    3rd Qu.:  26.0   3rd Qu.:  -73    3rd Qu.: 737    
##  Max.   : 923    Max.   : 291.0   Max.   :  672    Max.   :1480    
##  magnet_forearm_z    classe         
##  Min.   :-973     Length:19622      
##  1st Qu.: 191     Class :character  
##  Median : 511     Mode  :character  
##  Mean   : 394                       
##  3rd Qu.: 653                       
##  Max.   :1090
```

Looking at the summary it is possible to see that 

* some variables that have lot of missing values,

* some variables seems to be not relevant for the prediction __e.g. X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window, num_window, ..__

For comodity lets split the training dataset into a predictors and outcome datasets


```r
training.predictors <- data.training[, -160]
training.outcome <- data.training[, 160]
```

### Dealing with Missing Values
Lets find out the percentage of missing values for each variable using the following simplification, missing value == NA ...


```r
percentageOfMissingValues <- function(x){
    (sum(is.na(x))/length(x))* 100
}

#Missing Values into predictors
missingValueInfo.predictors <- apply(training.predictors, 2, percentageOfMissingValues)
missingValueInfo.predictors[order(missingValueInfo.predictors, decreasing = TRUE)]
##       kurtosis_roll_belt      kurtosis_picth_belt        kurtosis_yaw_belt 
##                    97.93                    97.93                    97.93 
##       skewness_roll_belt     skewness_roll_belt.1        skewness_yaw_belt 
##                    97.93                    97.93                    97.93 
##            max_roll_belt           max_picth_belt             max_yaw_belt 
##                    97.93                    97.93                    97.93 
##            min_roll_belt           min_pitch_belt             min_yaw_belt 
##                    97.93                    97.93                    97.93 
##      amplitude_roll_belt     amplitude_pitch_belt       amplitude_yaw_belt 
##                    97.93                    97.93                    97.93 
##     var_total_accel_belt            avg_roll_belt         stddev_roll_belt 
##                    97.93                    97.93                    97.93 
##            var_roll_belt           avg_pitch_belt        stddev_pitch_belt 
##                    97.93                    97.93                    97.93 
##           var_pitch_belt             avg_yaw_belt          stddev_yaw_belt 
##                    97.93                    97.93                    97.93 
##             var_yaw_belt            var_accel_arm             avg_roll_arm 
##                    97.93                    97.93                    97.93 
##          stddev_roll_arm             var_roll_arm            avg_pitch_arm 
##                    97.93                    97.93                    97.93 
##         stddev_pitch_arm            var_pitch_arm              avg_yaw_arm 
##                    97.93                    97.93                    97.93 
##           stddev_yaw_arm              var_yaw_arm        kurtosis_roll_arm 
##                    97.93                    97.93                    97.93 
##       kurtosis_picth_arm         kurtosis_yaw_arm        skewness_roll_arm 
##                    97.93                    97.93                    97.93 
##       skewness_pitch_arm         skewness_yaw_arm             max_roll_arm 
##                    97.93                    97.93                    97.93 
##            max_picth_arm              max_yaw_arm             min_roll_arm 
##                    97.93                    97.93                    97.93 
##            min_pitch_arm              min_yaw_arm       amplitude_roll_arm 
##                    97.93                    97.93                    97.93 
##      amplitude_pitch_arm        amplitude_yaw_arm   kurtosis_roll_dumbbell 
##                    97.93                    97.93                    97.93 
##  kurtosis_picth_dumbbell    kurtosis_yaw_dumbbell   skewness_roll_dumbbell 
##                    97.93                    97.93                    97.93 
##  skewness_pitch_dumbbell    skewness_yaw_dumbbell        max_roll_dumbbell 
##                    97.93                    97.93                    97.93 
##       max_picth_dumbbell         max_yaw_dumbbell        min_roll_dumbbell 
##                    97.93                    97.93                    97.93 
##       min_pitch_dumbbell         min_yaw_dumbbell  amplitude_roll_dumbbell 
##                    97.93                    97.93                    97.93 
## amplitude_pitch_dumbbell   amplitude_yaw_dumbbell       var_accel_dumbbell 
##                    97.93                    97.93                    97.93 
##        avg_roll_dumbbell     stddev_roll_dumbbell        var_roll_dumbbell 
##                    97.93                    97.93                    97.93 
##       avg_pitch_dumbbell    stddev_pitch_dumbbell       var_pitch_dumbbell 
##                    97.93                    97.93                    97.93 
##         avg_yaw_dumbbell      stddev_yaw_dumbbell         var_yaw_dumbbell 
##                    97.93                    97.93                    97.93 
##    kurtosis_roll_forearm   kurtosis_picth_forearm     kurtosis_yaw_forearm 
##                    97.93                    97.93                    97.93 
##    skewness_roll_forearm   skewness_pitch_forearm     skewness_yaw_forearm 
##                    97.93                    97.93                    97.93 
##         max_roll_forearm        max_picth_forearm          max_yaw_forearm 
##                    97.93                    97.93                    97.93 
##         min_roll_forearm        min_pitch_forearm          min_yaw_forearm 
##                    97.93                    97.93                    97.93 
##   amplitude_roll_forearm  amplitude_pitch_forearm    amplitude_yaw_forearm 
##                    97.93                    97.93                    97.93 
##        var_accel_forearm         avg_roll_forearm      stddev_roll_forearm 
##                    97.93                    97.93                    97.93 
##         var_roll_forearm        avg_pitch_forearm     stddev_pitch_forearm 
##                    97.93                    97.93                    97.93 
##        var_pitch_forearm          avg_yaw_forearm       stddev_yaw_forearm 
##                    97.93                    97.93                    97.93 
##          var_yaw_forearm                        X                user_name 
##                    97.93                     0.00                     0.00 
##     raw_timestamp_part_1     raw_timestamp_part_2           cvtd_timestamp 
##                     0.00                     0.00                     0.00 
##               new_window               num_window                roll_belt 
##                     0.00                     0.00                     0.00 
##               pitch_belt                 yaw_belt         total_accel_belt 
##                     0.00                     0.00                     0.00 
##             gyros_belt_x             gyros_belt_y             gyros_belt_z 
##                     0.00                     0.00                     0.00 
##             accel_belt_x             accel_belt_y             accel_belt_z 
##                     0.00                     0.00                     0.00 
##            magnet_belt_x            magnet_belt_y            magnet_belt_z 
##                     0.00                     0.00                     0.00 
##                 roll_arm                pitch_arm                  yaw_arm 
##                     0.00                     0.00                     0.00 
##          total_accel_arm              gyros_arm_x              gyros_arm_y 
##                     0.00                     0.00                     0.00 
##              gyros_arm_z              accel_arm_x              accel_arm_y 
##                     0.00                     0.00                     0.00 
##              accel_arm_z             magnet_arm_x             magnet_arm_y 
##                     0.00                     0.00                     0.00 
##             magnet_arm_z            roll_dumbbell           pitch_dumbbell 
##                     0.00                     0.00                     0.00 
##             yaw_dumbbell     total_accel_dumbbell         gyros_dumbbell_x 
##                     0.00                     0.00                     0.00 
##         gyros_dumbbell_y         gyros_dumbbell_z         accel_dumbbell_x 
##                     0.00                     0.00                     0.00 
##         accel_dumbbell_y         accel_dumbbell_z        magnet_dumbbell_x 
##                     0.00                     0.00                     0.00 
##        magnet_dumbbell_y        magnet_dumbbell_z             roll_forearm 
##                     0.00                     0.00                     0.00 
##            pitch_forearm              yaw_forearm      total_accel_forearm 
##                     0.00                     0.00                     0.00 
##          gyros_forearm_x          gyros_forearm_y          gyros_forearm_z 
##                     0.00                     0.00                     0.00 
##          accel_forearm_x          accel_forearm_y          accel_forearm_z 
##                     0.00                     0.00                     0.00 
##         magnet_forearm_x         magnet_forearm_y         magnet_forearm_z 
##                     0.00                     0.00                     0.00

#Missing Values into outcome
percentageOfMissingValues(training.outcome)
## [1] 0
```

It is possible to see that the variables ends up in two possible bins:

* variables with mainly missing values (__percentage of missing values >90%__)
* variables without missing values (__percentage of missing values 0%__)

For the assignment lets start to consider only the variables that do not have missing values - lets keep those variables


```r
idx_variablesToKeeps <- which(missingValueInfo.predictors == 0)
training.predictors <- training.predictors[, idx_variablesToKeeps]
```

The simplified `training.predictors` dataset contains only 59 predictors.

### Removing not relevant predictors

Some of the predictors seem to be not relevant (at first sight) for the prediction. Specifically the following predictors __X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window, num_window__ seem to contain data that is not going to be relevant for the prediction.  


```r
idx_variablesToRemove <- 1:7
training.predictors <- training.predictors[, -idx_variablesToRemove]
```

The simplified `training.predictors` dataset contains only 52 predictors.

### Transformation on Individual Predictors

The initial idea is to predict using trees for the prediction. Trees are in general more resilent to skewness and scaling/ centering issues than other models. For this reason those issues will not be considered for the time being.

Focus will be given to remove near-zero-variance predictors and higly correlated predictors.

#### Near-Zero Variance Predictors

For the selected predictors there are not prolematic predictors with Near-Zero Variance.


```r
nsv <- nearZeroVar(training.predictors, saveMetrics = TRUE)
nsv
##                      freqRatio percentUnique zeroVar   nzv
## roll_belt                1.102        6.7781   FALSE FALSE
## pitch_belt               1.036        9.3772   FALSE FALSE
## yaw_belt                 1.058        9.9735   FALSE FALSE
## total_accel_belt         1.063        0.1478   FALSE FALSE
## gyros_belt_x             1.059        0.7135   FALSE FALSE
## gyros_belt_y             1.144        0.3516   FALSE FALSE
## gyros_belt_z             1.066        0.8613   FALSE FALSE
## accel_belt_x             1.055        0.8358   FALSE FALSE
## accel_belt_y             1.114        0.7288   FALSE FALSE
## accel_belt_z             1.079        1.5238   FALSE FALSE
## magnet_belt_x            1.090        1.6665   FALSE FALSE
## magnet_belt_y            1.100        1.5187   FALSE FALSE
## magnet_belt_z            1.006        2.3290   FALSE FALSE
## roll_arm                52.338       13.5256   FALSE FALSE
## pitch_arm               87.256       15.7323   FALSE FALSE
## yaw_arm                 33.029       14.6570   FALSE FALSE
## total_accel_arm          1.025        0.3364   FALSE FALSE
## gyros_arm_x              1.016        3.2769   FALSE FALSE
## gyros_arm_y              1.454        1.9162   FALSE FALSE
## gyros_arm_z              1.111        1.2639   FALSE FALSE
## accel_arm_x              1.017        3.9598   FALSE FALSE
## accel_arm_y              1.140        2.7367   FALSE FALSE
## accel_arm_z              1.128        4.0363   FALSE FALSE
## magnet_arm_x             1.000        6.8240   FALSE FALSE
## magnet_arm_y             1.057        4.4440   FALSE FALSE
## magnet_arm_z             1.036        6.4468   FALSE FALSE
## roll_dumbbell            1.022       84.2065   FALSE FALSE
## pitch_dumbbell           2.277       81.7450   FALSE FALSE
## yaw_dumbbell             1.132       83.4828   FALSE FALSE
## total_accel_dumbbell     1.073        0.2191   FALSE FALSE
## gyros_dumbbell_x         1.003        1.2282   FALSE FALSE
## gyros_dumbbell_y         1.265        1.4168   FALSE FALSE
## gyros_dumbbell_z         1.060        1.0498   FALSE FALSE
## accel_dumbbell_x         1.018        2.1659   FALSE FALSE
## accel_dumbbell_y         1.053        2.3749   FALSE FALSE
## accel_dumbbell_z         1.133        2.0895   FALSE FALSE
## magnet_dumbbell_x        1.098        5.7486   FALSE FALSE
## magnet_dumbbell_y        1.198        4.3013   FALSE FALSE
## magnet_dumbbell_z        1.021        3.4451   FALSE FALSE
## roll_forearm            11.589       11.0896   FALSE FALSE
## pitch_forearm           65.983       14.8558   FALSE FALSE
## yaw_forearm             15.323       10.1468   FALSE FALSE
## total_accel_forearm      1.129        0.3567   FALSE FALSE
## gyros_forearm_x          1.059        1.5187   FALSE FALSE
## gyros_forearm_y          1.037        3.7764   FALSE FALSE
## gyros_forearm_z          1.123        1.5646   FALSE FALSE
## accel_forearm_x          1.126        4.0465   FALSE FALSE
## accel_forearm_y          1.059        5.1116   FALSE FALSE
## accel_forearm_z          1.006        2.9559   FALSE FALSE
## magnet_forearm_x         1.012        7.7668   FALSE FALSE
## magnet_forearm_y         1.247        9.5403   FALSE FALSE
## magnet_forearm_z         1.000        8.5771   FALSE FALSE
```

#### Between-Predictor Correlation

Lets calculate the correlation between predictors


```r
training.predictors.corr <- cor(training.predictors)
```

and let visually examine the corretion structure between the predictors


```r
par(cex = 0.6)
corrplot(training.predictors.corr, order = "hclust")
```

![](pml_assignment_files/figure-html/coorelationPlot-1.png) 

from the plot is possible to see that there are cluster of highly correlated predictors (see dark blue and dark red points). Another possible simplification is toremove the highly correlated predictors


```r
idx_variableToRemove.highCorr <- findCorrelation(training.predictors.corr, cutoff = 0.75)

#Variables with high correlation 
names(training.predictors[idx_variableToRemove.highCorr])
##  [1] "accel_belt_z"      "roll_belt"         "accel_belt_y"     
##  [4] "accel_arm_y"       "total_accel_belt"  "accel_dumbbell_z" 
##  [7] "accel_belt_x"      "pitch_belt"        "magnet_dumbbell_x"
## [10] "accel_dumbbell_y"  "magnet_dumbbell_y" "accel_arm_x"      
## [13] "accel_dumbbell_x"  "accel_arm_z"       "magnet_arm_y"     
## [16] "magnet_belt_z"     "accel_forearm_y"   "gyros_forearm_y"  
## [19] "gyros_dumbbell_x"  "gyros_dumbbell_z"  "gyros_arm_x"

training.predictors <- training.predictors[, -idx_variableToRemove.highCorr]
```


```r
training.predictors.corr <- cor(training.predictors)
par(cex = 0.8)
corrplot(training.predictors.corr, order = "hclust")
```

![](pml_assignment_files/figure-html/predictorsCorrelationsUpdated-1.png) 

The simplified `training.predictors` dataset contains only 31 predictors.


## References

__[1]__ Ugulino, W.; Cardador, D.; Vega, K.; Velloso, E.; Milidiu, R.; Fuks, H. Wearable Computing: Accelerometers' Data Classification of Body Postures and Movements. Proceedings of 21st Brazilian Symposium on Artificial Intelligence. Advances in Artificial Intelligence - SBIA 2012. In: Lecture Notes in Computer Science. , pp. 52-61. Curitiba, PR: Springer Berlin / Heidelberg, 2012. ISBN 978-3-642-34458-9. DOI: 10.1007/978-3-642-34459-6_6.




* You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.

* Add a reference to the data

