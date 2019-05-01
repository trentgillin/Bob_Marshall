Bob Marshall Wilderness Center
========================================================
author: Trent Gillingham
date: 
autosize: true

Objectives: 
========================================================

- Introduction
- Analysis
- Results
- Future Steps 

Introduction: Why I Chose This Project
========================================================

- Ecology Dataset 
  - USDA.gov: Forestry Research
- Find the right machine learning model with the highest accuracy

Analysis: Overview of Data
========================================================
<font size = "3">

```r
library(tidyverse)
BMWC_1982_Data <- read_csv("RDS-2017-0015/Data/BMWC_1982_Data.csv")
Marshall <- BMWC_1982_Data
Marshall %>%
  dplyr::select(c(SATIS, PHOTO, NUMHORS, NATUR, HIKE, TRAVEL, FISH)) %>%
  head()
```

```
# A tibble: 6 x 7
  SATIS PHOTO NUMHORS NATUR  HIKE TRAVEL  FISH
  <int> <int>   <int> <int> <int>  <int> <int>
1     1     1      NA     2     2      1     1
2     1     2      99     2     2      3     1
3     2     1      NA     2     2      1     1
4     1     2       8     1     2      5     2
5     3     1       4     1     1      4     2
6     2     1      NA     1     2      1     1
```
</font>
Analysis: Output Variable
========================================================
<font size = "3">

```r
Marshall%<>%
  mutate(satisfied = as.factor(ifelse(SATIS == 1, TRUE, FALSE)))%>%
  filter(SATIS != 9)
Marshall%>%
  ggplot(aes(satisfied, fill = satisfied))+geom_histogram(stat = "count")+
  ggtitle("Histogram of Satisfied Variable")+
  theme(plot.title = element_text(hjust = 0.5))
```

<img src="Bob Marshall Wilderness Center-figure/unnamed-chunk-2-1.png" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" style="display: block; margin: auto;" />
</font>

Analysis: More Data Cleaning
========================================================
<font size = "3">

```r
library(caret)
Marshall_complete <- read_csv("/Users/trent/Bob_Marshall/Marshall_complete.csv")
masscoercion <- function(x){
  if (nlevels(as.factor(x))>1 & nlevels(as.factor(x))<8) {
    x <- as.factor(x)
 } else {
   x
   }
}

Marshall_complete<- as.data.frame(map(Marshall_complete, masscoercion))

set.seed(3000)
trainindex <- createDataPartition(Marshall_complete$satisfied, p = 0.8,
                                  list = FALSE,
                                  times = 1)

Marshall_train <- Marshall_complete[trainindex,]
Marshall_test <- Marshall_complete[-trainindex,]
```
</font>
- Needed to designate variables as factors
- Filled missing data with MICE package
- Split data

Analysis: First Random Forest
========================================================
<font size = "3">

```r
library(randomForest)

set.seed(3000)

Marshall.rf <- randomForest(satisfied~., data = Marshall_train, importance = TRUE)

varImpPlot(Marshall.rf, type = 1)
```

<img src="Bob Marshall Wilderness Center-figure/unnamed-chunk-4-1.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" style="display: block; margin: auto;" />
</font>
- Accuracy of 73% 

Analysis: Second Random Forest
========================================================
<font size = "3">

```r
set.seed(3000)

bestmtry <- tuneRF(Marshall_train[,-181], Marshall_train$satisfied, stepFactor=1.5, improve=1e-5, ntree=500, doBest = TRUE)
```

```
mtry = 13  OOB error = 31.31% 
Searching left ...
mtry = 9 	OOB error = 31.14% 
0.005376344 1e-05 
mtry = 6 	OOB error = 31.82% 
-0.02162162 1e-05 
Searching right ...
mtry = 19 	OOB error = 27.78% 
0.1081081 1e-05 
mtry = 28 	OOB error = 29.46% 
-0.06060606 1e-05 
```

<img src="Bob Marshall Wilderness Center-figure/unnamed-chunk-5-1.png" title="plot of chunk unnamed-chunk-5" alt="plot of chunk unnamed-chunk-5" style="display: block; margin: auto;" />
</font>
- Accuracy of 74%

Analysis: Third Random Forest
========================================================
<font size = "3">

```r
set.seed(3000)
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
model_fit <- train(satisfied~., data = Marshall_train, method = "rf",
                   trControl = ctrl, tuneLength = 5)
plot(model_fit)
```

<img src="Bob Marshall Wilderness Center-figure/unnamed-chunk-6-1.png" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" style="display: block; margin: auto;" />
</font>
- Accuracy of 76%

Analysis:Bayesian and K-Nearest Neighbor 
========================================================

<img src="Bob Marshall Wilderness Center-figure/unnamed-chunk-7-1.png" title="plot of chunk unnamed-chunk-7" alt="plot of chunk unnamed-chunk-7" style="display: block; margin: auto;" />

Results
========================================================

- Accuracy for Each Model 
  - Best Random Forest Model: 76%
  - Bayesian Logistic Regression: 65%
  - K-Nearest Neighbor: 56%
  
Next Steps
========================================================

- Further Train Models
- Further Analysis into Important Variables
  
