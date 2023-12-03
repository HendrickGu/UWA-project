library(dplyr)
library(fpc)
library(forcats)
library(ggplot2)
library(gridExtra)
library(grDevices)
library(lime)
library(pROC)
library(purrr)
library('ROCR')
library(ROCit)
library('rpart')
library(scales)
library(stringr)
library(shiny)

data.path <- 'D:/WA/Study/2023 S2/CITS4009/Project/youtube_UTF_8.csv'
youtube <- read.csv(data.path)

# Part 1: Data cleaning and transformation

# Replace all values with 'nan' or 'NAN' with NA
youtube[youtube == "nan" | youtube == "NAN"] <- NA

# Delete duplicate rows
youtube <- youtube[!duplicated(youtube), ]

# Replace 0 with NA
youtube <- mutate_all(youtube, ~replace(., . == 0, NA))

# Filter out rows containing more than 60% NA
youtube <- youtube %>%
  filter(rowSums(is.na(.)) / ncol(.) <= 0.6)

# If it is a numerical variable, replace the NA value with mean, and if it is a text variable, replace NA with 'Missing'
num_cols <- c("subscribers", "video.views", "uploads", "created_year", 
              "Gross.tertiary.education.enrollment....", "Population", 
              "Unemployment.rate", "Urban_population", "Latitude", "Longitude", "lowest_monthly_earnings", "highest_monthly_earnings", "lowest_yearly_earnings", "highest_yearly_earnings")

chr_cols <- c("category", "Country", "channel_type", "created_month")

for (col in num_cols) {
  youtube[[col]][is.na(youtube[[col]])] <- mean(youtube[[col]], na.rm = TRUE)
}

for (col in chr_cols) {
  youtube[[col]][is.na(youtube[[col]])] <- "Missing"
}
for (col in chr_cols) {
  youtube[[col]] <- as.factor(youtube[[col]])
}

# Delete created_ Rows with 'Missing' in the month column
youtube <- youtube[!(youtube$created_month == "Missing"), ]

# Filter out unreasonable years. YouTube was created in 2005 and delete data from before 2005
youtube <- youtube %>%
  filter(`created_year` >= 2005)

# Delete inappropriate lines, such as the highest yearly earning being less than the lowest early earning
youtube <- youtube %>%
  filter(
    !(
      highest_yearly_earnings < lowest_monthly_earnings | 
        highest_yearly_earnings < highest_monthly_earnings |
        highest_yearly_earnings < lowest_yearly_earnings
    )
  )

month_mapping <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  created_Month = 1:12
)

youtube <- youtube %>%
  left_join(month_mapping, by = c(created_month = "Month")) %>%
  mutate(created_month = coalesce(created_Month, median(created_Month, na.rm = TRUE))) %>%
  select(-created_Month) 

# Part 2: Classification

### Selecting the Response(Target) Variable

# Load GDP per capita data
gdp_data <- read.csv("API_NY.GDP.csv", skip=4, stringsAsFactors = FALSE, fileEncoding="utf-8")

# Define a mapping function to standardize country names
map_country_name <- function(name) {
  if (name == "south korea") return("korea rep")
  if (name == "russia") return("russian federation")
  if (name == "turkey") return("turkiye")
  if (name == "venezuela") return("venezuela rb")
  if (name == "egypt") return("egypt arab rep")
  return(name)
}

# Define a function to clean country names
clean_country_name <- function(name) {
  name <- tolower(name) # Convert to lowercase
  name <- gsub("[^a-z0-9 ]", "", name) # Remove special characters
  name <- gsub(" +", " ", trimws(name)) # Trim white space and replace multiple spaces with a single space
  name <- map_country_name(name) # Map to standard name
  return(name)
}

# Apply the cleaning functions
gdp_data$Country.Name <- sapply(gdp_data$Country.Name, clean_country_name)
youtube$Country <- sapply(youtube$Country, clean_country_name)


# Define a function to obtain log GDP per capita for a country
get_log_gdp_per_capita <- function(country_name) {
  if(country_name == "missing") {
    avg_gdp <- mean(gdp_data$X2022, na.rm = TRUE)
    return(log(avg_gdp))
  }
  
  country_gdp_data <- gdp_data[gdp_data$Country.Name == country_name, ]
  
  # Check if matching country data is found
  if(nrow(country_gdp_data) == 0) {
    cat("Country not found:", country_name, "\n")
    return(NA)
  }
  
  # Grab GDP in year order and take logarithms
  for(year in 2022:1960) {
    gdp_value <- country_gdp_data[[paste0("X", year)]]
    if (!is.na(gdp_value) && gdp_value > 0) {
      log_gdp_per_capita <- log(gdp_value)
      return(log_gdp_per_capita)
    }
  }
  return(NA)
}

# Calculate the earnings_multiplier for each channel
youtube <- youtube %>%
  mutate(
    log_country_gdp = map_dbl(Country, get_log_gdp_per_capita),
    log_highest_yearly_earnings = log(highest_yearly_earnings),
    earnings_multiplier = ifelse(is.na(log_country_gdp), NA, log_highest_yearly_earnings - log_country_gdp)
  )

# Determine the threshold for high-earning channels
quantile_75 <- quantile(youtube$earnings_multiplier, 0.75, na.rm = TRUE)

# Label channels as high-earning (1) or low-earning (0)
youtube <- youtube %>%
  mutate(
    earning_category = ifelse(earnings_multiplier > quantile_75, 1, 0)
  )

### Selecting Variables

# Calculate lowest_ Monthly_ Earnings and Highest_ Yearly_ The correlation of earnings
cor1 <- cor(youtube$lowest_monthly_earnings, youtube$highest_yearly_earnings, use = "complete.obs")
# Calculate Highest_ Monthly_ Earnings and Highest_ Yearly_ The correlation of earnings
cor2 <- cor(youtube$highest_monthly_earnings, youtube$highest_yearly_earnings, use = "complete.obs")
# Calculate lowest_ Yearly_ Earnings and Highest_ Yearly_ The correlation of earnings
cor3 <- cor(youtube$lowest_yearly_earnings, youtube$highest_yearly_earnings, use = "complete.obs")

cat("Correlation between lowest_monthly_earnings and highest_yearly_earnings: ", cor1, "\n")
cat("Correlation between highest_monthly_earnings and highest_yearly_earnings:", cor2, "\n")
cat("Correlation between lowest_yearly_earnings and highest_yearly_earnings:  ", cor3, "\n")

# Select specific columns as candidate and response variables
youtube <- youtube %>%
  select(subscribers, video.views, category, uploads, Country, channel_type, created_year, created_month,
         `Gross.tertiary.education.enrollment....`, Population, `Unemployment.rate`, 
         Urban_population, Latitude, Longitude, earning_category)
outcome <- "earning_category"
pos.label <- "1"

## The Null Model

Npos <- sum(youtube[,"earning_category"] == 1)
pred_Null <- Npos / nrow(youtube)

cat("Proportion of high-earning in the data:", pred_Null)

### Splitting the data

set.seed(4009)  # Ensure repeatability
vars = setdiff(colnames(youtube), c("earning_category","group"))

# Split data into category variables and numerical variables
catVars = vars[sapply(youtube[, vars], class) %in%
c('character','factor')]

numericVars = vars[sapply(youtube[, vars], class) %in%
c('numeric','integer')]

youtube$group = runif(dim(youtube)[1])
trainingSet = subset(youtube, group<= 0.9)
test_set = subset(youtube, group>0.9)


calib.set = rbinom(dim(trainingSet)[1], size=1, prob=0.2)>0
calibration_set = subset(trainingSet, calib.set)
train_set = subset(trainingSet, !calib.set)


## Single Variable Models

outcome <- "earning_category"
pos = '1' # We are interested in when 'earning_category' is positive. We need to put quotes around number 1 as it is the column name of the table created

table.1 = table(trainingSet[,'channel_type'], trainingSet[,outcome], useNA='ifany')
print(table.1[,2]/(table.1[,1]+table.1[,2]))

### Single Variable Models with Categorical Variables

catVars

mkPredC <- function(outCol,varCol,appCol, pos=pos.label) {
  pPos <- sum(outCol==pos)/length(outCol)
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  pPosWna <- (naTab/sum(naTab))[pos]
  vTab <- table(as.factor(outCol),varCol)
  pPosWv <- (vTab[pos,]+1.0e-3*pPos)/(colSums(vTab)+1.0e-3)
  pred <- pPosWv[appCol]
  pred[is.na(appCol)] <- pPosWna
  pred[is.na(pred)] <- pPos
  pred
}


# Call the mkPredC() function for all the categorical columns
for (v in catVars) {
  pred_var <- paste('pred', v, sep='')
  train_set[, pred_var] <- mkPredC(train_set[, outcome],train_set[, v], train_set[, v])
  calibration_set[, pred_var] <- mkPredC(train_set[, outcome], train_set[, v], calibration_set[, v])
  test_set[, pred_var] <- mkPredC(train_set[, outcome], train_set[, v], test_set[, v])
}

rows <- c(123,108,98,65,27,19,9)
calibration_set[rows,c("category","predcategory")]

calcAUC <- function(predcol, outcol, pos=pos.label) {
  perf <- performance(prediction(predcol, outcol==pos),'auc')
  as.numeric(perf@y.values)
}

betcatVars = c()
for (v in catVars) {
  pred_var <- paste('pred', v, sep='')
  aucTrain <- calcAUC(train_set[, pred_var], train_set[, outcome])
  # The calibration set AUC is calculated and the results are printed only when the training set AUC is greater than or equal to 0.54
  if (aucTrain >= 0.54) {
    aucCal <- calcAUC(calibration_set[, pred_var], calibration_set[, outcome])
    print(sprintf(
      "%s: trainAUC: %4.3f; calibrationAUC: %4.3f",
      pred_var, aucTrain, aucCal
    ))
    betcatVars = c(betcatVars, v)
  }
}

### 10-Fold Cross-Validation

for (var in betcatVars) {
  aucs <- rep(0,10)
  for (rep in 1:length(aucs)) {
    useForCalRep <- rbinom(n=nrow(trainingSet), size=1, prob=0.1) > 0
    predRep <- mkPredC(trainingSet[!useForCalRep, outcome],
                       trainingSet[!useForCalRep, var],
                       trainingSet[useForCalRep, var])
    aucs[rep] <- calcAUC(predRep, trainingSet[useForCalRep, outcome])
  }
  print(sprintf("%s: mean: %4.3f; sd: %4.3f", var, mean(aucs), sd(aucs)))
}


### Single Variable Models with Numerical Variables

numericVars

q1 <- quantile(train_set[,"subscribers"], probs=seq(0, 1, 0.1), na.rm=T)
dis.Varsub = cut(train_set[,"subscribers"], unique(q1))
q2 <- quantile(train_set[,"video.views"], probs=seq(0, 1, 0.1), na.rm=T)
dis.Varvideo = cut(train_set[,"video.views"], unique(q2))
q3 <- quantile(train_set[,"uploads"], probs=seq(0, 1, 0.1), na.rm=T)
dis.Varuploads = cut(train_set[,"uploads"], unique(q3))
q4 <- quantile(train_set[,"created_year"], probs=seq(0, 1, 0.1), na.rm=T)
dis.Varyear = cut(train_set[,"created_year"], unique(q4))
q5 <- quantile(train_set[,"created_month"], probs=seq(0, 1, 0.1), na.rm=T)
dis.Varmonth = cut(train_set[,"created_month"], unique(q5))
q6 <- quantile(train_set[,"Gross.tertiary.education.enrollment...."], probs=seq(0, 1, 0.1), na.rm=T)  
dis.Varedu = cut(train_set[,"Gross.tertiary.education.enrollment...."], unique(q6))
q7 <- quantile(train_set[,"Population"], probs=seq(0, 1, 0.1), na.rm=T)
dis.Varpop = cut(train_set[,"Population"], unique(q7))
q8 <- quantile(train_set[,"Unemployment.rate"], probs=seq(0, 1, 0.1), na.rm=T)
dis.Varrate = cut(train_set[,"Unemployment.rate"], unique(q8))
q9 <- quantile(train_set[,"Urban_population"], probs=seq(0, 1, 0.1), na.rm=T)
dis.Varurban = cut(train_set[,"Urban_population"], unique(q9))
q10 <- quantile(train_set[,"Latitude"], probs=seq(0, 1, 0.1), na.rm=T)
dis.Varla = cut(train_set[,"Latitude"], unique(q10))
q11 <- quantile(train_set[,"Longitude"], probs=seq(0, 1, 0.1), na.rm=T)
dis.Varlo = cut(train_set[,"Longitude"], unique(q11))

mkPredN <- function(outCol,varCol,appCol) {
  cuts <- unique(as.numeric(quantile(varCol, probs=seq(0, 1, 0.1), na.rm=T)))
  varC <- cut(varCol, cuts)
  appC <- cut(appCol, cuts)
  mkPredC(outCol, varC, appC)
}

betnumericVars = c()
for(v in numericVars) {
  pred_var<-paste('pred',v,sep='')
  train_set[,pred_var] <- mkPredN(train_set[,outcome], train_set[,v], train_set[,v])
  test_set[,pred_var] <- mkPredN(train_set[,outcome], train_set[,v], test_set[,v])
  calibration_set[,pred_var] <- mkPredN(train_set[,outcome], train_set[,v], calibration_set[,v])
  aucTrain <- calcAUC(train_set[,pred_var],train_set[,outcome])
  
  if(aucTrain>=0.54) {
    aucCal<-calcAUC(calibration_set[,pred_var],calibration_set[,outcome])
    print(sprintf(
      "%s, trainAUC: %4.3f calibrationAUC: %4.3f",
      pred_var,aucTrain,aucCal))
    betnumericVars = c(betnumericVars, v)
  }
}

### 10-fold Cross-Validation

for (var in betnumericVars) {
aucs <- rep(0,10)
for (rep in 1:length(aucs)) {
useForCalRep <- rbinom(n=nrow(trainingSet), size=1, prob=0.1) > 0
predRep <- mkPredN(trainingSet[!useForCalRep, outcome],
trainingSet[!useForCalRep, var],
trainingSet[useForCalRep, var])
aucs[rep] <- calcAUC(predRep, trainingSet[useForCalRep, outcome])
}
print(sprintf("%s: mean: %4.3f; sd: %4.3f", var, mean(aucs), sd(aucs)))
}





### Feature Selection using log Likelihood

logLikelihood <- function(ytrue, ypred, epsilon=1e-6) {
  sum(ifelse(ytrue==pos, log(ypred+epsilon), log(1-ypred-epsilon)), na.rm=T)
}
outcome <- 'earning_category'
logNull <- logLikelihood(
  calibration_set[,outcome], sum(calibration_set[,outcome]==pos)/nrow(calibration_set)
)
cat(logNull)

selCatVars <- c()
minDrop <- 5
for (v in catVars) {
  pi <- paste('pred', v, sep='')
  devDrop <- 2*(logLikelihood(calibration_set[,outcome], calibration_set[,pi]) - logNull)
  if (devDrop >= minDrop) {
    print(sprintf("%s, deviance reduction: %g", pi, devDrop))
    selCatVars <- c(selCatVars, pi)
  }
}

selNumVars <- c()
minDrop <- 5
for (v in numericVars) {
  pi <- paste('pred', v, sep='')
  devDrop <- 2*(logLikelihood(calibration_set[,outcome], calibration_set[,pi]) - logNull)
  if (devDrop >= minDrop) {
    print(sprintf("%s, deviance reduction: %g", pi, devDrop))
    selNumVars <- c(selNumVars, pi)
  }
}

selVars = c(selCatVars, selNumVars)
selVars


### ROC Curves for selected categorical and numerical variables

plot_roc <- function(predcol, outcol, colour_id=2, overlaid=F) {
  ROCit_obj <- rocit(score=predcol, class=outcol==pos)
  par(new=overlaid)
  plot(ROCit_obj, col = c(colour_id, 1),
       legend = FALSE, YIndex = FALSE, values = FALSE)}



## Multivariate models

### Decision Tree

tVars <- paste('pred', c(catVars, numericVars), sep='')
(fV <- paste(outcome,'> 0 ~ ',
             paste(tVars, collapse=' + '), sep=''))
tmodel <- rpart(fV, data = train_set)


### Performance Measures

logLikelihood <- function(ytrue, ypred, epsilon=1e-6) {
  sum(ifelse(ytrue, log(ypred+epsilon), log(1-ypred+epsilon)), na.rm=T)
}
performanceMeasures <- function(ytrue, ypred, model.name = "model", threshold=0.5) {
  # Compute the normalised deviance
  dev.norm <- -2 * logLikelihood(ytrue, ypred)/length(ypred)
  # Compute the confusion matrix
  cmat <- table(actual = ytrue, predicted = ypred >= threshold)
  accuracy <- sum(diag(cmat)) / sum(cmat)
  precision <- cmat[2, 2] / sum(cmat[, 2])
  recall <- cmat[2, 2] / sum(cmat[2, ])
  f1 <- 2 * precision * recall / (precision + recall)
  data.frame(model = model.name, precision = precision,
             recall = recall, f1 = f1, dev.norm = dev.norm)
}

### Pander Formatting

panderOpt <- function(){
  library(pander)
  # setting up Pander Options
  panderOptions("plain.ascii", TRUE)
  panderOptions("keep.trailing.zeros", TRUE)
  panderOptions("table.style", "simple")
}

pp_table = function(model, xtrain, ytrain, xtest, ytest, xcal, ycal, threshold=0.5){
  panderOpt()
  perf_justify = 'lrrrrr'
  
  pred_train = predict(model, newdata = xtrain)
  pred_test = predict(model, newdata = xtest)
  pred_cal = predict(model, newdata = xcal)
  
  train_df = performanceMeasures(ytrain,pred_train, model.name = 'training', threshold=threshold)
  cal_df = performanceMeasures(ycal,pred_cal, model.name = 'calibration', threshold=threshold)
  test_df = performanceMeasures(ytest,pred_test, model.name = 'test', threshold=threshold)
  
  perftable <- rbind(train_df, cal_df, test_df)
  pandoc.table(perftable)
}

pp_table(tmodel, train_set[tVars],train_set[,outcome]==pos,test_set[tVars],test_set[,outcome]==pos, calibration_set[tVars],calibration_set[,outcome]==pos)

### Plotting the AUC

plot_roc <- function(predcoltrain, outcoltrain, predcolcal, outcolcal,predcoltest, outcoltest){
  roc_train <- rocit(score=predcoltrain, class=outcoltrain==pos)
  roc_cal <- rocit(score=predcolcal, class=outcolcal==pos)
  roc_test <- rocit(score=predcoltest,class=outcoltest==pos)
  
  plot(roc_train, col = c("blue","green"), lwd = 3, legend = FALSE,YIndex = FALSE, values = TRUE, asp=1)
  lines(roc_cal$TPR ~ roc_cal$FPR, lwd = 3, col = c("red","green"), asp=1)
  lines(roc_test$TPR ~ roc_test$FPR, lwd = 3, col = c("purple","green"), asp=1)
  legend("bottomright", col = c("blue","red", "purple"),
         c("Training", "Calibration", "Test"), lwd = 2)
}
pred_test_roc <- predict(tmodel, newdata=test_set)
pred_train_roc <- predict(tmodel, newdata=train_set)
pred_cal_roc <- predict(tmodel, newdata = calibration_set)



### Using Selected Features

selVars

f <- paste(outcome,'>0 ~ ',
           paste(selVars, collapse=' + '), sep='')
tmodel2 <- rpart(f, data=train_set)
print(calcAUC(predict(tmodel2, newdata=train_set[selVars]), train_set[,outcome]))
print(calcAUC(predict(tmodel2, newdata=test_set[selVars]), test_set[,outcome]))
print(calcAUC(predict(tmodel2, newdata=calibration_set[selVars]), calibration_set[,outcome]))


pp_table(tmodel2, train_set[tVars],train_set[,outcome]==pos,test_set[tVars],test_set[,outcome]==pos,calibration_set[tVars],calibration_set[,outcome]==pos)

### Visualising a Decision Tree



### Plotting the ROC


pred_test_roc2 <- predict(tmodel2, newdata=test_set)
pred_train_roc2 <- predict(tmodel2, newdata=train_set)
pred_cal_roc2 <- predict(tmodel2, newdata = calibration_set)



## Classification Models (Logistic Regression model)

# Fitting logistic regression model
formula <- paste(outcome, '> 0 ~ ', paste(selVars, collapse=' + '), sep='')

model_logr <- glm(formula=formula, data=train_set, family=binomial(link="logit"))
train_set$pred <- predict(model_logr, newdata=train_set, type="response")
test_set$pred <- predict(model_logr, newdata=test_set, type="response")
calibration_set$pred <- predict(model_logr, newdata=calibration_set, type="response")


train_performance <- performanceMeasures(train_set[, outcome], train_set$pred, model.name = 'training', threshold = 0.5)
test_performance <- performanceMeasures(test_set[, outcome], test_set$pred, model.name = 'test', threshold = 0.5)
cal_performance <- performanceMeasures(calibration_set[, outcome], calibration_set$pred, model.name = 'calibration', threshold = 0.5)
performance_table <- rbind(train_performance, cal_performance, test_performance)
pandoc.table(performance_table)







## Feature selection using Fisher Score


# Calculate Fisher's Score
calculate_fisher_score <- function(feature, outcome) {
  positive_values <- feature[outcome == 1]
  negative_values <- feature[outcome == 0]
  
  # Check if the set of positive and negative examples is empty
  if (length(positive_values) == 0 || length(negative_values) == 0) {
    return(0)  # Returns 0 or other appropriate value, indicating that the feature is not relevant
  }
  
  mean_positive <- mean(positive_values)
  mean_negative <- mean(negative_values)
  var_positive <- var(positive_values)
  var_negative <- var(negative_values)
  
  # Check if the variance is zero and avoid having a zero denominator
  if (var_positive == 0 || var_negative == 0) {
    return(0)  # Returns 0 or other appropriate value, indicating that the feature is not relevant
  }
  
  fisher_score <- ((mean_positive - mean_negative)^2) / (var_positive + var_negative)
  return(fisher_score)
}

# select feature variables
features <- youtube[, c("subscribers", "video.views", "uploads", "created_year", "created_month", "Gross.tertiary.education.enrollment....", "Population", "Unemployment.rate", "Urban_population", "Latitude", "Longitude","earning_category")]

# Perform one hot encoding on factor columns
categorical_features <- youtube[, c("category", "Country", "channel_type")]
encoded_features <- model.matrix(~ . - 1, data = categorical_features)

# Merge numerical columns and factor columns after unique heat coding
all_features <- cbind(features, encoded_features)

# Calculate Fisher's Score for each feature
fisher_scores <- sapply(all_features, calculate_fisher_score, outcome = outcome)

# Sort in descending order by Fisher's Score
sorted_features <- names(sort(fisher_scores, decreasing = TRUE))

# Select the top N features as the most relevant features
N <- 7  
selected_features <- sorted_features[1:N]

print(selected_features)


### ROC Curves for selected categorical and numerical variables

plot_roc <- function(predcol, outcol, colour_id=2, overlaid=F) {
  ROCit_obj <- rocit(score=predcol, class=outcol==pos)
  par(new=overlaid)
  plot(ROCit_obj, col = c(colour_id, 1),
       legend = FALSE, YIndex = FALSE, values = FALSE)
}

### Decision Tree


fVars <- paste('pred', selected_features, sep='')
(fV <- paste(outcome,'> 0 ~ ',
             paste(fVars, collapse=' + '), sep=''))
tmodel3 <- rpart(fV, data = train_set)

# To inspect the model, type: summary (tmodel)
print(calcAUC(predict(tmodel3, newdata=train_set), train_set[,outcome]))
print(calcAUC(predict(tmodel3, newdata=test_set), test_set[,outcome]))
print(calcAUC(predict(tmodel3, newdata=calibration_set), calibration_set[,outcome]))




pp_table(tmodel3, train_set[fVars],train_set[,outcome]==pos,test_set[fVars],test_set[,outcome]==pos,calibration_set[tVars],calibration_set[,outcome]==pos)



### Visualising a Decision Tree



### Plotting the AUC


plot_roc <- function(predcoltrain, outcoltrain, predcolcal, outcolcal,predcoltest, outcoltest){
  roc_train <- rocit(score=predcoltrain, class=outcoltrain==pos)
  roc_cal <- rocit(score=predcolcal, class=outcolcal==pos)
  roc_test <- rocit(score=predcoltest,class=outcoltest==pos)
  
  plot(roc_train, col = c("blue","green"), lwd = 3, legend = FALSE,YIndex = FALSE, values = TRUE, asp=1)
  lines(roc_cal$TPR ~ roc_cal$FPR, lwd = 3, col = c("red","green"), asp=1)
  lines(roc_test$TPR ~ roc_test$FPR, lwd = 3, col = c("purple","green"), asp=1)
  legend("bottomright", col = c("blue","red", "purple"),
         c("Training", "Calibration", "Test"), lwd = 2)
}
pred_test_roc3 <- predict(tmodel3, newdata=test_set)
pred_train_roc3 <- predict(tmodel3, newdata=train_set)
pred_cal_roc3 <- predict(tmodel3, newdata = calibration_set)


### Logistic Regression Model


formula <- paste(outcome, '> 0 ~ ', paste(fVars, collapse=' + '), sep='')

model_logr <- glm(formula=formula, data=train_set, family=binomial(link="logit"))
train_set$pred2 <- predict(model_logr, newdata=train_set, type="response")
test_set$pred2 <- predict(model_logr, newdata=test_set, type="response")
calibration_set$pred2 <- predict(model_logr, newdata=calibration_set, type="response")

train_performance2 <- performanceMeasures(train_set[, outcome], train_set$pred2, model.name = 'training', threshold = 0.5)
test_performance2 <- performanceMeasures(test_set[, outcome], test_set$pred2, model.name = 'test', threshold = 0.5)
cal_performance2 <- performanceMeasures(calibration_set[, outcome], calibration_set$pred2, model.name = 'calibration', threshold = 0.5)

performance_table2 <- rbind(train_performance2, cal_performance2, test_performance2)

pandoc.table(performance_table2)



### Plotting the AUC






## Model Comparision




pp_table(tmodel2, train_set[tVars],train_set[,outcome]==pos,test_set[tVars],test_set[,outcome]==pos,calibration_set[tVars],calibration_set[,outcome]==pos)



pandoc.table(performance_table)



pp_table(tmodel3, train_set[tVars],train_set[,outcome]==pos,test_set[tVars],test_set[,outcome]==pos,calibration_set[tVars],calibration_set[,outcome]==pos)



pandoc.table(performance_table2)







# Part3: Clustering



youtube <- youtube[youtube$Country != "missing", ] # Delete the missing value
# Classify the country, keeping only one column for countries that appear multiple times, and replace the remaining variables with averages
youtube <- youtube %>%
  group_by(Country) %>%
  summarise(across(everything(), mean, na.rm = TRUE))
selected_features_df <- youtube[, selected_features]
scaled_df <- scale(selected_features_df)


### Finding K


# Function to return the squared Euclidean distance of two given points x and y
sqr_euDist <- function(x, y) {
  sum((x - y)^2)
}
# Function to calculate WSS of a cluster, represented as a n-by-d matrix
# (where n and d are the numbers of rows and columns of the matrix)
# which contains only points of the cluster.
wss <- function(clustermat) {
  c0 <- colMeans(clustermat)
  sum(apply( clustermat, 1, FUN=function(row) {sqr_euDist(row, c0)} ))
}

# the cluster ID (starting at 1) for each row of the data frame.
wss_total <- function(scaled_df, labels) {
  wss.sum <- 0
  k <- length(unique(labels))
  for (i in 1:k)
    wss.sum <- wss.sum + wss(subset(scaled_df, labels == i))
  wss.sum
}

# Function to calculate total sum of squared (TSS) distance of data
# points about the (global) mean. This is the same as WSS when the
# number of clusters (k) is 1.
tss <- function(scaled_df) {
  wss(scaled_df)
}

# Function to return the CH indices computed using hierarchical
# clustering (function `hclust`) or k-means clustering (`kmeans`)
# for a vector of k values ranging from 1 to kmax.
CH_index <- function(scaled_df, kmax, method="kmeans") {
  if (!(method %in% c("kmeans", "hclust")))
    stop("method must be one of c('kmeans', 'hclust')")
  npts <- nrow(scaled_df)
  wss.value <- numeric(kmax) # create a vector of numeric type
  # wss.value[1] stores the WSS value for k=1 (when all the
  # data points form 1 large cluster).
  wss.value[1] <- wss(scaled_df)
  if (method == "kmeans") {
    # kmeans
    for (k in 2:kmax) {
      clustering <- kmeans(scaled_df, k, nstart=10, iter.max=100)
      wss.value[k] <- clustering$tot.withinss
    }
  } else {
    # hclust
    d <- dist(scaled_df, method="euclidean")
    pfit <- hclust(d, method="ward.D2")
    for (k in 2:kmax) {
      labels <- cutree(pfit, k=k)
      wss.value[k] <- wss_total(scaled_df, labels)
    }
  }
  bss.value <- tss(scaled_df) - wss.value # This is a vector
  B <- bss.value / (0:(kmax-1)) # also a vector
  W <- wss.value / (npts - 1:kmax) # also a vector
  data.frame(k = 1:kmax, CH_index = B/W, WSS = wss.value)
}


### Plot the CH and WSS Index







## Hierarchical Clustering

d <- dist(scaled_df, method="euclidean")
pfit <- hclust(d, method="ward.D2") # perform hierarchical clustering




### Visualising Clusters - Data Preparation


groups <- cutree(pfit, k=6)

print_clusters <- function(df, groups, cols_to_print) {
  Ngroups <- max(groups)
  for (i in 1:Ngroups) {
    print(paste("cluster", i))
    print(df[groups == i, cols_to_print])
  }
}

princ <- prcomp(scaled_df)
nComp <- 2 
project2D <- as.data.frame(predict(princ, newdata=scaled_df)[,1:nComp])
hclust.project2D <- cbind(project2D, cluster=as.factor(groups), country=youtube$Country)
head(hclust.project2D)


### Visualising Clusters - Finding the Convex Hull


find_convex_hull <- function(proj2Ddf, groups) {
  do.call(rbind,
          lapply(unique(groups),
                 FUN = function(c) {
                   f <- subset(proj2Ddf, cluster==c);
                   f[chull(f),]
                 }
          )
  )
}
hclust.hull <- find_convex_hull(hclust.project2D, groups)


### Visualising Clusters





### Using Clusterboot




kbest.p <- 6
cboot.hclust <- clusterboot(scaled_df, clustermethod=hclustCBI,
                            method="ward.D2", k=kbest.p)
summary(cboot.hclust$result)



groups.cboot <- cboot.hclust$result$partition
print_clusters(youtube, groups.cboot, "Country")


### Finding the Stable Clusters


(values <- 1 - cboot.hclust$bootbrd/100) # large values here => highly stable
cat("So clusters", order(values)[5], "and", order(values)[4], "are highly stable")





# Define UI
ui <- fluidPage(
  titlePanel("Interactive Data Visualization"),
  
  sidebarLayout(
    # Sidebar with radio buttons for analysis type
    sidebarPanel(
      radioButtons("main_type", "Analysis Type:", 
                   choices = c("Classification", "Clustering"))
    ),
    
    mainPanel(
      conditionalPanel(
        # Conditional panel for 'Classification'
        condition = "input.main_type == 'Classification'",
        tabsetPanel(
          tabPanel("single-variate model",
                   tabsetPanel(
                    # Define panels for each density plot
                    tabPanel("Density Plot - predchannel_type", plotOutput("plot1")),
                    tabPanel("Density Plot - predcategory", plotOutput("plot2")),
                    tabPanel("Density Plot - predCountry", plotOutput("plot3")),
                    tabPanel("Density Plot - predsubscribers", plotOutput("plot4")),
                    tabPanel("Density Plot - predvideo.views", plotOutput("plot5")),
                    tabPanel("Density Plot - preduploads", plotOutput("plot6")),
                    tabPanel("Density Plot - predcreated_year", plotOutput("plot7")),
                    tabPanel("Density Plot - predcreated_month", plotOutput("plot8")),
                    tabPanel("Density Plot - predGross.tertiary.education.enrollment....", plotOutput("plot9")),
                    tabPanel("Density Plot - predPopulation", plotOutput("plot10")),
                    tabPanel("Density Plot - predUnemployment.rate", plotOutput("plot11")),
                    tabPanel("Density Plot - predUrban_population", plotOutput("plot12")),
                    tabPanel("Density Plot - predLatitude", plotOutput("plot13")),
                    tabPanel("Density Plot - predLongitude", plotOutput("plot14"))
                   )
          ),
          tabPanel("multi-variate model",
                   tabsetPanel(
                     # Define panels for ROC plots
                     tabPanel("log Likelihood model", 
                          tabsetPanel(
                              tabPanel("ROC - Decision Tree", plotOutput("roc_dt")),
                              tabPanel("ROC - Logistic Regression", plotOutput("roc_lr"))
                       )
            ),
            tabPanel("Fisher Score model", 
              tabsetPanel(
                tabPanel("ROC - Decision Tree", plotOutput("roc_fisher_dt")),
                tabPanel("ROC - Logistic Regression", plotOutput("roc_fisher_lr"))
              )))
            )
          )
         ),
      
         # Conditional panel for 'Clustering'
         conditionalPanel(
           condition = "input.main_type == 'Clustering'",
           tabsetPanel(
             # Define panels for dendrogram and clusters
             tabPanel("Dendrogram", plotOutput("dendrogram_plot")),
             tabPanel("Clusters", plotOutput("clusters_plot"))
           )
         )
        )
      )
    )
  

# Define server logic
server <- function(input, output) {
  # Define reactive expressions and generate plots here
  output$plot1 <- renderPlot({
    fig1 <- ggplot(calibration_set) + geom_density(aes(x=predchannel_type, color=as.factor(earning_category)))
    print(fig1)
  })
  
  output$plot2 <- renderPlot({
    fig2 <- ggplot(calibration_set) + geom_density(aes(x=predcategory, color=as.factor(earning_category)))
    print(fig2)
  })
  
  output$plot3 <- renderPlot({
    fig3 <- ggplot(calibration_set) + geom_density(aes(x=predCountry, color=as.factor(earning_category)))
    print(fig3)
  })
  
  output$plot4 <- renderPlot({
    fig4 <- ggplot(calibration_set) + geom_density(aes(x=predsubscribers, color=as.factor(earning_category)))
    print(fig4)
  })
  
  output$plot5 <- renderPlot({
    fig5 <- ggplot(calibration_set) + geom_density(aes(x=predvideo.views, color=as.factor(earning_category)))
    print(fig5)
  })
  
  output$plot6 <- renderPlot({
    fig6 <- ggplot(calibration_set) + geom_density(aes(x=preduploads, color=as.factor(earning_category)))
    print(fig6)
  })
  
  output$plot7 <- renderPlot({
    fig7 <- ggplot(calibration_set) + geom_density(aes(x=predcreated_year, color=as.factor(earning_category)))
    print(fig7)
  })
  
  output$plot8 <- renderPlot({
    fig8 <- ggplot(calibration_set) + geom_density(aes(x=predcreated_month, color=as.factor(earning_category)))
    print(fig8)
  })
  
  output$plot9 <- renderPlot({
    fig9 <- ggplot(calibration_set) + geom_density(aes(x=predGross.tertiary.education.enrollment...., color=as.factor(earning_category)))
    print(fig9)
  })
  
  output$plot10 <- renderPlot({
    fig10 <- ggplot(calibration_set) + geom_density(aes(x=predPopulation, color=as.factor(earning_category)))
    print(fig10)
  })
  
  output$plot11 <- renderPlot({
    fig11 <- ggplot(calibration_set) + geom_density(aes(x=predUnemployment.rate, color=as.factor(earning_category)))
    print(fig11)
  })
  
  output$plot12 <- renderPlot({
    fig12 <- ggplot(calibration_set) + geom_density(aes(x=predUrban_population, color=as.factor(earning_category)))
    print(fig12)
  })
  
  output$plot13 <- renderPlot({
    fig13 <- ggplot(calibration_set) + geom_density(aes(x=predLatitude, color=as.factor(earning_category)))
    print(fig13)
  })
  
  output$plot14 <- renderPlot({
    fig14 <- ggplot(calibration_set) + geom_density(aes(x=predLongitude, color=as.factor(earning_category)))
    print(fig14)
  })
  
  output$roc_dt <- renderPlot({
    plot_roc(pred_train_roc2, train_set[[outcome]], pred_cal_roc2, calibration_set[[outcome]], pred_test_roc2, test_set[[outcome]])
  })
  
  output$roc_lr <- renderPlot({
    plot_roc(
      train_set$pred, train_set[[outcome]], calibration_set$pred, calibration_set[[outcome]], test_set$pred, test_set[[outcome]])
  })
  
  output$roc_fisher_dt <- renderPlot({
    plot_roc(
      pred_train_roc3, train_set[[outcome]], pred_cal_roc3, calibration_set[[outcome]], pred_test_roc3, test_set[[outcome]])
  })
  
  output$roc_fisher_lr <- renderPlot({
    plot_roc(
      train_set$pred2, train_set[[outcome]], calibration_set$pred2, calibration_set[[outcome]], test_set$pred2, test_set[[outcome]])
  })
  
  output$dendrogram_plot <- renderPlot({
    plot(pfit, labels=youtube$Country, main="Cluster Dendrogram for Country")
    rect.hclust(pfit, k=6)
  })
  
  output$clusters_plot <- renderPlot({
    ggplot(hclust.project2D, aes(x=PC1, y=PC2)) +
      geom_point(aes(shape=as.factor(cluster), color=cluster)) +
      scale_shape_manual(values = c(1, 2, 3, 4, 5, 6, 7)) +
      geom_polygon(data=hclust.hull, aes(group=cluster, fill=as.factor(cluster)),
                   alpha=0.4, linetype=0) +
      theme(text=element_text(size=10))
  })
  
}

# Run the Shiny app
shinyApp(ui, server)


