if("ggplot2" %in% rownames(installed.packages()) == FALSE)  {install.packages("ggplot2")}
if("gmodels" %in% rownames(installed.packages()) == FALSE)  {install.packages("gmodels")}
if("e1071" %in% rownames(installed.packages()) == FALSE)  {install.packages("e1071")}
if("Amelia" %in% rownames(installed.packages()) == FALSE)  {install.packages("Amelia")}
if("caret" %in% rownames(installed.packages()) == FALSE)  {install.packages("caret")}
if("ROCR" %in% rownames(installed.packages()) == FALSE)  {install.packages("ROCR")}


require(ggplot2)
require(gmodels)
require(e1071)
require(Amelia)
require(caret)
require(ROCR)

## --- Step 1: Get the data ----

### SELECT SPECIFIC FILE IN SPECIFIC LOCATION FOR DATA INPUT
setwd("/Users/kevinfinney/Dropbox/'BELLARMINE/'MSA/`SUMMER-16/Assignments/2-Classifier/")
# setwd("C:\Users\bgubser\OneDrive\Class Files\Data Mining - Summer 2015\Assignments")
# setwd('C:\\Users\\bgubser\\OneDrive\\Class Files\\Data Mining - Summer 2015\\Assingments')

raw.wbcd <- read.table("BreastCancerData.txt"
                       , header = TRUE
                       , sep = ","
                       , na.strings = c("NaN", "NA"))

## ---- Step 2: Exploring and preparing the data ----

### Only work with some columns with string "_Mean" in column name and Diagnosis, DROP ALL OTHER COLUMNS
wbcd.data <- raw.wbcd[,colnames(raw.wbcd[c(grep("Diag", colnames(raw.wbcd)), grep("_Mean", colnames(raw.wbcd)) )])]
colnames(wbcd.data)

# Look at the summary, identify any abnormalities, like NA's
summary(wbcd.data) # summary shows 44 NA's in Texture_Mean column
wbcd.data[!complete.cases(wbcd.data),] #shows me the records that have NA's
sapply(wbcd.data, function(x) sum(is.na(x))) #shows me by column, where the NA's are
missmap(wbcd.data, main = "Missing Values vs. observed") #Visualize the missing data (NA's)

### Visualize data with Boxplots and Histograms
# Check out boxplots to see the rest of the variables
par.default <- par()
par(mfrow=c(1, 1), mar = c(5,10,2,2))
boxplot(wbcd.data[,2:11], horizontal = TRUE, main = "Breast Cancer Data", las = 1)
### Boxplot shows large range, only a couple variables even show up

#Check out histograms of variables to see what else we can identify

par(mfrow=c(4, 3))
column.names <- dimnames(wbcd.data)[[2]]
for (i in 2:11) {
  hist(wbcd.data[,i]
       , xlim=c(0, max(wbcd.data[,i], na.rm = TRUE))
       , breaks= 10
       , main=column.names[i]
       , probability=TRUE
       , col="gray"
       , border="white")
  d <- density(wbcd.data[,i], na.rm = TRUE)
  lines(d, col="red")
}
par(par.default)
### This shows many different distributions to work with

########## Radius_Mean ##########
# relatively normal looking
ggplot(data = wbcd.data, aes(log(Radius_Mean), ..density..)) +
  geom_histogram( fill = "blue", bins = 20) +
  geom_density(fill = "cornsilk", color = "red", alpha = .5)

#Check out the distribution of values by 20% buckets - 5 buckets
quantile(wbcd.data$Radius_Mean, probs = seq(0,1,.2))

# set breaks approximately equal to vaules from the quantile check
wbcd.data$RadM.cat <- cut(wbcd.data$Radius_Mean
                          , breaks = c(floor(min(wbcd.data$Radius_Mean) - 1)
                                       , 11.5, 13, 14.5, 17.5
                                       , ceiling(max(wbcd.data$Radius_Mean) + 1))
                          , right = TRUE)

# look at the number of records and proportion of the categorization
CrossTable(wbcd.data$RadM.cat, max.width = 3)


########## Texture_Mean ##########
# We know there are NA's from our previous inspection, look again at only this variable
summary(wbcd.data$Texture_Mean) ## 44 NA's

# Look at the shape of the values that can be reviewed with histogram & density curve
ggplot(data = wbcd.data, aes(Texture_Mean, ..density..)) +
  geom_histogram( fill = "green", bins = 20) +
  geom_density(fill = "cornsilk", color = "red", alpha = .5)

# Remove the NA's, and replace them with the mean of the other variables
# Find all the NA's, create an index vector, and apply the mean to the values in those indexes
wbcd.data$Texture_Mean[which(is.na(wbcd.data$Texture_Mean))] <- mean(wbcd.data$Texture_Mean, na.rm = TRUE)
# Verify no more NA's
summary(wbcd.data$Texture_Mean)
wbcd.data$Texture_Mean[!complete.cases(wbcd.data$Texture_Mean)]

## Look at the shape now
# conversion of all the NA's to means has inflated the presence of the mean value
ggplot(data = wbcd.data, aes(Texture_Mean, ..density..)) +
  geom_histogram( fill = "green", bins = 20) +
  geom_density(fill = "cornsilk", color = "red", alpha = .5)
# shape is good enough, to proceed to categorization

# Check out the distribution of values by 20% buckets - 5 buckets
quantile(wbcd.data$Texture_Mean, probs = seq(0,1,.2))

# set breaks approximately equal to vaules from the quantile check
wbcd.data$TexM.cat <- cut(wbcd.data$Texture_Mean
                          , breaks = c(floor(min(wbcd.data$Texture_Mean) - 1)
                                       , 16, 18.5, 19.5, 22.5
                                       , ceiling(max(wbcd.data$Texture_Mean) + 1))
                          , right = TRUE)

# look at the number of records and proportion of the categorization
CrossTable(wbcd.data$TexM.cat, max.width = 3)

########## Perimeter_Mean ##########
# clearly has bad data input as "999" - must fix, otherwise normal
# the one extreme data point was identified in boxplot earlier
ggplot(data = wbcd.data, aes(Perimeter_Mean, ..density..)) +
  geom_histogram( fill = "orange", bins = 20) +
  geom_density(fill = "cornsilk", color = "red", alpha = .5)

# We are going to replace the 999's with mean values of the column
mean(wbcd.data$Perimeter_Mean) # This value is inflated due to the 999's
mean(wbcd.data$Perimeter_Mean[wbcd.data$Perimeter_Mean < 200]) # This is replacement value

# Run the replacement command, "200" as breakpoint was derived from histogram review
wbcd.data$Perimeter_Mean[wbcd.data$Perimeter_Mean > 200] <-
  mean(wbcd.data$Perimeter_Mean[wbcd.data$Perimeter_Mean < 200])

# Look at shape again
ggplot(data = wbcd.data, aes(Perimeter_Mean, ..density..)) +
  geom_histogram( fill = "Purple", bins = 20) +
  geom_density(fill = "cornsilk", color = "red", alpha = .5)
# Looks good, proceed to categorization

# Check out the distribution of values by 20% buckets - 5 buckets
quantile(wbcd.data$Perimeter_Mean, probs = seq(0,1,.2))

# set breaks approximately equal to vaules from the quantile check
wbcd.data$PerM.cat <- cut(wbcd.data$Perimeter_Mean
                          , breaks = c(floor(min(wbcd.data$Perimeter_Mean) - 1)
                                       , 73, 82.5, 92, 111.5
                                       , ceiling(max(wbcd.data$Perimeter_Mean) + 1))
                          , right = TRUE)

# look at the number of records and proportion of the categorization
CrossTable(wbcd.data$PerM.cat, max.width = 3)


########## Area_Mean ##########
# skew right, review extreme observations, very large range, do transformation
ggplot(data = wbcd.data, aes(Area_Mean, ..density..)) +
  geom_histogram( fill = "dark red", bins = 20) +
  geom_density(fill = "cornsilk", color = "red", alpha = .5)

# Look at histogram if variable is transformed using log
ggplot(data = wbcd.data, aes(log(Area_Mean), ..density..)) +
  geom_histogram( fill = "blue", bins = 20) +
  geom_density(fill = "cornsilk", color = "red", alpha = .5)
# I like this better, we'll stick with this, fairly normal-ish

# Check out the distribution of values by 20% buckets - 5 buckets
quantile(log(wbcd.data$Area_Mean), probs = seq(0,1,.2))

# set breaks approximately equal to vaules from the quantile check
wbcd.data$AreaM.cat <- cut(log(wbcd.data$Area_Mean)
                          , breaks = c(floor(min(log(wbcd.data$Area_Mean)) - 1)
                                       , 6, 6.2, 6.4, 6.8
                                       , ceiling(max(log(wbcd.data$Area_Mean)) + 1))
                          , right = TRUE)

# look at the number of records and proportion of the categorization
CrossTable(wbcd.data$AreaM.cat, max.width = 3)


########## Smoothness_Mean ##########
# clearly has bad data observations at "999", fix somehow
# the one extreme data point was identified in boxplot earlier

# Check out the histogram & Density curve
ggplot(data = wbcd.data, aes(Smoothness_Mean)) +
  geom_histogram( fill = "violet", bins = 20) +
  geom_density(fill = "cornsilk", color = "red", alpha = .5)

# Check out the boxplot
ggplot(data = wbcd.data, aes(x = factor(0), y = Smoothness_Mean)) +
  geom_boxplot() + coord_flip()

# We are going to replace the 999's with mean values of the column
mean(wbcd.data$Smoothness_Mean) # This value is inflated due to the 999's
mean(wbcd.data$Smoothness_Mean[wbcd.data$Smoothness_Mean < 1]) # This is replacement value

# Run the replacement command, "200" as breakpoint was derived from histogram review
wbcd.data$Smoothness_Mean[wbcd.data$Smoothness_Mean > 1] <-
  mean(wbcd.data$Smoothness_Mean[wbcd.data$Smoothness_Mean < 1])

## RE-Visualize
# Histogram
ggplot(data = wbcd.data, aes(Smoothness_Mean)) +
  geom_histogram( fill = "blue", bins = 20) +
  geom_density(fill = "cornsilk", color = "red", alpha = .5)

# Boxplot
ggplot(data = wbcd.data, aes(x = factor(0), y = Smoothness_Mean)) +
  geom_boxplot() + coord_flip()
# Looks good, proceed to categorize

# Check out the distribution of values by 20% buckets - 5 buckets
quantile(wbcd.data$Smoothness_Mean, probs = seq(0,1,.2))

# set breaks approximately equal to vaules from the quantile check
wbcd.data$SmoM.cat <- cut(wbcd.data$Smoothness_Mean
                           , breaks = c(floor(min(wbcd.data$Smoothness_Mean) - 1)
                                        , .084, .092, .099, .107
                                        , ceiling(max(wbcd.data$Smoothness_Mean) + 1))
                           , right = TRUE)

# look at the number of records and proportion of the categorization
CrossTable(wbcd.data$SmoM.cat, max.width = 3)


########## Compactness_Mean ##########
# somewhat skew right - reveiw extreme observations for correctness
ggplot(data = wbcd.data, aes(Compactness_Mean, ..density..)) +
  geom_histogram( fill = "green", bins = 20) +
  geom_density(fill = "cornsilk", color = "red", alpha = .5)
# data is not very spread out, we'll handle skewness by calculating bin size

# Check out the distribution of values by 20% buckets - 5 buckets
quantile(wbcd.data$Compactness_Mean, probs = seq(0,1,.2))

# set breaks approximately equal to vaules from the quantile check
wbcd.data$ComM.cat <- cut(wbcd.data$Compactness_Mean
                          , breaks = c(floor(min(wbcd.data$Compactness_Mean) - 1)
                                       , .06, .08, .11, .15
                                       , ceiling(max(wbcd.data$Compactness_Mean) + 1))
                          , right = TRUE)

# look at the number of records and proportion of the categorization
CrossTable(wbcd.data$ComM.cat, max.width = 3)


########## Concavity_Mean ##########
# skew right - look at extreme observations, possibly transform
ggplot(data = wbcd.data, aes(Concavity_Mean, ..density..)) +
  geom_histogram( fill = "purple", bins = 20) +
  geom_density(fill = "cornsilk", color = "red", alpha = .5)

# Check out the distribution of values by 20% buckets - 5 buckets
quantile(wbcd.data$Concavity_Mean, probs = seq(0,1,.2))

# set breaks approximately equal to vaules from the quantile check
wbcd.data$ConM.cat <- cut(wbcd.data$Concavity_Mean
                          , breaks = c(floor(min(wbcd.data$Concavity_Mean) - 1)
                                       , .025, .045, .086, .15
                                       , ceiling(max(wbcd.data$Concavity_Mean) + 1))
                          , right = TRUE)

# look at the number of records and proportion of the categorization
CrossTable(wbcd.data$ConM.cat, max.width = 3)

########## Concave_pts_Mean ##########
# skew right - look at extreme observations - possibly transform
ggplot(data = wbcd.data, aes(Concave_pts_Mean, ..density..)) +
  geom_histogram( fill = "blue", bins = 20) +
  geom_density(fill = "cornsilk", color = "red", alpha = .5)

# Check out the distribution of values by 20% buckets - 5 buckets
quantile(wbcd.data$Concave_pts_Mean, probs = seq(0,1,.2))

# set breaks approximately equal to vaules from the quantile check
wbcd.data$CnpM.cat <- cut(wbcd.data$Concave_pts_Mean
                          , breaks = c(floor(min(wbcd.data$Concave_pts_Mean) - 1)
                                       , .017, .027, .048, .084
                                       , ceiling(max(wbcd.data$Concave_pts_Mean) + 1))
                          , right = TRUE)

# look at the number of records and proportion of the categorization
CrossTable(wbcd.data$CnpM.cat, max.width = 3)



########## Symmetry_Mean ##########
# looks pretty good - maybe a couple extreme samples
ggplot(data = wbcd.data, aes(Symmetry_Mean, ..density..)) +
  geom_histogram( fill = "blue", bins = 20) +
  geom_density(fill = "cornsilk", color = "red", alpha = .5)

# Check out the distribution of values by 20% buckets - 5 buckets
quantile(wbcd.data$Symmetry_Mean, probs = seq(0,1,.2))

# set breaks approximately equal to vaules from the quantile check
wbcd.data$SymM.cat <- cut(wbcd.data$Symmetry_Mean
                          , breaks = c(floor(min(wbcd.data$Symmetry_Mean) - 1)
                                       , .158, .172, .185, .20
                                       , ceiling(max(wbcd.data$Symmetry_Mean) + 1))
                          , right = TRUE)

# look at the number of records and proportion of the categorization
CrossTable(wbcd.data$SymM.cat, max.width = 3)


########## Fractal_dim_mean ##########
# somewhat skew right, may need transformation
ggplot(data = wbcd.data, aes(Fractal_dim_Mean, ..density..)) +
  geom_histogram( fill = "blue", bins = 20) +
  geom_density(fill = "cornsilk", color = "red", alpha = .5)

# Check out the distribution of values by 20% buckets - 5 buckets
quantile(wbcd.data$Fractal_dim_Mean, probs = seq(0,1,.2))

# set breaks approximately equal to vaules from the quantile check
wbcd.data$FraM.cat <- cut(wbcd.data$Fractal_dim_Mean
                          , breaks = c(floor(min(wbcd.data$Fractal_dim_Mean) - 1)
                                       , .056, .06, .063, .067
                                       , ceiling(max(wbcd.data$Fractal_dim_Mean) + 1))
                          , right = TRUE)

# look at the number of records and proportion of the categorization
CrossTable(wbcd.data$FraM.cat, max.width = 3)


### Another visualization look at the raw data, after our fixes
par(mfrow=c(4, 3), mar = c(5,10,2,2))
column.names <- dimnames(wbcd.data)[[2]]
for (i in 2:11) {
  hist(wbcd.data[,i]
       , xlim=c(0, max(wbcd.data[,i], na.rm = TRUE))
       , breaks= 10
       , main=column.names[i]
       , probability=TRUE
       , col="gray"
       , border="white")
  d <- density(wbcd.data[,i], na.rm = TRUE)
  lines(d, col="red")
}
par(par.default)
# Looks like the data shape is solid - no more NA's or 999's

# Create a column to break data into training and testing
set.seed(852369741)  #For replication purposes
rand <- rbinom(nrow(wbcd.data), 1, .7)

#Identify training and test cases:
wbcd.data$seg <- "test"
wbcd.data$seg[rand==1] <- "train"
CrossTable(wbcd.data$seg)
wbcd.data$seg <- as.factor(wbcd.data$seg)

#Create data frame "train.data" and "test.data"- not necessary but makes coding a bit easier
train.wbcd.data <- wbcd.data[wbcd.data$seg =="train",]
train.wbcd.data$seg <- NULL  #field 'seg' is no longer needed
train.wbcd.data[grep("_Mean", colnames(train.wbcd.data))] <- list(NULL) #remove quantitative columns
names(train.wbcd.data)

test.wbcd.data <- wbcd.data[wbcd.data$seg =="test",]
test.wbcd.data$seg <- NULL #field 'seg' is no longer needed
test.wbcd.data[grep("_Mean", colnames(test.wbcd.data))] <- list(NULL) #remove quantitative columns
names(test.wbcd.data)

# Save everything to this point
save(wbcd.data, file="WBCD.Data.RData")
# Load if we wish to start from here
# load(file="IncomeAnalysisData.RData")

## ---- Step 3: Training a model on the data ----
model.train.wbcd <- naiveBayes(Diagnosis ~ ., data=train.wbcd.data)
pred.train.wbcd <- predict(model.train.wbcd, train.wbcd.data)

# Verify prediciton equal to data
length(pred.train.wbcd)==length(train.wbcd.data$Diagnosis)

# Check out the table of prediction versus data
CrossTable(pred.train.wbcd, train.wbcd.data$Diagnosis,
           prop.chisq = FALSE, prop.t = T, prop.r = FALSE,
           dnn = c("Predicted", "Actual"))
# Using the 'caret' package, create Confusion Matrix, and Sensitivity & Specificity Values
# Be sure to define the positive outcome as Diagnosis identified as Malignant
confusionMatrix(pred.train.wbcd, train.wbcd.data$Diagnosis, positive = "M")
sensitivity(pred.train.wbcd, train.wbcd.data$Diagnosis, positive = "M")
specificity(pred.train.wbcd, train.wbcd.data$Diagnosis, negative = "B")


## ---- Step 4: Evaluating model performance ----
pred.test.wbcd <- predict(model.train.wbcd, test.wbcd.data)

# Reveiw the predictions on the test data to see how close it matches training
CrossTable(pred.test.wbcd, test.wbcd.data$Diagnosis,
           prop.chisq = FALSE, prop.t = T, prop.r = FALSE,
           dnn = c("Predicted", "Actual"))
# Reveiw the Confusion Matrix, Sensitivity, & Specificity
confusionMatrix(pred.test.wbcd, test.wbcd.data$Diagnosis, positive = "M")
sensitivity(pred.test.wbcd, test.wbcd.data$Diagnosis, positive = "M")
specificity(pred.test.wbcd, test.wbcd.data$Diagnosis, negative = "B")


######### Create classification table, and ROC curves
thresh <- seq(from = 0.01, to = .99, by = 0.01)
results.df <- data.frame(pred.prob = numeric(length(train.wbcd.data$Diagnosis)))
results.df$pred.prob <- predict(model.train.wbcd, train.wbcd.data, type = "raw")[,1]
results.df$actual.diag <- factor(pred.train.wbcd)


class.bayes.df <- data.frame(cutoff = numeric(length(cutoff)),
                       true.neg = numeric(length(cutoff)),
                       true.pos = numeric(length(cutoff)),
                       false.neg = numeric(length(cutoff)),
                       false.pos = numeric(length(cutoff)),
                       misclass.rate = numeric(length(cutoff)),
                       sensitivity = numeric(length(cutoff)),
                       specificity = numeric(length(cutoff)) )

# levels(train.wbcd.lreg$Diagnosis)  command tells us which is category is 0 and which category is 1
for (i in 1:length(cutoff)){
  print(paste(i, "of", length(cutoff)))
  results.df$pred.diag[1:length(results.df$pred.prob)] <- "B"
  results.df$pred.diag[results.df$pred.prob < cutoff[i]] <- "M"
  confmat <- with(results.df, table(actual.diag, pred.diag))
  class.bayes.df[i,] <- 0
  class.bayes.df$cutoff[i] <- cutoff[i]
  if("B" %in% rownames(confmat)){
    class.bayes.df$true.neg[i] <- confmat["B", c(levels(train.wbcd.data$Diagnosis)[1])]
    class.bayes.df$false.pos[i] <- confmat["B", c(levels(train.wbcd.data$Diagnosis)[2])]
  }
  if("M" %in% rownames(confmat)){
    class.bayes.df$true.pos[i] <- confmat["M", c(levels(train.wbcd.data$Diagnosis)[2])]
    class.bayes.df$false.neg[i] <- confmat["M",c(levels(train.wbcd.data$Diagnosis)[1])]
  }
}

head(class.bayes.df)
class.bayes.df$misclass.rate <-
  with(class.bayes.df, (false.neg + false.pos)/ nrow(train.wbcd.data) )
class.bayes.df$sensitivity   <-
  with(class.bayes.df, true.pos / (true.pos + false.neg))
class.bayes.df$specificity   <-
  with(class.bayes.df, true.neg / (true.neg + false.pos))

ggplot(class.bayes.df, aes(x = 1-specificity, y = sensitivity)) +
  geom_point() + xlim(0,1) + ylim(0,1)
