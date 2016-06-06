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

# Create dataframe to work with from the raw data, eliminate all columns except Diagnosis & Means
wbcd.lreg <- raw.wbcd[,colnames(raw.wbcd[c(grep("Diag", colnames(raw.wbcd)), grep("_Mean", colnames(raw.wbcd)) )])]
summary(wbcd.lreg) # shows we have to handle the NA's & "999" values

# Handle imputations with Mean method
wbcd.lreg$Texture_Mean[is.na(wbcd.lreg$Texture_Mean)] <- mean(wbcd.lreg$Texture_Mean, na.rm = TRUE)
wbcd.lreg$Perimeter_Mean[wbcd.lreg.Perimeter_Mean > 200] <- mean(wbcd.lreg$Perimeter_Mean[wbcd.lreg$Perimeter_Mean < 200])
wbcd.lreg$Smoothness_Mean[wbcd.lreg$Smoothness_Mean > 1] <- mean(wbcd.lreg$Smoothness_Mean[wbcd.lreg$Smoothness_Mean < 1])
summary(wbcd.lreg)


# Create a column to break data into training and testing
set.seed(852369741)  #For replication purposes
wbcd.lreg$seg <- rbinom(nrow(wbcd.lreg), 1, .7)
wbcd.lreg$seg[wbcd.lreg$seg == 1] <- "train"
wbcd.lreg$seg[wbcd.lreg$seg == 0] <- "test"
wbcd.lreg$seg <- as.factor(wbcd.lreg$seg)

#Create data frame "train.wbcd.lreg" and "test.data"- not necessary but makes coding a bit easier
train.wbcd.lreg <- wbcd.lreg[wbcd.lreg$seg == "train",]
train.wbcd.lreg$seg <- NULL  #field 'seg' is no longer needed
test.wbcd.lreg <- wbcd.lreg[wbcd.lreg$seg == "test",]
test.wbcd.lreg$seg <- NULL #field 'seg' is no longer needed

# Save everything to this point
save(raw.wbcd,wbcd.lreg, train.wbcd.lreg, test.wbcd.lreg, file = "WBCD.LogReg.RData")
# Load if we wish to start from here
# load(file="WBCD.LogReg.RData")

model.wbcd.lreg <- glm(formula = Diagnosis ~ ., family = binomial(link = logit), data = train.wbcd.lreg)
# Look at model results, see what we can identify
summary(model.wbcd.lreg)
anova(model.wbcd.lreg, test = "Chisq")

pred.train.wbcd.lreg <- predict(model.wbcd.lreg, train.wbcd.lreg, type = "response")

#confusionMatrix(pred.train.wbcd.lreg, train.wbcd.lreg$Diagnosis)

######### Create classification table, and ROC curves
train.wbcd.lreg$pred.prob <- pred.train.wbcd.lreg

cutoff <- seq(from = 0.01, to = .99, by = 0.01)
class.df <- data.frame(cutoff = numeric(length(cutoff)),
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
  train.wbcd.lreg$pred.class[1:length(model.wbcd.lreg$fitted.values)] <- "B"
  train.wbcd.lreg$pred.class[model.wbcd.lreg$fitted.values > cutoff[i]] <- "M"
  ct <- with(train.wbcd.lreg, table(pred.class,Diagnosis))
  class.df[i,] <- 0
  class.df$cutoff[i] <- cutoff[i]
  if("B" %in% rownames(ct)){
    class.df$true.neg[i] <- ct["B", c(levels(train.wbcd.lreg$Diagnosis)[1])]
    class.df$false.pos[i] <- ct["B", c(levels(train.wbcd.lreg$Diagnosis)[2])]
  }
  if("M" %in% rownames(ct)){
    class.df$true.pos[i] <- ct["M", c(levels(train.wbcd.lreg$Diagnosis)[2])]
    class.df$false.neg[i] <- ct["M",c(levels(train.wbcd.lreg$Diagnosis)[1])]
  }
}

head(class.df)
class.df$misclass.rate <-
  with(class.df, (false.neg + false.pos)/ nrow(train.wbcd.lreg) )
class.df$sensitivity   <-
  with(class.df, true.pos / (true.pos + false.neg))
class.df$specificity   <-
  with(class.df, true.neg / (true.neg + false.pos))


# Identify which cutoff Maximizes Specificity and Sensitivity
best.cutoff <- class.df[which(class.df$misclass.rate == min(class.df$misclass.rate)), "cutoff"]
1- class.df$specificity[which(class.df$misclass.rate == min(class.df$misclass.rate))]
max(1 - class.df$sensitivity + class.df$specificity)

# goal is to be in the upper left corner where sensitivity and specificity are both large

ggplot(data=class.df, aes(x = 1-specificity, y=sensitivity)) + geom_point() +
  geom_vline(xintercept = 1- class.df$specificity[which(class.df$misclass.rate == min(class.df$misclass.rate))])


#########-------Test the "test" data for model accuracy
pred.test.lreg <- predict(model.wbcd.lreg, test.wbcd.lreg, type = "response")

test.wbcd.lreg$pred.prob <- pred.test.lreg
test.wbcd.lreg$pred.class <- ""

test.wbcd.lreg$pred.class[1:length(pred.test.lreg)] <- "B"
test.wbcd.lreg$pred.class[test.wbcd.lreg$pred.prob > best.cutoff[1]] <- "M"

# Compare the training set, and the testing set
# reset the training data set to the best cutoff calculated point
train.wbcd.lreg$pred.class[1:length(pred.train.wbcd)] <- "B"
train.wbcd.lreg$pred.class[train.wbcd.lreg$pred.prob > best.cutoff[1]] <- "M"


confusionMatrix(train.wbcd.lreg$Diagnosis, train.wbcd.lreg$pred.class, positive = "M")
confusionMatrix(test.wbcd.lreg$Diagnosis, test.wbcd.lreg$pred.class, positive = "M")

CrossTable(train.wbcd.lreg$Diagnosis, train.wbcd.lreg$pred.class,
           prop.r = F, prop.c = F, prop.chisq = F, dnn = c("actual diag", "predicted diag"))
CrossTable(test.wbcd.lreg$Diagnosis, test.wbcd.lreg$pred.class,
           prop.r = F, prop.c = F, prop.chisq = F, dnn = c("actual diag", "predicted diag"))
