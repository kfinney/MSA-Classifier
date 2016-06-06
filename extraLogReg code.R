## ---- Everything below is discarded code, left in place for future conversastion ----

### Only work with some columns with string "_Mean" in column name and Diagnosis, DROP ALL OTHER COLUMNS
wbcd.lreg.2 <- raw.wbcd[,colnames(raw.wbcd[c(grep("Diag", colnames(raw.wbcd)), grep("_Mean", colnames(raw.wbcd)) )])]
colnames(wbcd.lreg.2)

# Look at the summary, identify any abnormalities, like NA's
summary(wbcd.lreg.2) # summary shows 44 NA's in Texture_Mean column
wbcd.lreg.2[!complete.cases(wbcd.lreg.2),] #shows me the records that have NA's
sapply(wbcd.lreg.2, function(x) sum(is.na(x))) #shows me by column, where the NA's are
missmap(wbcd.lreg.2, main = "Missing Values vs. observed") #Visualize the missing data (NA's)

### This shows many different distributions to work with

########## Radius_Mean ##########
quantile(wbcd.lreg.2$Radius_Mean, probs = seq(0,1,.2))
wbcd.lreg.2$RadM.cat <- cut(wbcd.lreg.2$Radius_Mean, breaks = c(floor(min(wbcd.lreg.2$Radius_Mean) - 1)
                                                                , 11.5, 13, 14.5, 17.5, ceiling(max(wbcd.lreg.2$Radius_Mean) + 1)), right = TRUE)

########## Texture_Mean ##########
# Find all the NA's, and apply the mean to the values in those indexes
wbcd.lreg.2$Texture_Mean[is.na(wbcd.lreg.2$Texture_Mean)] <- mean(wbcd.lreg.2$Texture_Mean, na.rm = TRUE)
summary(wbcd.lreg.2$Texture_Mean) # Verify no more NA's

# Check out the distribution of values by 20% buckets - 5 buckets
quantile(wbcd.lreg.2$Texture_Mean, probs = seq(0,1,.2))
wbcd.lreg.2$TexM.cat <- cut(wbcd.lreg.2$Texture_Mean, breaks = c(floor(min(wbcd.lreg.2$Texture_Mean) - 1)
                                                                 , 16, 18.5, 19.5, 22.5, ceiling(max(wbcd.lreg.2$Texture_Mean) + 1)), right = TRUE)

########## Perimeter_Mean ##########
# Run the replacement command, "200" as breakpoint was derived from histogram review
wbcd.lreg.2$Perimeter_Mean[wbcd.lreg.2$Perimeter_Mean > 200] <- mean(wbcd.lreg.2$Perimeter_Mean[wbcd.lreg.2$Perimeter_Mean < 200])

# Check out the distribution of values by 20% buckets - 5 buckets
quantile(wbcd.lreg.2$Perimeter_Mean, probs = seq(0,1,.2))
wbcd.lreg.2$PerM.cat <- cut(wbcd.lreg.2$Perimeter_Mean, breaks = c(floor(min(wbcd.lreg.2$Perimeter_Mean) - 1)
                                                                   , 73, 82.5, 92, 111.5, ceiling(max(wbcd.lreg.2$Perimeter_Mean) + 1)), right = TRUE)

########## Area_Mean ##########
# Check out the distribution of values by 20% buckets - 5 buckets
quantile(log(wbcd.lreg.2$Area_Mean), probs = seq(0,1,.2))
wbcd.lreg.2$AreaM.cat <- cut(log(wbcd.lreg.2$Area_Mean), breaks = c(floor(min(log(wbcd.lreg.2$Area_Mean)) - 1)
                                                                    , 6, 6.2, 6.4, 6.8, ceiling(max(log(wbcd.lreg.2$Area_Mean)) + 1)), right = TRUE)

########## Smoothness_Mean ##########
# Run the replacement command, "200" as breakpoint was derived from histogram review
wbcd.lreg.2$Smoothness_Mean[wbcd.lreg.2$Smoothness_Mean > 1] <- mean(wbcd.lreg.2$Smoothness_Mean[wbcd.lreg.2$Smoothness_Mean < 1])

# Check out the distribution of values by 20% buckets - 5 buckets
quantile(wbcd.lreg.2$Smoothness_Mean, probs = seq(0,1,.2))
wbcd.lreg.2$SmoM.cat <- cut(wbcd.lreg.2$Smoothness_Mean, breaks = c(floor(min(wbcd.lreg.2$Smoothness_Mean) - 1)
                                                                    , .084, .092, .099, .107, ceiling(max(wbcd.lreg.2$Smoothness_Mean) + 1)), right = TRUE)

########## Compactness_Mean ##########
# Check out the distribution of values by 20% buckets - 5 buckets
quantile(wbcd.lreg.2$Compactness_Mean, probs = seq(0,1,.2))
wbcd.lreg.2$ComM.cat <- cut(wbcd.lreg.2$Compactness_Mean, breaks = c(floor(min(wbcd.lreg.2$Compactness_Mean) - 1)
                                                                     , .06, .08, .11, .15, ceiling(max(wbcd.lreg.2$Compactness_Mean) + 1)), right = TRUE)

########## Concavity_Mean ##########
# Check out the distribution of values by 20% buckets - 5 buckets
quantile(wbcd.lreg.2$Concavity_Mean, probs = seq(0,1,.2))
wbcd.lreg.2$ConM.cat <- cut(wbcd.lreg.2$Concavity_Mean, breaks = c(floor(min(wbcd.lreg.2$Concavity_Mean) - 1)
                                                                   , .025, .045, .086, .15, ceiling(max(wbcd.lreg.2$Concavity_Mean) + 1)), right = TRUE)

########## Concave_pts_Mean ##########
# Check out the distribution of values by 20% buckets - 5 buckets
quantile(wbcd.lreg.2$Concave_pts_Mean, probs = seq(0,1,.2))
wbcd.lreg.2$CnpM.cat <- cut(wbcd.lreg.2$Concave_pts_Mean, breaks = c(floor(min(wbcd.lreg.2$Concave_pts_Mean) - 1)
                                                                     , .017, .027, .048, .084, ceiling(max(wbcd.lreg.2$Concave_pts_Mean) + 1)), right = TRUE)

########## Symmetry_Mean ##########
# Check out the distribution of values by 20% buckets - 5 buckets
quantile(wbcd.lreg.2$Symmetry_Mean, probs = seq(0,1,.2))
wbcd.lreg.2$SymM.cat <- cut(wbcd.lreg.2$Symmetry_Mean, breaks = c(floor(min(wbcd.lreg.2$Symmetry_Mean) - 1)
                                                                  , .158, .172, .185, .20, ceiling(max(wbcd.lreg.2$Symmetry_Mean) + 1)), right = TRUE)

########## Fractal_dim_mean ##########
# Check out the distribution of values by 20% buckets - 5 buckets
quantile(wbcd.lreg.2$Fractal_dim_Mean, probs = seq(0,1,.2))
wbcd.lreg.2$FraM.cat <- cut(wbcd.lreg.2$Fractal_dim_Mean, breaks = c(floor(min(wbcd.lreg.2$Fractal_dim_Mean) - 1)
                                                                     , .056, .06, .063, .067, ceiling(max(wbcd.lreg.2$Fractal_dim_Mean) + 1)), right = TRUE)


#### unneeded columns and create train and test dataframes
wbcd.lreg.2[grep("_Mean", colnames(wbcd.lreg.2))] <- list(NULL) #remove quantitative columns

# Create a column to break data into training and testing
set.seed(852369741)  #For replication purposes
wbcd.lreg.2$seg <- rbinom(nrow(wbcd.lreg.2), 1, .7)
wbcd.lreg.2$seg[wbcd.lreg.2$seg==1] <- "train"
wbcd.lreg.2$seg[wbcd.lreg.2$seg==0] <- "test"
wbcd.lreg.2$seg <- as.factor(wbcd.lreg.2$seg)

#Create data frame "train.data" and "test.data"- not necessary but makes coding a bit easier
train.wbcd.lreg.2 <- wbcd.lreg.2[wbcd.lreg.2$seg =="train",]
train.wbcd.lreg.2$seg <- NULL  #field 'seg' is no longer needed
test.wbcd.lreg.2 <- wbcd.lreg.2[wbcd.lreg.2$seg =="test",]
test.wbcd.lreg.2$seg <- NULL #field 'seg' is no longer needed

# Save everything to this point
save(raw.wbcd,wbcd.lreg.2, train.wbcd.lreg.2, test.wbcd.lreg.2, file="WBCD.LogReg.RData")
# Load if we wish to start from here
# load(file="WBCD.LogReg.RData")

## ---- Step 3: Training a model on the data ----
model.wbcd.lreg.2 <- glm(formula = Diagnosis ~ ., family = binomial(link = "logit"),
                         data = train.wbcd.lreg.2)
summary(model.wbcd.lreg.2)
anova(model.wbcd.lreg.2)















