###### Name : Kadiatou KABA ######
########## CHOOSE YOUR OWN PROJECT ###########


## Loading libraries
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)
if (!require(caret)) install.packages('caret')
library(caret)
if (!require(rpart.plot)) install.packages('rpart.plot')
library(rpart.plot)
if (!require(rmarkdown)) install.packages('rmarkdown')
library(rmarkdown)
if (!require(questionr)) install.packages('questionr')
library(questionr)


## Read the data
heart <- read.csv('../Project Choose Your Own/heart.csv')

## Cleaning the dataset
### Drop the null values
missing_ca_indeces <- which(heart$ca %in% 4)
missing_thal_indeces <-which(heart$thal %in% 0)
missing_values_indeces <- c(missing_ca_indeces, missing_thal_indeces)
heart <- heart[-missing_values_indeces, ]

### Transform categorical variable to R factors
heart$sex <- as.factor(heart$sex)
heart$cp <- as.factor(heart$cp)
heart$fbs <- as.factor(heart$fbs)
heart$restecg <- as.factor(heart$restecg)
heart$exang <- as.factor(heart$exang)
heart$slope <- as.factor(heart$slope)
heart$thal <- as.factor(heart$thal)
heart$target <- as.factor(heart$target)

### Changing names to the factor values for the graphs
levels(heart$sex) <- c("Female", "Male")
levels(heart$cp) <- c("Asymptomatic", "Atypical angina", "No angina", "Typical angina")
levels(heart$fbs) <- c("No", "Yes")
levels(heart$restecg) <- c("Hypertrophy", "Normal", "Abnormalities")
levels(heart$exang) <- c("No", "Yes")
levels(heart$slope) <- c("Descending", "Flat", "Ascending")
levels(heart$thal) <- c("Fixed defect", "Normal flow", "Reversible defect")
levels(heart$target) <- c("Yes", "No")

### renaming the "age" variable
heart <- rename.variable(heart, "ï..age","age")

## Dataset features
head(heart)

## Data vizualisation

##### Target

prop.table(table(heart$target))
ggplot(heart, aes(target, fill=target)) + 
  geom_bar() +
  labs(x="Disease", y="Number of patients") +
  guides(fill=FALSE)

##### Age
## Histogram
ggplot(heart, aes(age, fill=target)) + 
  geom_histogram(binwidth=1) +
  labs(fill="Disease", x="Age", y="Number of patients")
summary(heart$age)

##### Sex
## Bar plot
ggplot(heart, aes(sex, fill=target)) + 
  geom_bar() +
  labs(fill="Disease", x="Sex", y="Number of patients")
prop.table(table(heart$sex))

##### CP
## Histogram
ggplot(heart, aes(cp, fill=target)) +
  geom_bar() +
  labs(fill="Disease", x="Chest pain type", y="Number of patients")
prop.table(table(heart$cp))

##### trestbps
## Histogram
ggplot(heart, aes(trestbps, fill=target)) +
  geom_histogram(binwidth=3) +
  labs(fill="Disease", x="Blood pressure (mm Hg)", y="Number of patients")
summary(heart$trestbps)

##### Chol
## Histogram
ggplot(heart, aes(chol, fill=target)) +
  geom_histogram(binwidth=10) +
  labs(fill="Disease", x="Cholesterol (mg/dl)", y="Number of patients")

##### Fbs
## Bar plot
ggplot(heart, aes(fbs, fill=target)) +
  geom_bar() +
  labs(fill="Disease", x="Sugar level > 120 mg/dl", y="Number of patients")

##### Restecg
## Bar plot
ggplot(heart, aes(restecg, fill=target)) +
  geom_bar() +
  labs(fill="Disease", x="Electrocardiogram on rest", y="Number of patients")

##### Thalach
## Histogram
ggplot(heart, aes(thalach, fill=target)) +
  geom_histogram(binwidth=10) +
  labs(fill="Disease", x="Maximum heart rate during exercise", y="Number of patients")

heart[heart$thalach < 100, c("age", "thalach", "target")]

heart[heart$thalach > 180, c("age", "thalach", "target")]


##### Exang
## Bar plot
ggplot(heart, aes(exang, fill=target)) +
  geom_bar() +
  labs(fill="Disease", x="Presence of angina during exercise", y="Number of patients")

##### Oldpeak
## Histogram
ggplot(heart, aes(oldpeak, fill=target)) +
  geom_histogram(binwidth=0.25) +
  labs(fill="Disease", x="Depression of the ST segment", y="Number of patients")

##### slope
## Bar plot
ggplot(heart, aes(slope, fill=target)) +
  geom_bar() +
  labs(fill="Disease", x="Slope of the ST segment", y="Number of patients")

## Boxplot
ggplot(heart, aes(x=slope, y=oldpeak, fill=target)) +
  geom_boxplot() +
  labs(fill="Disease", x="Slope of the ST segment", y="Depression of the ST segment")

##### Thal
## Bar plot
ggplot(heart, aes(thal, fill=target)) +
  geom_bar() +
  labs(fill="Disease", x="Results of the blood flow", y="Number of patients")

##### Ca
## Bar plot
ggplot(heart, aes(ca, fill=target)) +
  geom_bar() +
  labs(fill="Disease", x="Number of main blood vessels coloured", y="Number of patients")


## MODELS

### Splitting the dataset & setting the cross-validation
set.seed(1) # set seed to 1
training_indeces <- createDataPartition(heart$target, p = .7, list = FALSE)
heart.train <- heart[ training_indeces,]
heart.test  <- heart[-training_indeces,]

# 10 fold Cross-validation
fitControl <- trainControl(method="cv", number=10)

### LOGISTIC REGRESSION
set.seed(1)
model.lr <- train(target ~ ., 
                  data = heart.train,
                  method = "glm",
                  family=binomial(),
                  trControl = fitControl)
model.lr

### DECISION TREE
set.seed(1)
model.tree <- train(target ~ ., 
                    data = heart.train,
                    method = "rpart",
                    trControl = fitControl)
model.tree

plot(model.tree) 
rpart.plot(model.tree$finalModel)

### Naive Bayes
set.seed(1)
model.nb <- train(target ~ ., 
                  data = heart.train,
                  method = "naive_bayes",
                  trControl = fitControl)
model.nb

plot(model.nb)

### Random Forest
set.seed(1)
model.rf <- train(target ~ ., 
                  data = heart.train,
                  method = "rf",
                  trControl = fitControl)
model.rf

plot(model.rf)

## RESULTS

### Comparison of the trainings

model.all <- list(LogisticReg=model.lr, Tree=model.tree, NaiveBayes=model.nb, RandomForest=model.rf)
results <- resamples(model.all)
summary(results, metric=c("Kappa", "Accuracy"))

## Final results

### Naive Bayes
preds.nb <- predict(model.nb, heart.test)
confusionMatrix(preds.nb, heart.test$target)

### Random Forest

preds.rf <- predict(model.rf, heart.test)
confusionMatrix(preds.rf, heart.test$target)


