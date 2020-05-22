#
# PAckages & Libaries required for execution
#
install.packages("caret")
install.packages("C50")
install.packages("gmodels")
install.packages("RWeka")
library(RWeka)
library(C50)
library(gmodels)

#*********************************identifying risky bank loans using C5.0 decision trees******
#
# Step 1 – collecting data
#
credit <- read.csv(file.choose())  # using the german loan data set downloaded from the github
head(credit)
str(credit)
#
# Step 2 – exploring and preparing the data
#
## Descrptive data analysis on varibles whcih likely to predict the defult 
# Catogorical features 
#
table(credit$checking_balance)
table(credit$savings_balance)
#
# Numeric Features 
#
summary(credit$months_loan_duration)
summary(credit$amount)

#Data Transformation
credit$default <- factor(credit$default, levels = c(0,1), labels = c("no", "yes"))
table(credit$default)
#
#Spliting data set into test and train data sets
set.seed(12345)
credit_rand <- credit[order(runif(1000)), ]

credit_train <- credit_rand[1:900, ]
credit_test <- credit_rand[901:1000, ]

prop.table(table(credit_train$default)) 
prop.table(table(credit_test$default))
#
# Step 3 – training a model on the data
#
credit_model <- C5.0(credit_train[-21], credit_train$default)
credit_model
plot(credit_model,type='s')
summary(credit_model)
#
# Step 4 – evaluating model performance
#
credit_pred <- predict(credit_model, credit_test)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))
#
#Step 5 – improving model performance
#
credit_boost10 <- C5.0(credit_train[-21], credit_train$default, trials = 10)
credit_boost10
summary(credit_boost10)
 #
# Accuracey of the model with Boosting 
#
credit_boost_pred10 <- predict(credit_boost10, credit_test) 
CrossTable(credit_test$default, credit_boost_pred10,prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))

#
#Step 5 – Adding penality to errors to reduce the false Negatives 
#
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)
error_cost

credit_cost <- C5.0(credit_train[-21], credit_train$default, costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))




#*******************************Part -II  identifying poisonous mushrooms with rule learners***
#
#Step 1 – collecting data
#
url_file <-  "https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"
mushrooms <- read.csv(url(url_file), stringsAsFactors = TRUE,header=FALSE)
#
#Step 2 – exploring and preparing the data
#
str(mushrooms)
fields <- c("type","cap_shape",
            "cap_surface",
            "cap_color",
            "bruises",
            "odor",
            "gill_attachment",
            "gill_spacing",
            "gill_size",
            "gill_color",
            "stalk_shape",
            "stalk_root",
            "stalk_surface_above_ring",
            "stalk_surface_below_ring",
            "stalk_color_above_ring",
            "stalk_color_below_ring",
            "veil_type",
            "veil_color",
            "ring_number",
            "ring_type",
            "spore_print_color",
            "population",
            "habitat")
colnames(mushrooms) <- fields
head(mushrooms)
mushrooms$veil_type<-NULL
table(mushrooms$veil_type)
mushrooms$type <- factor(mushrooms$type, levels = c("e","p"), labels = c("edible", "poisonous"))
table(mushrooms$type)
mushrooms$odor <- factor(mushrooms$odor, levels = c("a","l","c","y","f","m","n","p","s"), 
                         labels = c("almond", "anise","creosote","fishy","foul","musty","none","pungent","spicy"))
#
#Step 3 – training a model on the data
#
mushroom_1R <- OneR(type ~ ., data = mushrooms)
mushroom_1R
#
#Step 4 – evaluating model performance
#
summary(mushroom_1R)
#
#Step 5 – improving model performance
#
mushroom_JRip <- JRip(type ~ ., data = mushrooms)
mushroom_JRip
