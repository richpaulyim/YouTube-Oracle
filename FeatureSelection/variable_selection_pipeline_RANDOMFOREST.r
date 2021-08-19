# ============================= #
#  SETTING UP THE WORKSPACE
# ============================= #
#  LIBRARIES AND PATH
library(gbm) # for boosting
library(glmnet) # for lasso and ridge
library(randomForest) #for randomForest and bagging
library(tree)
library(rattle)
library(rpart) # for decision trees
library(ipred) # for bagged decision trees
library(caret) # for cv 


training <- read.csv("training.csv", header = TRUE)
t#for each factor, convert it to numeric
for (var in names(training))    #loop through each name of the variables with the alias "var"
{
  if(is.factor(training[,var]))
  {
    training[,var] = as.numeric(training[,var]) - 1 #TA likes to have 0 indexed, so values are 0 and 1
  }
}

#for each factor, convert it to numeric
for (var in names(test))    #loop through each name of the variables with the alias "var"
{
  if(is.factor(test[,var]))
  {
    test[,var] = as.numeric(test[,var]) - 1 #TA likes to have 0 indexed, so values are 0 and 1
  }
}
est <- read.csv("test.csv", header = TRUE)

rf_model <- randomForest(growth_2_6~., data = training[,-1], mtry = 258/2, ntree=500, importance=TRUE) 
rfimportance <- rf_model$importance 

# ***************************************
# if you have the csv
rfimportance <- read.csv('~/101C/KaggleFinal/rf_importance.csv', header=TRUE)
# ***************************************

best_subset_rf <- rfimportance$X[rfimportance$IncNodePurity>123]
best_subset_rf <- c("PublishedDate"                 ,"Duration"                     
                      ,"views_2_hours"              ,   "hog_341"                      
                      ,"hog_641"                    ,   "cnn_10"                       
                      ,"cnn_12"                     ,   "cnn_17"                       
                      ,"cnn_19"                     ,   "cnn_25"                       
                      , "cnn_68"                    ,    "cnn_86"                       
                      , "cnn_88"                    ,    "cnn_89"                       
                      , "sd_red"                    ,    "mean_green"                   
                      , "sd_blue"                   ,    "doc2vec_2"                    
                      , "doc2vec_3"                 ,    "doc2vec_10"                   
                      , "doc2vec_13"                ,    "doc2vec_16"                   
                      , "punc_num_..28"             ,    "num_words"                    
                      , "num_chars"                 ,    "num_uppercase_chars"          
                      , "num_digit_chars"           ,    "Num_Subscribers_Base_low_mid" 
                      , "Num_Subscribers_Base_mid_high", "Num_Views_Base_mid_high"      
                      , "avg_growth_low"            ,    "avg_growth_low_mid"           
                      , "avg_growth_mid_high"       ,    "count_vids_low_mid"           
                      , "count_vids_mid_high") 
