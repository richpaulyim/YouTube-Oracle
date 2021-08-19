# ============================= #
#  LIBRARIES AND PATH
# ============================= #
set.seed(42)
library(ggplot2)
library(boot)
library(caret)
library(leaps)
library(glmnet)
library(pls)
library(dplyr)

# ============================= #
#  LOADING DATA 
# ============================= #
setwd('~/101C/KaggleFinal/')
training <- read.csv("training.csv", header=TRUE)
testing <- read.csv("test.csv", header=TRUE)
cat(dim(training), dim(testing))
youtube_data <- data.frame(training)


# observation selection
unleveraged <- TRUE
if(unleveraged){
    youtube_data <- youtube_data[unleveraged_rows,]
} 
attach(youtube_data)

# ============================================= #
# == MAKE SURE VARIABLE_SELECTION_PIPELINE.R == #
# == HAS BEEN RUN ============================= #
# ============================================= #

# =============================================== #
#  MULTIPLE LINEAR REGRESSION (model 1)
# =============================================== #
glm.fit <- glm(growth_2_6~., data=youtube_data[,best_subset_cor])
cv.MLR.error <- cv.glm(data.frame(youtube_data[,best_subset_cor],growth_2_6) 
                      ,glm.fit, K=10)$delta[1]
summary.MLR <- summary(glm.fit)
trainRMSE.MLR <- sqrt(cv.MLR.error) 
trainRMSE.MLR
# RMSE: 1.833286, 1.709845 after unleveraged values
glm.fit.final <- glm.fit
mlr.pred <- predict(glm.fit.final, testing[,best_subset_cor])

# =============================================== #
#  RIDGE REGRESSION (model 2)
# =============================================== #
x <- model.matrix(growth_2_6~.,youtube_data[,best_subset_cor])
y <- youtube_data$growth_2_6
ridge.fit <- cv.glmnet(x, y, family="gaussian", 
                       alpha=0, standardize=TRUE, nfolds=10)
# ridge regression lambda: 0.1278217
ridge.lambda <- ridge.fit$lambda.min
# ridge regression RMSE
trainRMSE.ridge <- sqrt(ridge.fit$cvm[ridge.fit$lambda.min==ridge.fit$lambda])
trainRMSE.ridge
# RMSE: 1.834998, 1.711879 after unleveraged values
ridge.fit.final <- glmnet(x, y, family="gaussian", 
                       alpha=0, standardize=TRUE, lambda = ridge.lambda)
ridge.pred <- predict(ridge.fit.final, newx = model.matrix(~.,testing[,best_subset_cor]))

# =============================================== #
#  LASSO REGRESSION (model 3)
# =============================================== #
x <- model.matrix(growth_2_6~.,youtube_data[,best_subset_cor])
y <- youtube_data$growth_2_6
lasso.fit <- cv.glmnet(x, y, family="gaussian", 
                       alpha=1, standardize=TRUE, nfolds=10)
# lasso regression lambda: 0.002286282
lasso.lambda <- lasso.fit$lambda.min
# lasso regression RMSE
trainRMSE.lasso <- sqrt(ridge.fit$cvm[lasso.fit$lambda.min==lasso.fit$lambda])
trainRMSE.lasso
# RMSE: 1.906478, 1.76755 after unleveraged values
lasso.fit.final <- glmnet(x, y, family="gaussian", 
                       alpha=1, standardize=TRUE, lambda = lasso.lambda)
lasso.pred <- predict(lasso.fit.final, newx = model.matrix(~.,testing[,best_subset_cor]))

# =============================================== #
#  Principal Component Regression  (model 4)
# =============================================== #
pcr.fit <- pcr(growth_2_6~., data=youtube_data[,best_subset_cor], 
               scale=TRUE, validation="CV")
# Best number of components: 30
validationplot(pcr.fit, val.type="MSEP")
summary(pcr.fit)
# RMSEP: 1.836, 1.712 with unleveraged observations
pcr.fit.final <- pcr(growth_2_6~., data=youtube_data[,best_subset_cor], 
               scale=TRUE)
pcr.pred <- predict(pcr.fit.final, testing[,best_subset_cor], ncomp = 30)


# =============================================== #
#  Partial Least Squares Regression  (model 5)
# =============================================== #
pls.fit <- plsr(growth_2_6~., data=youtube_data[,best_subset_cor], 
                scale=TRUE, validation="CV")
# Best number of components: 10
validationplot(pls.fit ,val.type="MSEP")
summary(pls.fit)
# RMSE: 1.834, same after unleveraged values
pls.fit.final <- plsr(growth_2_6~., data=youtube_data[,best_subset_cor], 
                      scale=TRUE)
pls.pred <- predict(pls.fit.final, newdata = testing[,best_subset_cor],ncomp = 10)

# =============================================== #
#  PRODUCING TEST OUTPUTS (models 1-5)
# =============================================== #
# MLR
predictions_mlr <- cbind(testing$id, mlr.pred) %>% as.data.frame()
names(predictions_mlr) <- c("id", "growth_2_6") 
write.csv(predictions_mlr, "predictions_mlr.csv", row.names = FALSE) 
# RIDGE 
predictions_ridge <- cbind(testing$id, ridge.pred) %>% as.data.frame()
names(predictions_ridge) <- c("id", "growth_2_6") 
write.csv(predictions_ridge, "predictions_ridge.csv", row.names = FALSE) 
# LASSO
predictions_lasso <- cbind(testing$id, lasso.pred) %>% as.data.frame()
names(predictions_lasso) <- c("id", "growth_2_6") 
write.csv(predictions_lasso, "predictions_lasso.csv", row.names = FALSE) 
# PCR
predictions_pcr <- cbind(testing$id, pcr.pred) %>% as.data.frame()
names(predictions_pcr) <- c("id", "growth_2_6") 
write.csv(predictions_pcr, "predictions_pcr.csv", row.names = FALSE) 
# PLS
predictions_pls <- cbind(testing$id, pls.pred) %>% as.data.frame()
names(predictions_pls) <- c("id", "growth_2_6") 
write.csv(predictions_pls, "predictions_pls.csv", row.names = FALSE) 

