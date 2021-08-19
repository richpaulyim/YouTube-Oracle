# ============================= #
#  LIBRARIES AND PATH
# ============================= #
set.seed(42)
library(car)
library(MASS)

# ============================= #
#  LOADING DATA 
# ============================= #
setwd('~/101C/KaggleFinal/')
training <- read.csv("training.csv", header=TRUE)
testing <- read.csv("test.csv", header=TRUE)
cat(dim(training), dim(testing))
youtube_data <- data.frame(training)
attach(youtube_data)

# ============================= #
#  VARIABLE SUBSET
# ============================= # 
# TRY EITHER THE 
# BEST_SUBSET_COR SUBSET # OR
# BEST_SUBSET_RF SUBSET

# **************************************** #
# FIT VARIABLES TO LINEAR MODEL (cor)
# **************************************** #
glm.fit <- glm(growth_2_6~., data=youtube_data[,best_subset_cor])
# studentized pearson residual Q-Q plot 
plot(glm.fit,2)
influencePlot(glm.fit)
# default hat tolerance for more than 1,000 data points *heuristic*
hat_tolerance <- 4/dim(youtube_data)[2]
hat_tolerance

# cutoff at studentized residuals and hat values ranges
unleveraged_rows <- (studres(glm.fit)<2.5) & (hatvalues(glm.fit) < 0.015)
