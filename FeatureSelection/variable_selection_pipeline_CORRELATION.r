# ============================= #
#  SETTING UP THE WORKSPACE
# ============================= #
#  LIBRARIES AND PATH
library(ggplot2)
library(boot)
library(caret)
library(leaps)

#  LOADING DATA 
setwd('~/101C/KaggleFinal/')
training <- read.csv("training.csv", header=TRUE)
testing <- read.csv("test.csv", header=TRUE)
cat("Training Dimensions:",dim(training), 
    "Testing Dimensions",dim(testing))
# remove the ID number
youtube_data <- data.frame(training[,-1])
attach(youtube_data)

# ============================= #
#  VARIABLE SELECTION
# ============================= #
# STATISTICAL SUMMARIES
summary(youtube_data)

# ***************************** #
# THUMBNAIL IMAGE FEATURES
# ***************************** #
# histogram of gradient features
hog_tau <- 0.00005
hog_vars <- which(grepl("hog", names(youtube_data)))
hog_important <- hog_vars[which(abs(cor(youtube_data[,hog_vars], youtube_data[,hog_vars])) < hog_tau
      , arr.ind=TRUE)[,"col"]]
hog_important

# most frequent and important cnn extracted features
cnn_tau <- 0.4
cnn_vars <- which(grepl("cnn", names(youtube_data)))
cnn_freq <- cnn_vars[which(apply(youtube_data[,cnn_vars]==0, FUN=sum, MARGIN=2) < 7000)]
cnn_important <- cnn_freq[which(abs(cor(youtube_data[,cnn_freq], youtube_data[,cnn_freq])) < cnn_tau
      , arr.ind=TRUE)[,"col"]]
cnn_important

# pixel and rgb value features
pixel_tau <- 0.1
pixel_vars <- which(grepl(c("red|blue|green|pixel|edge"), names(youtube_data)))
pixel_vars <- pixel_vars[!grepl("min|max",names(youtube_data[,pixel_vars]))]
pixel_important <- pixel_vars[unique(which(abs(cor(youtube_data[,pixel_vars], youtube_data[,pixel_vars])) < pixel_tau
      , arr.ind=TRUE)[,1])]
pixel_important

# ***************************** #
# VIDEO TITLE FEATURES
# ***************************** #
# pixel and rgb value features
nlp_tau <- 0.001
nlp_vars <- which(grepl(c("num|doc"), names(youtube_data)))
nlp_freq <- nlp_vars[which(apply(youtube_data[,nlp_vars]==0, FUN=sum, MARGIN=2) < 7000)]
nlp_important <- nlp_freq[unique(which(abs(cor(youtube_data[,nlp_freq], youtube_data[,nlp_freq])) < nlp_tau
      , arr.ind=TRUE)[,1])]
nlp_important

# ***************************** #
# CHANNEL FEATURES
# ***************************** #
channel_tau <- 0.01
channel_vars <- which(grepl(c("count|avg_g|Num"), names(youtube_data)))
channel_important <- channel_vars[unique(which(abs(cor(youtube_data[,channel_vars], youtube_data[,channel_vars])) < channel_tau
      , arr.ind=TRUE)[,1])]
channel_important

# ***************************** #
# FINAL VARIABLES INDEX SET
# ***************************** #
important_families <- unique(c(hog_important, cnn_important, pixel_important, nlp_important, channel_important))
important_families
families_tau <- 0.01
important_vars <- important_families[unique(which(abs(cor(youtube_data[,important_families], youtube_data[,important_families])) < families_tau
      , arr.ind=TRUE)[,1])]
length(important_vars)

# ============================= #
#  SUBSET SELECTION
# ============================= #
# using important_vars from the variable selection pipeline
regfit.full <- regsubsets(growth_2_6~.,
                          data=youtube_data[,important_vars], 
                          nvmax = length(important_vars))
reg.summary <- summary(regfit.full)
# which subset performed the best
best.subset <- which(min(reg.summary$rss)==reg.summary$rss)
length(important_vars)
best.subset
best_subset_cor <- names(reg.summary$which[best.subset,])[reg.summary$which[best.subset,]][-1]

best_subset_cor <- c("hog_828"        ,"cnn_12"        ,"cnn_17"                       ,"cnn_19"              ,"cnn_25"                    ,  
                "cnn_88"         ,"cnn_89"        ,"doc2vec_16"                   ,"avg_growth_low_mid"  ,"Num_Views_Base_low"        ,  
                 "count_vids_low", "hog_514"      , "hog_747"                     , "cnn_86"             , "pct_nonzero_pixels"       ,   
                 "doc2vec_5"     , "punc_num_..13", "hog_452"                     , "hog_649"            , "cnn_10"                   ,   
                 "cnn_68"        , "doc2vec_6"    , "hog_316"                     , "count_vids_mid_high", "hog_279"                  ,   
                 "edge_avg_value", "hog_797"      , "Num_Subscribers_Base_low_mid", "doc2vec_17"         , "Num_Subscribers_Base_low" ,   
                 "doc2vec_1"     , "hog_139"      , "doc2vec_19"                  , "doc2vec_3"          , "doc2vec_10")
