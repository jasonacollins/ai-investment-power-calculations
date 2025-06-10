## Jianhua 


install.packages("simr")
install.packages("lme4")
library(simr)
library(lme4) # for fitting GLMMs
library(tidyverse) # for data manipulation
library(haven)

data <- read_dta("C:/Users/80992/Desktop/OneDrive - UTS/RA_Jason/power calculation/germann-and-merkle-2023.dta")

## sample slection
valid_data <- data[!is.na(data$asset_human) & !is.na(data$asset_algo), ]
valid_data <- data[!is.na(data$asset_human) & !is.na(data$asset_algo) & data$fund_manager==0,]
valid_data$human_correct <- ifelse(valid_data$asset_human == valid_data$asset_algo, 1, 0)

ambiguous_cases <- valid_data$algo_random == 1
non_ambiguous_data <- valid_data[!ambiguous_cases,]
ambiguous_data <- valid_data[ambiguous_cases,]
non_ambiguous_data$rgroup <- sample(1:6, nrow(non_ambiguous_data), replace = TRUE)

data_1 <- non_ambiguous_data[non_ambiguous_data$rgroup == 1, ]
data_2 <- non_ambiguous_data[non_ambiguous_data$rgroup == 2, ]
data_3 <- non_ambiguous_data[non_ambiguous_data$rgroup == 3, ]
data_4 <- non_ambiguous_data[non_ambiguous_data$rgroup == 4, ]
data_5 <- non_ambiguous_data[non_ambiguous_data$rgroup == 5, ]
data_6 <- non_ambiguous_data[non_ambiguous_data$rgroup == 6, ]

# Create a vector to store accuracies
human_accuracies <- numeric(6)

for (i in 1:6) {
  
  # Subset data for rgroup == i
  data_i <- subset(non_ambiguous_data, rgroup == i)
  
  # Compute number of correct responses
  human_correct_i <- sum(data_i$asset_human == data_i$asset_algo, na.rm = TRUE)
  
  # Compute accuracy percentage
  human_accuracy_i <- (human_correct_i / nrow(data_i)) * 100
  
  # Store result
  human_accuracies[i] <- human_accuracy_i
}

# View accuracies
print(human_accuracies)

for(i in 1:10) {
  non_ambiguous_data[[paste0("block_", i) ]] <- ifelse(non_ambiguous_data$block == i, 1, 0)
}
for(i in 1:6) {
  non_ambiguous_data[[paste0("trial_", i) ]] <- ifelse(non_ambiguous_data$trial == i, 1, 0)
}

## baseline: non_ambiguous_data
non_ambiguous_data$human_correct <- ifelse(non_ambiguous_data$asset_human == non_ambiguous_data$asset_algo, 1, 0)
human_correct <- sum(non_ambiguous_data$asset_human == non_ambiguous_data$asset_algo)
human_accuracy <- (human_correct / nrow(non_ambiguous_data)) * 100
choice_equal_fee <- sum(non_ambiguous_data$asset_human ==1)





# Initialize a list to store models
glmm_list <- list()

# Loop over rgroup values 1 to 6
for (i in 1:6) {
  cat("\nRunning model for rgroup =", i, "\n")
  
  # Fit model on subset where rgroup == i
  glmm_list[[i]] <- glmer(
    human_correct ~ block_1 + block_2 + block_3 + block_4 + block_5 + block_6 + block_7 + block_8 + block_9 +
      trial_1 + trial_2 + trial_3 + trial_4 + trial_5 + (1 | id),
    family = binomial(link = "logit"),
    data = subset(non_ambiguous_data, rgroup == i)
  )
  
  # Print summary
  print(summary(glmm_list[[i]]))
}


# Fit the GLMM
# choice: 1 algo, 0 human
###############################################
filtered_data <- subset(non_ambiguous_data, rgroup %in% c(1, 3))

save(filtered_data, file = "filtered_data.RData")

save(filtered_data)
# Fit the model
glmm2 <- glmer(
  human_correct ~ rgroup + block_1 + block_2 + block_3 + block_4 + block_5 + 
    block_6 + block_7 + block_8 + block_9 +
    trial_1 + trial_2 + trial_3 + trial_4 + trial_5 + (1 | id),
  family = binomial(link = "logit"),
  data = filtered_data
)
summary(glmm2)
# fixef(glmm2)["rgroup"]<-0.025

glmm2_ext<-extend(glmm2 , along="rgroup", n=500)

pc2<-powerCurve(glmm2_ext, nsim=1)
print (pc2)
plot(pc2)

pc1<-powerSim(glmm2_ext,nsim=10)
pc1<-powerSim(glmm2,nsim=10)
pc2<-powerSim(glmm2,nsim=50,breaks=c(4,6,8,10))
pc3<-powerCurve(glmm2, along = "id", nsim=50)
print (pc1)
print (pc2)
plot(pc3)
###############################################






