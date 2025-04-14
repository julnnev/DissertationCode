rm(list = ls())
set.seed(123)
library(mgcv)
library(readxl)
library(corrplot)
library(moments)
library(lmtest)
library(MASS)
library(glmtoolbox)
library(DHARMa)

data <- read_excel("~")

# Descriptive Statistics

head(data)
summary(data)
means <- sapply(data, mean)
medians <- sapply(data, median)
mins <- sapply(data, min)
maxs <- sapply(data, max)
std_devs <- sapply(data, sd)

summary_df <- data.frame(
  Mean = means,
  Median = medians,
  Min = mins,
  Max = maxs,
  Std_Dev = std_devs
)



pdf("correlogram.pdf", width = 8, height = 6)
corrplot(cor(data, method="spearman"), 
         type="upper", 
         col = colorRampPalette(c("blue", "white", "red"))(200),  
         tl.cex = 0.6, 
         tl.col = "black",  
         number.cex = 0.7, 
         diag = FALSE)  
dev.off()
# Calculate the correlation matrix
cor_matrix <- cor(data, method="spearman")
cor_matrix[lower.tri(cor_matrix)] <- NA  # Set lower triangle to NA

# Function to split the matrix into chunks
split_matrix <- function(cor_matrix, n_cols) {
  # Split the matrix into chunks of n_cols columns
  split_matrices <- list()
  num_chunks <- ceiling(ncol(cor_matrix) / n_cols)
  
  for (i in 1:num_chunks) {
    start_col <- ((i - 1) * n_cols) + 1
    end_col <- min(i * n_cols, ncol(cor_matrix))
    split_matrices[[i]] <- cor_matrix[, start_col:end_col]
  }
  
  return(split_matrices)
}

png("scatter1.png", width = 2000, height = 1500, res = 300)
plot(data$`Petrol Station`, data$CPAH,xlab = "Petrol Station", ylab = "CPAH")
dev.off()

png("scatter2.png", width = 2000, height = 1500, res = 300)
plot(data$`Time ETS`, data$CPAH,xlab = "Time ETS", ylab = "CPAH")
dev.off()

png("scatter3.png", width = 2000, height = 1500, res = 300)
plot(data$`Time Cooking`, data$CPAH,xlab = "Time Cooking", ylab = "CPAH")
dev.off()

png("scatter4.png", width = 2000, height = 1500, res = 300)
plot(data$`Time Travelling`, data$CPAH,xlab = "Time Travelling", ylab = "CPAH")
dev.off()


png("scatter5.png", width = 2000, height = 1500, res = 300)
plot(data$Floor, data$CPAH,xlab = "Floor", ylab = "CPAH")
dev.off()

png("scatter6.png", width = 2000, height = 1500, res = 300)
plot(data$`Use Gas Cooker`, data$CPAH,xlab = "Use Gas Cooker", ylab = "CPAH")
dev.off()


png("scatter7.png", width = 2000, height = 1500, res = 300)
plot(data$`Use Gas Cooker Weekend`, data$CPAH,xlab = "Use Gas Cooker Weekend", ylab = "CPAH")
dev.off()


png("scatter8.png", width = 2000, height = 1500, res = 300)
plot(data$Total_VOC, data$CPAH,xlab = "Total VOC", ylab = "CPAH")
dev.off()

png("scatter9.png", width = 2000, height = 1500, res = 300)
plot(data$VOC, data$CPAH,xlab = "VOC", ylab = "CPAH")
dev.off()

png("scatter10.png", width = 2000, height = 1500, res = 300)
plot(data$VVOC, data$CPAH,xlab = "VVOC", ylab = "CPAH")
dev.off()

png("scatter11.png", width = 2000, height = 1500, res = 300)
plot(data$BTEX, data$CPAH,xlab = "BTEX", ylab = "CPAH")
dev.off()


# encoding categorical variables as factors
data$Summer <- as.factor(data$Summer)
data$TR <- as.factor(data$TR) 
data$Open_plan <- as.factor(data$Open_plan)
data$Laminated_floor  <- as.factor(data$Laminated_floor)
data$Aerosol_use <- as.factor(data$Aerosol_use)
data$Air_freshner <- as.factor(data$Air_freshner)
data$Redecoration <- as.factor(data$Redecoration)
data$New <- as.factor(data$New)
data$stqhmephc <- as.factor(data$stqhmephc)
data$stqhmeik <- as.factor(data$stqhmeik)
data$urban <- as.factor(data$urban)
data$suburban <- as.factor(data$suburban)
data$rural <- as.factor(data$rural)
data$IG <- as.factor(data$IG) 
data$FL <- as.factor(data$FL) 
data$ETS_home <- as.factor(data$ETS_home) 
data$WM <- as.factor(data$WM) 
data$ETS <- as.factor(data$ETS)
data$Photocopying <- as.factor(data$Photocopying)
data$New_Carpet <- as.factor(data$New_Carpet)
data$recent_carpet_or_lino <- as.factor(data$recent_carpet_or_lino)
data$Car_in_garage <- as.factor(data$Car_in_garage)
data$Use_Bus <- as.factor(data$Use_Bus)
data$Walk_busy_road <- as.factor(data$Walk_busy_road)
data$Petrol_car_garage <- as.factor(data$Petrol_car_garage)
data$Not_Connected <- as.factor(data$Not_Connected)
data$Gas_main_heating <- as.factor(data$Gas_main_heating)
data$Electricity_main_heating <- as.factor(data$Electricity_main_heating)
data$SUM_Additional_Heating <- as.factor(data$SUM_Additional_Heating)
data$Sometimes_cooker_hood <- as.factor(data$Sometimes_cooker_hood)

for (col in colnames(data)) {
  if (is.factor(data[[col]])) {
    print(paste("Frequency table for", col))
    print(table(data[[col]]))
    cat("\n") 
    print(prop.table(table(data[col])))
  }
}

results <- list()
for (col in colnames(data)) {
  if (is.factor(data[[col]])) {
    one_count <- sum(data[[col]] == 1, na.rm = TRUE)
    zero_count <- sum(data[[col]] == 0, na.rm = TRUE)
    
    total_count <- sum(!is.na(data[[col]]))
    
    one_percentage <- (one_count / total_count) * 100
    zero_percentage <- (zero_count / total_count) * 100
    
    results[[col]] <- c(as.integer(one_count), round(one_percentage, 2),
                        as.integer(zero_count), round(zero_percentage, 2))
  }
}

summary_table <- do.call(rbind, results)
colnames(summary_table) <- c("1 Count", "1 Percentage", "0 Count", "0 Percentage")
rownames(summary_table) <- names(results)
summary_table[ , c("1 Count", "0 Count")] <- apply(summary_table[ , c("1 Count", "0 Count")], 2, as.integer)
print(summary_table)

# split into test and train set
select_rows <- which(data$Not_Connected == 0 & data$SUM_Additional_Heating == 0 & data$Air_freshner==0 & data$stqhmephc==0& data$rural==0 & data$IG==0 & data$ETS_home==0& data$Photocopying==0& data$recent_carpet_or_lino==0& data$Car_in_garage==0 & data$Use_Bus==0)
chosen_rows <- sample(select_rows, 3) # 52 74 51
train_set <- data[c(1:50, 53:73, 75:80, 82:84), ]
test_set <- data[c(51, 52, 74, 81, 85:89),]



#scale columns pertaining to continuous variables
attach(train_set)
Distance_PetrolStation <- scale(Distance_PetrolStation, center = TRUE, scale = TRUE)
Distance_PetrolStation_trainMean <- mean(Distance_PetrolStation)
Distance_PetrolStation_trainSD <- sd(Distance_PetrolStation)

Time_ETS <- scale(Time_ETS, center = TRUE, scale = TRUE)
Time_ETS_trainMean <- mean(Time_ETS)
Time_ETS_trainSD <- sd(Time_ETS)

Time_cooking <- scale(Time_cooking, center = TRUE, scale = TRUE)
Time_cooking_trainMean <- mean(Time_cooking)
Time_ETS_trainSD <- sd(Time_cooking)

Time_travelling <- scale(Time_travelling, center = TRUE, scale = TRUE)
Time_travelling_trainMean <- mean(Time_travelling)
Time_travelling_trainSD <- sd(Time_travelling)

VVOC <- scale(VVOC, center = TRUE, scale = TRUE)
VVOC_trainMean <- mean(VVOC)
VVOC_trainSD <- sd(VVOC)

VOC <- scale(VOC, center = TRUE, scale = TRUE)
VOC_trainMean <-mean(VOC)
VOC_trainSD <- sd(VOC)

Total_VOC <- scale(Total_VOC, center = TRUE, scale = TRUE)
Total_VOC_trainMean <-mean(Total_VOC)
Total_VOC_trainSD <- sd(Total_VOC)

BTEX <- scale(BTEX, center = TRUE, scale = TRUE)
BTEX_trainMean <-mean(BTEX)
BTEX_trainSD <- sd(BTEX)

Use_gas_cooker <- scale(Use_gas_cooker, center = TRUE, scale = TRUE)
Use_gas_cooker_trainMean <- mean(Use_gas_cooker)
Use_gas_cooker_trainSD <- sd(Use_gas_cooker)

Use_gas_cooker_wekend <- scale(Use_gas_cooker_wekend, center = TRUE, scale = TRUE)
Use_gas_cooker_wekend_trainMean <- mean(Use_gas_cooker_wekend)
Use_gas_cooker_wekend_trainSD <- sd(Use_gas_cooker_wekend)

detach(train_set)

# Scaling test set on the training set's parameters
attach(test_set)
Distance_PetrolStation <- scale(Distance_PetrolStation, center = Distance_PetrolStation_trainMean, scale = Distance_PetrolStation_trainSD)
Time_ETS <- scale(Time_ETS, center = Time_ETS_trainMean, scale = Time_ETS_trainSD)
Time_cooking <- scale(Time_cooking, center = Time_cooking_trainMean, scale = Time_ETS_trainSD)
Time_travelling <- scale(Time_travelling, center = Time_travelling_trainMean, scale = Time_travelling_trainSD)
VVOC <- scale(VVOC, center = VVOC_trainMean, scale = VVOC_trainSD)
VOC <- scale(VOC, center = VOC_trainMean, scale = VOC_trainSD)
Total_VOC <- scale(VOC, center = Total_VOC_trainMean, scale = Total_VOC_trainSD)
BTEX <- scale(BTEX, center = BTEX_trainMean, scale = BTEX_trainSD)
Use_gas_cooker <- scale(Use_gas_cooker, center = Use_gas_cooker_trainMean, scale = Use_gas_cooker_trainSD)
Use_gas_cooker_wekend <- scale(Use_gas_cooker_wekend, center = Use_gas_cooker_wekend_trainMean, scale = Use_gas_cooker_wekend_trainSD)
detach(test_set)

attach(train_set)

# Note: In the text, models labeled as 1A–1D are referred to here as 2A–2D.
# Similarly, model 2 in the text is referred to as model 3 here,
# and model 3 in the text is referred to as model 6 below.

# Fitting GLMs

### GLM - Gaussian distribution and identity link 
glm_model2a <- glm(CPAH ~ FL + urban + New_Carpet + Gas_main_heating+ Photocopying 
                   + Summer  + Open_plan + Laminated_floor + Aerosol_use + Air_freshner + Redecoration + New + stqhmephc + stqhmeik  + suburban 
                   + IG  + WM  + recent_carpet_or_lino + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                     Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + ETS_home
                   + VVOC + Time_travelling  + Distance_PetrolStation + Use_gas_cooker + Time_cooking + Time_ETS
                   , data=train_set, family=gaussian()) 

summary(glm_model2a)
par(mfrow = c(2, 2))


glm_model2a_devexpl <- (glm_model2a$null.deviance - glm_model2a$deviance)/ glm_model2a$null.deviance
glm_model2a_aic <- glm_model2a$aic
#glm_model2a_rsq <- glm_model2a$r.sq




glm_model2b <- glm(CPAH ~ TR +  Electricity_main_heating  + Photocopying
                   + Summer+ Open_plan  + Laminated_floor + Aerosol_use + Air_freshner +
                     Redecoration + New + stqhmephc + stqhmeik  + suburban + IG  + WM  + recent_carpet_or_lino +   + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                     + Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + ETS + ETS_home + rural
                   + Distance_PetrolStation  + Time_travelling + BTEX + whichfloorisflatlocated + Use_gas_cooker_wekend + Time_cooking + New_Carpet, data=train_set)
summary(glm_model2b)
glm_model2b_devexpl <- (glm_model2b$null.deviance - glm_model2b$deviance)/ glm_model2b$null.deviance
glm_model2b_aic <- glm_model2b$aic


glm_model2c <- glm(CPAH ~ VOC + FL + urban  + New_Carpet + Electricity_main_heating   
                   + Time_ETS + Distance_PetrolStation + Time_travelling + Photocopying
                   + Summer + Open_plan  + Laminated_floor + Aerosol_use + Air_freshner + ETS_home
                   + Redecoration + New + stqhmephc + stqhmeik  + suburban + IG  + WM  + recent_carpet_or_lino 
                   + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                     Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + Time_cooking + Use_gas_cooker, data=train_set)

summary(glm_model2c) 
glm_model2c_devexpl <- (glm_model2c$null.deviance - glm_model2c$deviance)/ glm_model2c$null.deviance
glm_model2c_aic <- glm_model2c$aic


glm_model2d <- glm(CPAH ~ Total_VOC + FL + urban  + New_Carpet 
                   + Time_ETS + Electricity_main_heating   + Distance_PetrolStation + Time_travelling + Photocopying 
                   + Summer + Open_plan  + Laminated_floor + Aerosol_use + Air_freshner + ETS_home +
                     Redecoration + New + stqhmephc + stqhmeik  + suburban + IG  + WM  + recent_carpet_or_lino + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                     Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + Time_cooking + Use_gas_cooker, data=train_set)

summary(glm_model2d)
glm_model2d_devexpl <- (glm_model2d$null.deviance - glm_model2d$deviance)/ glm_model2d$null.deviance
glm_model2d_aic <- glm_model2d$aic

detach(train_set)

# Predictions
predicted2a <- predict(glm_model2a, test_set, type = "response")
rmse_2a <- sqrt(mean((test_set$CPAH - predicted2a)^2))

predicted2b <- predict(glm_model2b, test_set)
rmse_2b <- sqrt(mean((test_set$CPAH - predicted2b)^2))

predicted2c <- predict(glm_model2c, test_set)
rmse_2c <- sqrt(mean((test_set$CPAH - predicted2c)^2))


predicted2d <- predict(glm_model2d, test_set)
rmse_2d <- sqrt(mean((test_set$CPAH - predicted2d)^2))

### GLM -  Gaussian distribution and log link

attach(train_set)
glm_model3a <- glm(CPAH ~ FL + urban + New_Carpet + Gas_main_heating+ Photocopying 
                   + Summer  + Open_plan + Laminated_floor + Aerosol_use + Air_freshner + Redecoration + New + stqhmephc + stqhmeik  + suburban 
                   + IG  + WM  + recent_carpet_or_lino + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                     Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + ETS_home
                   + VVOC + Time_travelling  + Distance_PetrolStation+ Use_gas_cooker + Time_cooking + Time_ETS
                   , data=train_set, family=gaussian(link=log)) 
summary(glm_model3a)
glm_model3a_devexpl <- (glm_model3a$null.deviance - glm_model3a$deviance)/ glm_model3a$null.deviance
glm_model3a_aic <- glm_model3a$aic


glm_model3b <- glm(CPAH ~ TR +  Electricity_main_heating  + Photocopying
                   + Summer+ Open_plan  + Laminated_floor + Aerosol_use + Air_freshner +
                     Redecoration + New + stqhmephc + stqhmeik  + suburban + IG  + WM  + recent_carpet_or_lino +   + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                     + Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + ETS + ETS_home + rural
                   + Distance_PetrolStation  + Time_travelling + BTEX+ whichfloorisflatlocated + Use_gas_cooker_wekend + Time_cooking+ New_Carpet, data=train_set, family=gaussian(link=log))

summary(glm_model3b)
glm_model3b_devexpl <- (glm_model3b$null.deviance - glm_model3b$deviance)/ glm_model3b$null.deviance
glm_model3b_aic <- glm_model3b$aic


glm_model3c <- glm(CPAH ~ VOC + FL + urban  + New_Carpet   
                   + Time_ETS + Electricity_main_heating   + Distance_PetrolStation + Time_travelling + Photocopying
                   + Summer + Open_plan  + Laminated_floor + Aerosol_use + Air_freshner + ETS_home
                   +  Redecoration + New + stqhmephc + stqhmeik  + suburban + IG  + WM  + recent_carpet_or_lino 
                   + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                     Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + Time_cooking + Use_gas_cooker, data=train_set, family=gaussian(link=log))
summary(glm_model3c)
glm_model3c_devexpl <- (glm_model3c$null.deviance - glm_model3c$deviance)/ glm_model3c$null.deviance
glm_model3c_aic <- glm_model3c$aic

glm_model3d <- glm(CPAH ~ Total_VOC + FL + urban  + New_Carpet 
                   + Time_ETS + Electricity_main_heating   + Distance_PetrolStation + Time_travelling + Photocopying 
                   + Summer + Open_plan  + Laminated_floor + Aerosol_use + Air_freshner + ETS_home +
                     Redecoration + New + stqhmephc + stqhmeik  + suburban + IG  + WM  + recent_carpet_or_lino + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                     Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + Time_cooking + Use_gas_cooker, data=train_set,family=gaussian(link=log))
summary(glm_model3d)
glm_model3d_devexpl <- (glm_model3d$null.deviance - glm_model3d$deviance)/ glm_model3d$null.deviance
glm_model3d_aic <- glm_model3d$aic

detach(train_set)

# Predictions
predicted3a <- predict(glm_model3a, test_set, type="response")
rmse_3a <- sqrt(mean((test_set$CPAH - predicted3a)^2))

predicted3b <- predict(glm_model3b, test_set, type="response")
rmse_3b <- sqrt(mean((test_set$CPAH - predicted3b)^2))

predicted3c <- predict(glm_model3c, test_set, type="response")
rmse_3c <- sqrt(mean((test_set$CPAH - predicted3c)^2))


predicted3d <- predict(glm_model3d, test_set, type="response")
rmse_3d <- sqrt(mean((test_set$CPAH - predicted3d)^2))



### GLM -  gamma distribution and log link
attach(train_set)
glm_model6a <- glm(CPAH ~ FL + urban + New_Carpet + Gas_main_heating+ Photocopying 
                   + Summer  + Open_plan + Laminated_floor + Aerosol_use + Air_freshner + Redecoration + New + stqhmephc + stqhmeik  + suburban 
                   + IG  + WM  + recent_carpet_or_lino + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                     Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + ETS_home
                   + VVOC + Time_travelling  + Distance_PetrolStation+ Use_gas_cooker + Time_cooking  + Time_ETS
                   , data=train_set, family=Gamma( link = "log" ))
summary(glm_model6a)

glm_model6a_devexpl <- (glm_model6a$null.deviance - glm_model6a$deviance)/ glm_model6a$null.deviance
glm_model6a_aic <- glm_model6a$aic

glm_model6b <- glm(CPAH ~ TR +  Electricity_main_heating  + Photocopying
                   + Summer+ Open_plan  + Laminated_floor + Aerosol_use + Air_freshner +
                     Redecoration + New + stqhmephc + stqhmeik  + suburban + IG  + WM  + recent_carpet_or_lino + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                     Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + ETS+ ETS_home + rural
                   + Distance_PetrolStation + Time_travelling  + BTEX+ whichfloorisflatlocated + Use_gas_cooker_wekend + Time_cooking+ New_Carpet
                   , data=train_set,family=Gamma(link = "log"))

summary(glm_model6b)
glm_model6b_devexpl <- (glm_model6b$null.deviance - glm_model6b$deviance)/ glm_model6b$null.deviance
glm_model6b_aic <- glm_model6b$aic



glm_model6c <- glm(CPAH ~ VOC + FL + urban  + New_Carpet   
                   + Time_ETS + Electricity_main_heating   + Distance_PetrolStation + Time_travelling + Photocopying
                   + Summer + Open_plan  + Laminated_floor + Aerosol_use + Air_freshner + ETS_home
                   +  Redecoration + New + stqhmephc + stqhmeik  + suburban + IG  + WM  + recent_carpet_or_lino 
                   + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                     Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + Time_cooking + Use_gas_cooker , data=train_set, family=Gamma(link="log"))
summary(glm_model6c)
glm_model6c_devexpl <- (glm_model6c$null.deviance - glm_model6c$deviance)/ glm_model6c$null.deviance
glm_model6c_aic <- glm_model6c$aic

glm_model6d <- glm(CPAH ~ Total_VOC + FL + urban  + New_Carpet   
                   + Time_ETS + Electricity_main_heating   + Distance_PetrolStation + Time_travelling + Photocopying
                   + Summer + Open_plan  + Laminated_floor + Aerosol_use + Air_freshner + ETS_home
                   +  Redecoration + New + stqhmephc + stqhmeik  + suburban + IG  + WM  + recent_carpet_or_lino 
                   + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                     Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + Time_cooking + Use_gas_cooker , data=train_set, family=Gamma(link="log"))
summary(glm_model6d)
glm_model6d_devexpl <- (glm_model6d$null.deviance - glm_model6d$deviance)/ glm_model6d$null.deviance
glm_model6d_aic <- glm_model6d$aic


detach(train_set)

# Predictions
predicted6a <- predict(glm_model6a, test_set, type="response")
rmse_6a <- sqrt(mean((test_set$CPAH - predicted6a)^2))

predicted6b <- predict(glm_model6b, test_set, type="response")
rmse_6b <- sqrt(mean((test_set$CPAH - predicted6b)^2))

predicted6c <- predict(glm_model6c, test_set, type="response")
rmse_6c <- sqrt(mean((test_set$CPAH - predicted6c)^2))


predicted6d <- predict(glm_model6d, test_set, type="response")
rmse_6d <- sqrt(mean((test_set$CPAH - predicted6d)^2))


# Note: In the text, models labeled as 1A–1D are referred to here as 2A–2D.
# Similarly, model 2 in the text is referred to as model 3 here,
# and model 3 in the text is referred to as model 6 below.
# Fitting GAMs

### GAMs using REML estimation, Gaussian distribution and identity link
attach(train_set)

model2a <- gam(CPAH ~ FL + urban + New_Carpet + Gas_main_heating+ Photocopying + Distance_PetrolStation +Time_travelling
               + Summer  + Open_plan + Laminated_floor + Aerosol_use + Air_freshner + Redecoration + New + stqhmephc + stqhmeik  + suburban 
               + IG  + WM  + recent_carpet_or_lino + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                 Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + ETS_home
               + s(VVOC) + Time_ETS + Use_gas_cooker + Time_cooking
               , data=train_set, family=gaussian(), method="REML", select=TRUE) 

summary(model2a)
coef(model2a)
set.seed(123)
gam.check(model2a)
concurvity(model2a)
concurvity(model2a, full=FALSE)
model2a_devexpl <- summary.gam(model2a)$dev.expl
model2a_aic <- AIC(model2a)



model2b <- gam(CPAH ~ TR +  Electricity_main_heating  + Photocopying + Distance_PetrolStation  + Time_travelling 
               + Summer+ Open_plan  + Laminated_floor + Aerosol_use + Air_freshner +
                 Redecoration + New + stqhmephc + stqhmeik  + suburban + IG  + WM  + recent_carpet_or_lino +   + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                 + Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + ETS + ETS_home + rural +
                 s(BTEX) + Use_gas_cooker_wekend + Time_cooking + whichfloorisflatlocated + New_Carpet
               , data=train_set, method="REML", select=TRUE)

summary(model2b)
coef(model2b)
set.seed(123)
gam.check(model2b)
model2b_devexpl <- summary.gam(model2b)$dev.expl
model2b_aic <- AIC(model2b)
concurvity(model2b)
concurvity(model2b, full=FALSE)

model2c <- gam(CPAH ~ s(VOC) + FL + urban  + New_Carpet + Distance_PetrolStation + Time_travelling
               + Time_ETS + Electricity_main_heating + Photocopying
               + Summer + Open_plan  + Laminated_floor + Aerosol_use + Air_freshner + ETS_home
               +  Redecoration + New + stqhmephc + stqhmeik  + suburban + IG  + WM  + recent_carpet_or_lino + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                 Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + Time_cooking + Use_gas_cooker, data=train_set, method="REML", select=TRUE)

summary(model2c)
coef(model2c)
set.seed(123)
gam.check(model2c)
model2c_devexpl <- summary.gam(model2c)$dev.expl
model2c_aic <- AIC(model2c)
concurvity(model2c)
concurvity(model2c, full=FALSE)



model2d <- gam(CPAH ~ s(Total_VOC) + FL + urban  + New_Carpet 
               + Time_ETS + Electricity_main_heating   + Distance_PetrolStation + Time_travelling + Photocopying 
               + Summer + Open_plan  + Laminated_floor + Aerosol_use + Air_freshner + ETS_home +
                 Redecoration + New + stqhmephc + stqhmeik  + suburban + IG  + WM  + recent_carpet_or_lino + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                 Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + Time_cooking + Use_gas_cooker, data=train_set, method="REML", select=TRUE)
summary(model2d)
simulationOutput_model2d <- simulateResiduals(fittedModel = model2d, plot = T, refit = F)
coef(model2d)
model2d$sp
set.seed(123)
gam.check(model2d)
model2d_devexpl <- summary.gam(model2d)$dev.expl
model2d_aic <- AIC(model2d)
concurvity(model2d)

detach(train_set)

# Predictions
predicted2a <- predict.gam(model2a, test_set)
rmse_2a <- sqrt(mean((test_set$CPAH - predicted2a)^2))

predicted2b <- predict.gam(model2b, test_set)
rmse_2b <- sqrt(mean((test_set$CPAH - predicted2b)^2))

predicted2c <- predict.gam(model2c, test_set)
rmse_2c <- sqrt(mean((test_set$CPAH - predicted2c)^2))

predicted2d <- predict.gam(model2d, test_set)
rmse_2d <- sqrt(mean((test_set$CPAH - predicted2d)^2))

### GAMs using REML estimation, Gaussian distribution and log link

attach(train_set)
model3a <- gam(CPAH ~ FL + urban + New_Carpet + Gas_main_heating+ Photocopying 
               + Summer  + Open_plan + Laminated_floor + Aerosol_use + Air_freshner + Redecoration + New + stqhmephc + stqhmeik  + suburban 
               + IG  + WM  + recent_carpet_or_lino + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                 Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + ETS_home
               + s(VVOC) + Distance_PetrolStation + Time_travelling + Time_ETS + Use_gas_cooker + Time_cooking
               , data=train_set, family=gaussian(link=log), method="REML", select=TRUE) 


summary(model3a)
coef(model3a)
par(mfrow = c(2, 2))
set.seed(123)
gam.check(model3a)
model3a_devexpl <- summary.gam(model3a)$dev.expl
model3a_aic <- AIC(model3a)
concurvity(model3a)
concurvity(model3a, full=FALSE)



model3b <- gam(CPAH ~ TR +  Electricity_main_heating  + Photocopying
               + Summer+ Open_plan  + Laminated_floor + Aerosol_use + Air_freshner +
                 Redecoration + New + stqhmephc + stqhmeik  + suburban + IG  + WM  + recent_carpet_or_lino + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                 Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + ETS + ETS_home + rural
               + Distance_PetrolStation  + Time_travelling + s(BTEX) + Use_gas_cooker_wekend + Time_cooking + whichfloorisflatlocated+ New_Carpet
               , data=train_set, family=gaussian(link=log), method="REML", select=TRUE )

summary(model3b)
coef(model3b)
set.seed(123)
gam.check(model3b)
model3b_devexpl <- summary.gam(model3b)$dev.expl
model3b_aic <- AIC(model3b)
concurvity(model3b)
concurvity(model3b, full=FALSE)



model3c <- gam(CPAH ~ s(VOC) + FL + urban  + New_Carpet   
               + Time_ETS + Electricity_main_heating   + Distance_PetrolStation + Time_travelling + Photocopying 
               + Summer + Open_plan  + Laminated_floor + Aerosol_use + Air_freshner + ETS_home
               + Redecoration + New + stqhmephc + stqhmeik  + suburban + IG  + WM  + recent_carpet_or_lino + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                 Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + Time_cooking + Use_gas_cooker, data=train_set, family=gaussian(link=log), method="REML", select=TRUE)
summary(model3c)
coef(model3c)
set.seed(123)
gam.check(model3c) 
model3c_devexpl <- summary.gam(model3c)$dev.expl
model3c_aic <- AIC(model3c)
concurvity(model3c)
concurvity(model3c, full=FALSE)


model3d <- gam(CPAH ~ s(Total_VOC) + FL + urban  + New_Carpet 
               + Time_ETS + Electricity_main_heating   + Distance_PetrolStation + Time_travelling + Photocopying 
               + Summer + Open_plan  + Laminated_floor + Aerosol_use + Air_freshner + ETS_home +
                 Redecoration + New + stqhmephc + stqhmeik  + suburban + IG  + WM  + recent_carpet_or_lino + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                 Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + Time_cooking + Use_gas_cooker, data=train_set,family=gaussian(link=log), method="REML", select=TRUE)

summary(model3d)
simulationOutput_model3d <- simulateResiduals(fittedModel = model3d, plot = T, refit = F)
coef(model3d)
model3d$sp
set.seed(123)
gam.check(model3d)
model3d_devexpl <- summary.gam(model3d)$dev.expl
model3d_aic <- AIC(model3d)
concurvity(model3d)

detach(train_set)

# Predictions
predicted3a <- predict.gam(model3a, test_set, type="response")
rmse_3a <- sqrt(mean((test_set$CPAH - predicted3a)^2))

predicted3b <- predict.gam(model3b, test_set, type="response")
rmse_3b <- sqrt(mean((test_set$CPAH - predicted3b)^2))

predicted3c <- predict.gam(model3c, test_set, type="response")
rmse_3c <- sqrt(mean((test_set$CPAH - predicted3c)^2))


predicted3d <- predict.gam(model3d, test_set, type="response")
rmse_3d <- sqrt(mean((test_set$CPAH - predicted3d)^2))


### GAMs using REML estimation, gamma distribution and log link

attach(train_set)


model6a <- gam(CPAH ~ FL + urban + New_Carpet + Gas_main_heating+ Photocopying 
               + Summer  + Open_plan + Laminated_floor + Aerosol_use + Air_freshner + Redecoration + New + stqhmephc + stqhmeik  + suburban 
               + IG  + WM  + recent_carpet_or_lino + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                 Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + ETS_home
               + s(VVOC) + Distance_PetrolStation + Time_travelling + Time_ETS + Use_gas_cooker + Time_cooking
               , data=train_set, family=Gamma( link = "log" ), method="REML", select=TRUE) 
summary(model6a)
coef(model6a)
model6a$sp
par(mfrow = c(2, 2))
set.seed(123)
gam.check(model6a)
model6a_devexpl <- summary.gam(model6a)$dev.expl
model6a_aic <- AIC(model6a)

concurvity(model6a)
concurvity(model6a, full=FALSE)



model6b <- gam(CPAH ~ TR +  Electricity_main_heating  + Photocopying
               + Summer+ Open_plan  + Laminated_floor + Aerosol_use + Air_freshner +
                 Redecoration + New + stqhmephc + stqhmeik  + suburban + IG  + WM  + recent_carpet_or_lino + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                 Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + ETS+ ETS_home + rural
               + Distance_PetrolStation  + Time_travelling  + s(BTEX) + Use_gas_cooker_wekend + Time_cooking + whichfloorisflatlocated+ New_Carpet
               , data=train_set,family=Gamma(link = "log"), method="REML" , select=TRUE)

summary(model6b)
coef(model6b)
model6b$sp
set.seed(123)
gam.check(model6b)
model6b_devexpl <- summary.gam(model6b)$dev.expl
model6b_aic <- AIC(model6b)
concurvity(model6b)

model6c <- gam(CPAH ~ s(VOC) + FL + urban  + New_Carpet 
               + Time_ETS + Electricity_main_heating   + Distance_PetrolStation + Time_travelling + Photocopying 
               + Summer + Open_plan  + Laminated_floor + Aerosol_use + Air_freshner + ETS_home +
                 Redecoration + New + stqhmephc + stqhmeik  + suburban + IG  + WM  + recent_carpet_or_lino + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                 Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + Time_cooking + Use_gas_cooker, data=train_set, family=Gamma( link = "log" ), method="REML", select=TRUE)



summary(model6c)
coef(model6c)
model6c$sp
set.seed(123)
gam.check(model6c)
model6c_devexpl <- summary.gam(model6c)$dev.expl
model6c_aic <- AIC(model6c)



concurvity(model6c)
concurvity(model6c, full=FALSE)



model6d <- gam(CPAH ~ s(Total_VOC) + FL + urban  + New_Carpet 
               + Time_ETS + Electricity_main_heating   + Distance_PetrolStation + Time_travelling + Photocopying 
               + Summer + Open_plan  + Laminated_floor + Aerosol_use + Air_freshner + ETS_home +
                 Redecoration + New + stqhmephc + stqhmeik  + suburban + IG  + WM  + recent_carpet_or_lino + Car_in_garage + Use_Bus + Walk_busy_road  + Petrol_car_garage + 
                 Not_Connected   + SUM_Additional_Heating + Sometimes_cooker_hood + Time_cooking + Use_gas_cooker, data=train_set, family=Gamma( link = "log" ), method="REML", select=TRUE)


summary(model6d)
coef(model6d)
model6d$sp
set.seed(123)
gam.check(model6d)
model6d_devexpl <- summary.gam(model6d)$dev.expl
model6d_aic <- AIC(model6d)

plot(data$Distance_PetrolStation, data$Time_travelling)
plot(data$Distance_PetrolStation, data$Total_VOC)
plot(data$Time_travelling, data$Total_VOC)

concurvity(model6d)
concurvity(model6d, full=FALSE)


detach(train_set)

# Predictions
predicted6a <- predict.gam(model6a, test_set, type="response")
rmse_6a <- sqrt(mean((test_set$CPAH - predicted6a)^2))

predicted6b <- predict.gam(model6b, test_set, type="response")
rmse_6b <- sqrt(mean((test_set$CPAH - predicted6b)^2))

predicted6c <- predict.gam(model6c, test_set, type="response")
rmse_6c <- sqrt(mean((test_set$CPAH - predicted6c)^2))

test_set$Total_VOC <- test_set$VVOC +  test_set$VOC +  test_set$BTEX
predicted6d <- predict.gam(model6d, test_set, type="response")
rmse_6d <- sqrt(mean((test_set$CPAH - predicted6d)^2))






#2. Generalized Linear Models (GLMs) - If a Gamma or Inverse Gaussian distribution is used for modeling a continuous response, under-dispersion means that the estimated variance is smaller than predicted by the model.
# This can lead to underestimated standard errors, making confidence intervals too narrow and p-values artificially small.
simulationOutput_model3a <- simulateResiduals(fittedModel = model3a, plot = T, n=4000) # if n is not set, outer newton does not converge error

# This function tests if the number of observations outside the simulatio envelope are larger or smaller than expected
testOutliers(simulationOutput_model3a)

# This function performs simulation-based tests for over/underdispersion.
testDispersion(simulationOutput_model3a)

# The function fits quantile regressions (via package qgam) on the residuals, and compares their location to the expected location (because of the uniform distributionm, the expected location is 0.5 for the 0.5 quantile).
# A significant p-value for the splines means the fitted spline deviates from a flat line at the expected location (p-values of intercept and spline are combined via Benjamini & Hochberg adjustment to control the FDR)
testQuantiles(simulationOutput_model3a)

testDispersion(simulationOutput_model3a, alternative = "less", plot = FALSE) # only under dispersion
testDispersion(simulationOutput_model3a, alternative = "greater", plot = FALSE) # only over dispersion

simulationOutput_model3b <- simulateResiduals(fittedModel = model3b, plot = T)
testOutliers(simulationOutput_model3b)
testDispersion(simulationOutput_model3b)
testQuantiles(simulationOutput_model3b) 

simulationOutput_model3c <- simulateResiduals(fittedModel = model3c, plot = T)
testOutliers(simulationOutput_model3c)
testDispersion(simulationOutput_model3c)
testQuantiles(simulationOutput_model3c)



sessionInfo(package = NULL)


summary(model2a)
summary(model3a)
summary(model6a)

summary(model2b)
summary(model3b)
summary(model6b)

summary(model2c)
summary(model3c)
summary(model6c)

summary(model2d)
summary(model3d)
summary(model6d)

model3a$scale
model3b$scale
model3c$scale
model3d$scale

# Parsimonous Models

parsimonous_model3a <- gam(CPAH ~ Summer + Aerosol_use + Air_freshner + Use_Bus   + Petrol_car_garage + 
                             + ETS_home + s(VVOC) + Time_ETS, data=train_set, family=gaussian(link=log), method="REML", select=TRUE) 
summary(parsimonous_model3a)
parsionous3aAIC <- AIC(parsimonous_model3a)
parsimonous_model3a_devexpl <- summary.gam(parsimonous_model3a)$dev.expl
predictedpar3a <- predict.gam(parsimonous_model3a, test_set, type="response")
rmse_parsimonous3a <- sqrt(mean((test_set$CPAH - predictedpar3a)^2))


#  model 3b  parsimonous
model3bpar <- gam(CPAH ~ Summer   + Aerosol_use  + Redecoration + WM  + recent_carpet_or_lino     + Petrol_car_garage 
                  + Sometimes_cooker_hood + ETS      + s(BTEX) + Use_gas_cooker_wekend  
                  , data=train_set, family=gaussian(link=log), method="REML", select=TRUE )

summary(model3bpar)
parsionous3bAIC <- AIC(model3bpar)
parsimonous_model3b_devexpl <- summary.gam(model3bpar)$dev.expl
predictedpar3b <- predict.gam(model3bpar, test_set, type="response")
rmse_parsimonous3b <- sqrt(mean((test_set$CPAH - predictedpar3b)^2))

#  model 3c  parsimonous
model3cpar <- gam(CPAH ~ s(VOC)    + New_Carpet   
                  + Time_ETS       
                  + Summer    + Aerosol_use + Air_freshner + ETS_home
                  + Redecoration  + stqhmephc  + WM    + Use_Bus   + Petrol_car_garage 
                  + Use_gas_cooker, data=train_set, family=gaussian(link=log), method="REML", select=TRUE)

summary(model3cpar)

parsionous3cAIC <- AIC(model3cpar)
parsimonous_model3c_devexpl <- summary.gam(model3cpar)$dev.expl
predictedpar3c <- predict.gam(model3cpar, test_set, type="response")
rmse_parsimonous3c <- sqrt(mean((test_set$CPAH - predictedpar3c)^2))

#  model 3d parsimonous

model3dpar <- gam(CPAH ~ s(Total_VOC) + Time_ETS  + Summer + Air_freshner + ETS_home + stqhmeik  + Car_in_garage + Use_Bus, data=train_set,family=gaussian(link=log), method="REML", select=TRUE)
summary(model3dpar)
parsionous3dAIC <- AIC(model3dpar)
parsimonous_model3d_devexpl <- summary.gam(model3dpar)$dev.expl
predictedpar3d <- predict.gam(model3dpar, test_set, type="response")
rmse_parsimonous3d <- sqrt(mean((test_set$CPAH - predictedpar3d)^2))