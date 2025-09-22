rm(list=ls()) ## PREPARING ENVIRONMENT

#INSTALLING AND OPENING LIBRARIES
#install.packages("glmnet")
#install.packages("freqparcoord")
#install.packages("randomForest")
#install.packages("dplyr")#Used for case_when
library(dplyr)
library("freqparcoord")
library(car)
library(glmnet)
library(randomForest)
library(caret)
library(OptimalCutpoints)

##########PREPARING DATA####################

#LOADING DATASET
data(prgeng)

#INVESTIGATING INITIAL DATASET
View(prgeng)


#ASSIGNING DATASET A NAME
data1<-prgeng

str(data1)

# WE HAVE 2 NUMERICAL AND 8 CATEGORICAL PREDICTORS

#-----------------------------------------------------------------------------------------
#CONVERTING CATEGORIES PREDICTORS

##1.CONVERTING SEX
data1$sex <- factor(prgeng$sex, levels = c(1, 2), labels = c("Male", "Female"))
str(data1)
summary(data1$sex)
table(data1$sex)

##2.CONVERTING CITIZENSHIP
data1$cit <- factor(prgeng$cit, levels = c(1, 2,3,4,5), labels = c("Born in US", "Born in US territory","Born abroad to US citizens","Naturalized Citizen","Not a Citizen"))
str(data1)
summary(data1$cit)
table(data1$cit)

##3.CONVERTING EDUCATION
data1$educ <- factor(
  prgeng$educ,
  levels = c(1:16),  # All possible numeric levels in the dataset
  labels = c(
    rep("No college", 9),      # 1-9 are "No college"
    rep("Some college", 3),    # 10-12 are "Some college"
    "Bachelor’s degree",       # 13
    "Master’s degree",         # 14
    "Professional degree",     # 15
    "Doctorate"                # 16
  )
)
summary(data1$educ)
str(data1)
table(data1$educ)


##4.CONVERTING OCCUPATION
data1$occ <- factor(prgeng$occ, levels = c(100,101,102,106,140,141), labels = c("Computer Scientists and Systems Analysts", "Computer Programmers
","Computer Software Engineer","Database Administrators","Computer Hardware Engineers","Electrical and Electronics Engineers
"))
summary(data1$occ)
str(data1)
table(data1$occ)


##5.CONVERTING BIRTHPLACE
# Create a mapping of codes to categories
data1$birth <- case_when(
  data1$birth %in% 1:59 ~ "United States",
  data1$birth %in% 60:99 ~ "U.S. Island Areas",
  data1$birth %in% c(100:157, 160, 162:199) ~ "Europe",
  data1$birth %in% c(158, 159, 161, 200:299) ~ "Asia",
  data1$birth %in% 300:399 ~ "Latin America",
  data1$birth %in% 400:499 ~ "Africa",
  data1$birth %in% 500:553 ~ "Oceania",
  data1$birth %in% 554:999 ~ "At Sea/Abroad",
  TRUE ~ "Unknown"
)
summary(data1$birth)
str(data1$birth)
table(data1$birth)

##6.CONVERTING PLACE OF WORK
data1$powspuma <- ifelse(data1$powspuma >= 6010 & data1$powspuma <= 6700, "California", "Other")
summary(data1$powspuma)
hist(data1$powspuma)
table(data1$powspuma)


##7.CONVERTING YEAR OF ENTRY
data1$yrentry<- ifelse(data1$yrentry == 0, "Native", 
                       ifelse(data1$yrentry <= 1950, "Before 1950", 
                              ifelse(data1$yrentry <= 1960, "1951-1960",
                                     ifelse(data1$yrentry <= 1970, "1961-1970",
                                            ifelse(data1$yrentry <= 1980, "1971-1980",
                                                   ifelse(data1$yrentry <= 1990, "1981-1990",
                                                          ifelse(data1$yrentry <= 2000, "1991-2000", "2001+")))))))

summary(data1$yrentry)
hist(data1$yrentry)
table(data1$yrentry)


##8.ENGLISH PROFICIENCY
data1$engl <- ifelse(data1$engl == 0, "Under 5years or they speak", 
                     ifelse(data1$engl %in% c(10, 11), "Speak Very Well", 
                            ifelse(data1$engl %in% c(20, 21), "Speak Well",
                                   ifelse(data1$engl %in% c(30, 31), "Speak Not Well",
                                          ifelse(data1$engl %in% c(40, 41), "Speak Not at all", 
                                                 NA)))))




summary(data1$engl)
hist(data1$engl)
table(data1$engl)

#-------------------------------------------------------------------------------------
#INVESTIGATING AND REMOVING LOGICAL ERRORS 

##1.WAGEINCOME: MADE RANGE BETWEEN 30K and 170K
data1 <- subset(data1, !(data1$wageinc %in% c(0:29999, 174999:325000)))

##2.PLACE OF WORK:ELIMINATING DATA ERRORS-->0 AND 1
table(data1$powspuma)
data1 <- subset(data1, !(data1$powspuma %in% c(0, 1)))#CODE DID NOT WORK 

##3.WEEKS WORKED: ELMINATING WEEKS WITH VALUE '0'
data1<- data1[!(data1$wkswrkd == 0), ]

##4.AGE: INVALID AGE 
data1<-data1[!(data1$age < 15 | data1$age > 100), ]

##5.EDUCATION: CHECK LEVELS
unique(data1$educ)
table(data1$educ)

##6.ENGLISH PROFICIENCY: CHECK LEVELS
unique(data1$engl)
table(data1$engl)

##7.OCCUPATION: CHECK LEVELS
unique(data1$occ)
table(data1$occ)

##8.YEAR OF ENTRY: CHECK LEVELS
unique(data1$yrentry)
table(data1$yrentry)

##9.CITIZENSHIP: CHECK LEVELS
unique(data1$cit)
table(data1$cit)

##10.BIRTHPLACE: CHECK LEVELS
unique(data1$birth)
table(data1$birth)


##11. CHECK FOR ROWS WITH MISSING VALUES 
missing_rows <- data1[is.na(data1$wageinc) | is.na(data1$age) | 
                                             is.na(data1$cit) | 
                                             is.na(data1$educ) | 
                                             is.na(data1$engl) | 
                                             is.na(data1$occ) | 
                                             is.na(data1$birth) | 
                                             is.na(data1$wkswrkd) | 
                                             is.na(data1$yrentry) | 
                                             is.na(data1$powspuma) | 
                                             is.na(data1$sex), ]

# View the rows with missing values
print(missing_rows)

#--------------------------------------------------------------------------
#SCATTERPLOT OF DATASET

pairs(data1)
#Error in pairs.default(data1) : non-numeric argument to 'pairs'

#CONVERTING ISSUE FACING CATEGORICAL VARIABLES TO FACTORS 
data1$birth <- as.factor(data1$birth)
data1$yrentry <- as.factor(data1$yrentry)  # or as.numeric(), depending on context
data1$powspuma <- as.factor(data1$powspuma)
data1$engl <- as.factor(data1$engl)

pairs(data1)#NOW IT WORKS!

attach(data1)
#-------------------------------------------
#Histogram of each predictor

hist(data1$age)
hist(data1$cit)#Error: not numeric
hist(data1$educ)#Error: not numeric
hist(data1$engl)#Error: not numeric
hist(data1$occ)#Error: not numeric
hist(data1$birth)#Error: not numeric
hist(data1$sex)#Error: not numeric
hist(data1$wageinc)
hist(data1$wkswrkd)
hist(data1$yrentry)#Error: not numeric
hist(data1$powspuma)#Error: not numeric


#----------------------------------------------
#Check marginal relation for multicollinearity

cor(data1$age,data1$wkswrkd)#only numericals

subset_data<-data1[,c(2:7,10:11)]#only factors included

pairs(subset_data,main="Scatter Plot Matrix")

#----------------------------------------------------
#FIT THE FULL MODEL AND CHECK MULTICOLLINEARITY

###################FULL MODEL################################ 
m<-lm(wageinc~age+cit+educ+engl+occ+birth+sex+wkswrkd+yrentry+powspuma,data=data1)

summary(m)
vif(m)#Error in vif.default(m) : there are aliased coefficients in the model

m1<-m

#SINCE THE PVALUE OF PREDICTORS BIRTH AND YEAR OF ENTRY ARE HIGH WE ARE ELIMINATING THEM!

# 1. Scale-Location Plot
plot(m1, which = 3)  
# 2. Normal Q-Q Plot
plot(m1, which = 2)  

# 3. Residuals vs Fitted
plot(m1, which = 1)  

# 4. Leverage vs Residuals (Cook's Distance)
plot(m1, which = 5)  


#######################REDUCED MODEL 1###################### 
m2<-lm(wageinc~age+cit+educ+engl+occ+sex+wkswrkd+powspuma,data=data1)

summary(m2)# HERE powspuma is Marginally non-significant (p = 0.0947), SO MORE JUSTIFICATION NEEDED TO ELIMINATE IT.
vif(m2)#Since no VIF values exceed 5, multicollinearity is not a concern in this model.

# 1. Scale-Location Plot
plot(m2, which = 3)  

# 2. Normal Q-Q Plot
plot(m2, which = 2)  
# 3. Residuals vs Fitted
plot(m2, which = 1) 

# 4. Leverage vs Residuals (Cook's Distance)
plot(m2, which = 5) 

#############REDUCED MODEL 2#############################
m3<-lm(wageinc~age+cit+educ+engl+occ+sex+wkswrkd,data=data1)

summary(m3)
vif(m3)

# 1. Scale-Location Plot
plot(m3, which = 3)  

# 2. Normal Q-Q Plot
plot(m3, which = 2)  

# 3. Residuals vs Fitted
plot(m3, which = 1)  
# 4. Leverage vs Residuals (Cook's Distance)
plot(m3, which = 5)  

###########Compare adjusted R-squared###########
summary(m2)$adj.r.squared
summary(m3)$adj.r.squared
#The adjusted R² barely changes after removing powspuma. This indicates that powspuma 
#does not meaningfully improve the model's ability to explain variability in wageinc.

# Compare AIC/BIC
AIC(m2)
AIC(m3)
#The AIC is slightly lower for m2 by just 0.8 points. This difference is negligible and
#suggests no meaningful advantage in retaining powspuma in the model.

BIC(m2)
BIC(m3)
#The BIC is slightly lower for m3 by about 6.8 points. A lower BIC favors simpler models, 
#indicating that removing powspuma may be better in terms of model parsimony


#Removing powspuma simplifies the model, reduces potential overfitting, and has negligible
#impact on the model’s explanatory power

#THUS, WE REMOVE PLACE OF WORK, powspuma

##############REDUCED MODEL 3#######################
m4<-lm(wageinc~age+cit+educ+engl+occ+sex+wkswrkd,data=data1)
#MODEL m4 exclude the less significant predictors while still maintaining good performance


# 1. Scale-Location Plot
plot(m4, which = 3) 

# 2. Normal Q-Q Plot
plot(m4, which = 2) 

# 3. Residuals vs Fitted
plot(m4, which = 1)  

# 4. Leverage vs Residuals (Cook's Distance)
plot(m4, which = 5)  

#-------------------------

#CHECKING RESIDUAL PLOT(SIMILIAR CODE ABOVE)
residualPlot(m4,fitted(m4))
# OR
plot(fitted(m4), residuals(m4), xlab = "Fitted Values", ylab = "Residuals", main = "Residual Plot")
abline(h = 0, col = "red")

#--------------------------
#TRANSFORMATION
lnwageinc<-log(data1$wageinc)
m5<-lm(lnwageinc~age+cit+educ+engl+occ+sex+wkswrkd,data=data1)

summary(m5)


plot(fitted(m5), residuals(m5), xlab = "Fitted Values", ylab = "Residuals", main = "Residual Plot")
abline(h = 0, col = "red")

# 1. Scale-Location Plot
plot(m5, which = 3)  
# 2. Normal Q-Q Plot
plot(m5, which = 2) 

# 3. Residuals vs Fitted
plot(m5, which = 1) 
# 4. Leverage vs Residuals (Cook's Distance)
plot(m5, which = 5)  

#--------------------------------------------------

##Forward selection with p-value (and AIC)

# Base model with no predictors
base_model <- lm(lnwageinc ~ 1, data = data1)
summary(base_model)
AIC(base_model)

###Single-variable models###
mod1_age <- lm(lnwageinc ~ age, data = data1)
summary(mod1_age)$coefficients
AIC(mod1_age)

mod1_cit <- lm(lnwageinc ~ cit, data = data1)
summary(mod1_cit)$coefficients
AIC(mod1_cit)

mod1_educ <- lm(lnwageinc ~ educ, data = data1)
summary(mod1_educ)$coefficients
AIC(mod1_educ)

mod1_engl <- lm(lnwageinc ~ engl, data = data1)
summary(mod1_engl)$coefficients
AIC(mod1_engl)

mod1_occ <- lm(lnwageinc ~ occ, data = data1)
summary(mod1_occ)$coefficients
AIC(mod1_occ)

mod1_sex <- lm(lnwageinc ~ sex, data = data1)
summary(mod1_sex)$coefficients
AIC(mod1_sex)

mod1_wkswrkd <- lm(lnwageinc ~ wkswrkd, data = data1)
summary(mod1_wkswrkd)$coefficients
AIC(mod1_wkswrkd)

#mod1_educ: Best single predictor based on AIC

#####Two-variable models####
mod2_educ_age <- lm(lnwageinc ~ educ + age, data = data1)
summary(mod2_educ_age)$coefficients
AIC(mod2_educ_age)

mod2_educ_cit <- lm(lnwageinc ~ educ + cit, data = data1)
summary(mod2_educ_cit)$coefficients
AIC(mod2_educ_cit)

mod2_educ_engl <- lm(lnwageinc ~ educ + engl, data = data1)
summary(mod2_educ_engl)$coefficients
AIC(mod2_educ_engl)

mod2_educ_occ <- lm(lnwageinc ~ educ + occ, data = data1)
summary(mod2_educ_occ)$coefficients
AIC(mod2_educ_occ)

mod2_educ_sex <- lm(lnwageinc ~ educ + sex, data = data1)
summary(mod2_educ_sex)$coefficients
AIC(mod2_educ_sex)

mod2_educ_wkswrkd <- lm(lnwageinc ~ educ + wkswrkd, data = data1)
summary(mod2_educ_wkswrkd)$coefficients
AIC(mod2_educ_wkswrkd)

#The model with education and age (mod2_educ_age) has the lowest AIC value (11510.38),
#indicating the best fit among the two-variable models.

####Three variable models#####
mod3_educ_age_cit <- lm(lnwageinc ~ educ + age + cit, data = data1)
mod3_educ_age_engl <- lm(lnwageinc ~ educ + age + engl, data = data1)
mod3_educ_age_occ <- lm(lnwageinc ~ educ + age + occ, data = data1)
mod3_educ_age_sex <- lm(lnwageinc ~ educ + age + sex, data = data1)
mod3_educ_age_wkswrkd <- lm(lnwageinc ~ educ + age + wkswrkd, data = data1)

# Summary and AIC for all three-variable models
summary(mod3_educ_age_cit)$coefficients
AIC(mod3_educ_age_cit)

summary(mod3_educ_age_engl)$coefficients
AIC(mod3_educ_age_engl)

summary(mod3_educ_age_occ)$coefficients
AIC(mod3_educ_age_occ)

summary(mod3_educ_age_sex)$coefficients
AIC(mod3_educ_age_sex)

summary(mod3_educ_age_wkswrkd)$coefficients
AIC(mod3_educ_age_wkswrkd)


# Model 1: Education + Age + Weeks Worked + Citizenship
mod4_educ_age_wkswrkd_cit <- lm(lnwageinc ~ educ + age + wkswrkd + cit, data = data1)
summary(mod4_educ_age_wkswrkd_cit)$coefficients
AIC(mod4_educ_age_wkswrkd_cit)

# Model 2: Education + Age + Weeks Worked + English Proficiency
mod4_educ_age_wkswrkd_engl <- lm(lnwageinc ~ educ + age + wkswrkd + engl, data = data1)
summary(mod4_educ_age_wkswrkd_engl)$coefficients
AIC(mod4_educ_age_wkswrkd_engl)

# Model 3: Education + Age + Weeks Worked + Sex
mod4_educ_age_wkswrkd_sex <- lm(lnwageinc ~ educ + age + wkswrkd + sex, data = data1)
summary(mod4_educ_age_wkswrkd_sex)$coefficients
AIC(mod4_educ_age_wkswrkd_sex)

# Model 4: Education + Age + Weeks Worked + Occupation
mod4_educ_age_wkswrkd_occ <- lm(lnwageinc ~ educ + age + wkswrkd + occ, data = data1)
summary(mod4_educ_age_wkswrkd_occ)$coefficients
AIC(mod4_educ_age_wkswrkd_occ)




# 5-variable model: Education, Age, Weeks Worked, Occupation, and Sex
mod5_educ_age_wkswrkd_occ_sex <- lm(lnwageinc ~ educ + age + wkswrkd + occ + sex, data = data1)

# 5-variable model: Education, Age, Weeks Worked, Occupation, and Citizenship
mod5_educ_age_wkswrkd_occ_cit <- lm(lnwageinc ~ educ + age + wkswrkd + occ + cit, data = data1)

# 5-variable model: Education, Age, Weeks Worked, Occupation, and English Proficiency
mod5_educ_age_wkswrkd_occ_engl <- lm(lnwageinc ~ educ + age + wkswrkd + occ + engl, data = data1)

# Summaries for the 5-variable models
summary(mod5_educ_age_wkswrkd_occ_sex)
summary(mod5_educ_age_wkswrkd_occ_cit)
summary(mod5_educ_age_wkswrkd_occ_engl)

# Calculate AIC for model comparison
AIC(mod5_educ_age_wkswrkd_occ_sex)
AIC(mod5_educ_age_wkswrkd_occ_cit)
AIC(mod5_educ_age_wkswrkd_occ_engl)



#####six variable model##########

mod6_educ_age_wkswrkd_occ_sex_cit<-lm(lnwageinc ~ educ + age + wkswrkd + occ + sex+cit, data = data1)
mod6_educ_age_wkswrkd_occ_sex_engl<-lm(lnwageinc ~ educ + age + wkswrkd + occ + sex+engl, data = data1)

# Summaries for the 6-variable models
summary(mod6_educ_age_wkswrkd_occ_sex_cit)
summary(mod6_educ_age_wkswrkd_occ_sex_engl)


# Calculate AIC for model comparison
AIC(mod6_educ_age_wkswrkd_occ_sex_cit)
AIC(mod6_educ_age_wkswrkd_occ_sex_engl)


#FINAL ORDER
m6<-lm(lnwageinc ~ educ + age + wkswrkd + occ + sex+ engl+cit, data = data1)

#----------------------------------------------------------------
#Including all interaction terms

m7<-lm(lnwageinc~educ + age + wkswrkd + occ + sex+ engl+cit + I(age^2)+ I(wkswrkd^2) + 
         occ:age + occ:educ + occ:engl + occ:sex + occ:wkswrkd + 
        + age:educ + age:engl + age:sex + cit:engl + cit:sex +
         cit:occ + cit:wkswrkd + cit:age + cit:educ +
         age:wkswrkd + 
         educ:engl + educ:sex + educ:wkswrkd + 
         engl:sex + engl:wkswrkd  + 
          sex:wkswrkd, data = data1)


 


#Elimination-both
m7_both<-step(m7, direction='both')
AIC(m7_both)
summary(m7_both)

m8<-lm(lnwageinc ~ educ + age + wkswrkd + occ + sex + engl + 
         cit + I(age^2) + I(wkswrkd^2) + educ:occ + wkswrkd:occ + 
         age:engl + age:sex + sex:cit + occ:cit + age:cit + educ:sex + 
         educ:wkswrkd + wkswrkd:engl + wkswrkd:sex, data = data1)

AIC(m8)
summary(m8)

residualPlot(m8)

#Elimination-forward

m7_null<-lm(lnwageinc ~ 1, data = data1)
m7_forward<-step(m7_null, scope = ~ (educ + age + wkswrkd + occ + sex + engl + cit)^2 
                 + I(age^2) + I(wkswrkd^2) +  educ^2+  occ^2+  sex^2+  engl^2
                 + cit^2, 
                 direction = "forward")

summary(m7_forward)

m9<-lm(lnwageinc ~ educ + age + I(wkswrkd^2) + occ + I(age^2) + 
         sex + engl + cit + wkswrkd + occ:cit + sex:cit + educ:occ + 
         occ:wkswrkd + educ:wkswrkd + age:cit + educ:sex + sex:wkswrkd + 
         age:sex, data = data1)

#Elimination-backward
m7_back<-step(m7, direction='backward')
summary(m7_back)

m10<-lm(lnwageinc ~ educ + age + wkswrkd + occ + sex + engl + 
          cit + I(age^2) + I(wkswrkd^2) + educ:occ + wkswrkd:occ + 
          age:engl + age:sex + sex:cit + occ:cit + age:cit + educ:sex + 
          educ:wkswrkd + wkswrkd:engl + wkswrkd:sex, data = data1)

#Combining all the interaction to get the best fit

m11<-lm(lnwageinc ~ educ + age + wkswrkd + occ + sex + engl + 
          cit + I(age^2) + I(wkswrkd^2) + educ:occ + wkswrkd:occ + 
          age:engl + age:sex + sex:cit  + age:cit + educ:sex + educ:age +
          sex:engl+ occ:cit+ 
          educ:wkswrkd + wkswrkd:engl + wkswrkd:sex , data = data1)



summary(m8)$adj.r.squared#both
summary(m9)$adj.r.squared#forward
summary(m10)$adj.r.squared#backward
summary(m11)$adj.r.squared#BEST MODEL

summary(m)$adj.r.squared#BASE MODEL

AIC(m8)
AIC(m9)
AIC(m10)
AIC(m11)

AIC(m)

############################################

data2<-data1


###################################

#Use 5-fold or 10-fold cross-validation to assess whether max.md2
# generalizes well to new data, as its complexity increases the risk of overfitting

####################
data2$lnwageinc<-lnwageinc
data2$wageinc <- NULL



## Implementing the 5-fold cross-validation process

# (1) Divide data2 into 5 random folds
set.seed(100)
rand.seq <- sample(nrow(data1), nrow(data1))
fold.size <- floor(nrow(data1) / 5)

test.1 <- rand.seq[1:fold.size]
test.2 <- rand.seq[(fold.size + 1):(2 * fold.size)]
test.3 <- rand.seq[(2 * fold.size + 1):(3 * fold.size)]
test.4 <- rand.seq[(3 * fold.size + 1):(4 * fold.size)]
test.5 <- rand.seq[(4 * fold.size + 1):nrow(data2)]


# (2) Define a function that returns R2.prediction and y.hats

R2.pred.lm <- function(formula, train.data, test.data, test.y) {
  m <- lm(formula, data = train.data)
  y.hat <- predict(m, newdata = test.data)
  R2.pred <- cor(y.hat, test.y) ^ 2
  return(list(R2.pred = R2.pred, y.hat = y.hat))
}


# (3) 5-fold cross validation R2 for the base model

formula.full <- formula(wageinc ~age + cit + educ + engl + occ + birth + 
                          sex + wkswrkd + yrentry + powspuma
                        , data = data1)
# An illustration of the R2.pred.lm() function using the first-fold data
R2.pred.lm(formula=formula.full,
           train.data=data1[-test.1, ],
           test.data=data1[test.1, ],
           test.y=data1[test.1, "wageinc"]) 


# Prepare for the for-loop
test.list <- list(test.1, test.2, test.3, test.4, test.5)
y.and.y.hat.all.full <- NULL

# The for-loop
for (i in 1:5){
  test.indices <- test.list[[i]]
  output.tmp <- R2.pred.lm(formula=formula.full,
                           train.data=data1[-test.indices, ],
                           test.data=data1[test.indices, ],
                           test.y=data1[test.indices, "wageinc"])
  y.and.y.hat.tmp <- cbind(data1[test.indices, "wageinc"],
                           output.tmp$y.hat)
  y.and.y.hat.all.full <- rbind(y.and.y.hat.all.full, y.and.y.hat.tmp)
}


head(y.and.y.hat.all.full)

dim(y.and.y.hat.all.full)

R2.cv.5.full <- cor(y.and.y.hat.all.full[,1], y.and.y.hat.all.full[,2]) ^ 2


R2.cv.5.full

# Compare with the in-sample R2
m.full <- lm(formula.full, data=data2)
s.full <- summary(m.full)
s.full$r.squared

# (4) 5-fold cross validation R2 for the stepwise reduced model

set.seed(100)
rand.seq <- sample(nrow(data2), nrow(data2))
fold.size <- floor(nrow(data2) / 5)

test.1 <- rand.seq[1:fold.size]
test.2 <- rand.seq[(fold.size + 1):(2 * fold.size)]
test.3 <- rand.seq[(2 * fold.size + 1):(3 * fold.size)]
test.4 <- rand.seq[(3 * fold.size + 1):(4 * fold.size)]
test.5 <- rand.seq[(4 * fold.size + 1):nrow(data2)]


# (2) Define a function that returns R2.prediction and y.hats

R2.pred.lm <- function(formula, train.data, test.data, test.y) {
  m <- lm(formula, data = train.data)
  y.hat <- predict(m, newdata = test.data)
  R2.pred <- cor(y.hat, test.y) ^ 2
  return(list(R2.pred = R2.pred, y.hat = y.hat))
}

formula.reduced <- formula(lnwageinc ~ educ + age + wkswrkd + occ + sex + engl + 
                             cit + I(age^2) + I(wkswrkd^2) + educ:occ + wkswrkd:occ + 
                             age:engl + age:sex + sex:cit  + age:cit + educ:sex + educ:age +
                             sex:engl+ occ:cit+ 
                             educ:wkswrkd + wkswrkd:engl + wkswrkd:sex , data = data2)



# Prepare for the for-loop
y.and.y.hat.all.reduced <- NULL

# The for-loop
for (i in 1:5){
  test.indices <- test.list[[i]]
  output.tmp <- R2.pred.lm(formula=formula.reduced,
                           train.data=data2[-test.indices, ],
                           test.data=data2[test.indices, ],
                           test.y=data2[test.indices, "lnwageinc"])
  y.and.y.hat.tmp <- cbind(data2[test.indices, "lnwageinc"],
                           output.tmp$y.hat)
  y.and.y.hat.all.reduced <- rbind(y.and.y.hat.all.reduced,
                                   + y.and.y.hat.tmp)
}


R2.cv.5.reduced <- cor(y.and.y.hat.all.reduced[, 1],
                       + y.and.y.hat.all.reduced[, 2]) ^ 2


# In-sample R2 for the reduced model
m.reduced <- lm(formula.reduced, data=data2)
s.reduced <- summary(m.reduced)


# Compare the two models

#formula.full <- formula(wageinc ~age + cit + educ + engl + occ + birth + 
#                        sex + wkswrkd + yrentry + powspuma, data = data1)

#formula.reduced <- formula(lnwageinc ~ educ + age + wkswrkd + occ + sex + engl + 
#                             cit + I(age^2) + I(wkswrkd^2) + educ:occ + wkswrkd:occ + 
#                             educ:age + age:engl + educ:sex + educ:wkswrkd + sex:engl + 
#                             wkswrkd:engl + wkswrkd:sex, data = data2)


# Print results
cat("In-sample R^2 for full model:", s.full$r.squared, "\n")
cat("In-sample R^2 for reduced model:", s.reduced$r.squared, "\n")

cat("Cross-validated R^2 for full model:", R2.cv.5.full, "\n")
cat("Cross-validated R^2 for reduced model:", R2.cv.5.reduced, "\n")


summary(m)
summary(m11)

plot(m6,which=1)
plot(m11,which=1)

plot(m6,which=2)
plot(m11,which=2)

plot(m,which=3)
plot(m11,which=3)

plot(m,which=4)
plot(m11,which=4)

plot(m,which=5)
plot(m11,which=5)



