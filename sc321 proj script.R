br_cancer <- read.csv("cleaned_breast_cancer_UPDATED.csv")
search()
#write.csv(br_cancer, file = "cleaned_breast_cancer_UPDATED2.csv")
br_cancer$EARNINGS <- relevel(br_cancer$EARNINGS2, c('Unknown'))
br_cancer$CNBRES <- relevel(br_cancer$CNBRES, c('Not mentioned'))
br_cancer$SEX <- relevel(br_cancer$SEX, c('Male'))


br_cancer$HEALTH <- br_cancer$HEALTH2
br_cancer$HEALTH <- factor(br_cancer$HEALTH, c('Poor', 'Fair', 'Good', 'Very Good', 'Excellent'))

br_cancer$EARNINGS <- br_cancer$EARNINGS2
br_cancer$SMOKFREQNOW <- br_cancer$SMOKFREQNOW2
br_cancer$SMOKFREQNOW <- factor(br_cancer$SMOKFREQNOW, c('Not at all', 'Some days', 'Every day'))
br_cancer$HRSLEEP <- br_cancer$HRSLEEP2
br_cancer$BACHELOR <- br_cancer$i.bachelor
br_cancer$BACHELOR <- factor(br_cancer$BACHELOR)
levels(br_cancer$BACHELOR) <- c("Below Bachelors", "Bachelors and up")

write.csv(br_cancer, file = "final_breast_cancer.csv")

br_cancer$HEALTH2 <- as.numeric(br_cancer$HEALTH)

br_cancer$i.poor_fair_health[br_cancer$HEALTH2 == 1| br_cancer$HEALTH2 == 2] <- 1
br_cancer$i.poor_fair_health[br_cancer$HEALTH2 != 1| br_cancer$HEALTH2 != 2] <- 0

br_cancer$i.good_excel_health[br_cancer$HEALTH2 == 3|
                                br_cancer$HEALTH2 == 4|
                                br_cancer$HEALTH2 == 5] <- 1
br_cancer$i.good_excel_health[br_cancer$HEALTH2 != 3|
                                br_cancer$HEALTH2 != 4|
                                br_cancer$HEALTH2 != 5] <- 0


freq(CNBRES, main = "Bar Chart for CNBRES ")

hist(AGE, main = "Histogram of Age", col = "light grey")
hist(HRSLEEP2, main = "Histogram of Hours of Sleep", xlab = "HRSLEEP", col = 'light grey')
#we could potentially square it for "more normal"
#hist((AGE)^2)
hist(VIG10FWKln, main = "Histogram for VIG10FWKln", col = 'light grey',
     xlab = "Log(VIG10FWK + 1)")
hist(cnbres.df$VIG10FWK2, main = "Histogram of VIG10FWK", xlab = "VIG10FWK", col = 'light grey')
freq(REGION, main = "Bar chart of Region")

freq(SEX, main = "Bar chart of Sex")
freq(EARNINGS2, main = 'Bar chart for Earnings')
freq(HEALTH2, main = 'Bar chart for Health')
freq(ALCSTAT1, main = "Bar chart of ALCSTAT1")
freq(REGION)
freq(HEALTH)
freq(SEX)
freq(EARNINGS)
freq(MARSTAT)
freq(SMOKFREQNOW)
freq(EDUC)
freq(FSBALANC)

hist(log(br_cancer$ALC5UPYR2+1), xlab = "Log(ALC5UPYR+1)", main = "Histogram of Log(ALC5UPYR+1)")
hist(br_cancer$ALC5UPYR2, xlab = "Number of days with 5+ drinks")



hist(br_cancer$MOD10FWKln, xlab = "Log(MOD10FWK", main = "Histogram of Log(MOD10FWK+1)")
hist(br_cancer$VIG10FWKln, xlab = "Log(VIG10FWK+1)", main = "Histogram of Log(VIG10FWK+1)")

crosstab(br_cancer$CNBRES, br_cancer$REGION)

freq(br_cancer, )

detach(br_cancer)
br_cancer <- br_cancer[br_cancer$MARSTAT != "Unknown marital status",]
br_cancer <- br_cancer[br_cancer$ALCSTAT1 != "Drinking status unknown",]

br_cancer$Marital_Status <-NULL
br_cancer$Marital_Status[br_cancer$MARSTAT == "Married"] <- "Married"
br_cancer$Marital_Status[br_cancer$MARSTAT == "Divorced"|
                           br_cancer$MARSTAT == "Separated" |
                           br_cancer$MARSTAT == "Widowed"] <- "Formerly Married"
br_cancer$Marital_Status[br_cancer$MARSTAT == "Never married"] <- "Never Married"
br_cancer$Marital_Status <- factor(br_cancer$Marital_Status, levels = c("Never Married", "Married", "Formerly Married"))

freq(br_cancer$HEALTH)

br_cancer$Health_reduced[br_cancer$HEALTH == "Poor"|
                           br_cancer$HEALTH == "Fair"] <- "Poor/Fair"
br_cancer$Health_reduced[br_cancer$HEALTH == "Good"] <- "Good"
br_cancer$Health_reduced[br_cancer$HEALTH == "Very Good"|
                           br_cancer$HEALTH == "Excellent"] <- "Very Good/Excellent"
br_cancer$Health_reduced <- factor(br_cancer$Health_reduced, levels = c("Poor/Fair","Good","Very Good/Excellent" ))

br_cancer$Alcohol_Status <- NULL
br_cancer$Drinker[br_cancer$ALCSTAT1 == "Current drinker (1+ drinks past year)"] <- "Current Drinker"
br_cancer$Drinker[br_cancer$ALCSTAT1 == "Former drinker (no drinks past year)"
                    | br_cancer$ALCSTAT1 == "Lifetime abstainer (lt 12 drinks in life)"] <- "No Drinks"
br_cancer$Drinker <- factor(br_cancer$Drinker, levels = c("Current Drinker", "No Drinks"))


br_cancer$Balanced_meal[br_cancer$FSBALANC == "Never true"] <- "Always balanced meals"
br_cancer$Balanced_meal[br_cancer$FSBALANC == "Often True"|
                          br_cancer$FSBALANC == "Sometimes true"] <- "Not always balanced meals"

br_cancer$Balanced_meal <- factor(br_cancer$Balanced_meal, levels = c("Not always balanced meals", "Always balanced meals"))

br_cancer$SMOKFREQNOW <- as.numeric(br_cancer$SMOKFREQNOW)
br_cancer$Smoker[br_cancer$SMOKFREQNOW == 2| 
                   br_cancer$SMOKFREQNOW == 3] <- "Current Smoker"
br_cancer$Smoker[br_cancer$SMOKFREQNOW == 1] <- "Doesn't Smoke"
br_cancer$Smoker <- factor(br_cancer$Smoker, levels = "Current Smoker", "Doesn't Smoke")

br_cancer$CNBRES2[br_cancer$CNBRES == "Mentioned"]

attach(br_cancer)
br_cancer2 <- subset(br_cancer, 
                     select = c(AGE, REGION, SEX, HRSLEEP, Vigor_Excercise, Marital_Status, EARNINGS, Balanced_meal, Drinker, Health_reduced,
                               SMOKFREQNOW2,CNBRES, i.bachelor ))
write.csv(br_cancer2, "reduced_breast_cancer.csv")


br_cancer$Alcohol_Status <- br_cancer$ALCSTAT1
br_cancer$Balanced_Meal <-br_cancer$FSBALANC
br_cancer$Smoking_Status <- br_cancer$SMOKFREQNOW

br_cancer$Vigor_Excercise[br_cancer$VIG10FWKln == 0] <- "No Vigorous Exercise"
br_cancer$Vigor_Excercise[br_cancer$VIG10FWKln != 0] <- "Some Vigorous Exercise"
br_cancer$Vigor_Excercise <- factor(br_cancer$Vigor_Excercise, levels = c("No Vigorous Exercise", 
                                                                          "Some Vigorous Exercise"))

br_cancer$i.vigor_excercise <- br_cancer$Vigor_Excercise



#initial.glm <- glm(data = br_cancer, CNBRES ~ REGION + AGE + i.female +RACESR.white + i.married + i.not_together + i.never_married  + EARNINGS2 +  i.bachelor + i.balance_meal + SMOKFREQNOW2  + HRSLEEP2 + VIG10FWKln + HEALTH2 , family = "binomial" )
summary(initial.glm)


#initial.glm <- glm(CNBRES ~ AGE + HRSLEEP + VIG10FWKln + SEX + ALCSTAT1 + REGION + EARNINGS + SMOKFREQNOW + HEALTH + FSBALANC + MARSTAT+ BACHELOR, family = binomial)
initial.glm <- glm(CNBRES ~ AGE +HRSLEEP + VIG10FWKln + i.northeast + i.north_centralmidwest + i.south   + i.female + i.married + i.not_together + i.never_married 
                   + i.low_income + i.unknown_income   + i.balance_meal + i.never_alc + i.former_alc + i.current_alc + i.no_smoke + i.some_smoke   
                   + i.poor_health + i.fair_health + i.good_health + i.verygood_health  + i.bachelor, family = binomial)
                     
                     
summary(initial.glm)

initial.glm2 <- glm(CNBRES ~ AGE +HRSLEEP + VIG10FWKln + i.northeast + i.north_centralmidwest + i.south   + i.female + i.married + i.not_together + i.never_married 
                   + i.low_income + i.unknown_income   + i.balance_meal + i.never_alc + i.former_alc + i.current_alc + i.no_smoke + i.some_smoke +
                   i.poor_fair_health + i.bachelor, family = binomial)


summary(initial.glm2)


reduced.glm <- stepAIC(initial.glm2)
summary(reduced.glm)

tree1 <- rpart(reduced.glm ,control=rpart.control(minsplit=30, cp=0.001))
plot(tree1)
text(tree1)


br_cancer$i.female <- factor(br_cancer$i.female)



summary(glm(data = br_cancer, CNBRES ~ MOD10FWK2ln , family = binomial))
summary(glm(data = br_cancer, CNBRES ~ VIG10FWK2ln , family = binomial))


#add title in document main = "Crosstab of Food Nutrition Balance and Breast Cancer"
crosstab(CNBRES,FSBALANC )

br_cancer$i.white <- br_cancer$RACESR == "White"

plot(CNBRES, AGE, xlab = "CNBRES", ylab = "AGE", main = "Boxplot of AGE by CNBRES")
plot(CNBRES, HRSLEEP, xlab = "CNBRES", ylab = "HRSLEEP" ,main = "Boxplot of HRSLEEP by CNBRES")
plot(CNBRES, VIG10FWKln, xlab = "CNBRES", ylab = "VIG10FWKln" ,main = "Boxplot of VIG10FWKln by CNBRES")

age.glm <- (glm(CNBRES ~ AGE, family = binomial))
summary(age.glm)
lrtest(age.glm)

hrsleep.glm <-(glm(CNBRES ~ HRSLEEP, family = binomial))
summary(hrsleep.glm)
lrtest(hrsleep.glm)

summary(glm(CNBRES ~ VIG10FWKln, family = binomial))
br_cancer$ALCSTAT1 <- relevel(br_cancer$ALCSTAT1, c('Drinking status unknown'))
summary(glm(CNBRES ~ REGION, family = binomial))

summary(glm(CNBRES ~ ALCSTAT1, family = binomial))
summary(glm(CNBRES ~ MARSTAT, family = binomial))
summary(glm(CNBRES ~ SEX, family = binomial))

summary(glm(CNBRES ~ HEALTH, family = binomial))
summary(glm(CNBRES ~ HEALTH2, family = binomial))

summary(glm(CNBRES ~ EARNINGS, family = binomial))
summary(glm(CNBRES ~ FSBALANC, family = binomial))
summary(glm(CNBRES ~ SMOKFREQNOW, family = binomial))
summary(glm(CNBRES ~ EDUC, family = binomial))

scatterplotMatrix(br_cancer[c(3,11,44)])
res2 <- rcorr(as.matrix(br_cancer[c(3,11,44)]))
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
flattenCorrMatrix(res2$r, res2$P)

scatterplotMatrix(br_cancer[c(2,4,5,8,9,11,41,42,43,44,45)], smooth = F)
scatterplotMatrix(br_cancer[c(16:40)], smooth = F)

chisq.test(ALCSTAT1, CNBRES)
chisq.test(MARSTAT, CNBRES)
chisq.test(REGION, CNBRES)
chisq.test(HEALTH, CNBRES)
chisq.test(SEX, CNBRES)

chisq.test(EARNINGS, CNBRES)
chisq.test(FSBALANC, CNBRES)
chisq.test(SMOKFREQNOW, CNBRES)
chisq.test(BACHELOR, CNBRES)

t.test(AGE~ CNBRES)                  
t.test(HRSLEEP~CNBRES)
t.test(VIG10FWKln~CNBRES)


crosstab(CNBRES, MARSTAT, las =2, main = "Crosstab of MARSTAT by CNBRES")
counts0 <- table(CNBRES, ALCSTAT1)
barplot(counts0, beside = T,legend = rownames(counts), xlab = "ALCSTAT1", ylab = "Count",
        main = "Distribution by ALCSTAT1 and CNBRES", args.legend = list(x="topright"))

counts <- table(CNBRES, MARSTAT)
barplot(counts, beside = T,legend = rownames(counts), xlab = "MARSTAT", ylab = "Count",
        main = "Distribution by MARSTAT and CNBRES")

counts2 <- table(CNBRES, SEX)
barplot(counts2, beside = T,legend = rownames(counts), xlab = "SEX", ylab = "Count",
        main = "Distribution by SEX and CNBRES")

counts3 <- table(CNBRES, REGION)
barplot(counts3, beside = T, legend = rownames(counts3), xlab = "REGION", ylab = "Count",
        main = "Distribution by REGION and CNBRES",args.legend = list(x="topright"))

counts4 <- table(CNBRES, HEALTH)

barplot(counts4, beside = T, legend = rownames(counts4), xlab = "HEALTH", ylab = "Count",
        main = "Distribution by HEALTH and CNBRES",args.legend = list(x="topleft"))

counts5 <- table(CNBRES, EARNINGS)

barplot(counts5, beside = T, legend = rownames(counts4), xlab = "EARNINGS", ylab = "Count",
        main = "Distribution by EARNINGS and CNBRES",args.legend = list(x="topleft"))


counts6 <- table(CNBRES, BACHELOR)
chisq.test(counts6)
barplot(counts6, beside = T, legend = rownames(counts4), xlab = "BACHELOR", ylab = "Count",
        main = "Distribution by BACHELOR and CNBRES",args.legend = list(x="topright"))
plot(REGION, main = "Bar chart of Region", xlab = "Region", ylab = 'Frequency')
plot(ALCSTAT1, main = "Bar chart of ALCSTAT1", xlab = "ALCSTAT1", ylab = 'Frequency')


counts7 <- table(CNBRES, AGE)
chisq.test(counts7)
crosstab(CNBRES, EARNINGS)

chisq.test(table(i.west, i.bachelor))

#things to do 
#drop drinking status unknown


final_br_cancer <- read.csv("reduced_breast_cancer.csv", header = TRUE)
names(final_br_cancer)

#indicator for cnbres 

#indicator variables for region 
#[1] "North Central/Midwest" "Northeast"     "South"                 "West"                 
#take out one of them for (make northcentral the baseline)
final_br_cancer$i.northcentral_midwest[final_br_cancer$REGION=="North Central/Midwest"] <- 1
final_br_cancer$i.northcentral_midwest[final_br_cancer$REGION!="North Central/Midwest"] <- 0

final_br_cancer$i.northeast[final_br_cancer$REGION == "Northeast"] <- 1
final_br_cancer$i.northeast[final_br_cancer$REGION != "Northeast"] <- 0

final_br_cancer$i.south[final_br_cancer$REGION == "South"] <- 1
final_br_cancer$i.south[final_br_cancer$REGION != "South"] <- 0

final_br_cancer$i.west[final_br_cancer$REGION == "West"] <- 1
final_br_cancer$i.west[final_br_cancer$REGION != "West"] <- 0

#indicator for vig
# [1] "No Vigorous Exercise"   "Some Vigorous Exercise"
#some exercise is the baseline 
final_br_cancer$i.no_vig_exercise[final_br_cancer$Vigor_Excercise == "No Vigorous Exercise"] <- 1
final_br_cancer$i.no_vig_exercise[final_br_cancer$Vigor_Excercise != "No Vigorous Exercise"] <- 0


#indicator for sex
#male is the baseline 
final_br_cancer$i.female[final_br_cancer$SEX == "Female"] <- 1
final_br_cancer$i.female[final_br_cancer$SEX != "Female"] <- 0


#indicator for marital status 
levels(final_br_cancer$Marital_Status)
#"Formerly Married" "Married"          "Never Married"   
#never married is the baseline 
final_br_cancer$i.married[final_br_cancer$Marital_Status == "Married"]<- 1
final_br_cancer$i.married[final_br_cancer$Marital_Status != "Married"]<- 0

final_br_cancer$i.former_married[final_br_cancer$Marital_Status == "Formerly Married"]<- 1
final_br_cancer$i.former_married[final_br_cancer$Marital_Status != "Formerly Married"]<- 0


#indicator for earnings 
levels(final_br_cancer$EARNINGS)
#"Below Poverty Line"     "Low Income"             "Unknown"                "Upper and Middle Class"
#upper and middle class is baseline 
final_br_cancer$i.below_poverty[final_br_cancer$EARNINGS == "Below Poverty Line"] <- 1
final_br_cancer$i.below_poverty[final_br_cancer$EARNINGS != "Below Poverty Line"] <- 0

final_br_cancer$i.low_income[final_br_cancer$EARNINGS == "Low Income"] <- 1
final_br_cancer$i.low_income[final_br_cancer$EARNINGS != "Low Income"] <- 0

final_br_cancer$i.unknown_income[final_br_cancer$EARNINGS == "Unknown"] <- 1
final_br_cancer$i.unknown_income[final_br_cancer$EARNINGS != "Unknown"] <- 0


#indicator for balancemeals 
levels(final_br_cancer$Balanced_meal)
#always balanced is baseline 
final_br_cancer$i.not_balanced_meals[final_br_cancer$Balanced_meal == "Not always balanced meals"] <- 1
final_br_cancer$i.not_balanced_meals[final_br_cancer$Balanced_meal != "Not always balanced meals"] <- 0

#indicator for alochol 
levels(final_br_cancer$Drinker)
#no drink is baseline 
final_br_cancer$i.drinker[final_br_cancer$Drinker == "Current Drinker"] <- 1
final_br_cancer$i.drinker[final_br_cancer$Drinker != "Current Drinker"] <- 0

#indicator for smoking 
final_br_cancer$Smoker[final_br_cancer$SMOKFREQNOW2 == "Every day"|
                         final_br_cancer$SMOKFREQNOW2 == "Some days"] <- "Smoker"
final_br_cancer$Smoker[final_br_cancer$SMOKFREQNOW2 == "Not at all"] <- "Non Smoker"
final_br_cancer$Smoker <- factor(final_br_cancer$Smoker, levels = c("Smoker", "Non Smoker"))
freq(final_br_cancer$Smoker)

final_br_cancer$i.smoking[final_br_cancer$Smoker == "Smoker"] <- 1
final_br_cancer$i.smoking[final_br_cancer$Smoker != "Smoker"] <- 0

#indicator for health 
levels(final_br_cancer$Health_reduced)
final_br_cancer$i.good_health[final_br_cancer$Health_reduced == 'Good'] <- 1
final_br_cancer$i.good_health[final_br_cancer$Health_reduced != 'Good'] <- 0

final_br_cancer$i.fair_poor_health[final_br_cancer$Health_reduced == 'Poor/Fair'] <- 1
final_br_cancer$i.fair_poor_health[final_br_cancer$Health_reduced != 'Poor/Fair'] <- 0

#indicator for breast cancer
levels(final_br_cancer$CNBRES)
final_br_cancer$i.breast_cancer[final_br_cancer$CNBRES == "Mentioned"] <- 1
final_br_cancer$i.breast_cancer[final_br_cancer$CNBRES != "Mentioned"] <- 0



write.csv(final_br_cancer, "hopefully_final_breast_cancer.csv")

levels(final_br_cancer$Health_reduced)
# "Good"                "Poor/Fair"           "Very Good/Excellent"



 

attach(final_br_cancer)



scatterplotMatrix(final_br_cancer[c(2,5)], main = "Scatterplot Matrix of Age and Hours of Sleep")

res2 <- rcorr(as.matrix(final_br_cancer[c(2,5)]))
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
flattenCorrMatrix(res2$r, res2$P)

age.glm <- glm(i.breast_cancer~AGE, family = binomial)
summary(age.glm)
lrtest(age.glm)

sleep.glm <- glm(i.breast_cancer~HRSLEEP, family = binomial)
summary(sleep.glm)
lrtest(sleep.glm)
confint(sleep.glm)

vig.glm <- glm(i.breast_cancer~ i.no_vig_exercise, family = binomial)
summary(vig.glm)
lrtest(vig.glm)
confint(vig.glm)

region.glm <-  glm(i.breast_cancer~i.northcentral_midwest + i.south + i.west , family = binomial)
summary(region.glm)
lrtest(region.glm)
confint(region.glm)

sex.glm <- glm(i.breast_cancer~i.female, family = binomial)
summary(sex.glm)
lrtest(sex.glm)
confint(sex.glm)

married.glm <- glm(i.breast_cancer~ i.married + i.former_married, family = binomial )
summary(married.glm)
lrtest(married.glm)
confint(married.glm)

earning.glm <- glm(i.breast_cancer~i.below_poverty + i.low_income + i.unknown_income, family = binomial )
summary(earning.glm)
lrtest(earning.glm)
confint(earning.glm)

meal.glm <- glm(i.breast_cancer~ i.not_balanced_meals, family = binomial)
summary(meal.glm)
lrtest(meal.glm)
confint(meal.glm)

drink.glm <- glm(i.breast_cancer~ i.drinker, family = binomial)
summary(drink.glm)
lrtest(drink.glm)
confint(drink.glm)


smoke.glm <- glm(i.breast_cancer~ i.smoking, family = binomial)
summary(smoke.glm)
lrtest(smoke.glm)
confint(smoke.glm)

health.glm <- glm(i.breast_cancer~ i.fair_poor_health + i.good_health, family = binomial)
summary(health.glm)
lrtest(health.glm)
confint(health.glm)

bachelor <- glm(i.breast_cancer ~ i.bachelor, family = binomial)
summary(bachelor)
lrtest(bachelor)
confint(bachelor)


confint(age.glm)








#making full model 
final.initial.glm <- glm(i.breast_cancer ~ AGE + HRSLEEP + i.northcentral_midwest + i.south + i.west + i.no_vig_exercise + i.female + 
                           i.married + i.former_married + i.below_poverty + i.low_income + i.unknown_income + i.not_balanced_meals + 
                           i.drinker + i.smoking + i.fair_poor_health + i.good_health + i.bachelor, family = binomial)
summary(final.initial.glm)
lrtest(final.initial.glm)

#reducing using stepwise aic 
reduced.model.glm <- stepAIC(final.initial.glm)
summary(reduced.model.glm)
lrtest(reduced.model.glm, final.initial.glm)

#regression tree 
#full tree
tree1 <- rpart(reduced.model.glm)
plot(tree1)
text(tree1)
#pruning
prune_tree1 <- prune.rpart(tree1, cp =0.01)
plot(prune_tree1)
text(prune_tree1)

reduced.model.glm.2 <- glm(formula = i.breast_cancer ~ AGE + i.northcentral_midwest + 
                             i.no_vig_exercise + i.female + i.married + i.former_married + 
                             i.below_poverty + i.unknown_income + i.fair_poor_health + 
                             i.good_health + i.bachelor +
                             
                             i.married:i.no_vig_exercise +  
                             i.bachelor:i.good_health + 
                             i.bachelor:i.below_poverty + 
                             i.married:i.bachelor + 
                             i.below_poverty:i.good_health + 
                             AGE:i.good_health + 
                             AGE:i.below_poverty + 
                             i.northcentral_midwest:i.good_health + 
                             I(AGE^2), family = binomial)
summary(reduced.model.glm.2)

lrtest(reduced.model.glm, reduced.model.glm.2)

final_model <- (stepAIC(reduced.model.glm.2))
summary(final_model)

lrtest(reduced.model.glm.2, final_model)

hoslem.test(final_br_cancer$i.breast_cancer, fitted(final_model))

library(tidyverse)
library(caret)

training.samples <- final_br_cancer$i.breast_cancer %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- final_br_cancer[training.samples, ]
test.data <- final_br_cancer[-training.samples, ]
# Build the model
# Make predictions and compute the R2, RMSE and MAE
predictions <- final_model %>% predict(test.data)
data.frame( R2 = R2(predictions, test.data$i.breast_cancer),
            RMSE = RMSE(predictions, test.data$i.breast_cancer),
            MAE = MAE(predictions, test.data$i.breast_cancer))

RMSE(predictions, test.data$i.breast_cancer)/mean(test.data$i.breast_cancer)

final_br_cancer2 = final_br_cancer[final_br_cancer$SEX == "Female",]


final_model2 <- glm(data = final_br_cancer2,formula = i.breast_cancer ~ AGE + HRSLEEP + i.northcentral_midwest + 
                      i.south + i.west + i.no_vig_exercise + i.married + 
                      i.former_married + i.below_poverty + i.low_income + i.unknown_income + 
                      i.not_balanced_meals + i.drinker + i.smoking + i.fair_poor_health + 
                      i.good_health + i.bachelor, family = binomial)
summary(final_model)

hoslem.test(final_br_cancer2$i.breast_cancer, fitted(final_model2))



predictions <- final.initial.glm %>% predict(test.data)
data.frame( R2 = R2(predictions, test.data$i.breast_cancer),
            RMSE = RMSE(predictions, test.data$i.breast_cancer),
            MAE = MAE(predictions, test.data$i.breast_cancer))

RMSE(predictions, test.data$i.breast_cancer)/mean(test.data$i.breast_cancer)

predictions <- reduced.model.glm %>% predict(test.data)
data.frame( R2 = R2(predictions, test.data$i.breast_cancer),
            RMSE = RMSE(predictions, test.data$i.breast_cancer),
            MAE = MAE(predictions, test.data$i.breast_cancer))

RMSE(predictions, test.data$i.breast_cancer)/mean(test.data$i.breast_cancer)

predictions <- reduced.model.glm.2 %>% predict(test.data)
data.frame( R2 = R2(predictions, test.data$i.breast_cancer),
            RMSE = RMSE(predictions, test.data$i.breast_cancer),
            MAE = MAE(predictions, test.data$i.breast_cancer))

RMSE(predictions, test.data$i.breast_cancer)/mean(test.data$i.breast_cancer)

predictions <- final_model %>% predict(test.data)
data.frame( R2 = R2(predictions, test.data$i.breast_cancer),
            RMSE = RMSE(predictions, test.data$i.breast_cancer),
            MAE = MAE(predictions, test.data$i.breast_cancer))

RMSE(predictions, test.data$i.breast_cancer)/mean(test.data$i.breast_cancer)
# library(boot)
# 
# my.cv.error=cv.glm(final_br_cancer , final.initial.glm, K=154)
# my.cv.error$delta[2]

cv.error=cv.glm(final_br_cancer, final.initial.glm, K=100)
cv.error$delta[2]

cv.error2 = cv.glm(final_br_cancer, reduced.model.glm, K = 100)
cv.error2$delta[2]

cv.error3 = cv.glm(final_br_cancer, reduced.model.glm.2, K = 100)
cv.error3$delta[2]

cv.error4 = cv.glm(final_br_cancer, final_model, K = 100)
cv.error4$delta[2]

errors <- c(cv.error$delta[1],cv.error2$delta[1],cv.error3$delta[1],cv.error4$delta[1])
model <- c("Full","Reduced1","Reduced2","Final")

data.frame(model,errors)



