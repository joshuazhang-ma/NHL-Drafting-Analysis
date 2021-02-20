#### Libraries and other Initializations ####
install.packages("readxl")
library(dplyr)
library(readxl)
library(tree)
library(boot)
library(pls)
library("readxl")
library(tree)
require(pls)
par(mfrow = c(1, 1))
hockey <- read_excel("ST635 Hockey Data.xlsx")
# hockey <- read_excel("Data/hockey.xlsx")


#### Checking for Outliers in AE ####
q1 = quantile(hockey$AE, 0.25)
q3 = quantile(hockey$AE, 0.75)
IQR = q3-q1
high <- q3+1.5*IQR
low <- q1-1.5*IQR
hockey$`Player Name`[hockey$AE>high]
hockey$`Player Name`[hockey$AE<low]

#### Split the whole dataset into forwards and defense, because the way to determine a good player is different for the two positions. ####
## Creating training and testing data set for all players
set.seed(100)
sampleIndex <- sample(1:nrow(hockey), 0.75*nrow(hockey))
hockey.train <- hockey[sampleIndex,]
hockey.test <- hockey[-sampleIndex,]

## Forwards are generally coveted for their production, either through goals or assists.
forwards <- hockey[hockey$Position == "F",]
forwards$Top33 <- as.factor(ifelse(forwards$Top33 == 0, "No", "Yes"))
## Defensemen are generally coveted for their prevention of goals, which can be proxied by Plus/Minus.
defense <- hockey[hockey$Position == "D",]

## Creating training and testing data set for forwards.
set.seed(100)
sampleIndex <- sample(1:nrow(forwards), 0.75*nrow(forwards))
forwards.train <- forwards[sampleIndex,]
forwards.test <- forwards[-sampleIndex,]

## Creating training and testing data set for defense.
set.seed(100)
sampleIndex <- sample(1:nrow(defense), 0.75*nrow(defense))
defense.train <- defense[sampleIndex,]
defense.test <- defense[-sampleIndex,]

#### Response - Action Events (Points + Penalties) ####
# Linear Regression
empty.model <- glm(log(AE) ~ 1, data = hockey.train)
full.model <- glm(log(AE) ~ Country + Height + Weight + Position + Handedness + GP_1 + PRE_G_82 + PRE_A_82 + PRE_PIM_82 + League_1, data = hockey.train)

# Stepwise variable selection
model.backwards <- step(full.model, scope = list(lower = empty.model$formula, upper = full.model$formula), direction = 'backward', k = 2, trace = 0)
attr(model.backwards$terms,"variables")
model.forwards <- step(empty.model, scope = list(lower = empty.model$formula, upper = full.model$formula), direction = 'forward', k = 2, trace = 0)
attr(model.forwards$terms,"variables")

# Based on the stepwise variable selection above, we now run regressions.
reg <- lm(log(AE) ~ Weight + Position + Handedness + GP_1 + PRE_G_82 + PRE_PIM_82, data = hockey.train)
summary(reg)


#predict in testing error 
(hockey.pred<- predict(reg ,hockey.test))
sqrt(mean((hockey.test$AE - hockey.pred) ^ 2))




# Checking linear regression residual assumptions
par(mfrow = c(2, 2))
plot(reg)

# PCA
pr.out <- prcomp(hockey[,c(5, 6,10,11,12,13)], scale = TRUE)
biplot(pr.out)

# Create PVE plots
(pve <- pr.out$sdev^2/sum(pr.out$sdev^2))
cumsum(pve)

# Plot PVE
# par(mfrow = c(1, 2))
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = 'b')
plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", type = 'b')

# Optimal PC = 4

# Run Regression Tree Algorithm 500 times to get a distribution of optimal prune sizes to gain understanding.
# OPS.hockey <- rep(0, 500)
# for (i in 1:500){

  # Regression Tree 
  hockey.b.tree <- tree(log(AE) ~ Country + Height + Weight + Position + Handedness + GP_1 + PRE_G_82 + PRE_A_82 + PRE_PIM_82 + League_1, data = hockey.train)
  summary(hockey.b.tree)

  # Plot un-cut tree
  plot(hockey.b.tree)
  text(hockey.b.tree, pretty = 0)

  # Prune the tree
  set.seed(1)
  cv.hockey.b.tree <- cv.tree(hockey.b.tree, FUN = prune.tree, K = 10)
  cv.hockey.b.tree

  # Print best tree size
   plot(cv.hockey.b.tree$size, cv.hockey.b.tree$dev, type = 'b', xlab = 'Tree Size', ylab = 'Deviance')
  (bestsize <- cv.hockey.b.tree$size[which.min(cv.hockey.b.tree$dev)])
  # OPS.hockey[i] <- bestsize
  
  # Based on the distribution of 500 regression trees, the manually determined optimal tree size is 2.
  # bestsize <- 2
  
  # Optimal size for pruned tree is 2. Prune the tree, Captain!
  prune.hockey.b.tree <- prune.tree(hockey.b.tree, best = bestsize)
  (prune.hockey.b.tree)
  
  
  # Predict using the regression tree and determine the error.
  hockey.b.tree.predict <- predict(prune.hockey.b.tree, newdata = hockey.test)
  sqrt(mean((hockey.b.tree.predict - hockey.test$AE)^2))

  # Plot cut tree
  plot(prune.hockey.b.tree)
  text(prune.hockey.b.tree, pretty = 0)
# }

#### Response Forwards[c] - Action Events (Points + Penalties) ####
# Linear Regression
empty.model <- glm(log(AE) ~ 1, data = forwards.train)
full.model <- glm(log(AE) ~ Country + Height + Weight + Handedness + GP_1 + PRE_G_82 + PRE_A_82 + PRE_PIM_82 + League_1, data = forwards.train)

model.backwards <- step(full.model, scope = list(lower = empty.model$formula, upper = full.model$formula), direction = 'backward', k = 2, trace = 0)
attr(model.backwards$terms,"variables")
model.forwards <- step(empty.model, scope = list(lower = empty.model$formula, upper = full.model$formula), direction = 'forward', k = 2, trace = 0)
attr(model.forwards$terms,"variables")

# Based on the stepwise variable selection above, we now run regressions.
reg3 <- lm(log(AE) ~Weight + Handedness + GP_1 + PRE_G_82 + PRE_PIM_82, data = forwards.train)
summary(reg3)
# Based on the results of the two models, reg3a seems to be better in both error and adj-R2.



#predict in testing error 
(forwards.pred<- predict(reg3,forwards.test))
sqrt(mean((forwards.test$AE - forwards.pred) ^ 2))


# Checking linear regression residual assumptions
par(mfrow = c(2, 2))
plot(reg3)

# Run Regression Tree Algorithm 500 times to get a distribution of optimal prune sizes to gain understanding.
# OPS.forwards <- rep(0, 500)
# for (i in 1:500){

  # Regression Tree
  forwards.c.tree <- tree(log(AE) ~ Country + Height + Weight + Handedness + GP_1 + PRE_G_82 + PRE_A_82 + PRE_PIM_82 + League_1, data = forwards.train)
  summary(forwards.c.tree)

  # Plot un-cut tree
  plot(forwards.c.tree)
  text(forwards.c.tree, pretty = 0)

  # Prune the tree
  set.seed(1)
  cv.forwards.c.tree <- cv.tree(forwards.c.tree, FUN = prune.tree, K = 10)
  cv.forwards.c.tree

  # Print best tree size
  (bestsize <- cv.forwards.c.tree$size[which.min(cv.forwards.c.tree$dev)])
  # OPS.forwards[i] <- bestsize
  
  # Based on the distribution of 500 regression trees, the manually determined optimal tree size is 6.
  #bestsize <- 6

  # Optimal size for pruned tree is 6. Prune the tree, Captain!
  prune.forwards.c.tree <- prune.tree(forwards.c.tree, best = 6)

  # Predict using the regression tree and determine the error.
  forwards.c.tree.predict <- predict(prune.forwards.c.tree, newdata = forwards.test)
  sqrt(mean((forwards.c.tree.predict - forwards.test$AE)^2))
  
  # Plot cut tree
  plot(prune.forwards.c.tree)
  text(prune.forwards.c.tree, pretty = 0)
# }

## Analysis on Defense ##

#### Response Defense[c] - Action Events (Points + Penalties) ####
# Linear Regression
empty.model <- glm(log(AE) ~ 1, data = defense.train)
full.model <- glm(log(AE) ~ Country + Height + Weight + Handedness + GP_1 + PRE_G_82 + PRE_A_82 + PRE_PIM_82 + League_1, data = defense.train)

model.backwards <- step(full.model, scope = list(lower = empty.model$formula, upper = full.model$formula), direction = 'backward', k = 2, trace = 0)
attr(model.backwards$terms,"variables")
model.forwards <- step(empty.model, scope = list(lower = empty.model$formula, upper = full.model$formula), direction = 'forward', k = 2, trace = 0)
attr(model.forwards$terms,"variables")

# Based on the stepwise variable selection above, we now run regressions.
reg6 <- lm(log(AE) ~  Weight + Handedness + PRE_PIM_82, data = defense.train)
summary(reg6)

# Checking linear regression residual assumptions
par(mfrow = c(2, 2))
plot(reg6)
# Scale-Location mean is not centered around 0.


#MSE for predict in testing 

(defense.pred<- predict(reg6,defense.test))
sqrt(mean((defense.test$AE - defense.pred) ^ 2))

# Run Regression Tree Algorithm 500 times to get a distribution of optimal prune sizes to gain understanding.
# OPS.defense <- rep(0, 500)
# for (i in 1:500){

  # Regression Tree
  defense.c.tree <- tree(log(AE) ~ Country + Height + Weight + Handedness + GP_1 + PRE_G_82 + PRE_A_82 + PRE_PIM_82 + League_1, data = defense.train)
  summary(defense.c.tree)

  # Plot un-cut tree
  plot(defense.c.tree)
  text(defense.c.tree, pretty = 0)

  # Prune the tree
  set.seed(1)
  cv.defense.c.tree <- cv.tree(defense.c.tree, FUN = prune.tree, K = 10)
  cv.defense.c.tree

  # Print best tree size
  (bestsize <- cv.defense.c.tree$size[which.min(cv.defense.c.tree$dev)])
  # OPS.defense[i] <- bestsize

  # Based on the distribution of 500 regression trees, the manually determined optimal tree size is 2.
  # bestsize <- 3
  
  # Optimal size for pruned tree is 3. Prune the tree, Captain!
  prune.defense.c.tree <- prune.tree(defense.c.tree, best = bestsize)

  # Predict using the regression tree and determine the error.
  defense.c.tree.predict <- predict(prune.defense.c.tree, newdata = defense.test)
  sqrt(mean((defense.c.tree.predict - defense.test$AE)^2))
  
  # Plot cut tree
  plot(prune.defense.c.tree)
  text(prune.defense.c.tree, pretty = 0)
# }

#### 12/5/19 @ 1:42PM, 500 runs of Regression Tree CV were run on Response = AE with the following most common non-1 sized Regression Trees: ####

# hist(OPS.hockey, breaks = seq(1, max(OPS.hockey), 1))
# Hockey : 7
# hist(OPS.forwards, breaks = seq(1, max(OPS.forwards), 1))
# Forwards : 3
# hist(OPS.defense, breaks = seq(1, max(OPS.hockey), 1))
# Defenseman : 2

#### Clustering ####
# With pr.out with PC = 3
x <- pr.out$x[,1:3]

kmDist <- rep(0, 20)
for (i in 1:20){
  hockey.km <- kmeans(x, centers = i, iter.max = 50, nstart = 50)
  kmDist[i] <- hockey.km$tot.withinss
}

par(mfrow = c(1, 1))
plot(kmDist, type = 'b')
# Based on the above plot, it seems that four clusters seems to be a good choice.
hockey.km <- kmeans(x, centers = 4, iter.max = 50, nstart = 50)
par(mfrow = c(3, ))
plot(x[,1], x[,2], col = hockey.km$cluster)
plot(x[,1], x[,3], col = hockey.km$cluster)
plot(x[,2], x[,3], col = hockey.km$cluster)
hockey$`Player Name`[hockey.km$cluster==1]

# With pr.out with Pc = 4
x <- pr.out$x[,1:4]

kmDist <- rep(0, 20)
for (i in 1:20){
  hockey.km <- kmeans(x, centers = i, iter.max = 50, nstart = 50)
  kmDist[i] <- hockey.km$tot.withinss
}

par(mfrow = c(1, 1))
plot(kmDist, type = 'b')
# Based on the above plot, it seems that four clusters seems to be a good choice.
hockey.km <- kmeans(x, centers = 5, iter.max = 50, nstart = 50)
par(mfrow = c(3, 2))
plot(x[,1], x[,2], col = hockey.km$cluster)
plot(x[,1], x[,3], col = hockey.km$cluster)
plot(x[,1], x[,4], col = hockey.km$cluster)
plot(x[,2], x[,3], col = hockey.km$cluster)
plot(x[,2], x[,4], col = hockey.km$cluster)
plot(x[,3], x[,4], col = hockey.km$cluster)
hockey$`Player Name`[hockey.km$cluster==1]

pr.out$rotation
# PC1 are pre-draft scorer with slightly above average pre-draft penalties. Below average physical size. Average pre-draft games played
# PC2 are above average physical size and moderately high penalties. Low pre-draft scoring ability. Below average pre-draft games played
# PC3 are poor physical size and scoring ability, but played a large amount of games and took many pre-draft penalties
# PC4 are players who played a ton of games, represents an average physical size, and slightly below average ability to score and do not take pre-draft penalties. Low impact players pre-draft
# PC5 are players who scored goals extremely well but performed below average on all other characteristics. Probably possessed physical density due to low height and high weight.

#### PCR Analysis ####
set.seed(1)
hockey.pcr.fit <- pcr(log(AE) ~ Height + Weight + GP_1 + PRE_G_82 + PRE_A_82 + PRE_PIM_82, data = hockey, scale = TRUE, validation = "CV", segments = 10)
summary(hockey.pcr.fit)
validationplot(hockey.pcr.fit, val.type = 'RMSEP', estimate = "CV")

hockey.train.pcr.fit <- pcr(log(AE) ~ Height + Weight + GP_1 + PRE_G_82 + PRE_A_82 + PRE_PIM_82, data = hockey.train, scale = TRUE, validation = "CV", segments = 10)
validationplot(hockey.train.pcr.fit, val.type = 'RMSEP', estimate = "CV")
(M <- which.min(hockey.train.pcr.fit$validation$PRESS))
hockey.train.pcr.pred <- predict(hockey.train.pcr.fit, hockey.test, ncomp = M)
sqrt(mean((hockey.train.pcr.pred - hockey.test$AE)^2))
hockey.pcr.fit.M <- pcr(log(AE) ~ Height + Weight + GP_1 + PRE_G_82 + PRE_A_82 + PRE_PIM_82, data = hockey, scale = TRUE, validation = "CV", segments = 10, ncomp = M)
summary(hockey.pcr.fit.M)


#### Response - Action Events (Points + Penalties) ####
# Linear Regression
empty.model <- glm(isStarPlayer ~ 1, data = hockey.train, family = 'binomial')
full.model <- glm(isStarPlayer ~ Country + Height + Weight + Position + Handedness + GP_1 + PRE_G_82 + PRE_A_82 + PRE_PIM_82 + League_1, data = hockey.train, family = 'binomial')

# Stepwise variable selection
model.backwards <- step(full.model, scope = list(lower = empty.model$formula, upper = full.model$formula), direction = 'backward', k = 2, trace = 0)
attr(model.backwards$terms,"variables")
model.forwards <- step(empty.model, scope = list(lower = empty.model$formula, upper = full.model$formula), direction = 'forward', k = 2, trace = 0)
attr(model.forwards$terms,"variables")

# Based on the stepwise variable selection above, we now run regressions.
reg <- glm(isStarPlayer ~ Height+ Weight+ Position+ Handedness+ GP_1+ PRE_G_82+ PRE_A_82+ League_1, data = hockey.train, family = 'binomial')
summary(reg)

# Checking linear regression residual assumptions
par(mfrow = c(2, 2))
plot(reg)

# PCA
pr.out <- prcomp(hockey[,c(5, 6, 10,11,12,13,14)], scale = TRUE)
biplot(pr.out)

# Create PVE plots
(pve <- pr.out$sdev^2/sum(pr.out$sdev^2))

# Plot PVE
# par(mfrow = c(1, 2))
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = 'b')
plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", type = 'b')

# Optimal PC = 3 or 4

# Run Regression Tree Algorithm 500 times to get a distribution of optimal prune sizes to gain understanding.
# OPS.hockey <- rep(0, 500)
# for (i in 1:500){

# Regression Tree 
hockey.b.tree <- tree(isStarPlayer ~ Team + Country + Height + Weight + Handedness + GP_1 + PRE_G_82 + PRE_A_82 + PRE_PIM_82 + League_1, data = hockey.train, split = 'deviance')
summary(hockey.b.tree)

# Plot un-cut tree
plot(hockey.b.tree)
text(hockey.b.tree, pretty = 0)

# Prune the tree
set.seed(1)
cv.hockey.b.tree <- cv.tree(hockey.b.tree, FUN = prune.tree, K = 10)
cv.hockey.b.tree

# Print best tree size
plot(cv.hockey.b.tree$size, cv.hockey.b.tree$dev, type = 'b', xlab = 'Tree Size', ylab = 'Deviance')
(bestsize <- cv.hockey.b.tree$size[which.min(cv.hockey.b.tree$dev)])
# OPS.hockey[i] <- bestsize

# Based on the distribution of 500 regression trees, the manually determined optimal tree size is 2.
# bestsize <- 2

# Optimal size for pruned tree is 4. Prune the tree, Captain!
prune.hockey.b.tree <- prune.tree(hockey.b.tree, best = bestsize)

# Predict using the regression tree and determine the error.
hockey.b.tree.predict <- predict(prune.hockey.b.tree, newdata = hockey.test)
sqrt(mean((hockey.b.tree.predict - hockey.test$AE)^2))
table(hockey.b.tree.predict, hockey.test$AE)

# Plot cut tree
plot(prune.hockey.b.tree)
text(prune.hockey.b.tree, pretty = 0)
# }
