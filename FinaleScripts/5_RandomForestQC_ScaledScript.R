
library(randomForestSRC)
library(caret)
library(ROSE)
library(imbalance)
library(corrplot)
library(nortest)
library(GGally)

### script using random forest quantile classifier to create models and calculate variable importance
# variable importamce does not svae automatically so this will need to be saved manually and formated for further analysis 

### setting seed 

set.seed(101)

### setting variables

dir <- "results_good/CellInfo/"

datName1 <- "1_48_PAA_60_3"
datName2 <- "1_48_PAA_60_2"
datName3 <- "null"
  
dir.og <- "results_good/CellInfoOG/"

datName1.og <- "1_48_PAA_60_1_OG"
datName2.og <- "1_48_PAA_60_2_OG"
datName3.og <- "null"
  
varSelect <- c("condition", "densityVal", "liveMeanDist", "deadMeanDist", "deadNV", "liveNV", "n.tri", "dir.area")
#varSelect <- c("condition", "densityVal", "n.tri", "dir.area", "dist.1", "dist.2", "dist.3")

outputName <- "1_48_PAA_60"

### if functions for whether there are 2 or 3 images avalible 

if(datName3 == "null") {

  #### my segmentation 

  dat1 <- read.csv(paste0(dir, datName1, ".All.Inf.csv"))
  dat1$condition <- as.factor(dat1$condition)
  dat2 <- read.csv(paste0(dir, datName2, ".All.Inf.csv"))
  dat2$condition <- as.factor(dat2$condition)

  datAll <- rbind(dat1,dat2)

  train1 <- dat1[, varSelect] 
  train2 <- dat2[, varSelect]


  indexAll <- createDataPartition(datAll$condition, p = 0.7, list = FALSE)
  trainAll <- datAll[indexAll, varSelect]
  testAll <- datAll[-indexAll, varSelect]

  mod.1 <- imbalanced(condition ~ ., train1, importance = F)
  mod.2 <- imbalanced(condition ~ ., train2, importance = F)
  mod.All <- imbalanced(condition ~ ., trainAll, importance = T)


  pred1 <- predict(mod.1, train2)
  pred2 <- predict(mod.2, train1)
  predAll <- predict(mod.All, testAll)
  
  mod.ALL.sub <- subsample(mod.All, verbose = T)

#### Original data 
  
  dat1.og <- read.csv(paste0(dir.og, datName1.og, ".All.Inf.csv"))
  dat1.og$condition <- as.factor(dat1.og$condition)
  dat2.og <- read.csv(paste0(dir.og, datName2.og, ".All.Inf.csv"))
  dat2.og$condition <- as.factor(dat2.og$condition)
 
  datAll.og <- rbind(dat1.og,dat2.og)

  train1.og <- dat1.og[, varSelect]
  train2.og <- dat2.og[, varSelect]

  
  indexAll.og <- createDataPartition(datAll.og$condition, p = 0.7, list = FALSE)
  trainAll.og <- datAll.og[indexAll.og, varSelect]
  testAll.og <- datAll.og[-indexAll.og, varSelect]
  
  mod.1.og <- imbalanced(condition ~ ., train1.og, importance = F)
  mod.2.og <- imbalanced(condition ~ ., train2.og, importance = F)
  mod.All.og <- imbalanced(condition ~ ., trainAll.og, importance = T)
  
  pred1.og <- predict(mod.1.og, train2.og)
  pred2.og <- predict(mod.2.og, train1.og)
  predAll.og <- predict(mod.All.og, testAll.og)
  
  mod.ALL.og.sub <- subsample(mod.All.og, verbose = T)
  
  ### gathering results
  
  stats1 <- get.imbalanced.performance(pred1)
  stats2 <- get.imbalanced.performance(pred2)
  statsAll <- get.imbalanced.performance(predAll)
  
  stats1.og <- get.imbalanced.performance(pred1.og)
  stats2.og <- get.imbalanced.performance(pred2.og)
  statsAll.og <- get.imbalanced.performance(predAll.og)

  results <- cbind(stats1, stats2, statsAll)
  results.og <- cbind(stats1.og, stats2.og, statsAll.og)
  
} else {
  
  #### my segmentation 
  
  dat1 <- read.csv(paste0(dir, datName1, ".All.Inf.csv"))
  dat1$condition <- as.factor(dat1$condition)
  dat2 <- read.csv(paste0(dir, datName2, ".All.Inf.csv"))
  dat2$condition <- as.factor(dat2$condition)
  dat3 <- read.csv(paste0(dir, datName3, ".All.Inf.csv"))
  dat3$condition <- as.factor(dat3$condition)
  datAll <- rbind(dat1,dat2,dat3)
  
  train1 <- rbind(dat1[, varSelect], dat2[,varSelect]) ## test = dat3
  train2 <- rbind(dat2[, varSelect], dat3[,varSelect]) ## test = dat1
  train3 <- rbind(dat1[, varSelect], dat3[,varSelect]) ## test = dat2
  
  indexAll <- createDataPartition(datAll$condition, p = 0.7, list = FALSE)
  trainAll <- datAll[indexAll, varSelect]
  testAll <- datAll[-indexAll, varSelect]
  
  mod.1 <- imbalanced(condition ~ ., train1, importance = F)
  mod.2 <- imbalanced(condition ~ ., train2, importance = F)
  mod.3 <- imbalanced(condition ~ ., train3, importance = F)
  mod.All <- imbalanced(condition ~ ., trainAll, importance = T)
  
  
  pred1 <- predict(mod.1, train3)
  pred2 <- predict(mod.2, train1)
  pred3 <- predict(mod.3, train2)
  predAll <- predict(mod.All, testAll)
  
  mod.ALL.sub <- subsample(mod.All, verbose = T)
  
  #### Original data 
  
  dat1.og <- read.csv(paste0(dir.og, datName1.og, ".All.Inf.csv"))
  dat1.og$condition <- as.factor(dat1.og$condition)
  dat2.og <- read.csv(paste0(dir.og, datName2.og, ".All.Inf.csv"))
  dat2.og$condition <- as.factor(dat2.og$condition)
  dat3.og <- read.csv(paste0(dir.og, datName3.og, ".All.Inf.csv"))
  dat3.og$condition <- as.factor(dat3.og$condition)
  datAll.og <- rbind(dat1.og,dat2.og,dat3.og)
  
  train1.og <- rbind(dat1.og[, varSelect], dat2.og[,varSelect]) ## test = dat3
  train2.og <- rbind(dat2.og[, varSelect], dat3.og[,varSelect]) ## test = dat1
  train3.og <- rbind(dat1.og[, varSelect], dat3.og[,varSelect]) ## test = dat2
  
  indexAll.og <- createDataPartition(datAll.og$condition, p = 0.7, list = FALSE)
  trainAll.og <- datAll.og[indexAll.og, varSelect]
  testAll.og <- datAll.og[-indexAll.og, varSelect]
  
  mod.1.og <- imbalanced(condition ~ ., train1.og, importance = F)
  mod.2.og <- imbalanced(condition ~ ., train2.og, importance = F)
  mod.3.og <- imbalanced(condition ~ ., train3.og, importance = F)
  mod.All.og <- imbalanced(condition ~ ., trainAll.og, importance = T)
  
  pred1.og <- predict(mod.1.og, train3.og)
  pred2.og <- predict(mod.2.og, train1.og)
  pred3.og <- predict(mod.3.og, train2.og)
  predAll.og <- predict(mod.All.og, testAll.og)
  
  mod.ALL.og.sub <- subsample(mod.All.og, verbose = T)
  
  ### gathering results 
  
  stats1 <- get.imbalanced.performance(pred1)
  stats2 <- get.imbalanced.performance(pred2)
  stats3 <- get.imbalanced.performance(pred3)
  statsAll <- get.imbalanced.performance(predAll)
  
  stats1.og <- get.imbalanced.performance(pred1.og)
  stats2.og <- get.imbalanced.performance(pred2.og)
  stats3.og <- get.imbalanced.performance(pred3.og)
  statsAll.og <- get.imbalanced.performance(predAll.og)
  
  results <- cbind(stats1, stats2, stats3, statsAll)
  results.og <- cbind(stats1.og, stats2.og, stats3.og, statsAll.og)
  
}

par(mfrow = c(1,2))
plot.subsample(mod.ALL.sub, main = "my segmentation")
plot.subsample(mod.ALL.og.sub, main = "original segmentation")

mod.ALL.sub
mod.ALL.og.sub


write.csv(results, paste0("results_good/ML_ind/", outputName, ".csv"))
write.csv(results.og, paste0("results_good/ML_ind/", outputName, "_OG.csv"))




