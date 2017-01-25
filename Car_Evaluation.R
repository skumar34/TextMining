setwd("F:\\15BM6JP41_KAGGLE\\TEXT_MINING")

car_df = read.csv("CarEvalDataSet.csv", header = TRUE)

car_df = transform(car_df, CLASS_2 = as.factor(CLASS_2))

summary(car_df)

# Naive Bayes Algorithm
# Assumption : Ther feature are independent of each other.
# Compute the priors
library(class)
library(e1071)

results = data.frame(cbind(c(rep(0,10)),c(rep(0,10)),c(rep(0,10)),c(rep(0,10))))
names(results)[] = c("NO_TRAIN_SET","RECALL","PRECISION","F_SCORE")
t_set = 500

set.seed(1)
n = nrow(car_df)

for (i in 1:10){
t_set = t_set + 100
train_index = sample(c(seq(1,n)), t_set)

classifier = naiveBayes(car_df[train_index,1:6],car_df[train_index,8])
classifier$apriori
classifier$tables
classifier$levels
classifier$call
# Predict actual(rows) v/s predicted(cols). Creates the confusion matrix
contigency_tables = matrix(table(car_df[-train_index,8],predict(classifier,car_df[-train_index,-c(7,8)])), ncol = 2, nrow = 2)

# Note: Randon selection refers to a uniform distribution
# Precision : Fraction of Accepted instance that are actually accepted. The probability that a randonmly selected
# car from accepted is actually accepted
Precision = contigency_tables[2,2]/(sum(contigency_tables[,2]))

# Recall : Fraction of actually accepted instances that are predicted as accepted. The probability that a randonmly 
# selected car from actually selected pool is in predicted results
Recall = contigency_tables[2,2]/(sum(contigency_tables[2,]))

# F-Score : It is harmonic mean of precision and recall. 
F_score = 2/((1/Precision)+(1/Recall))

results$NO_TRAIN_SET[i] = t_set
results$PRECISION[i] = Precision
results$RECALL[i] = Recall
results$F_SCORE[i] = F_score
}
