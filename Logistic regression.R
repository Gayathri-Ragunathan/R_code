getwd()


'Importing the dataset'
dataset = read.csv('property-sales.csv')

'Encoding the target feature as factor'
dataset$Fireplace= factor(dataset$Fireplace, levels = c('N', 'Y'), labels = c(0,1))

'Creating a new data-frame'
dataset = dataset[c(9,13,15)]

'Splitting the dataset into the Training set and Test set'
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Fireplace, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

'Feature Scaling'
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

'Fitting Logistic Regression to the Training set'
logreg = glm(formula = Fireplace ~ GrLivArea+KitchenAbvGr, family = binomial, data = training_set)
summary(logreg)

'Predicting the Test set results'
prob_pred = predict(logreg, type = 'response', newdata = test_set[-3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)
y_pred

'Building Confusion Matrix'
cm = table(test_set[, 3], y_pred > 0.5)
cm

'Visualising the Training set results'
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('GrLivArea', 'KitchenAbvGr')
prob_set = predict(logreg, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3], main = 'Logistic Regression (Training set)', xlab = 'GrLivArea', ylab = 'KitchenAbvGr', xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('GrLivArea', 'KitchenAbvGr')
prob_set = predict(logreg, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'GrLivArea', ylab = 'KitchenAbvGr',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
