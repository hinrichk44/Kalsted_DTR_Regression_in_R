#For this project, we will be working with Decision Tree Regression
#In general Decision Trees do not need Feature Scaling because they are not based on Euclidean distances
#They are based on conditions

#Here we are importing our dataset
#We have 10 observations of three variables
#We have a person's position, level, and salary
salaries = read.csv('positions.csv')


#Here we will remove the Position column since it's redundant with Level
salaries =  salaries[2:3]



#Now we will get started on making the DTR regressor

#First we will install the rpart package
#The rpart code builds classification or regression models of a very general structure using a two stage procedure; 
#The resulting models can be represented as binary trees.
#install.packages('rpart')

#Here we will import the rpart library
library(rpart)


#Here we are officially setting up the DTR regressor
#We have to add a minsplit becuase that will create some conditions needed for our Decision Tree branches
regressor = rpart(formula = Salary ~ ., data = salaries, control = rpart.control(minsplit = 1))

#Here we are making a single prediction using our DTR regressor
#We are predicting the salary of someone with Level 6.5
#The result is $250,000
y_pred = predict(regressor, data.frame(Level = 6.5))


#Now we will visualize our Decision Tree results

#First we will import the ggplot2 library
library(ggplot2)


#Here we will initialize the plot for our DTR regression
ggplot() + 
  #Here we are stating that we are plotting points/observed variables with the color red
  geom_point(aes(x = salaries$Level, y = salaries$Salary), color = 'red') +
  #Here we are adding the predictions from the linear regression via a line plot
  #Remember that the salaries must be predicted so we use the predict function via our DTR regressor
  #The color of our prediction line will be blue
  geom_line(aes(x = salaries$Level, y = predict(regressor, newdata = salaries)), color = 'blue') +
  #Here is simple graph formatting with titles and axes
  ggtitle("Salaries vs. Level (DTR Regression)") +
  xlab('Level') +
  ylab('Salary')




#Here we will initialize the plot for our DTR regression, but with x_grid
x_grid = seq(min(salaries$Level), max(salaries$Level), 0.1)
ggplot() +
  geom_point(aes(x = salaries$Level, y = salaries$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Decision Tree Regression)') +
  xlab('Level') +
  ylab('Salary')