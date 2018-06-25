install.packages('carData')
library(carData)
library(MASS)
library(ggplot2)

#It is important to run this code line by line inorder to best understand it
#To show the first five observations of the dataset
head(Prestige,5)

#Summary of the relationship about the data
summary(Prestige)

#To perforrm linear regression
#step1 get a dataset to perform the analysis on
#Pick the first five columns of the dataset because regression analysis deals with only numeric values
newData <- Prestige[,1:5]

#optional steps
#Just to show the relationship about the data we are analyzing
cor(newData)
pairs(newData)
summary(newData)

#step 2 create a relational model between the variables
lmout = lm(income~education,data = newData)

#step3 find the coeficients from the model created
print(lmout)
#income =-2853.6 +898.8education

#step4 get the summary of the model to know the average error in prediction also know as residuals 
summary(lmout)
#step5 predict the values of the target
# our model takes on the regression eqn: income = -2853.6 +898.8education
#however this is not a perfect relationship i can illustrate using the predict function
a <- data.frame(education =13.11)
View(newData)
result <-  predict(lmout,a)
print(result)

#step6 visualize our model graphically
plot(Prestige$income~Prestige$education, main="A plot of income against level of education", xlab="education",ylab = "income")
#To add the line of bestfit on the graph
abline(lmout)
#other graphs
plot(lmout)


#For multiple regression
#It considers more than one predictor unlike linear regression but the process is all the same
#To create a relational model use the lm() again
lmout2 <- lm(income~education+prestige+census,data = newData)
print(lmout2)
#Comparing the values from the summary function, you will see that multiple regression gives a better fit of the data compared to linear regression
summary(lmout2)
residuals(lmout)

c <-data.frame(education=12.26,prestige=69.1,census=1130)
# Multiple regressoin model gives a better prediction compared to linear regression
result2 <-predict(lmout2,c)
print(result2)
plot(income~education+prestige+census,data = newData)




