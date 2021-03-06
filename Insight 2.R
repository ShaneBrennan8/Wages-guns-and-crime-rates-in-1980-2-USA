#Step 2.1 Call I access my Guns dataset in read only format. 
Guns <- read.csv("Guns.csv", head=TRUE, sep=",")

#Step 2.2 I select all the rows from the columns and prisoners using the select function. I also converted it into a dataframe so I can 
#do the statistical analysis on it that I want to do. 
GunsYI <- data.frame(Guns[c("year","income")])
GunsYI

GunsYP <- data.frame(Guns[c("year","prisoners")])
GunsYP

#Step 2.3 Plot the graph and then print the graph with an added title.  
#ggplot has already been installed from insight 1  so no need to install or call it this time.
GunsYIGGplot = ggplot(data=GunsYI,
                         aes(x=year, y=income)) +
  geom_point(alpha=5) +
  geom_smooth()

print(GunsYIGGplot + ggtitle ("Income USA 1977 - 1999"))

GunsYPGGplot = ggplot(data=GunsYP,
                      aes(x=year, y=prisoners)) +
  geom_point(alpha=5) +
  geom_smooth()

print(GunsYPGGplot + ggtitle ("Prisoners 1977 - 1999"))

#Step 2.4 We want to find out what how many people will be in prison when I start earning $100K. To determine this I need to figure out
#If we know the a intercept (coefficients), the slope (b) and the x value (100,000) we can determine the value of y. 
#To get these values will apply the lm() function to our dataset.

lm(GunsYI)

#Step2.5 After running lm(GunsYI) we can see that our Coefficent a =  1.969e+03, the slope for the income (b) is 1.364e-03. 
#applying y = a + bx we determine that it will be the year 2105.4 ad before the average income of the USA will be $100,000
a=1.969e+03
b=1.364e-03
x=100000
y = a+(b*x)
y

#Step 2.6 Determine how many prisoners will be in prison in 2105.4
#must determine linear regression of 
lm(GunsYP)

#After applying the linear regression model on the dataframe GunsYP we determined that a=1983.7633 and b=0.0187. We already have y, the year 2105.4 from the previous calculation
#Now we must determine x
#a=1983.7633
#b=0.0187
#y = a+(b*x)
#x = 6504.63

#Step 2.7 Use of predcit() to check my maths. Firstly I used lm() to get the slope of the year to prisoner model
GunsModel <- lm(year ~ prisoners, data = GunsYP)
GunsModel

predict(GunsModel, newdata = data.frame(prisoners=6504))

###Part two
#Step 2.8 Perform a Pearson's correlation test between on the prisoners and income columns in our dataset
cor(Guns$income, Guns$prisoners, method = "pearson")

#Step 2.9
cor.test(Guns$income, Guns$prisoners) 


