#Step 3.1 is to call my dataset in read only mode. We are now using a different dataset then that we used for insight 1 and 2.
Wages <- read.csv("Wages.csv", head=TRUE, sep=",")

#Step 3.2 Ggplot has already been installed through I work I did in other parts of the project

#Step 3.2 Below I use a jitter plot to plot both wages with the average for each sex being displayed in separates lines of differnt colours colours (achieved with the geom_smooth()).
ggplot(data=Wages[Wages$exp>3 & Wages$exp<35,], 
       aes(x=ed, y=lwage, colour=sex)) +
  geom_point(alpha=0.5) +
  geom_smooth()

# Step 3.3 Count the values that are women and the values that are men. First I have to install and call the package "plyr"
install.packages("plyr")
library(plyr)

#Step 3.4 Create the data of the sex column
WagesSex <- data.frame(Wages[c("sex")])

#Step 3.5 Run the count function on that column
count(WagesSex, "sex")

#Step 3.6 Plot wage v experience levels of the sexes
ggplot(data=Wages[Wages$ed>1 & Wages$ed<60,], 
       aes(x=exp, y=lwage, colour=sex)) +
  geom_point(alpha=0.1) +
  geom_smooth()


#Step 3.7 Running a t-test to determine a significance between mean wages of females and mean wages of males
#First, I created a subset of all the rows where the column for sex is male and another subset for females
maleSS <- subset(Wages, sex == "male")
femaleSS <- subset(Wages, sex == "female")

# 3.8 Then I run a two sample t-test on both subsets to determine if there is a difference. The null hypothesis being that the means are not equal to 0 (paired = false). 
t.test(maleSS$lwage,femaleSS$lwage, alternative = "two.sided", paired = FALSE)

#Step 3.9 Plotting education v wages to determine where the most sample points are. Use of aplha level of 0.05 to give us
#a clearer image
ggplot(Wages, aes(ed, lwage),
       size=2, position = position_jitter(x = 2,y = 2) ) +
  geom_jitter(colour=alpha("black",0.05) )

#Step 3.10  Using a histogram to determine where the most sample points are. First part is to use the tapply function over a Ragged Array to give 
#us the sum of sample points per level of experience (column title 'ed' in the spreadsheet)
MWages <- tapply(Wages$lwage, Wages$ed, sum)

#Step 3.11 Plot it on a histogram. The x axis and y axs labels were changed to make it clearer.
#The bins of the histogram are 1 wide (1 year of education)
barplot(MWages, main="Where do the most sample points lie?", 
        ylab="Number of sample points", xlab="Years of education", col="orange")

#Step 3.12 Find the mean wages of men with 12 years of education vs the mean wages of women with 12 years of education. 
#The first part of this is to create a subset of our previous subset. This time we set ed to 12 for both male and female subset.
maleEd <- subset(maleSS, ed == 12)
femaleEd <- subset(femaleSS, ed == 12)

#Step 3.13 Apply the mean function to each subset to determine mean wages of boths males and females with 12 years of education. 
mean(maleEd$lwage)
mean(femaleEd$lwage)

#Step 3.14 Checking mean of people with 16 years of education. 
maleEd <- subset(maleSS, ed == 16)
femaleEd <- subset(femaleSS, ed == 16)

mean(maleEd$lwage)
mean(femaleEd$lwage)

#Step 3.15 Calculate gender pay gap percentage
x = (6.729774-6.255308)
paygap = (x/6.729774)*100

