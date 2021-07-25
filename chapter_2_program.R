# Predictive Model for Los Angeles Dodgers Promotion and Attendance (R)

## line 4 and 5 is to import "Companion to Applied Regression" and "Trellis Graphis for R" packages
library(car)  # special functions for linear regression
library(lattice)  # graphics package

# read in data and create a data frame called dodgers
dodgers <- read.csv("dodgers.csv")
print(str(dodgers))  # check the structure of the data frame

# define an ordered day-of-week variable 
# for plots and data summaries

## line 16 to 24 code factorizes the categorical data and also replaces Monday with a short form "Mon" and same for the other weekdays.

dodgers$ordered_day_of_week <- with(data=dodgers,
  ifelse ((day_of_week == "Monday"),1,
  ifelse ((day_of_week == "Tuesday"),2,
  ifelse ((day_of_week == "Wednesday"),3,
  ifelse ((day_of_week == "Thursday"),4,
  ifelse ((day_of_week == "Friday"),5,
  ifelse ((day_of_week == "Saturday"),6,7)))))))
dodgers$ordered_day_of_week <- factor(dodgers$ordered_day_of_week, levels=1:7,
labels=c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun"))

# exploratory data analysis with standard graphics: attendance by day of week

##line 29 to 31 codes displays a boxplot with 1st, 2nd and 3rd quartile, min and max attendance by days of week

with(data=dodgers,plot(ordered_day_of_week, attend/1000, 
xlab = "Day of Week", ylab = "Attendance (thousands)", 
col = "violet", las = 1))

# when do the Dodgers use bobblehead promotions

##line 37 shows a table indicating the number of bobblehead promotions on each week day
with(dodgers, table(bobblehead,ordered_day_of_week)) # bobbleheads on Tuesday

# define an ordered month variable 
# for plots and data summaries

## line 43 to 51 code factorizes the categorical data and also replaces April with a short form "Apr" and same for the other months.
dodgers$ordered_month <- with(data=dodgers,
  ifelse ((month == "APR"),4,
  ifelse ((month == "MAY"),5,
  ifelse ((month == "JUN"),6,
  ifelse ((month == "JUL"),7,
  ifelse ((month == "AUG"),8,
  ifelse ((month == "SEP"),9,10)))))))
dodgers$ordered_month <- factor(dodgers$ordered_month, levels=4:10,
labels = c("April", "May", "June", "July", "Aug", "Sept", "Oct"))

# exploratory data analysis with standard R graphics: attendance by month 

##line 57 to 58 codes displays a boxplot with 1st, 2nd and 3rd quartile, min and max attendance by months

with(data=dodgers,plot(ordered_month,attend/1000, xlab = "Month", 
ylab = "Attendance (thousands)", col = "light blue", las = 1))

# exploratory data analysis displaying many variables
# looking at attendance and conditioning on day/night
# the skies and whether or not fireworks are displayed
library(lattice) # used for plotting 
# let us prepare a graphical summary of the dodgers data

## line 67 to 81 shows a chart of the attendence by temperature, and creates additional splits such as day and night, clear and cloudy, fireworks and no fireworks.  
group.labels <- c("No Fireworks","Fireworks")
group.symbols <- c(21,24)
group.colors <- c("black","black") 
group.fill <- c("black","red")
xyplot(attend/1000 ~ temp | skies + day_night, 
    data = dodgers, groups = fireworks, pch = group.symbols, 
    aspect = 1, cex = 1.5, col = group.colors, fill = group.fill,
    layout = c(2, 2), type = c("p","g"),
    strip=strip.custom(strip.levels=TRUE,strip.names=FALSE, style=1),
    xlab = "Temperature (Degrees Fahrenheit)", 
    ylab = "Attendance (thousands)",
    key = list(space = "top", 
        text = list(rev(group.labels),col = rev(group.colors)),
        points = list(pch = rev(group.symbols), col = rev(group.colors),
        fill = rev(group.fill))))  
                
# attendance by opponent and day/night game

## line 87 to 100 shows a chart of the attendence by opponent teams, and shows the day and night games in differently colored dots.  

group.labels <- c("Day","Night")
group.symbols <- c(1,20)
group.symbols.size <- c(2,2.75)
bwplot(opponent ~ attend/1000, data = dodgers, groups = day_night, 
    xlab = "Attendance (thousands)",
    panel = function(x, y, groups, subscripts, ...) 
       {panel.grid(h = (length(levels(dodgers$opponent)) - 1), v = -1)
        panel.stripplot(x, y, groups = groups, subscripts = subscripts, 
        cex = group.symbols.size, pch = group.symbols, col = "darkblue")
       },
    key = list(space = "top", 
    text = list(group.labels,col = "black"),
    points = list(pch = group.symbols, cex = group.symbols.size, 
    col = "darkblue")))
     
# employ training-and-test regimen for model validation

##line 105 to 114 codes separates the dataset into two randonly chosen subsets for trainng and testing purposes. 
set.seed(1234) # set seed for repeatability of training-and-test split
training_test <- c(rep(1,length=trunc((2/3)*nrow(dodgers))),
rep(2,length=(nrow(dodgers) - trunc((2/3)*nrow(dodgers)))))
dodgers$training_test <- sample(training_test) # random permutation 
dodgers$training_test <- factor(dodgers$training_test, 
  levels=c(1,2), labels=c("TRAIN","TEST"))
dodgers.train <- subset(dodgers, training_test == "TRAIN")
print(str(dodgers.train)) # check training data frame
dodgers.test <- subset(dodgers, training_test == "TEST")
print(str(dodgers.test)) # check test data frame

# specify a simple model with bobblehead entered last
##line 118 specifies the outcome and predicator variables of the model
my.model <- {attend ~ ordered_month + ordered_day_of_week + bobblehead}

# fit the model to the training set

##line 124 makes a linear regressoin model based on my.model relationship spcified above and the train dataset

train.model.fit <- lm(my.model, data = dodgers.train)
# summary of model fit to the training set

##line 128 shows the model performance such as R2 and the statistital significance of coeffcients of variables
print(summary(train.model.fit))
# training set predictions from the model fit to the training set
##line 131 to predict outcome variables based on the linear model and the train data
dodgers.train$predict_attend <- predict(train.model.fit) 

# test set predictions from the model fit to the training set
##line 135 to predict outcome variables based on the linear model and the test data
dodgers.test$predict_attend <- predict(train.model.fit, 
  newdata = dodgers.test)

# compute the proportion of response variance
# accounted for when predicting out-of-sample

##line 142 to 144 shows the corrleation between the predicted outcomes of the model and the actual outcomes
cat("\n","Proportion of Test Set Variance Accounted for: ",
round((with(dodgers.test,cor(attend,predict_attend)^2)),
  digits=3),"\n",sep="")

# merge the training and test sets for plotting
dodgers.plotting.frame <- rbind(dodgers.train,dodgers.test)

# generate predictive modeling visual for management

##line 152 to 172 shows the correlation chart between actual and predicted outcomes, split by train and test data. Also, the outcomes with and without bobblehead promotions are colored different.
group.labels <- c("No Bobbleheads","Bobbleheads")
group.symbols <- c(21,24)
group.colors <- c("black","black") 
group.fill <- c("black","red")  
xyplot(predict_attend/1000 ~ attend/1000 | training_test, 
       data = dodgers.plotting.frame, groups = bobblehead, cex = 2,
       pch = group.symbols, col = group.colors, fill = group.fill, 
       layout = c(2, 1), xlim = c(20,65), ylim = c(20,65), 
       aspect=1, type = c("p","g"),
       panel=function(x,y, ...)
            {panel.xyplot(x,y,...)
             panel.segments(25,25,60,60,col="black",cex=2)
            },
       strip=function(...) strip.default(..., style=1),
       xlab = "Actual Attendance (thousands)", 
       ylab = "Predicted Attendance (thousands)",
       key = list(space = "top", 
              text = list(rev(group.labels),col = rev(group.colors)),
              points = list(pch = rev(group.symbols), 
              col = rev(group.colors),
              fill = rev(group.fill))))            
        
# use the full data set to obtain an estimate of the increase in
# attendance due to bobbleheads, controlling for other factors 

##line 178 to 179 code makes a linear model based on the combined train and test data
my.model.fit <- lm(my.model, data = dodgers)  # use all available data
print(summary(my.model.fit))
# tests statistical significance of the bobblehead promotion
# type I anova computes sums of squares for sequential tests
## line 183 test the statistical significance of the model, i.e. whether the coefficients of variables are statistically significant
print(anova(my.model.fit))  

##line 186 to 188 are to show the coefficient of the bobblehead promotion variable, indicating the magnitude of its influence.
cat("\n","Estimated Effect of Bobblehead Promotion on Attendance: ",
round(my.model.fit$coefficients[length(my.model.fit$coefficients)],
digits = 0),"\n",sep="")

# standard graphics provide diagnostic plots

##line 193 shows the residuals between predicted and actual outcomes by different predicted values
plot(my.model.fit)

# additional model diagnostics drawn from the car package
library(car)

##line 199 shows the residual plot of each variables in the model
residualPlots(my.model.fit)
marginalModelPlots(my.model.fit)
print(outlierTest(my.model.fit))

# Suggestions for the student:
# Examine regression diagnostics for the fitted model.
# Examine other linear predictors and other explanatory variables.
# See if you can improve upon the model with variable transformations.



