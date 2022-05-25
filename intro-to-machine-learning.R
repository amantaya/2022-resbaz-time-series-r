# install package “VIM”
# install.packages("VIM")

# To use the package in an R session, we need to load it in an R session via
library(VIM)

# Load dataset “sleep”, which comes within the package “VIM”
data(sleep, package = "VIM")

# call function head() to get a feeling about data, or call sleep to see all values
head(sleep)

# download package “mice” and load it into R
# install.packages("mice")
library(mice)

# install tidyverse
# install.packages("tidyverse")
library("tidyverse")

# there are 62 rows with data
nrow(sleep)

# there are 42 rows from the sleep column with complete data (i.e. each row does not contain missing data)
# I don't like that the object is named sleep and there is also a column named "sleep"
sleep[complete.cases(sleep),] %>% nrow()

# note that packages are case sensitive
VIM::aggr(sleep, prop = FALSE, numbers = TRUE)

# vim:aggr does not work
?aggr

# call function marginplot (), pch indicates notation of obs, col tells R how you
# would like to see results in different color
marginplot(sleep[c("Gest", "Dream")],
           pch = c(20),
           col = c("darkgray","red","blue"))

boxplot(mpg ~ cyl,
        data = mtcars,
        col = "grey",
        main = "Mileage Data",
        ylab = "MPG",
        xlab = "Number of Cylinders")

# install.packages("vioplot")
library(vioplot)

v1 <- mtcars$mpg[mtcars$cyl == 4]
v2 <- mtcars$mpg[mtcars$cyl == 6]
v3 <- mtcars$mpg[mtcars$cyl == 8]

# what is the data type?
typeof(v1)

# what does this object contain?
str(v1)
str(v2)
str(v3)

# draw violin plots for vectors
vioplot(v1, v2, v3,
        names = c("4 cyl", "6 cyl", "8 cyl"),
        col = "gold")

#--------------#
# Neural Networks
#--------------#

# install.packages("ISLR")

library("ISLR")

# install.packages("neuralnet")

library(neuralnet)

# install.packages("caTools)
library("caTools")

head(College, 2)

tidyr::tibble(College)

maxs <- apply(College[,2:18], 2, max)
mins <- apply(College[,2:18], 2, min)

scaled_data <- as.data.frame(
  scale(College[,2:18],
        center = mins,
        scale = maxs)
  )

tidyr::tibble(scaled_data)

Private = as.numeric(College$Private) - 1

data = cbind(Private, scaled_data)

set.seed(101)

# split the data into train and test sets
split = caTools::sample.split(data$Private, SplitRatio = 0.70)

train = subset(data, split == TRUE)

test = subset(data, split == FALSE)

feats <- names(scaled_data)

f <- paste(feats, collapse = ' + ')

f <- paste("Private ~", f)

f <- as.formula(f)

nn <- neuralnet::neuralnet(f,
                train,
                hidden = c(10, 10, 10),
                linear.output = FALSE)

predicted_nn_values <- neuralnet::compute(nn, test[,2:18])

tidyr::tibble(predicted_nn_values$net.result)

predicted_nn_values$net.result <- sapply(predicted_nn_values$net.result,
                                         round, digits = 0)

table(test$Private, predicted_nn_values$net.result)

plot(nn)
