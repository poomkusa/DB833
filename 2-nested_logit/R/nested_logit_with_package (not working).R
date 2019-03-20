install.packages("mlogit")
library(mlogit)
library(dplyr)

data <- read.delim("/home/poom/Desktop/DB833/2-nested_logit/OJ300.txt")
data <- select(data,c(15:31))
#data <- cbind(price.NP = abs(rnorm(nrow(data))), data)
#data <- cbind(price.NP = sample(10, size = nrow(data), replace = TRUE), data)
data <- cbind(price.NP = 0, data)
# reverse transform one-hot encoding
data$chosen_choice <- names(select(data,c(10:18)))[max.col(select(data,c(10:18)))]
#data = data[data$NP!=TRUE, ]
#data$chosen_choice <- names(select(data,c(10:17)))[max.col(select(data,c(10:17)))]
# rename column for logit reshape
data <- data %>% rename(
  price.br1 = price1, price.br2 = price2,
  price.br3 = price3, price.br4 = price4,
  price.br5 = price5, price.br6 = price6,
  price.br7 = price7, price.br8 = price8
)
# checking
summary(data)
which(is.na(data$price.br1))
data <- data[complete.cases(data), ]
cor(select(data,c(1:9)), use="complete")

# reshape from each row is an observation (long) to each row is an alternative (wide)
# varying - alternative specific var
new_data <- mlogit.data(select(data,-c(10:18)), varying = c(1:9),
                        alt.levels = c('br1', 'br2', 'br3', 'br4', 'br5', 'br6', 'br7', 'br8', 'NP'),
                        shape = "wide", choice = "chosen_choice")
# new_data <- mlogit.data(select(data,-c(10:18)), varying = c(2:9),
#                         alt.levels = c('br1', 'br2', 'br3', 'br4', 'br5', 'br6', 'br7', 'br8'),
#                         shape = "wide", choice = "chosen_choice")
nl <- mlogit(formula = chosen_choice~price, data = new_data, shape='long', alt.var='alt', reflevel = 'br8',
             nests=list(no_purchase="NP",
                        purchase=c('br1', 'br2', 'br3', 'br4', 'br5', 'br6', 'br7','br8')), 
             un.nest.el = FALSE, unscaled = FALSE)
