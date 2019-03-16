install.packages("mlogit")
library(mlogit)
library(dplyr)

data <- read.delim("C:/Users/ThisPC/Desktop/PhD/1y2s/DB833 Empirical Research in Marketing/8 nested logit/hw/OJ300.txt")
data <- select(data,c(15:31))
data <- cbind(price.NP = 0, data)
# reverse transform one-hot encoding
data$chosen_choice <- names(select(data,c(10:18)))[max.col(select(data,c(10:18)))]
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

# convert T/F to 1/0
new_data$chosen_choice <- as.integer(as.logical(new_data$chosen_choice))
# export
setwd("C:/Users/ThisPC/Desktop/")
write.csv(new_data, "manipulated_data.csv")
