install.packages("ggpubr")
library("ggpubr")
ggscatter(data, x = "price5", y = "price1", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

which(is.nan(data$price8), arr.ind = TRUE)
which(is.infinite(data$price8), arr.ind = TRUE)
