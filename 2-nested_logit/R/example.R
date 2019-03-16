install.packages("mlogit")
install.packages("AER")
library(mlogit)

data("TravelMode", package = "AER")
TravelMode$avincome <- with(TravelMode, income * (mode == "air"))
TravelMode$time <- with(TravelMode, travel + wait)/60
TravelMode$timeair <- with(TravelMode, time * I(mode == "air"))
TravelMode$income <- with(TravelMode, income / 10)
# Hensher and Greene (2002), table 1 p.8-9 model 5
# value == income if mode is air or car. otherwise value == 0
TravelMode$incomeother <- with(TravelMode, ifelse(mode %in% c('air', 'car'), income, 0))
# shape - long if each row is an alternative or wide if each row is an observation
# alt.var - alternative specification column
nl <- mlogit(choice~gcost+wait+incomeother, TravelMode,
             shape='long', alt.var='mode',
             nests=list(public=c('train', 'bus'), other=c('car','air')))
# same with a comon nest elasticity (model 1)
# update - refit model
# un.nest.el - if TRUE, the hypothesis of unique elasticity is imposed for nested logit models
nl2 <- update(nl, un.nest.el = TRUE)