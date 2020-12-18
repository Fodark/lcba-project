
################################################################
### Analysis of Choice Based Conjoint survey data            ### 
### The Multinomial Logit and Mixed Multinomial Logit models ###    
################################################################

# set the directory where the data are located
setwd("./LCBA/")

# load library for fitting multinomial logit models 
library(mlogit)

# import the data about the smartphoness Survey for conjoint analysis
smartphones <- read.csv("final.csv", sep=",")
names(smartphones) <- c("X", "smart.id", "quest.id", "smart.name", "profile", "price", "ram.storage", "os", "display", "dailyuse", "camera", "battery", "resp.id", "choice")
to_keep_cols <- c("resp.id", "quest.id", "profile", "price", "ram.storage", "os", "display", "dailyuse", "camera", "battery", "choice")
smartphones <- smartphones[,to_keep_cols]
smartphones$quest.id <- rep(1:1417, each= 3)

head(smartphones)

# see some descriptive statistics
summary(smartphones)
xtabs(choice ~ price, data=smartphones)
xtabs(choice ~ ram.storage, data=smartphones)
xtabs(choice ~ os, data=smartphones)
xtabs(choice ~ display, data=smartphones)
xtabs(choice ~ dailyuse, data=smartphones)
xtabs(choice ~ camera, data=smartphones)
xtabs(choice ~ battery, data=smartphones)



# recode some variables
smartphones$price <- as.factor(smartphones$price) # convert the variable as qualitative
smartphones$ram.storage <- factor(smartphones$ram.storage, levels = c("4/64", "8/128", "12/256")) # convert the variable as qualitative
smartphones$os <- factor(smartphones$os)
smartphones$display <- factor(smartphones$display, levels = c("Low", "Medium", "High"))
smartphones$dailyuse <- factor(smartphones$dailyuse, levels = c("Low", "Medium", "High"))
smartphones$camera <- factor(smartphones$camera, levels = c("Low", "Medium", "High"))
smartphones$battery <- factor(smartphones$battery, levels = c("Low", "Medium", "High"))
smartphones$profile <- as.factor(smartphones$profile)


# Fitting a choice model with "mlogit" function
# mlogit requires the choice data to be in a special data format created using the
# dfidx() function. You pass your choice data to dfidx, along
# with a few parameters telling it how the data is organized. 
# dfidx() accepts data in either a ?long? or a ?wide? format and you tell it 
# which you have using the shape parameter. 
smartphones.mlogit <- dfidx(smartphones, idx = list(c("quest.id", "resp.id"), "profile"), drop.index=F,
                        levels=c("1", "2", "3"))


# The resulting smartphones.mlogit is an mlogit.data object that can be used to estimate
# a model with mlogit(). The syntax for mlogit uses formula notation
# similarly to other functions for regression models in R.
# However, it requires the use of symbol "|" to distinguish between alternative-specific 
# and non-alternative specific variables, see ?Formula

m1 <- mlogit(choice ~ price + ram.storage + os + display + 
               dailyuse + camera + battery, data = smartphones.mlogit)
summary(m1)

# Fit the model without intercept parameters
m2 <- mlogit(choice ~ price + ram.storage + os + display + 
               dailyuse + camera + battery | -1, data = smartphones.mlogit)
summary(m2)

# Test the restriction on the intercepts by comparing the two models
# through a likelihood ratio test
lrtest(m2, m1)

# Fit the model without intercept parameters and with price as a quantitative variable
m3 <- mlogit(choice ~ as.numeric(as.character(price)) + ram.storage + os + display + 
               dailyuse + camera + battery | -1, data = smartphones.mlogit)
summary(m3)
lrtest(m3, m2)

# Compute the willingness to pay
coef(m3)["ram.storage8/128"]/(coef(m3)["as.numeric(as.character(price))"])
coef(m3)["ram.storage12/256"]/(coef(m3)["as.numeric(as.character(price))"])
coef(m3)["osiOS"]/(coef(m3)["as.numeric(as.character(price))"])
coef(m3)["displayMedium"]/(coef(m3)["as.numeric(as.character(price))"])
coef(m3)["displayHigh"]/(coef(m3)["as.numeric(as.character(price))"])
coef(m3)["dailyuseMedium"]/(coef(m3)["as.numeric(as.character(price))"])
coef(m3)["dailyuseHigh"]/(coef(m3)["as.numeric(as.character(price))"])
coef(m3)["cameraMedium"]/(coef(m3)["as.numeric(as.character(price))"])
coef(m3)["cameraHigh"]/(coef(m3)["as.numeric(as.character(price))"])
coef(m3)["batteryMedium"]/(coef(m3)["as.numeric(as.character(price))"])
coef(m3)["batteryHigh"]/(coef(m3)["as.numeric(as.character(price))"])

# Simulate preference shares using the "predict.mnl" function 
# Define the function
predict.mnl <- function(model, data) {
  # Function for predicting preference shares from a MNL model 
  # model: mlogit object returned by mlogit()
  # data: a data frame containing the set of designs for which you want to 
  #       predict shares.  Same format at the data used to estimate model. 
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  logitUtility <- data.model%*%model$coef
  share <- exp(logitUtility)/sum(exp(logitUtility))
  cbind(share, data)
}

# In order to use "predict.mnl", you need to define a data frame containing the set of designs 
# for which you want to predict the preference shares. 
# One way to do this is to create the full set of possible designs
# using expand.grid() and select the designs we want by row number
attributes <- list(ram.storage=names(table(smartphones.mlogit$ram.storage)),
               os=names(table(smartphones.mlogit$os)),
               display=names(table(smartphones.mlogit$display)),
               dailyuse=names(table(smartphones.mlogit$dailyuse)),
               camera=names(table(smartphones.mlogit$camera)),
               battery=names(table(smartphones.mlogit$battery)),
               price=names(table(smartphones.mlogit$price)))
allDesign <- expand.grid(attributes) 
allDesign #all possible design

# we choose a reasonable and realistic subset (where the first row indicates our design), such as
new.data <- allDesign[c(8, 1, 3, 41, 49, 26), ]
new.data

# We then pass these designs to predict.mnl() to determine what customers
# would choose if they had to pick among these six smartphones alternatives:
predict.mnl(m3, new.data) # using m3 specification
predict.mnl(m2, new.data) # using m2 specification

# Compute and plot preference share sensitivity
# Producing a sensitivity chart using R is relatively simple: we just need to loop through all
# the attribute levels, compute a preference share prediction, and save the predicted preference share for
# the target design. The "sensitivity.mnl" function does that.
sensitivity.mnl <- function(model, attrib, base.data, competitor.data) {
  # Function for creating data for a preference share-sensitivity chart
  # model: mlogit object returned by mlogit() function
  # attrib: list of vectors with attribute levels to be used in sensitivity
  # base.data: data frame containing baseline design of target product
  # competitor.data: data frame contining design of competitive set
  data <- rbind(base.data, competitor.data)
  base.share <- predict.mnl(model, data)[1,1]
  share <- NULL
  for (a in seq_along(attrib)) {
    for (i in attrib[[a]]) {
      data[1,] <- base.data
      data[1,a] <- i
      share <- c(share, predict.mnl(model, data)[1,1])
    }
  }
  data.frame(level=unlist(attrib), share=share, increase=share-base.share)
}
base.data <- new.data[1,]
competitor.data <- new.data[-1,]
(tradeoff <- sensitivity.mnl(m2, attributes, base.data, competitor.data))

barplot(tradeoff$increase, horiz=FALSE, names.arg=tradeoff$level,
        ylab="Change in Share for the Planned Product Design", 
        ylim=c(-0.1,0.11))
grid(nx=NA, ny=NULL)

#######################

### Controlling for consumer heterogeneity

# Fit a mixed MNL model
# The statistical term for coefficients that vary across respondents (or customers) is
# random coefficients or random effects. To estimate a multinomial
# logit model with random coefficients using "mlogit", we define a vector indicating
# which coefficients should vary across customers. "mlogit" requires a character
# vector the same length as the coefficient vector with a letter code indicating what
# distribution the random coefficients should follow across the respondents: ?n? for
# normal, ?l? for log normal, ?t? for truncated normal, and ?u? for uniform. For this
# analysis, we assume that all the coefficients are normally distributed across the population
# and call our vector "m2.rpar".

m2.rpar <- rep("n", length=length(m2$coef))
names(m2.rpar) <- names(m2$coef)
m2.rpar

# We pass this vector to mlogit as the rpar parameter, which is short for ?random
# parameters?. In addition, we tell mlogit that we have multiple choice observations
# for each respondent (panel=TRUE) and whether we want to allow the random
# parameters to be correlated with each other. For this first run, we assume that we do
# not want random parameters to be correlated (correlation=FALSE), a setting
# we reconsider below.
m2.mixed <- mlogit(choice ~ seat + engine + cargo + price | -1, 
                  data = smartphones.mlogit, 
                  panel=TRUE, rpar = m2.rpar, correlation = FALSE)
summary(m2.mixed)

# We can get a visual summary of the distribution of random effects and hence of the level of heterogeneity
plot(m2.mixed)

# We can extract the distribution of specific random effects using the function rpar()
seat8.distr <- rpar(m2.mixed, "seat8")
summary(seat8.distr)
mean(seat8.distr)
med(seat8.distr)
plot(seat8.distr)

# We can add that the random coefficients can be correlated
# This is easily done by including "correlations = TRUE" 
# as an option in the call to mlogit or by using the update function
# provided by mlogit
m2.mixed2 <- update(m2.mixed, correlation = TRUE)
summary(m2.mixed2)

# To get a better sense of the strength of the association among random coefficients, 
# we can extract the covariance matrix using "cov.mlogit" 
# and then convert it to a correlation matrix using "cov2cor" from base R.
cov2cor(cov.mlogit(m2.mixed2))

# We can also obtain the standard errors of the correlations among random effects,
# and hence perform significance test
summary(vcov(m2.mixed2, what = "rpar", type = "cor"))

# We may restrict the correlation to only random parameters with significant association
m2.mixed3 <- update(m2.mixed2, correlation = c("seat7", "seat8", "cargo3ft", "enginehyb",
                                             "price35", "price40"))

# The significant presence of random coefficients and their correlation 
# can be further investigated using the ML tests, such as the ML ratio test
lrtest(m2, m2.mixed) #Fixed effects vs. uncorrelated random effects
lrtest(m2.mixed, m2.mixed2) #Uncorrelated random effects vs. all correlated random effects
lrtest(m2.mixed3, m2.mixed2) #partially correlated random effects vs. all correlated random effects

# Simulating shares
# To compute share predictions with a mixed MNL model,
# we can use the "predict.mixed.mnl" function, which works in the same way as "predict.mnl",
# but with the difference that we now compute the preference shares for each of "nresp"
# newly sampled, representative respondents. The part worths for these respondents
# are drawn from a multivariate normal distribution with mean set at our estimated
# value of mu and covariance equal to our estimated value of Sigma (draws <-
# mvrnorm(n=nresp, coef.mu, coef.Sigma). The computation for each
# respondent is exactly the same as our computation in predict.mnl. Once we
# have the preference shares for all of the representative respondents, we average across respondents
# to get our overall preference share predictions. 

library(MASS)
predict.mixed.mnl <- function(model, data, nresp=1000) {
  # Function for predicting shares from a mixed MNL model 
  # model: mlogit object returned by mlogit()
  # data: a data frame containing the set of designs for which you want to 
  #       predict shares. Same format at the data used to estimate model. 
  # Note that this code assumes all model parameters are random
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  coef.Sigma <- cov.mlogit(model)
  coef.mu <- model$coef[1:dim(coef.Sigma)[1]]
  draws <- mvrnorm(n=nresp, coef.mu, coef.Sigma)
  shares <- matrix(NA, nrow=nresp, ncol=nrow(data))
  for (i in 1:nresp) {
    utility <- data.model%*%draws[i,]
    share = exp(utility)/sum(exp(utility))
    shares[i,] <- share
  }
  cbind(colMeans(shares), data)
}

set.seed(1111)
predict.mixed.mnl(m2.mixed2, data=new.data)

################################

### Assessing the effects of individual-level predictors
# To assess if consumer heterogeneity can be explained by their individual characteristics
# we can study the relationship between the individual part worth and the individual-level variables.
# Individual part worth can be extracted using fitted(), with the "type" argument set to "parameters". 
PW.ind <- fitted(m2.mixed2, type = "parameters")
head(PW.ind)

# We can use merge() to include the individual-level variable "carpool" 
carpool.data <- unique(smartphones[,c(1,4)])
names(PW.ind)[1] <- "resp.id"
PW.ind <- merge(PW.ind, carpool.data, by="resp.id")

# Let's focus on the seat8 random effect 
library(lattice)
histogram(~ seat8 | carpool, data=PW.ind)
boxplot(seat8 ~ carpool, data=PW.ind)
by(PW.ind$seat8, PW.ind$carpool, mean)
t.test(seat8 ~ carpool, data=PW.ind) # heterogeneity about preference for 8-seats is at least partially
                                     # significantly explained by carpool



