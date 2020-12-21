
################################################################
### Analysis of Choice Based Conjoint survey data            ### 
### The Multinomial Logit and Mixed Multinomial Logit models ###    
################################################################

# set the directory where the data are located
setwd("./university/3.lcba/report")

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

# New dataset with price ranges
new_df <- smartphones
new_df$price <- as.numeric(as.character(new_df$price))
new_df$price <- cut(new_df$price, c(-Inf, 200, 300, 500, 900, Inf), c("L","ML", "M", "MH", "H"))
new_df$price <- factor(new_df$price, levels = c("L","ML", "M", "MH", "H"))

new_df.mlogit <- dfidx(new_df, idx = list(c("quest.id", "resp.id"), "profile"), drop.index=F,
                            levels=c("1", "2", "3"))

# Fit the model without intercept parameters
m4 <- mlogit(choice ~ price + ram.storage + os + display + 
               dailyuse + camera + battery | -1, data = new_df.mlogit)
summary(m4)


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
attributes <- list(ram.storage=names(table(new_df.mlogit$ram.storage)),
               os=names(table(new_df.mlogit$os)),
               display=names(table(new_df.mlogit$display)),
               dailyuse=names(table(new_df.mlogit$dailyuse)),
               camera=names(table(new_df.mlogit$camera)),
               battery=names(table(new_df.mlogit$battery)),
               price=names(table(new_df.mlogit$price)))
allDesign <- expand.grid(attributes) 
allDesign #all possible design

allDesignLow <- allDesign[allDesign$price=="L" & allDesign$os=="Android" & allDesign$ram.storage=="4/64",]
row.names(allDesignLow) <- NULL
allDesignLow
new.data.low <- allDesignLow[c(67, 55, 68, 59, 32, 40, 31, 38, 41, 14, 23),]
new.data.low

allDesignMLow <- allDesign[allDesign$price=="ML" & allDesign$os=="Android" & allDesign$ram.storage!="12/256",]
row.names(allDesignMLow) <- NULL
allDesignMLow
new.data.medium.low <- allDesignMLow[c(143, 136, 119, 144, 132, 120, 88, 47, 48, 134, 83, 122, 11, 94),]
new.data.medium.low

allDesignMedium <- allDesign[allDesign$price=="M" & allDesign$os=="Android" & allDesign$ram.storage=="8/128",]
row.names(allDesignMedium) <- NULL
new.data.medium <- allDesignMedium[c(45, 69, 44, 68, 36, 60, 17, 18, 72),]
new.data.medium

allDesignMHigh <- allDesign[allDesign$price=="MH",]
row.names(allDesignMHigh) <- NULL
new.data.medium.high <- allDesignMHigh[c(319, 322, 242, 245, 302, 305, 249, 252, 320, 476, 297, 321, 429, 411),]
new.data.medium.high

allDesignHigh <- allDesign[allDesign$price=="H" & allDesign$ram.storage!="4/64",]
row.names(allDesignHigh) <- NULL
new.data.high <- allDesignHigh[c(202, 204, 213, 215, 94, 96, 178, 165, 18, 95, 20, 92, 90, 173, 175),]
new.data.high

# we choose a reasonable and realistic subset (where the first row indicates our design), such as
#new.data <- allDesign[c(8, 1, 3, 41, 49, 26), ]
#new.data

# We then pass these designs to predict.mnl() to determine what customers
# would choose if they had to pick among these six smartphones alternatives:
predict.mnl(m4, new.data.low) # using m4 specification
predict.mnl(m4, new.data.medium.low) # using m4 specification
predict.mnl(m4, new.data.medium) # using m4 specification
predict.mnl(m4, new.data.medium.high) # using m4 specification
predict.mnl(m4, new.data.high) # using m4 specification
#predict.mnl(m2, new.data) # using m2 specification

# 68 0.371647862        4/64 Android  Medium   Medium Medium    High     L
# 38 0.183155620        4/64 Android  Medium      Low Medium  Medium     L
# 132 0.2795641555       8/128 Android    High      Low Medium    High    ML
# 144 0.2623652226       8/128 Android    High     High Medium    High    ML
# 72 0.369881271       8/128 Android    High     High Medium    High     M
# 69 0.232265140       8/128 Android    High   Medium Medium    High     M
# 429 0.148297931      12/256 Android    High     High Medium    High    MH
# 321 0.121913149      12/256 Android    High     High   High  Medium    MH
# 305 0.117483196       8/128     iOS    High   Medium   High  Medium    MH
# 215 0.2581581870       8/128     iOS    High     High   High  Medium     H
# 204 0.2424517227      12/256     iOS    High   Medium   High  Medium     H
# 213 0.1124772553       8/128 Android    High     High   High  Medium     H

#allDesign[allDesign$ram.storage=="" & allDesign$os=="" & allDesign$display=="" & allDesign$dailyuse=="" & allDesign$camera=="" & allDesign$battery=="" & allDesign$price==""]
new.data <- allDesign[c(403, 223, 878, 914, 1400, 1382, 1887, 1779, 1763, 2267, 2250, 2264),]
predict.mnl(m4, new.data) # using m4 specification

library(parallel)
BootCI.predict.mnl <- function(model, data, nsim=500, conflevel=0.95) {
  dataModel <- model$model
  dataModel$probabilities <- NULL
  dataModel$linpred <- NULL
  idx <- dataModel$idx 
  dataModel$idx <- NULL
  dataModel <- data.frame(dataModel, idx)
  idVar <- unique(dataModel[,names(idx)[1]])
  
  bootstrapping <- function(x) {
    idbootsamp <- data.frame(sample(idVar, replace=T))
    names(idbootsamp) <- names(idx)[1]
    bootsamp <- merge(idbootsamp, dataModel, by=names(idx)[1], all.x=T)
    bootsamp[,names(idx)[1]] <- rep(1:length(table(idx[,1])), each=length(table(idx[,3])))
    bootsamp.mlogit  <- dfidx(bootsamp, idx = list(c(names(idx)[1:2]), names(idx)[3]),
                              drop.index=F)    
    bootfit <- update(model, data = bootsamp.mlogit)
    data.model <- model.matrix(update(bootfit$formula, 0 ~ .), data = data)[,-1]
    logitUtility <- data.model%*%bootfit$coef
    share <- exp(logitUtility)/sum(exp(logitUtility))
    share
  }
  
  cl <- makeCluster(detectCores())
  clusterEvalQ(cl, library(mlogit))
  clusterExport(cl, varlist=c("idVar", "dataModel", "idx", "model", "data"), 
                envir=environment())
  bootdistr <- parLapply(cl, 1:nsim, fun=bootstrapping)
  stopCluster(cl)
  
  bootdistr <- do.call(cbind, bootdistr)
  lowl <- (1-conflevel)/2
  upl <- 1-lowl  
  bootperc <- t(apply(bootdistr, 1, function(x) quantile(x, probs=c(lowl, upl))))
  pointpred <- predict.mnl(model, data)
  predictedShares <- cbind(pointpred[,1], bootperc, pointpred[,2:ncol(pointpred)])
  names(predictedShares)[1] <- "share" 
  predictedShares
}

# Confidence intervals for preference share
BootCI.predict.mnl(m4, new.data)

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
(tradeoff <- sensitivity.mnl(m4, attributes, base.data, competitor.data))

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

m4.rpar <- rep("n", length=length(m4$coef))
names(m4.rpar) <- names(m4$coef)
m4.rpar

# We pass this vector to mlogit as the rpar parameter, which is short for ?random
# parameters?. In addition, we tell mlogit that we have multiple choice observations
# for each respondent (panel=TRUE) and whether we want to allow the random
# parameters to be correlated with each other. For this first run, we assume that we do
# not want random parameters to be correlated (correlation=FALSE), a setting
# we reconsider below.
m4.mixed <- mlogit(choice ~ price + ram.storage + os + display + 
                     dailyuse + camera + battery | -1, data = new_df.mlogit,
                  panel=TRUE, rpar = m4.rpar, correlation = FALSE)
summary(m4.mixed)

# We can get a visual summary of the distribution of random effects and hence of the level of heterogeneity
plot(m4.mixed)

# We can extract the distribution of specific random effects using the function rpar()
iOS.distr <- rpar(m4.mixed, "osiOS")
summary(iOS.distr)
mean(iOS.distr)
med(iOS.distr)
plot(iOS.distr)

# We can add that the random coefficients can be correlated
# This is easily done by including "correlations = TRUE" 
# as an option in the call to mlogit or by using the update function
# provided by mlogit
m4.mixed2 <- update(m4.mixed, correlation = TRUE)
summary(m4.mixed2)

# To get a better sense of the strength of the association among random coefficients, 
# we can extract the covariance matrix using "cov.mlogit" 
# and then convert it to a correlation matrix using "cov2cor" from base R.
cov2cor(cov.mlogit(m4.mixed2))

# We can also obtain the standard errors of the correlations among random effects,
# and hence perform significance test
summary(vcov(m4.mixed2, what = "rpar", type = "cor"))

# We may restrict the correlation to only random parameters with significant association
m4.mixed3 <- update(m4.mixed2, correlation = c("ram.storage12/256", "osiOS",
                                             "displayHigh", "batteryMedium", "batteryHigh", "cameraHigh"))

# The significant presence of random coefficients and their correlation 
# can be further investigated using the ML tests, such as the ML ratio test
lrtest(m4, m4.mixed) #Fixed effects vs. uncorrelated random effects
lrtest(m4.mixed, m4.mixed2) #Uncorrelated random effects vs. all correlated random effects
lrtest(m4.mixed3, m4.mixed2) #partially correlated random effects vs. all correlated random effects

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
# Matrix isues with m4.mixed3, tet computed with m4.mixed2
# predict.mixed.mnl(m4.mixed3, data=new.data)
predict.mixed.mnl(m4.mixed2, data=new.data)

BootCI.predict.mixed.mnl <- function(model, data, nsim=500, conflevel=0.95, nresp=1000) {
  dataModel <- model$model
  dataModel$probabilities <- NULL
  dataModel$linpred <- NULL
  idx <- dataModel$idx 
  dataModel$idx <- NULL
  dataModel <- data.frame(dataModel, idx)
  idVar <- unique(dataModel[,names(idx)[1]])
  
  bootstrapping <- function(x) {
    idbootsamp <- data.frame(sample(idVar, replace=T))
    names(idbootsamp) <- names(idx)[1]
    bootsamp <- merge(idbootsamp, dataModel, by=names(idx)[1], all.x=T)
    bootsamp[,names(idx)[1]] <- rep(1:length(table(idx[,1])), each=length(table(idx[,3])))
    bootsamp.mlogit  <- dfidx(bootsamp, idx = list(c(names(idx)[1:2]), names(idx)[3]),
                              drop.index=F)    
    bootfit <- update(model, data = bootsamp.mlogit)
    data.model <- model.matrix(update(bootfit$formula, 0 ~ .), data = data)[,-1]
    coef.Sigma <- cov.mlogit(bootfit)
    coef.mu <- bootfit$coef[1:dim(coef.Sigma)[1]]
    draws <- mvrnorm(n=nresp, coef.mu, coef.Sigma)
    shares <- matrix(NA, nrow=nresp, ncol=nrow(data))
    for (i in 1:nresp) {
      utility <- data.model%*%draws[i,]
      share <- exp(utility)/sum(exp(utility))
      shares[i,] <- share
    }
    colMeans(shares)
  }
  
  cl <- makeCluster(detectCores())
  clusterEvalQ(cl, {
    library(mlogit)
    library(MASS) })
  clusterExport(cl, varlist=c("idVar", "dataModel", "idx", "model", "data", "nresp", 
                              paste(model$call$rpar)), envir=environment())
  bootdistr <- parLapply(cl, 1:nsim, fun=bootstrapping)
  stopCluster(cl)
  
  bootdistr <- do.call(cbind, bootdistr)
  lowl <- (1-conflevel)/2
  upl <- 1-lowl  
  bootperc <- t(apply(bootdistr, 1, function(x) quantile(x, probs=c(lowl, upl))))
  pointpred <- predict.mixed.mnl(model, data, nresp)
  predictedShares <- cbind(pointpred[,1], bootperc, pointpred[,2:ncol(pointpred)])
  names(predictedShares)[1] <- "share" 
  predictedShares
}

BootCI.predict.mixed.mnl(m4.mixed2, new.data, nresp = 5, nsim = 10)

#            share       2.5%      97.5% ram.storage      os display dailyuse camera battery price
# 403  0.031854328 0.04273707 0.04273707        4/64 Android  Medium   Medium Medium    High     L
# 223  0.058678450 0.09363941 0.09363941        4/64 Android  Medium      Low Medium  Medium     L
# 878  0.058508292 0.17224177 0.17224177       8/128 Android    High      Low Medium    High    ML
# 914  0.346623376 0.18093017 0.18093017       8/128 Android    High     High Medium    High    ML
# 1400 0.093521565 0.04122306 0.04122306       8/128 Android    High     High Medium    High     M
# 1382 0.057787223 0.07034814 0.07034814       8/128 Android    High   Medium Medium    High     M
# 1887 0.117215232 0.05581149 0.05581149      12/256 Android    High     High Medium    High    MH
# 1779 0.042145878 0.01301538 0.01301538      12/256 Android    High     High   High  Medium    MH
# 1763 0.081852186 0.19463009 0.19463009       8/128     iOS    High   Medium   High  Medium    MH
# 2267 0.045208413 0.03383555 0.03383555       8/128     iOS    High     High   High  Medium     H
# 2250 0.062132262 0.07041081 0.07041081      12/256     iOS    High   Medium   High  Medium     H
# 2264 0.004472795 0.03117705 0.03117705       8/128 Android    High     High   High  Medium     H
