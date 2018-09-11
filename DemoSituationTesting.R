require(data.table)
require(stats)
require(MASS)
require(ggplot2)

setwd("~/Desktop/")
cred = fread("german_credit.csv")
## data set description is here
## https://archive.ics.uci.edu/ml/datasets/statlog+(german+credit+data)

## First code up the distance metric
## Different distance metric depending on whether columns are nominal, interval-scaled, or ordinal

## interval scaled: duration, credit_amount, installment_commitment, age, existing_credits, num_dependents
## nominal: credit_history, purpose, personal_status, other_parties, residence_since, property_magnitude, housing,
## job, other_payment_plans, own_telephone, foreign_worker, class
## ordinal: checking_status, savings_status, employment

interval.scaled.vars <-c("duration", "credit_amount", "installment_commitment", "age", "existing_credits", "num_dependents")
nominal.vars <- c("credit_history", "purpose", "personal_status", "other_parties", "residence_since", "property_magnitude", "housing",
                "job", "other_payment_plans", "own_telephone", "foreign_worker", "class")
ordinal.vars <- c("checking_status", "savings_status", "employment")

## check how missing values are designated
lapply(cred, function(x) sum(is.na(x)))


## write distance metrics
## preprocessing for interval scaled distance metric
## first compute the mean and sd of the interval scaled distance metrics
interval.means <- sapply(interval.scaled.vars, function(n) {
                                                  mean(cred[[n]])
                                                })

interval.mad <- sapply(interval.scaled.vars, function(n) {
  mean(abs(cred[[n]] - interval.means[[n]]))
})

lapply(interval.scaled.vars, function(n) {
                                compute.string = sprintf("%s_Z := (%s - interval.means[['%s']]) / interval.mad[['%s']]", n, n, n, n)
                                cred[, eval(parse(text = compute.string))]
                              })


## preprocessing for ordinal distance metric
lapply(ordinal.vars, function(n) {
  num.values <- length(unique(cred[[n]]))
  compute.string = sprintf("%s_I := (%s - 1) / (num.values - 1)", n, n)
  cred[, eval(parse(text = compute.string))]
})

distance.interval <- function(val1, val2) {
  abs(val1 - val2)
}

distance.nominal <- function(val1, val2) {
  val1 != val2
}

distance.ordinal <- function(val1, val2) {
  abs(val1 - val2)
}

compute.dist <- function(tuple1, tuple2) {
  tuple1 <- unlist(tuple1)
  tuple2 <- unlist(tuple2)
  sum.dists <- 0
  num.names <- length(names(tuple1))
  for (nm  in names(tuple1)) {
    if (nm %in% interval.scaled.vars) {
      sum.dists <- sum.dists + distance.interval(tuple1[[paste0(nm, "_Z")]], tuple2[[paste0(nm, "_Z")]])
      } else if (nm %in% nominal.vars) {
      sum.dists <- sum.dists + distance.nominal(tuple1[[nm]], tuple2[[nm]])
      } else if (nm %in% ordinal.vars) {
      sum.dists <- sum.dists + distance.ordinal(tuple1[[paste0(nm, "_I")]], tuple2[[paste0(nm, "_I")]])
      }
  }
  sum.dists / num.names
}

permitted.col.idxs = c(1:21)[c(-1, -10, -14, -21)]
permitted.col.idxs = c(permitted.col.idxs, 22:30)

## TEST FIRST
##mat <- matrix(data = 0, nrow = nrow(cred), ncol = nrow(cred))
##for (i in 1:nrow(cred)) {
##  for (j in 1:i) {
##    mat[i, j] <- mat[j, i] <- compute.dist(cred[i, permitted.col.idxs, with = FALSE], 
##                                           cred[j, permitted.col.idxs, with = FALSE])
##  }
##}

write.matrix(mat, "distances.mat")

## now we will do this "by hand" because we want to focus on in-group and out-group for protected groups
table(cred$personal_status)
ggplot(cred, aes(x=credit_amount, y=age, color=class)) + geom_point() + facet_grid(personal_status ~ .)


## now we detect the k-nearest neighbors in the protected group and not in the protected group
## in particular we look at the treatment of individuals in the protected group who received 
## let's begin with group of personal status = 2 (married/divorced females)
## so we will study everyone with personal status = 2 and a bad outcome of class = 0
## bad = class == 0


## first we need the indices of those falling in the class of (1) protected and (2) with a bad outcome
par(mfrow = c(2, 2))

for (prot in 1:4) {
  cred[class == 0 & personal_status == prot] ## there are 109 of these
  cred[, id := 1:nrow(cred)] ## put in an explicit row number for referencing

  protected.ids.bad.outcome <-cred[class == 0 & personal_status == prot]$id
  protected.ids   <- cred[personal_status == prot]$id
  unprotected.ids <- cred[personal_status != prot]$id

  ## find k nearest neighbors in both protected and unprotected groups
  k <- 16
  delta.p <-numeric()
  p.distances <- numeric()
  u.distances <- numeric()
  for (p in protected.ids) {
    dists <- mat[p, ]
    ids   <- 1:nrow(mat)
    ids <- ids[order(dists)]
  
    p.ids <- intersect(ids, protected.ids)  [2:(k+1)]
    u.ids <- intersect(ids, unprotected.ids)[1:k]
  
    p.prop <- mean(cred[p.ids]$class)
    u.prop <- mean(cred[u.ids]$class)
  
    delta.p <- c(delta.p, (p.prop - u.prop))
  
    p.distances <- c(p.distances, mean(mat[p, p.ids]))
    u.distances <- c(u.distances, mean(mat[p, u.ids]))
  }

  plot(ecdf(delta.p), main = sprintf("%d", prot))
  print(prot)
  print(summary(delta.p))
  print(mean(p.distances))
  print(mean(u.distances))
}

## legally speaking you are usually going to set some threshold "t-discriminatory"
## and respond to / try to prevent that
## we can do a simple pre-processing step to lower the amount of discrimination
## it is common to set the threshold of t around .1
## in this case that means that if t < -.1 we are going to flip the decision for members
## of the protected group
cred[class == 0 & personal_status == 2] ## there are 109 of these
cred[, id := 1:nrow(cred)] ## put in an explicit row number for referencing

protected.ids.bad.outcome <-cred[class == 0 & personal_status == 2]$id
protected.ids   <- cred[personal_status == 2]$id
unprotected.ids <- cred[personal_status != 2]$id

## find k nearest neighbors in both protected and unprotected groups
k <- 16
delta.p <-numeric()
p.distances <- numeric()
u.distances <- numeric()
flip.ids <- numeric()
for (p in protected.ids) {
  dists <- mat[p, ]
  ids   <- 1:nrow(mat)
  ids <- ids[order(dists)]
  
  p.ids <- intersect(ids, protected.ids)  [2:(k+1)]
  u.ids <- intersect(ids, unprotected.ids)[1:k]
  
  p.prop <- mean(cred[p.ids]$class)
  u.prop <- mean(cred[u.ids]$class)
  
  delta.p <- c(delta.p, (p.prop - u.prop))
  if (p.prop - u.prop < -0.1) {
    flip.ids <- c(flip.ids, p)
  }
  p.distances <- c(p.distances, mean(mat[p, p.ids]))
  u.distances <- c(u.distances, mean(mat[p, u.ids]))
}

cred[, newClass := class]
cred[flip.ids, newClass := 1]



