require(arules)
require(arulesViz)
require(data.table)

cred = fread("german_credit.csv")
cred[, credScore := class]
cred[, ageBin := ifelse(age > 33, 0, 1)]
interval.scaled.vars <-c("duration", "credit_amount", "installment_commitment", "age", "existing_credits", "num_dependents")
nominal.vars <- c("credit_history", "purpose", "personal_status", "other_parties", "residence_since", "property_magnitude", "housing",
                  "job", "other_payment_plans", "own_telephone", "foreign_worker", "class")
ordinal.vars <- c("checking_status", "savings_status", "employment")

use.vars <- c(ordinal.vars, nominal.vars, "credScore", "ageBin")
cred.f <- cred[, use.vars, with = FALSE]
cred.f <- cred.f[, lapply(.SD, as.factor)]
cred.f[, class := NULL]
cred.transactions <- as(cred.f, "transactions")


credit_rules <- apriori(cred.transactions, 
                        parameter = list(support = 0.01, confidence = 0.7, maxlen = 4),
                        appearance = list(
                          rhs = c("credScore=1", "credScore=0")
                        ))

rules_conf <- sort (credit_rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(rules_conf[1:20])
## [10] {employment=5,                                                            
## personal_status=4}     => {credScore=1}   0.012          1 1.428571    12



credit_rules <- apriori(cred.transactions, 
                        parameter = list(support = 0.01, confidence = 0.7, maxlen = 4),
                                         appearance = list(
                                           rhs = c("credScore=1", "credScore=0"),
                                           lhs = c("personal_status=1", "personal_status=2",
                                                   "personal_status=3", "personal_status=4",
                                                   "ageBin=0", "ageBin=1")
                                           ))

rules_conf <- sort (credit_rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(rules_conf)

## but remember the baseline is .7!







