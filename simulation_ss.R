## simulating a 'simple' RCT

## Generate a data frame of trial data under the continuous difference
## normally distributed model

create_trial_norm = function(
    n,      # number of participants in each group
    muA,    # mean for group A 
    muB,    # mean for group B
    sigmaA, # SD for group A
    sigmaB  # SD for group B
){
  dfA = data.frame(
    group = rep("A", n),
    outcome = rnorm(n, mean = muA, sd = sigmaA) 
  )
  dfB = data.frame(
    group = rep("B", n),
    outcome = rnorm(n, mean = muB, sd = sigmaB)
  )
  trial_df = rbind(dfA, dfB)
  trial_df$group = as.factor(trial_df$group)
  trial_df
}

run_ttest = function(
    data,           # RCT data with columns 'group' and 'outcome'
    alpha=0.05      # significance level of test
){
  test_result = t.test(
    outcome~group, 
    data=data, 
    alternative = "two.sided"
    )
  return(as.integer(test_result$p.value < alpha))
}


# single test for ex 2.1

create_trial_norm(100, 0, 3, 8, 8)

## run this 1000 times

test_vec = rep(NA, 1000)
for (i in 1:1000){
  dfi = create_trial_norm(112, 0, 3, 8, 8)
  test_vec[i] = run_ttest(dfi)
}
sum(test_vec)/1000
