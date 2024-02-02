## ----eval=F, echo=T-------------------------------------------------------------------------------------------------------
## install.packages("<packagename>")


## ----eval=F, echo=T-------------------------------------------------------------------------------------------------------
## install.packages(c("medicaldata", "ggplot2", "Minirand", "blockrand", "dplyr", "randomizeR"))


## ----echo=T---------------------------------------------------------------------------------------------------------------
library(medicaldata)
data("licorice_gargle")


## ----echo=T---------------------------------------------------------------------------------------------------------------
?licorice_gargle


## ----echo=T---------------------------------------------------------------------------------------------------------------
lic_garg = licorice_gargle[ ,1:8]
# vector of names of columns to be coerced to factor
cols <- c("preOp_gender", "preOp_asa",  
"preOp_mallampati", "preOp_smoking", "preOp_pain", "treat")
# convert each of those columns to factors
lic_garg[cols] <- lapply(lic_garg[cols], factor) 

# Check the result:
str(lic_garg)


## ----eval=F, echo=T-------------------------------------------------------------------------------------------------------
## ggplot(data=lic_garg, aes(x=preOp_pain, fill=preOp_gender)) +
##   geom_bar(col=1) +
##   facet_wrap(~preOp_smoking, nrow=1) +
##   theme(legend.position = "bottom")


## ----echo=T, results='hide'-----------------------------------------------------------------------------------------------
library(Minirand)

randbalance(
  trt = lic_garg$treat, 
  covmat = lic_garg[,-8], 
  ntrt=2, 
  trtseq = c("0", "1"))


## ----echo=T---------------------------------------------------------------------------------------------------------------
rb_tab = randbalance(
  trt = lic_garg$treat, 
  covmat = lic_garg[,-8], 
  ntrt=2, 
  trtseq = c("0", "1"))
rb_tab$preOp_gender


## ----echo=T---------------------------------------------------------------------------------------------------------------
ggplot(data=lic_garg) + geom_histogram(aes(x=preOp_age), binwidth=5)


## ----echo=T---------------------------------------------------------------------------------------------------------------
lic_garg$age[lic_garg$preOp_age < 50] <- "Under 50"
lic_garg$age[lic_garg$preOp_age >= 50 & lic_garg$preOp_age < 70] <- "50 to 70"
lic_garg$age[lic_garg$preOp_age >= 70] <- "70 plus"
lic_garg$age = as.factor(lic_garg$age)


## ----echo=T---------------------------------------------------------------------------------------------------------------
lic_garg$BMI[lic_garg$preOp_calcBMI < 18.5] <- "low"
lic_garg$BMI[lic_garg$preOp_calcBMI >= 18.5 & lic_garg$preOp_calcBMI < 25] <- "medium"
lic_garg$BMI[lic_garg$preOp_calcBMI >= 25] <- "high"
lic_garg$BMI = as.factor(lic_garg$BMI)


## ----echo=T---------------------------------------------------------------------------------------------------------------
lg_df = lic_garg[,c(1,2,5,6,7,9,10,8)]


## -------------------------------------------------------------------------------------------------------------------------
imbalance = function(
    df,   # participant data frame with allocation column included
    alloc # name of allocation column
    ){
  alloc_vec = as.factor(df[ ,names(df)==alloc])
  alloc_lev = levels(alloc_vec) # how the treatment groups are coded
  n1 = nrow(df[df[alloc]==alloc_lev[1],])
  n2 = nrow(df[df[alloc]==alloc_lev[2],])
  abs(n1-n2)
}


## ----echo=T---------------------------------------------------------------------------------------------------------------
imbalance(df = lic_garg, alloc = "treat")


## ----echo=T---------------------------------------------------------------------------------------------------------------
srs = function(
    df, # DF should be the participant data frame. 
        # A column 'treat' will be added
    levels = c("0", "1") # Levels of treat factor
){
  n = nrow(df) # number of rows / participants
# Create a new column 'treat'
  df$treat = rep(NA, n)
# work through the rows, randomly allocating patients with probably 1/2
  for (i in 1:n){
    df$treat[i] = sample(levels, size=1, prob = c(0.5, 0.5))
  }
  df$treat = as.factor(df$treat)
  df
}



## ----echo=T---------------------------------------------------------------------------------------------------------------
lg_srs = srs(
  df = lg_df[,-7],
  levels = c("T", "C")
)


## ----echo=T---------------------------------------------------------------------------------------------------------------
rb_tab = randbalance(
  trt = lg_srs$treat, 
  covmat = lg_srs[,-8], 
  ntrt=2, 
  trtseq = c("T", "C"))



## ----echo=T---------------------------------------------------------------------------------------------------------------
imbalance(lg_srs, alloc = "treat")


## ----echo=F---------------------------------------------------------------------------------------------------------------
library(blockrand)


## ----echo=T, eval=F-------------------------------------------------------------------------------------------------------
## blockrand(n=100)


## ----echo=T---------------------------------------------------------------------------------------------------------------
rpb_lg = blockrand(n=235, levels = c("T", "C"))


## ----echo=T---------------------------------------------------------------------------------------------------------------
# create the new data frame, a copy of lg_df
lg_rpb = lg_df  

# Replace the original treat column with the RPB treatment column
# Using only the first 235 allocations
lg_rpb$treat = rpb_lg$treatment[1:235]


## ----echo=T, eval=F-------------------------------------------------------------------------------------------------------
## randbalance(
##   trt = lg_rpb$treat,
##   covmat = lg_rpb[,-8],
##   ntrt=2,
##   trtseq = c("T", "C"))


## ----echo=T, eval=T-------------------------------------------------------------------------------------------------------
biased_coin = function(
    data,
    levels = c("T", "C"),
    p=2/3
){
  Dn = 0 # starting value of imbalance
  n = nrow(data)
  alloc = rep(NA, n)
  
  for (i in 1:n){
    if (Dn==0){ # equally balanced
      alloc[i] = sample(levels, size=1, prob=c(0.5, 0.5) )
    } else if(Dn<0){ # More allocations to levels[2] up to this point
      alloc[i] = sample(levels, size=1, prob=c(p, 1-p) )
    } else if(Dn>0){ # More allocations to levels[1] up to this point
      alloc[i] = sample(levels, size=1, prob=c(1-p, p) )
    }
    # Compute imbalance at this stage
    alloc_to_n = alloc[1:i]
    Dn = sum(alloc_to_n==levels[1]) - sum(alloc_to_n == levels[2])
  }
  data$treat = as.factor(alloc)
  data
}


## ----echo=T---------------------------------------------------------------------------------------------------------------
lg_bc1 = biased_coin(
  data=lg_df[,-8],
  p=0.9
)



## ----echo=T, eval=F-------------------------------------------------------------------------------------------------------
## randbalance(
##   trt = lg_bc1$treat,
##   covmat = lg_bc1[,-8],
##   ntrt=2,
##   trtseq = c("T", "C"))


## ----echo=T---------------------------------------------------------------------------------------------------------------
ud_31 = udPar(235, 3, 1, c("0", "1"))
seq_ud31 = genSeq(ud_31)



## ----echo=T---------------------------------------------------------------------------------------------------------------
seq_ud31@M[1,]


## ----echo=T---------------------------------------------------------------------------------------------------------------
# Add an ID variable so that we can keep track of the order of participants
lg_df$ID = 1:nrow(lg_df)
# split the data frame according to levels of factors
strat_gen_sm <- lg_df %>%
  group_split(preOp_gender, preOp_smoking) 


## ----echo=T---------------------------------------------------------------------------------------------------------------
group_sizes = sapply(
  1:length(strat_gen_sm),
  function(i){
    nrow(strat_gen_sm[[i]])
  }
)
group_sizes


## ----echo=T---------------------------------------------------------------------------------------------------------------
marg_imbalance = function(
    df,  # participant data frame, including allocation and all factor variables
    alloc, # name of allocation column
    factors # names of prognostic factors to be included
    ){
  n_fact = length(factors) # the numbers of factors
  imb_sum=0                # a running total of imbalance
  for (i in 1:n_fact){     # loop through the factors 
    ind_i = (1:ncol(df))[names(df)==factors[i]]
    col_i = as.factor(df[ ,ind_i])
    levels_i = levels(col_i)
    nlevels_i = length(levels_i)
    for (j in 1:nlevels_i){ # loop through the levels of factor i
      df_ij = df[df[ ,ind_i]==levels_i[j] , ]
      imb_ij = imbalance(df=df_ij, alloc=alloc) # find the imbalance for the sub-data-frame in which factor i has level j
    }
  }
  imb_sum
}


## ----echo=T---------------------------------------------------------------------------------------------------------------

marg_imbalance(df=lg_df, alloc="treat", factors = c("preOp_gender", "age"))



## ----echo=T---------------------------------------------------------------------------------------------------------------
# This command creates an empty list, which we will fill with allocation data frames as we go through
alloc_list = list()
# The loop works through the stratified data frames, applies SRS to allocate patients
# and stores them in alloc_list
for (i in 1:length(strat_gen_sm)){
  alloc_list[[i]] = srs(strat_gen_sm[[i]])
}
# bind all the data frames back together again
alloc_full= dplyr::bind_rows(alloc_list)
# re-order according to ID variable
alloc_full[order(alloc_full$ID),]


## ----echo=T---------------------------------------------------------------------------------------------------------------

## Information about the treatment
ntrt <- 3 # There will three treatment groups
trtseq <- c(1, 2, 3) # the treatment groups are indexed 1, 2, 3
ratio <- c(2, 2, 1)  # the treatment groups will be allocated in a 2:2:1 ratio

## The next few rows generate the participant data frame
nsample <- 120 # we will have 120 participants
c1 <- sample(seq(1, 0), nsample, replace = TRUE, prob = c(0.4, 0.6)) 
c2 <- sample(seq(1, 0), nsample, replace = TRUE, prob = c(0.3, 0.7))
c3 <- sample(c(2, 1, 0), nsample, replace = TRUE, prob = c(0.33, 0.2, 0.5)) 
c4 <- sample(seq(1, 0), nsample, replace = TRUE, prob = c(0.33, 0.67)) 
covmat <- cbind(c1, c2, c3, c4) # generate the matrix of covariate factors for the subjects
# label of the covariates 
colnames(covmat) = c("Gender", "Age", "Hypertension", "Use of Antibiotics") 
covwt <- c(1/4, 1/4, 1/4, 1/4) # equal weights/importance applied to each factor

## Applying the algorithm - start here if you already have participant data!

res <- rep(NA, nsample) # Generate a vector to store the results (the allocations)
# generate treatment assignment for the 1st subject
res[1] = sample(trtseq, 1, replace = TRUE, prob = ratio/sum(ratio)) 
# work through the remaining patients sequentially
for (j in 2:nsample)
  {
  # get treatment assignment sequentially for all subjects
  # The vector res is updated and so all previous allocations are accounted for
  # covmat is the data frame of participant data
    res[j] <- Minirand(
      covmat=covmat, j, covwt=covwt, ratio=ratio, ntrt=ntrt, trtseq=trtseq, method="Range", result=res, p = 0.9
      )
}
## Store the allocation vector 'res' as 'trt1'
trt1 <- res

# Display the number of randomized subjects at covariate factors
balance1 <- randbalance(trt1, covmat, ntrt, trtseq) 
balance1
# Calculate the total imbalance of the allocation
totimbal(trt = trt1, covmat = covmat, covwt = covwt, 
ratio = ratio, ntrt = ntrt, trtseq = trtseq, method = "Range")


## ----echo=T---------------------------------------------------------------------------------------------------------------
data(polyps)


## ----echo=T---------------------------------------------------------------------------------------------------------------
ggplot(data=polyps, aes(x=baseline)) + geom_histogram()
ggplot(data=polyps, aes(x=number12m)) + geom_histogram()


## ----echo=T---------------------------------------------------------------------------------------------------------------
polyps$log_baseline = log10(polyps$baseline)
polyps$log_number12m = log10(polyps$number12m)


## ----echo=T---------------------------------------------------------------------------------------------------------------
polyps_df = na.omit(polyps) # two participants (001 and 018) don't have data for 12m, so remove these
mean_T = mean(polyps_df$log_number12m[polyps_df$treatment=="sulindac"]) 
sd_T = sd(polyps_df$log_number12m[polyps_df$treatment=="sulindac"])
mean_C = mean(polyps_df$log_number12m[polyps_df$treatment=="placebo"])
sd_C = sd(polyps_df$log_number12m[polyps_df$treatment=="placebo"])
# There are 11 patients on Placebo (group C) and 9 on Sulindac (group T)
# The pooled standard deviation 
pooled_sd_polypsX = sqrt((10*sd_C^2 + 8*sd_T^2)/(10+8))
# Finally we find the test statistic
test_stat = (mean_T - mean_C)/(pooled_sd_polypsX*sqrt(1/11 + 1/9))


## ----echo=T---------------------------------------------------------------------------------------------------------------
2*pt(test_stat, df=18)


## ----echo=T, eval=F-------------------------------------------------------------------------------------------------------
## 2*(1-pt(test_stat, df=18))


## ----echo=T---------------------------------------------------------------------------------------------------------------
estimate = mean_T - mean_C
error = qt(0.975, df=18)*pooled_sd_polypsX*sqrt(1/11 + 1/9)
c(estimate - error, estimate + error)


## ----echo=T---------------------------------------------------------------------------------------------------------------
t.test(
  x=polyps_df$log_number12m[polyps_df$treatment == "sulindac"],
  y=polyps_df$log_number12m[polyps_df$treatment == "placebo"],
  alternative = "two.sided",
  var.equal=T, # this makes the method use pooled variances, as we did in lectures
  conf.level = 0.95 # note that this is 1-alpha
)


## ----echo=T---------------------------------------------------------------------------------------------------------------
polyps_df$diff = polyps_df$number12m - polyps_df$baseline
ggplot(data=polyps_df, aes(x=diff, fill=treatment)) + geom_histogram(position="dodge", bins=10)


## ----echo=T---------------------------------------------------------------------------------------------------------------
polyps_df$diff_log = polyps_df$log_number12m - polyps_df$log_baseline
ggplot(data=polyps_df, aes(x=diff_log, fill=treatment)) + geom_histogram(position="dodge", bins=10)


## ----echo=T---------------------------------------------------------------------------------------------------------------
t.test(
  x=polyps_df$diff_log[polyps_df$treatment == "sulindac"],
  y=polyps_df$diff_log[polyps_df$treatment == "placebo"],
  alternative = "two.sided",
  var.equal=T, # this makes the method use pooled variances, as we did in lectures
  conf.level = 0.95 # note that this is 1-alpha
)



## ----echo=T---------------------------------------------------------------------------------------------------------------
data(opt)
dim(opt)
str(opt)
?opt


## ----echo=T---------------------------------------------------------------------------------------------------------------

ggplot(data=opt, aes(x=Birthweight, fill=Group)) + geom_histogram(position="dodge")



## ----echo=T---------------------------------------------------------------------------------------------------------------
opt_red = opt[ ,c(1:22,72)] 
# Change NAs to "None" for diabetic
# since in opt the non-diabetic people are coded as NA (and therefore excluded from the model)
diab = as.character(opt_red$BL.Diab.Type)
diab[is.na(diab)] = "None"
opt_red$BL.Diab.Type = as.factor(diab)
# similar problem with smokers and how many cigarettes per day

# If people are non-smokers and have missing for number of cigarettes per day
# change their number of cigarettes to zero
sm = opt_red$Use.Tob
cigs = opt_red$BL.Cig.Day
cigs[(is.na(cigs)&(sm=="No "))] = 0
opt_red$BL.Cig.Day = cigs

# Same for alcohol and drinks per day

alc = opt_red$Use.Alc
dr = opt_red$BL.Drks.Day
dr[(is.na(dr)&(alc=="No "))] = 0
opt_red$BL.Drks.Day = dr

# If a participant hasn't had a previous pregnancy, her N.prev.preg should be zero (not NA)

pp = opt_red$Prev.preg
npp = opt_red$N.prev.preg
npp[pp=="No "] = 0
opt_red$N.prev.preg = npp



## ----echo=T---------------------------------------------------------------------------------------------------------------
# Check SDs are fairly close before proceeding
sd(opt_red$Birthweight[opt_red$Group == "T"], na.rm=T)
sd(opt_red$Birthweight[opt_red$Group == "C"], na.rm=T)

t.test(
  x=opt_red$Birthweight[opt_red$Group == "T"],
  y=opt_red$Birthweight[opt_red$Group == "C"],
  alternative = "two.sided",
  var.equal=T, # this makes the method use pooled variances, as we did in lectures
  conf.level = 0.95 # note that this is 1-alpha
)


## ----echo=T---------------------------------------------------------------------------------------------------------------
cor(opt_red$Birthweight, opt_red$Age, use = "complete.obs")


## ----echo=T---------------------------------------------------------------------------------------------------------------

lm_full = lm(Birthweight ~ ., data = opt_red[ ,-1]) # don't include the ID column!
summary(lm_full)


## ----echo=T, eval=F-------------------------------------------------------------------------------------------------------
## lm_eg = lm(Birthweight ~ Group + Age + Hypertension, data=opt_red)
## 


## ----echo=T---------------------------------------------------------------------------------------------------------------
opt_diag = na.omit(opt_red) # lm only fits where all variables are present
opt_diag$resid = resid(lm_full)
opt_diag$fitted = fitted(lm_full)


## ----echo=T---------------------------------------------------------------------------------------------------------------
ggplot(data=opt_diag, aes(x=resid, fill=Group)) + geom_histogram(position="dodge")


ggplot(data=opt_diag, aes(x=fitted, y=resid, col=Group)) + geom_point()




## ----echo=T---------------------------------------------------------------------------------------------------------------
exc_ind = !(opt_red$PID %in% opt_diag$PID)

opt_exc = opt_red[exc_ind, ]
# we can check this has worked by doing
nrow(opt_exc) # should be the difference between nrow(opt_red) and nrow(opt_diag)
na.omit(opt_exc) # should be empty, since each row should have at least one NA

