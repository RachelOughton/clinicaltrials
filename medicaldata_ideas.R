### RCT possible examples

library(medicaldata)

data("indo_rct") # rx is the treatment arm

## Outcome is incidence of pancreatitits after endoscopic retrograde cholangiopancreatography - binary
# https://www.nejm.org/doi/full/10.1056/NEJMoa1111103


data("licorice_gargle") # all Likert scale outcomes, not sure about use? Preventing post-op cough type symptoms

data("opt") 
# The objective of this randomized controlled trial was to determine whether treatment of maternal periodontal disease can reduce risk of preterm birth and low birth weight
# Statistical analyses were carried out on an intent-to-treat basis. Gestational age can be thought of as ’time until end of pregnancy,’ for which certain survival analysis methods would be appropriate. The study used a log-rank test stratified by center to compare time until end of pregnancy for treatment and control groups.

data("polyps") # Number of polyps - baseline given, small numbers!

lm_polyps3m = lm(number3m ~ baseline + treatment, data=polyps)
summary(lm_polyps3m)

lm_polyps3m_int = lm(number3m ~ baseline + treatment + baseline:treatment, data=polyps)
summary(lm_polyps3m_int)

# 12m is a bit mad, 3m works well

lm_polyps12m = lm(number12m ~ baseline + treatment, data=polyps)
summary(lm_polyps12m)

lm_polyps12m_int = lm(number12m ~ baseline + treatment + baseline:treatment, data=polyps)
summary(lm_polyps12m_int)

data("strep_tb") # binary outcome. Also ethical issues!
# Results of a randomized, placebo-controlled, prospective 2-arm trial of streptomycin 2 grams daily (arm A2) vs. placebo (arm A1) to treat tuberculosis in 107 young patients, as reported by the Streptomycin in Tuberculosis Trials Committee in 1948 in the British Medical Journal


data("supraclavicular") # survival (with some censoring)

# Also 
library(HSAUR)

data("respiratory", package = "HSAUR") # cluster RCT with only two clusters

library(faraway)
data(toenail) # probably very good for introductory binary stuff!
# https://rss.onlinelibrary.wiley.com/doi/epdf/10.1111/1467-9876.00237


library(carData)
data("WeightLoss") # made up but might be useful

library(datarium) # quite a few!
data(stress) # ideal for ANCOVA with covariates!
lm_stresslin = lm(score ~ treatment + exercise + age, data = stress)
lm_sressint
