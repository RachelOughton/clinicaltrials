### RCT possible examples

library(medicaldata)

data("indo_rct") # rx is the treatment arm

## Outcome is incidence of pancreatitits after endoscopic retrograde cholangiopancreatography - binary
# https://www.nejm.org/doi/full/10.1056/NEJMoa1111103

summary(glm(outcome ~ site + risk + rx, data=indo_rct, family = binomial(link = "logit")))


summary(glm(outcome ~ ., data=indo_rct, family = binomial(link = "logit")))


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

lm_polyps12m = lm(log(number12m) ~ log(baseline) + treatment, data=polyps)
summary(lm_polyps12m)

lm_polyps12m_int = lm(number12m ~ baseline + treatment + baseline:treatment, data=polyps)
summary(lm_polyps12m_int)

data("strep_tb") # binary outcome. Also ethical issues!
# Results of a randomized, placebo-controlled, prospective 2-arm trial of streptomycin 2 grams daily (arm A2) vs. placebo (arm A1) to treat tuberculosis in 107 young patients, as reported by the Streptomycin in Tuberculosis Trials Committee in 1948 in the British Medical Journal
summary(glm(improved ~ arm + dose_strep_g, data=strep_tb, family = binomial(link = "logit")))


data("supraclavicular") # survival (with some censoring, but rather simple)

# Also 
library(HSAUR)

data("respiratory", package = "HSAUR") # Not a cluster RCT, . Binary outcome I think?
# Each patient recorded at months 0, 1, 2, 3, 4
# Works but not a super proper trial I don't think.

data(mastectomy)  # Survival Times after Mastectomy of Breast Cancer Patients

library(faraway)
data(toenail) # probably very good for introductory binary stuff!
# https://rss.onlinelibrary.wiley.com/doi/epdf/10.1111/1467-9876.00237


library(carData)
data("WeightLoss") # made up but might be useful. Although two treatment effects!

library(datarium) # quite a few!
data(stress) # ideal for ANCOVA with covariates!
lm_stresslin = lm(score ~ treatment + exercise + age, data = stress)
lm_sressint


library(MASS)

data(Melanoma) # Survival from Malignant Melanoma - no treatment but could still be good?
data(VA) # Veteran's Administration lung cancer trial from Kalbfleisch & Prentice. Survival data.
data(anorexia) # The anorexia data frame has 72 rows and 3 columns. Weight change data for young female anorexia patients. Continuous. Three treatments.
data(gehan) # A data frame from a trial of 42 leukaemia patients. Some were treated with the drug 6-mercaptopurine and the rest are controls. The trial was designed as matched pairs, both withdrawn from the trial when either came out of remission.

library(survival) # lots of data in here!

diabetic #  
cgd # Data are from a placebo controlled trial of gamma interferon in chronic granulotomous disease (CGD). Contains the data on time to serious infections observed through end of study for each patient.

flchain # stratified sample - useful for modelling?

gbsg # breast cancer. Lots of data, various covariates

lung # Survival in patients with advanced lung cancer from the North Central Cancer Treatment Group. Performance scores rate how well the patient can perform usual daily activities.

myeloma # Survival times of 3882 subjects with multiple myeloma, seen at Mayo Clinic from 1947–1996.

ovarian # simple trial, with two arms

## Haven't finished looking, but these are in the datasets part of the index of the survival manual
