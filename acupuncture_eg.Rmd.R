
The data for this example is taken from @vickers2004acupuncture.
In this study, 401 participants were recruited, all of whom suffered from chronic headaches. Participants were randomly allocated, using minimisation, to two groups:
  
  * Group $T$: up to 12 acupuncture treatments over 3 months
* Group $C$: usual care.

The primary outcome was 'headache score at 12 months', though many secondary outcomes were also included. The variables used in the minimisation algorithm were age, sex, diagnosis (migraine or tension type), headache score at baseline and number of years of headache disorder. The 'headache score' was found by asking each participant to complete a daily diary of headache and medication use for four weeks around each of the time points (baseline, 3 months, 12 months). 

```{r}
load("acu_df.Rdata")
# pk1 headache severity score baseline
# pk2 headache severity score 3 month
# pk5 headache severity score 1 year
# f1 headache frequency baseline
# f2 headache frequency 3 month
# f5 headache frequency 1 year

model_fit_df = data.frame(
  id = acu_df$id,
  score_baseline = acu_df$score_baseline,
  score_1year = acu_df$score_1year,
  group = acu_df$group
)
model_fit_df = model_fit_df[!is.na(model_fit_df$score_1year),]

lm_acu_lin = lm(score_1year ~ age + sex+ migraine + chronicity + score_baseline + group, data = acu_df )
summary(lm_acu_lin)

model_fit_df$fit_lin = fitted(lm_acu_lin)
model_fit_df$resid_lin = resid(lm_acu_lin)

ggplot(data=model_fit_df, aes(x=score_baseline, y=resid_lin, col=group)) + 
  geom_point() +
  geom_hline(yintercept=0)

```
This model clearly has a large amount of heteroscedasticity. There must be something we aren't capturing in our model. We can try a linear model with all interaction terms, to see if anything appears significant.

```{r}
lm_full_int = lm(score_1year ~ (age + sex+ migraine + chronicity + score_baseline + group)*(age + sex+ migraine + chronicity + score_baseline + group), data=acu_df)
summary(lm_full_int)

lm_acu_age = lm(score_1year ~ age + score_baseline + group, data = acu_df )
lm_acu_ageint = lm(score_1year ~ (age + score_baseline + group)*(age + score_baseline + group), data = acu_df )

summary(lm_acu_age)
summary(lm_acu_ageint)

fitted_age = fitted(lm_acu_age)

acu_df$fit_age = rep(NA, nrow(acu_df))
acu_df$fit_age[!is.na(acu_df$score_1year)] = fitted_age

ggplot(data = acu_df[!is.na(acu_df$score_1year), ], aes(x=score_baseline, y=score_1year, col=group)) + geom_point(alpha=0.25) +
  geom_point(aes(y=fit_age))+theme_bw()



ggplot(data=acu_df, aes(x=score_baseline, y=score_1year, col=group)) + 
  geom_point() + 
  geom_abline(
    slope = coef(lm_acu_lin)[["score_baseline"]],
    intercept = coef(lm_acu_lin)[["(Intercept)"]],
    colour = "red") +
  geom_abline(
    slope = coef(lm_acu_lin)[["score_baseline"]],
    intercept = coef(lm_acu_lin)[["(Intercept)"]] + coef(lm_acu_lin)[["group1"]],
    colour = "darkturquoise")
```
