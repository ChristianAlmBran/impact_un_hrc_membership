# Loading packages
pacman::p_load(tidyverse, performance, plm, lmtest, stargazer, tseries, gridExtra)

# Creating the T variable
refined_df$time_trend <- refined_df$year - min(refined_df$year) + 1

# Converting the df in pdata.frame
refined_df <- pdata.frame(refined_df, index = c("countries", "year"))

# Testing for Unobserved heterogeneity - Pooled OLS or Fixed Effects
## Pooled OLS Model
pooled_ols_model <- plm(
  civil_liberties_index ~ membership + world_civil_liberties_index +
  president_council + target_special_rapporteur + n_rat_hr_treaties +
  regional_civil_liberties_index + gdp_per_capita_ppp + total_conflict +
  democracy_index, 
  data = refined_df,
  index = c("countries", "year"),
  model = "pooling"
)

### Model's summary
summary(pooled_ols_model)

### Checking for multicollinearity
check_collinearity(pooled_ols_model)

## Fixed Effects Model
fe_model <- plm(
  civil_liberties_index ~ membership + world_civil_liberties_index +
  president_council + target_special_rapporteur + n_rat_hr_treaties +
  regional_civil_liberties_index + gdp_per_capita_ppp + total_conflict +
  democracy_index, 
  data = refined_df,
  index = c("countries", "year"),
  model = "within"
)

### Model's summary
summary(fe_model)

### Checking for multicollinearity
check_collinearity(fe_model)

## F test to check for Unobserved heterogeneity
pFtest(fe_model, pooled_ols_model)

# Choosing between Random Effects or Fixed Effects
## Random Effects Model
ra_model <- plm(
  civil_liberties_index ~ membership + world_civil_liberties_index +
  president_council + target_special_rapporteur + n_rat_hr_treaties +
  regional_civil_liberties_index + gdp_per_capita_ppp + total_conflict +
  democracy_index, 
  data = refined_df,
  index = c("countries", "year"),
  model = "random"
)

### Model's summary
summary(ra_model)

### Checking for multicollinearity
check_collinearity(ra_model)

## Hausman test
phtest(fe_model, ra_model)

# Testing the fe model for Stationarity; Serial Correlation; and Cross-Sectional Dependence
## Stationarity - visualization (Mesquita et al., 2021)
refined_df_est <- refined_df %>%
  group_by(year) %>%
  mutate(mean_civil_liberties = mean(civil_liberties_index, na.rm=T))

### Graph
ggplot(refined_df_est, aes(x=year, y=civil_liberties_index, group=countries, color=countries))+
  geom_line() + geom_line(aes(x=year, y=mean_civil_liberties, group=1),
                          linetype="dashed", linewidth=3, color="black")+ ylab("Civil Liberty") +
  xlab("") + guides(color="none")

# Test ADF
values <- unique(refined_df_est$mean_civil_liberties)

adf.test(values)

### Fixing the no-stationary problem
#### Creating new df
refined_df_stationary <- refined_df

#### Running the model again
fe_model <- plm(
  civil_liberties_index ~ membership + world_civil_liberties_index +
    president_council + target_special_rapporteur + n_rat_hr_treaties +
    regional_civil_liberties_index + gdp_per_capita_ppp + total_conflict +
    democracy_index + time_trend, 
  data = refined_df_stationary,
  index = c("countries", "year"),
  model = "within"
)
summary(fe_model)

## Serial Correlation
pwartest(fe_model)

## Cross-Sectional Dependence
pcdtest(fe_model, test = "cd")

# Creating a new robust model - to deal with the Serial Correlation
fe_model_rbst <- coeftest(fe_model, vcov = vcovDC)

## Model's summary
print(fe_model_rbst)
summary(fe_model)

# Combining the summaries of the two models
stargazer(fe_model, fe_model_rbst, type = "text", 
          title = "Comparison of Fixed Effects Models", 
          column.labels = c("FE Model", "FE Model with Robust SE"),
          digits = 5) 


# Testing lag membership
## Loading the df containing the lag variables
lag_df <- read.csv("did_hrc_un_lag.csv")

lag_df <- lag_df %>%
  select(countries, year, membership_lag)

lag_df$year <- as.factor(lag_df$year)

refined_df_stationary <- refined_df_stationary %>%
  left_join(lag_df, by = c("countries", "year"))

## Lag Fixed Effects Model
fe_model_lag <- plm(
  civil_liberties_index ~ membership_lag + world_civil_liberties_index +
    president_council + target_special_rapporteur + n_rat_hr_treaties +
    regional_civil_liberties_index + gdp_per_capita_ppp + total_conflict +
    democracy_index + time_trend, 
  data = refined_df_stationary,
  index = c("countries", "year"),
  model = "within"
)

### Model's summary
summary(fe_model_lag)

### Checking for multicollinearity
check_collinearity(fe_model_lag)

# Creating a new robust model - to deal with the Serial Correlation
fe_model_lag_rbst <- coeftest(fe_model_lag, vcov = vcovDC)

## Model's summary
print(fe_model_lag_rbst)
summary(fe_model_lag)

# Combining the summaries of the four models
summary_table_all_models <- stargazer(fe_model, fe_model_rbst, fe_model_lag, fe_model_lag_rbst, type = "text", 
                                      title = "Comparison of Fixed Effects Models", 
                                      column.labels = c("FE Model", "FE Model with Robust", "FE Model Lag",
                                                        "FE Model with Robust Lag"), digits = 3) 

# Combining the summaries of the four models to export
stargazer(fe_model, fe_model_rbst, fe_model_lag, fe_model_lag_rbst, type = "html", 
          out = "Comparison of Fixed Effects Models.html", 
          title = "Regression Models Summary", column.labels = c("FE Model", "FE Model with Robust",
                                                                 "FE Model Lag", "FE Model with Robust Lag"),
          digits = 3)
