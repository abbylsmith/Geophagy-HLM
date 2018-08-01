library(tidyverse)
library(lme4)
library(ggthemes)
library(dplyr)
library(ggplot2)


# load a mids object
fin_geo <- readRDS(file = "fin_geo.rds")

# Define glmer model formula
mod <- geo ~ preg +  diar + posneg + geo_child + hemo_avg + (1|pid)

complete_data <- mice::complete(fin_geo, 7) #we choose the 7th dataset

complete_data_tib <- complete_data %>% as_tibble()

calculating_avg_hemo <- complete_data_tib %>%
  dplyr::select("pid", "hemo")%>%
  group_by(pid) %>%
  # Calculating each patients mean hemo 
  dplyr::summarise(mean_p_hemo = mean(hemo))  %>%  
  ungroup() %>%
  # Calculating the mean of the patient mean hemo 
  # Not equal to mean of all hemo because number of patient visits 
  # with hemo measurement varies 
  mutate(mean_mp_hemo = mean(mean_p_hemo))

complete_update <- complete_data_tib %>%
  left_join(calculating_avg_hemo, by = "pid") %>%
  mutate(# group mean center patient hemo -- each patient's hemo is centered around their own mean 
    # good for estimating WITHIN effect
    hemo_grpmc = hemo - mean_p_hemo,
    # grand mean center patient hemo -- each patient's hemo is centered around the mean hemo for the sample
    # Not useful because number of patient visits with hemo measurement varies
    hemo_grdmc = hemo - mean(hemo), 
    # Decide to collapse hemo measurement to a level-2 variable only (doesn't vary over visit)
    # This implies you'll be using the mean patient hemo as a predictor (mean_p_hemo)
    # This will give a good estimate of BETWEEN effect
    # grand mean centering mean_p_hemo improves interpretation of intercept
    mp_hemo_grdmc = mean_p_hemo - mean_mp_hemo 
  )


### Define glmer models to fit 
# For WITHIN estimate of effect of hemo
within_hemo <- geo ~ preg + diar  + posneg + geo_child + hemo_grpmc + (1|pid)
# For BETWEEN estimate of effect of hemo
between_hemo <- geo ~ preg + diar  + posneg + geo_child + mp_hemo_grdmc + (1|pid)

### Fit and store glmer model
# For WITHIN estimate of effect of hemo
library(lme4)
mod_within_hemo <- glmer(within_hemo, 
                         data = complete_update, family = binomial(link = "logit"), 
                         control=glmerControl('bobyqa'), nAGQ = 25)

# For BETWEEN estimate of effect of hemo
mod_between_hemo <- glmer(between_hemo, 
                          data = complete_update, family = binomial(link = "logit"), 
                          control=glmerControl('bobyqa'), nAGQ = 25)

# Inspecting models
summary(mod_within_hemo)
summary(mod_between_hemo)

# Extracting and storing useful fixed effects coefs
coef_within_hemo <- fixef(mod_within_hemo)
coef_between_hemo <- fixef(mod_between_hemo)


# Building a data set for graphics building
plot_dat <- complete_update %>%
  dplyr::select(geo, hemo, hemo_grpmc, mp_hemo_grdmc) %>%
  mutate(logit_within = predict(mod_within_hemo),
         prob_within = predict(mod_within_hemo, type = "response"),
         logit_between = predict(mod_between_hemo),
         prob_between = predict(mod_between_hemo, type = "response"))


# Logit to probability
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# Function to plot BETWEEN relationship for hemo and Geo (response) 
between_hemo_fn <- function(x){
  pred_logit <- as.numeric(coef_between_hemo["(Intercept)"]) + x*as.numeric(coef_between_hemo["mp_hemo_grdmc"])
  return(pred_logit)
}

# Function to plot Within relationship for hemo and Geo (response)
# Not really needed for the graphic we are building (only need the slope)
within_hemo_fn <- function(x){
  pred_logit <- as.numeric(coef_within_hemo["(Intercept)"]) + x*as.numeric(coef_within_hemo["hemo_grpmc"])
  return(pred_logit)
}

# Extra dataset for building graphic
# x := are the breaks selected by ggplot when plotting response Geo logit by mp_hemo_grdmc
# y := the estimated logits for the BETWEEN model
extra_plot_dat01 <- tibble(x = c(-4, -2, 0, 2),
                           y = between_hemo_fn(x))

# Extra info for plotting WITHIN model
x <- c(-4,-2,0,2) 
m <- as.numeric(coef_within_hemo["hemo_grpmc"])
b <- between_hemo_fn(x) - m*x

# Construct & store plot
between_within_plot <- ggplot(data = plot_dat) +
  # Plot BETWEEN model first
  stat_function(aes(x = mp_hemo_grdmc, y = logit_between), 
                fun = between_hemo_fn) +
  # Add secondary Y axis (prob. scale) on RHS of plot
  # Selecting on breaks for secondary scale depends on selection of
  # scale breaks on logit scale which is automatic done by ggplot 
  # MEANING -- we need to manually input the seq for breaks for sec. scale
  scale_y_continuous(name = "Logit Scaled Mean Hemo",
                     sec.axis = sec_axis(~ logit2prob(.), 
                                         name = "Probability of Geophagy",
                                         breaks = logit2prob(seq(-1,-4.5,by = -.5)),
                                         labels = scales::percent)) +
  # Transforming labels for X axis to patient mean hemo measurements
  scale_x_continuous(name = "Patient's Mean Hemo",
                     breaks = c(-4,-2,0,2),
                     labels = round(c(-4,-2,0,2) + complete_update$mean_mp_hemo[1], 2)) +
  # Adding a 4 "patients" to display WITHIN patient hemo model
  geom_point(data = extra_plot_dat01, aes(x = x, y = y), color = "red") +
  # Adding the 4 WITHIN hemo models
  # The limits are set on the orginal cale for X and must be altered manually
  stat_function(fun = function(x) b[1]+x*m,
                color = "red", xlim = c(-5, -3), linetype = "dashed") +
  stat_function(fun = function(x) b[2]+x*m,
                color = "red", xlim = c(-3, -1), linetype = "dashed") +
  stat_function(fun = function(x) b[3]+x*m,
                color = "red", xlim = c(-1, 1), linetype = "dashed") +
  stat_function(fun = function(x) b[4]+x*m,
                color = "red", xlim = c(1, 3), linetype = "dashed") +
  ggtitle("Probability of Geophagy Behaviors vs. \n Hemoglobin Levels")

# Plot with default theme
#between_within_plot

# Plot using minimal theme 
between_within_plot +
  theme_minimal()  
