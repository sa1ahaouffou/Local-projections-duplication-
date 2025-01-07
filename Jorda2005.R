#install.packages("lpirfs")
library(lpirfs)

# Load (endogenous) data
endog_data <- interest_rules_var_data
# Estimate linear model
results_lin <- lp_lin(endog_data,
                      lags_endog_lin = 4,
                      trend=0,
                      shock_type=1,
                      confint=1.96,
                      hor=12)

# Show all impule responses
# Compare with Figure 5 in Jordà (2005)
plot(results_lin)

# Sauvegarde du graphique dans un fichier PNG
library(ggplot2)  # Pour utiliser ggsave
ggsave("results_lin_plot.png", plot(results_lin, type = "irf"), width = 8, height = 6)


# Make individual plots
linear_plots <- plot_lin(results_lin)
                     

# Show single plots
# * The first element of 'linear_plots' shows the response of the first
#   variable (GDP_gap) to a shock in the first variable (GDP_gap).
# * The second element of 'linear_plots' shows the response of the first
#   variable (GDP_gap) to a shock in the second variable (inflation).
# * ...
linear_plots[[1]]
linear_plots[[2]]

ggsave("results_lin_plot1.png", linear_plots[[1]], width = 8, height = 6)
ggsave("results_lin_plot2.png", linear_plots[[2]], width = 8, height = 6)

# Show diagnostics. The first element correponds to the first shock variable.
summary(results_lin)

# Load (endogenous) data
endog_data <- interest_rules_var_data

# Create exogenous data and data with contemporaneous impact (for illustration purposes only)
exog_data <- endog_data$GDP_gap*endog_data$Infl*endog_data$FF + rnorm(dim(endog_data)[1])
contemp_data <- endog_data$GDP_gap*endog_data$Infl*endog_data$FF + rnorm(dim(endog_data)[1])

# Exogenous data has to be a data.frame
exog_data    <- data.frame(xx = exog_data )
contemp_data <- data.frame(cc =  contemp_data)

# Estimate linear model
results_lin <- lp_lin(endog_data,
                      lags_endog_lin = 4,
                      trend= 0,
                      shock_type = 1,
                      confint=1.96,
                      hor = 12,
                      exog_data = exog_data,
                      lags_exog = 4,
                      contemp_data = contemp_data)

plot(results_lin)
summary(results_lin)






