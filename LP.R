#This example replicates results from the Supplementary 
#Appendix by Ramey and Zubairy (2018) (RZ-18). 
#They use local projections to re-evaluate findings in Auerbach and Gorodnichenko (2012) (AG-12).


library(lpirfs)

url <- "https://github.com/AdaemmerP/lpirfs/raw/master/data/ag_data.RData"
download.file(url, destfile = "ag_data.RData", mode = "wb")

#ou
load("ag_data.RData")

ag_data <- ag_data

sample_start      <- 8
sample_end        <- dim(ag_data)[1]

# Endogenous variables
endog_data        <- ag_data[sample_start:sample_end,3:5]

# Shock variable 
shock             <- endog_data[, 1]

# Estimate model with constant and 4 lags
results_lin_iv <- lp_lin_iv(endog_data,
                            shock= shock,
                            lags_endog_lin = 4,
                            trend          = 0,
                            confint        = 1.96,
                            hor            = 20)

# Make and save plots
iv_lin_plots    <- plot_lin(results_lin_iv)
iv_lin_plots[[1]]

GYsc<- mean(exp(endog_data$GDP)/exp(endog_data$Gov))

multiplier_mean <- results_lin_iv$irf_lin_mean*GYsc
multiplier_up   <- results_lin_iv$irf_lin_up*GYsc
multiplier_low  <- results_lin_iv$irf_lin_low*GYsc

results_lin_iv  <- list(irf_lin_mean  = multiplier_mean,
                        irf_lin_up    = multiplier_up,
                        irf_lin_low   = multiplier_low,
                        specs         = results_lin_iv$specs)

iv_lin_plots[[3]]


install.packages("tinytex")
tinytex::install_tinytex(force = TRUE)
