library(gtsummary)
library(tidyverse)
childcare_costs <- read_csv(here::here("childcare_costs.csv"))

tbl_summary(
	childcare_costs,
	by = study_year,
	include = c(unr_16,funr_16,unr_20to64,
							funr_20to64,flfpr_20to64,flfpr_20to64_6to17,pr_f,one_race_b),
	label= list(unr_16 ~ "Unemployment rate of the population ages 16 y/o <",
							funr_16 ~ "Unemployment rate of the female population ages 16 y/o <",
							unr_20to64 ~ "Unemployment rate of the population ages 20 - 64 y/o",
							funr_20to64 ~ "Unemployment rate of the female population ages 20 - 64 y/o",
							flfpr_20to64 ~ "Labor force participation rate of the female population ages 20 - 64 y/o",
							flfpr_20to64_6to17 ~ "Labor force participation rate of the female population ages 20 - 64 y/o who have children between 6 - 17 y/o",
							pr_f ~ "Poverty rate for families",
							one_race_b ~ "Percent of population that identifies as being one race and being only Black or African American"),
	missing_text = "Missing"
	)

tbl_uvregression(
	childcare_costs,
	y = mhi_2018,
	include = c(
		unr_16,funr_16,unr_20to64,
		funr_20to64,flfpr_20to64,flfpr_20to64_6to17,pr_f,one_race_b),
	method = lm)
linear_model <- lm(mhi_2018 ~ me_2018 + fme_2018 + mme_2018,
									 data = childcare_costs)
library(broom)
tidy(linear_model)

#Scatterplot Figure

library(ggplot2)

ggplot(childcare_costs,
			 aes(x = mme_2018, y = fme_2018)) +
	geom_point(aes(colour = mhi_2018)) +
	labs(
		x = "Median Earnings for the Male Population 16 y/o < (2018 $)",
		y = "Median Earnings for the Female Population 16 y/o < (2018 $)",
		color = "Median Household Income (2018 $)",
		title = "Median Incomes in 2018"
	) +
	theme_classic()

summarize_var <- function(data, variable) {
	data |>
	summarize(
						n = sum(!is.na({{variable}})),
						mean = mean({{ variable }}, na.rm = TRUE),
						sd = sd({{variable}}, na.rm = TRUE),
						min = min({{variable}}, na.rm = TRUE),
						max = max({{variable}}, na.rm = TRUE)
						)
}
summarize_var(childcare_costs, mhi_2018)
summarize_var(childcare_costs, unr_20to64)
summarize_var(childcare_costs, fme_2018)


