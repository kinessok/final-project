library(gtsummary)
library(tidyverse)
childcare_costs <- read_csv(here::here("childcare_costs.csv"))

tbl_summary(
	childcare_costs,
	by = study_year,
	include = c(unr_16,funr_16,unr_20to64,
							funr_20to64,flfpr_20to64,flfpr_20to64_6to17,pr_f,one_race_b
))

tbl_uvregression(
	childcare_costs,
	y = mhi_2018,
	include = c(
		unr_16,funr_16,unr_20to64,
		funr_20to64,flfpr_20to64,flfpr_20to64_6to17,pr_f,one_race_b),
	method = lm)
linear_model <- lm(mhi_2018 ~ unr_16 + funr_16 + funr_20to64,
									 data = childcare_costs)
ggplot(data = childcare_costs,
			 aes(x = flfpr_20to64,
			 		fill = flfpr_20to64))
geom_col(aes(y= mhi_2018))

#Scatterplot Figure

library(ggplot2)

ggplot(childcare_costs, aes(x = flfpr_20to64, y = mhi_2018)) +
	geom_point(color = "lightblue") +
	geom_smooth(method = "lm", color = "pink", se = TRUE) +
	labs(
		x = "Unemployment Rate of the Female Population (Ages 20–64, %)",
		y = "Median Household Income (2018 $)",
		title = "Relationship Between the Unemployment Rate of the Female Population and Household Income"
	) +
	theme_minimal()

