library(gtsummary)
library(tidyverse)
library(here)

# DATA
# The dataset `childcare_costs` comes from the National Database of Childcare
# Prices (NDCP). It is the most comprehensive federal source of childcare prices
# at the county level. The database offers childcare price data by childcare
# provider type, age of children, and county characteristics. The data available
# is from 2008 to 2018.

# TABLE
#(@tbl-one) shows the data association by each characteristics

childcare_costs <- read_csv(here::here("childcare_costs.csv"))

table1 <- tbl_summary(
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

table1

uv_regression <- tbl_uvregression(
	childcare_costs,
	y = mhi_2018,
	include = c(
		unr_16,funr_16,unr_20to64,
		funr_20to64,flfpr_20to64,flfpr_20to64_6to17,pr_f,one_race_b),
	method = lm)|>
	bold_labels()

uv_regression

# The regression for the Unemployment rate of the population aged 16 years old
# or older is `r inline_text(uv_regression, variable = unr_16)`


#Scatterplot Figure
# Figure
#(@fig-scatter) shows a relationship between median earnings by gender in scatterform

library(ggplot2)

scatterplot <- ggplot(childcare_costs,
			 aes(x = mme_2018, y = fme_2018)) +
	geom_point(aes(colour = mhi_2018)) +
	labs(
		x = "Median Earnings for the Male Population 16 y/o < (2018 $)",
		y = "Median Earnings for the Female Population 16 y/o < (2018 $)",
		color = "Median Household Income (2018 $)",
		title = "Median Incomes in 2018"
	) +
	theme_classic()

scatterplot

#SAVING IMAGE

ggsave(plot= scatterplot, filename = here::here("project-image.png"))

summarize_var <- function(data, variable) {
	data |>
	summarize(
						n = sum(!is.na({{variable}})),
						mean = mean({{ variable }}, na.rm = TRUE),
					 )
}
summarize_var(childcare_costs, mhi_2018)
summarize_var(childcare_costs, unr_20to64)
summarize_var(childcare_costs, fme_2018)


