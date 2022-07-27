UPDATING & PUBLISHING APP

1. update/run data_prep.R to generate the following files:
	- r_data/d01_sa_output.rda
	- r_data/d02_pct_impact.rda
2. copy the following files to app_data folder
	- r_data/d01_sa_output.rda
	- r_data/d02_pct_impact.rda
	- raw_data/metric_lookup.csv [update if necessary]
3. run app.R to check for errors
4. **publish to shinyapp.io by clicking the publish button in RStudio, 
   next to the Run App button in the upper right corner of the script window. 
   Select the following files to publish:
	- app.R
	- app_data/d01_sa_output.rda
	- app_data/d02_pct_impact.rda
	- app_data/metric_lookup.csv
	- www/legend_1.PNG
	- www/legend_2.PNG

ADDITIONAL STEPS BEFORE PUBLISHING FOR THE FIRST TIME

1. insteall rsconnect by running: install.packages(rsconnect)
2. obtain token from the shinyapps.io dashboard (more info at: https://shiny.rstudio.com/articles/shinyapps.html)
3. configure rsconnect account info (copy and run command that starts with rsconnect:setAccountInfo)

ADDITIONAL INFO

app url: https://ftmsensitivity.shinyapps.io/sa-app/
admin url: https://www.shinyapps.io/admin/#/login
email: kxiong@fieldtomarket.org
pw: FTMApp2019

HELPFUL SITES

other web hosting options: https://shiny.rstudio.com/tutorial/written-tutorial/lesson7/
configuring shinyapp.io: https://shiny.rstudio.com/articles/shinyapps.html

