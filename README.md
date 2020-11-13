# Introduction 
This repository contains code for Mathematica's entry to Phase 2 of the [AHRQ Visualization Resources of Community-Level Social Determinants of Health (SDoH) Challenge](https://www.ahrq.gov/sdoh-challenge/index.html).  

For more information, please contact Kelsey Skvoretz at KSkvoretz@mathematica-mpr.com and Margaret Luo at MLuo@mathematica-mpr.com.

# Contents
* data/
  * for storing intermediate and final datasets and dictionaries
* pipeline/
  * for scraping and cleaning demographic, social determinant of health (SDoH), and outcome data
  * all of the cleaned data is output into the data/cleaned folder, to be picked up, merged, and utilized by pipeline.py
* data-insights/
  * for examining preliminary insights
* methodology/
  * for calculating county-level SDoH scores and similarities
* DockerShinyApp/
  * for building the Docker container and launching the [Community Connector app](http://communityconnector.mathematica.org)

# Authors
* **Kelsey Skvoretz** - *back-end lead*
* **Margaret Luo** - *front-end lead*
* **Evelyn Cody** - *methodology & front-end support*
* **Addison Larson** - *front-end support*
* **Emma Pendl-Robinson** - *pipeline & front-end support*

# Advisors
* **Keri Calkins** - *research lead, methodology support, & back-end support*
* **Elena Saavedra Jimenez** - *UX lead and graphics designer*
* **Aaron White** - *technical consultant*
* **George Gallo** - *AWS support*
* Additional thanks to Stephanie Tuerk, Alex Bohl, and Ravi Goyal
