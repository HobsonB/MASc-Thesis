Name:         OccupancyForecaster.txt
Description:  The occupancy forecast as implemented in GCL+
Requires:     NA

Name:         co2_analysis.R	
Description:  Determines changes in CO2 concentrations between baseline and implementation year	
Requires:     CO2_2018.csv, CO2_2019.csv

Name:         tin_analysis.R	
Description:  Determines changes in indoor air temperature between baseline and implementation year	
Requires:     tmp_2018.csv, tmp_2019.csv

Name:         fad_analysis.R	
Description:  Determines changes in fresh air damper positions between baseline and implementation year	
Requires:     AHU1_2018.csv, AHU1_2019.csv, AHU2_2018.csv, AHU2_2019.csv

Name:         changepoint.R	
Description:  Trains changepoints models for baseline and implementation year	
Requires:     OAT_2018.csv, OAT_2019.csv, CW_2018.csv, CW_2019.csv, STM_2018.csv, STM_2019.csv, submeter_2018.csv, submeter_2019.csv

Name:         daytype.R	
Description:  Determines what the predicted occupancy was vs. the recorded occupancy for the study period	
Requires:     daytypes.csv, kmeans_elec.csv, kmeans_profiles.csv, kmeans_centers.csv, wifi_2019.csv
