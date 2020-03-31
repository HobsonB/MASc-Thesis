Name:         gt_wifi_phase1.R	
Description:  Wi-Fi devices and ground truth from phase 1	
Requires:     Floor A.csv, Floor AS.csv, Floor B.csv, Floor BS.csv, Floor C.csv, Floor D.csv

Name:         lm_gt_wifi_phase1.R	
Description:  Train linear model for occupancy-count estimation	
Requires:     gt_wifi_phase1.csv

Name:         gt_elec_day_phase2.R	
Description:  Produces column-wise daily occupancy and electrical load profiles for clustering	
Requires:     wifi_elec_2018.csv

Name:         AHC_cor_clustering.R	
Description:  Performs AHC 1-cor clustering on artificial occupancy data	
Requires:     gt_day_dat.csv
Name:         AHC_euc_clustering.R	
Description:  Performs AHC d clustering on artificial occupancy data	
Requires:     gt_day_dat.csv

Name:         kmeans_clustering.R	
Description:  Performs k-means clustering on artificial occupancy data	
Requires:     gt_day_dat.csv

Name:         elec_clust.R	
Description:  Creates representative electrical profiles based on k-means cluster membership data	
Requires:     kmeans_ind.csv, elec_day_dat.csv

Name:         forecasting_tree.R	
Description:  Trains classification tree based on past 7-day of cluster membership data	
Requires:     elec_ind.csv
