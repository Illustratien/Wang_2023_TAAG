
# Guide
To reproduce the data in publication "More than 1000 genotypes are required to derive robust relationships between yield, yield stability and physiological parameters: a computational study on wheat crop",
please follow the following 4 steps.

1. clone the repository to your local environment
2. Go to Wang, et al., 2022, 10.5281/zenodo.4729637!!,
	Please download the following two data from doi and move it to the *data* folder
	* Wang_et_al_TAAG_2022_physiological_parameter.rds
	* Wang_et_al_TAAG_2022_output_trait.rds
3.Folder structure should look like this
 
 R_script                                                
   ¦--data                                                
   ¦   ¦--Wang_et_al_TAAG_2022_output_trait.rds           
   ¦   °--Wang_et_al_TAAG_2022_physiological_parameter.rds
   ¦--result                                              
   ¦   ¦--Env_number                                      
   ¦   ¦   ¦--Fig2                                        
   ¦   ¦   ¦--Fig4                                        
   ¦   ¦   °--Figs3                                       
   ¦   ¦--Genotype_number                                 
   ¦   °--Networkdata                                     
   ¦       ¦--si_layout_circle.csv                        
   ¦       °--t_layout_circle.csv                         
   ¦--src                                                 
   ¦   ¦--Data_preparation.R                              
   ¦   ¦--Fig1_preparation.R                              
   ¦   ¦--Fig1_S1.R                                       
   ¦   ¦--Fig2.R                                          
   ¦   ¦--Fig2_prep_fun.R                                 
   ¦   ¦--Fig2_S3_4_preparation.R                         
   ¦   ¦--Fig3.R                                          
   ¦   ¦--Fig3_4_6_preparation.R                          
   ¦   ¦--Fig3_6_prep_fun.R                               
   ¦   ¦--Fig4.R                                          
   ¦   ¦--Fig5.R                                          
   ¦   ¦--Fig5_prep_fun.R                                 
   ¦   ¦--Fig5_preparation.R                              
   ¦   ¦--Fig6.R                                          
   ¦   ¦--FigS3.R                                         
   ¦   ¦--FigS4.R                                         
   ¦   ¦--TC_theme.R                                      
   ¦   °--TC_theme2.R                                     
   ¦--Wang_2022_TAAG.Rproj                                
   °--README.md     
4. following the display order in src as shown in the directory tree above, start with the Data_preparation to FigS4.

Enjoy!