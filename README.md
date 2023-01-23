
# Guide for reproducible scientific analysis

To reproduce the figure results in publication on Theoretical and Applied Genetics [Wang et al, 2023](https://doi.org/10.1007/s00122-023-04264-7), 

please do the following 3 steps:

1. Clone the repository to your local environment.
   Folder structure should look like this:
 
 R_script                                                
   ¦--Wang_2023_TAAG.Rproj                                
   ¦--README.md     
   ¦--data                                                
   ¦   °--Wang_et_al_TAAG_2023_physiological_parameter.rds 

   ¦--result                                                                        
   ¦   °--Networkdata                                     
   ¦       ¦--si_layout_circle.csv                        
   ¦       °--t_layout_circle.csv                         
   °--src                                                 
       ¦--Data_preparation.R                              
       ¦--Fig1_preparation.R                              
       ¦--Fig1_S1.R                                       
       ¦--Fig2.R                                          
       ¦--Fig2_prep_fun.R                                 
       ¦--Fig2_S3_4_preparation.R                         
       ¦--Fig3.R                                          
       ¦--Fig3_4_6_preparation.R                          
       ¦--Fig3_6_prep_fun.R                               
       ¦--Fig4.R                                          
       ¦--Fig5.R                                          
       ¦--Fig5_prep_fun.R                                 
       ¦--Fig5_preparation.R                              
       ¦--Fig6.R                                          
       ¦--FigS3.R                                         
       ¦--FigS4.R                                         
       ¦--TC_theme.R                                      
       °--TC_theme2.R                                     
2. Go to data repository on Zenodo (Wang, et al., 2023, https://doi.org/10.5281/zenodo.4729637),
	Please download the following data from Zenodo and place it to the **data** folder
	* Wang_et_al_TAAG_2022_output_trait.rds    
3. Run with the Data_preparation.R to FigS4.R in an ascending manner (order as shown above).

Enjoy!!


Citation info:  

Wang, T-C., Casadebaig, P., Chen, T-W. More than 1000 genotypes are required to derive robust relationships between yield, yield stability and physiological parameters: a computational study on wheat crop. Theor Appl Genet (2023). https://doi.org/10.1007/s00122-023-04264-7 