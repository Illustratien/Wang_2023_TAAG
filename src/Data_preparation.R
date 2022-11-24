# in order to run this script, 
# plase first download the raw_data from 10.5281/zenodo.4729637 
# and put it in the sub folder data


# clean the enivronment
rm(list = ls())

src_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
dat_dir <- sub('src','data',src_dir)
dir.create(dat_dir, showWarnings = FALSE)

# read the raw_data
system.time(df <- readr::read_rds(paste0(dat_dir,'/data/Wang_et_al_TAAG_2022_output_trait.rds')))# 124s
# physiological parameters
para <- readr::read_rds(paste0(dat_dir,'/data/Wang_et_al_TAAG_2022_physiological_parameter.rds'))

# selection for cleaning
# split data to list by genotype
list.dat <- split(df,df$Genotype)
# for each genotype, check the whether na exist in each genotype for all traits
na.check.df <- purrr::map_dfr(list.dat,~{
  # labeled 
  data.frame(Na=ifelse(dim(.x)[1]!=dim(na.omit(.x))[1],1,0),
             Genotype=.x$Genotype[1])
})
# extract the genotype with na 
geno.na.id <- dplyr::filter(na.check.df,Na==1)$Genotype
# exclude the Genotype which contain NA in any of the trait
new.comb <- dplyr::filter(list.dat,! Genotype%in% geno.na.id)
new.para <- dplyr::filter(para,!genotype %in% geno.na.id)
# paste environments column into one for futher use
new.comb$Environment <- with(new.comb,paste(sites,sowing,nitrogen,co2,sep='_'))

# save result
saveRDS(new.comb,paste0(dat_dir,'/nona_combine.rds'),compress = T)
saveRDS(new.para,paste0(dat_dir,'/nona_para.rds'),compress = T)
