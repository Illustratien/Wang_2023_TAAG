rm(list=ls())

pacman::p_load(purrr,dplyr,tidyr)
src_dir <-dirname(rstudioapi::getSourceEditorContext()$path)
dat_dir <- sub('src.*','data',src_dir)
res_dir <- sub('src.*','result',src_dir)

source(paste0(src_dir,'/Fig2_prep_fun.R'))

# read data-------------------------------------------------------------------------
system.time(df <- readr::read_rds(paste0(dat_dir,'/nona_combine.rds')))
# split in to list element
df.list<-split(df,df$Genotype)
# first calculate the mean yield
M.y<-dplyr::summarise(dplyr::group_by(df,Genotype),My=mean(yield))

# Environment -------------------------------------------------------------
ENV <- df.list[[1]]$Environment
# seed series number
env_seed <- list(seq(8902,9001,1))
names(env_seed)<-100

# genotype setting----------------------------------------------------------------

GEN <- unique(df$Genotype)
rm(df)# to save some local memory
gen_numb <- 100
# limited one length
gen_seed_len <-10

# Fig2 -------------------------------------------------------------------------
env_numb <-  c(3:15,seq(20,50,5),seq(60,120,10),seq(150,600,50))

dir.create(file.path(res_dir,('/Env_number')),showWarnings = FALSE)
dir.create(file.path(res_dir,('/Env_number/Fig2')),showWarnings = FALSE)
file_dir <- paste0(res_dir,'/Env_number/Fig2/random/')
dir.create(file_dir,showWarnings = FALSE)
exec_fun("random",pi_table=F)

# FigS3-------------------------------------------------------------------------
env_numb <-  c(seq(10,50,10),seq(100,600,100))
file_dir <- paste0(res_dir,'/Env_number/Figs3/')
dir.create(file.path(file_dir),showWarnings = FALSE)
for (sampling.vec in c("top","random","even")){
  
  exec_fun(sampling.vec,pi_table=T)
  
}

# Fig 4-------------------------------------------------------------------------
env_numb <- 100
gen_seed_len <-100
file_dir <- paste0(res_dir,'/Env_number/Fig4/')
dir.create(file_dir,showWarnings = FALSE)

for (sampling.vec in c("top","random","even")){
  exec_fun(sampling.vec,pi_table=F)
}
