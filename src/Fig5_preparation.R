rm(list=ls())
pacman::p_load(purrr,dplyr,tidyr,magrittr,toolStability)

src_dir <-dirname(rstudioapi::getSourceEditorContext()$path)
dat_dir <- sub('src.*','data',src_dir)
res_dir <- sub('src.*','result',src_dir)
dir.create(file.path(res_dir,'Genotype_number'), showWarnings = FALSE)
gen_res <- paste0(res_dir,'/Genotype_number/')

source(paste0(src_dir,'/Fig5_prep_fun.R'))

data <- readr::read_rds(paste0(dat_dir,'/nona_combine.rds'))

GEN<-seq(1,length(unique(data$Genotype)))
ENV<-unique(data$Environment)

data.split<-data[,c('Genotype','Environment','yield')] %>% 
  split(.,.['Genotype'])


# decide which combination of correlation to considered -------------------------------------------------------------------------

# only select the corresponding relation ship between trait and stability indices
node.condi <- rlang::quo(G!='PP'&G!='TT')
trait.numb<-1 # trait = yield

para <- readr::read_rds(paste0(dat_dir,'/nona_para.rds'))

# generate dataframe of id for all combination of parameters
mat <- as.data.frame(t(combn(2+ncol(para)-1,2)))
colnames(mat) <- c('from','to')

sel_seq <- mat %>% mutate(
  #from label 
  fL= cut(from,breaks = c(0,trait.numb,2*trait.numb,Inf),
          labels=c('T','S','P')),
  #to label
  tL= cut(to,breaks = c(1,trait.numb+1,Inf),
          labels=c('S','P')),
  #group
  G = paste0(fL,tL),
  #position of physiological parameters
  pos=1:nrow(mat)) %>%  
  dplyr::filter(.,!!node.condi) %>% 
  # get the id of condition matched nodes
  .[['pos']]

# decide genotype and environment number, replication number -------------------------------------------------------------------------

gen_seed <- list(seq(16902,17001,1))
names(gen_seed)<-100

env_seed <- list(seq(8892,8901,1))
names(env_seed)<-10

gen_numb <- c(5, 50, 100, 200, 300, 500, 700, 900, 1100)
env_numb <- c(5, 50, 100, 300, 500, 700)

# -------------------------------------------------------------------------

system.time(
  
  purrr::walk(env_seed,function(k){
    purrr::walk(env_numb,function(w){
      purrr::walk(gen_seed,function(y){
        purrr::walk(gen_numb,function(x){
          file_nam <- paste0('GS',length(y),'_GN',x,'_ES',length(k),'_EN',w,'.rds')
          file_path <- paste0(gen_res,file_nam)
          
          if(file.exists(file_path)){
            # if the result file alredy exist, skip 
          }else{
            Fig5_fun(gen_seed = y, gen_numb = x, GEN=GEN,
                     env_seed = k, env_numb = w, ENV=ENV,
                     trait = 'yield', Df = data.split,
                     para = para, hdir = gen_res,
                     sel_seq = sel_seq)
            print(paste('GS',length(y),'_GN',x,'_ES',length(k),'_EN',w,
                        format(Sys.time(), "%b %d %X ")))
            
          }  
        })})})})
)


