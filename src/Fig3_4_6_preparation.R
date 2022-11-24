pacman::p_load(purrr,dplyr,tidyr,magrittr,toolStability,igraph)
src_dir <-dirname(rstudioapi::getSourceEditorContext()$path)
dat_dir <- sub('src.*','data',src_dir)
res_dir <- sub('src.*','result',src_dir)
env_res <- paste0(res_dir,'/Env_number/Fig2/')
fig4_dir <- paste0(res_dir,'/Env_number/Fig4/')
dir.create(paste0(res_dir,'/Networkdata'), showWarnings = FALSE)

source(paste0(src_dir,"/Fig3_6_prep_fun.R"))

# read file ---------------------------------------------------------------
para <- readr::read_rds(paste0(dat_dir,"/nona_para.rds"))
# raw data 
raw_data <- readr::read_rds(paste0(dat_dir,"/nona_combine.rds")) %>% 
  rename("genotype"="Genotype")

# read 100 genotype name under Gs 100
fnam <- list.files(env_res,pattern='.rds')
nam <-sub('.rds','',fnam) %>% 
  strsplit(.,'_') %>% 
  purrr::invoke('rbind',.) %>% 
  data.frame(.,filenam=fnam) %>% 
  dplyr::filter(X1=='GS100',X3=='ES100',X2=='GN100')

sampled_dat <-  readr::read_rds(paste0(env_res,nam[1,'filenam']))# GS100_GN100_ES100_EN10.rds

# name of stability index
SIname <- 'Genotypic superiority measure'
SIshort <- 'Pi'

# randomly select three genotype seed label
gseed <- paste('100',c(3,15,2),sep='_')
# condition setting for fig3------------------------------------------------------------------
# show only absolute correlation coefficient over 0.33
thresh <- 0.33
# show only the correlation between physiological parameters and out put trait
node.condi <- rlang::quo(G!='PP'&G!='TT') 
tr <- 'yield'
# empty output
Net <- list()
Link <- list()


for(i in 1:length(gseed)){
  # use the chosen random combination in previous sampled dataset  
  # specifying genotype seed number (total sampling number)
  gs <- as.numeric(strsplit(gseed[[i]],'_')[[1]][1])
  # specifying for genotype seed label (one specific example)
  gsl <- as.numeric(strsplit(gseed[[i]],'_')[[1]][2])
  # get specific sampled data 
  sub <- dplyr::filter(sampled_dat,Gen_seed==gs,Gen_seed_label==gsl)
  
  geno <- unique(sub$Genotype)
  # subset original data based on the selected genotypes
  sub_data <- dplyr::filter(raw_data,genotype%in%geno)
  # select physiological parameter
  sub_para <- dplyr::filter(para,genotype%in%geno)
  
  stable_table <- toolStability::genotypic_superiority_measure(sub_data,tr,'genotype','Environment',unit.correct = T) %>% 
    rename_with(tolower,starts_with("Genotype")) %>% 
    rename_with(~sub("Mean.","",.x),starts_with("Mean")) %>% 
    rename_with(~sub("genotypic.superiority.measure",paste(SIshort,trait),.x),"genotypic.superiority.measure") 
  
  Net[[i]] <- multi_trait_net_prep(thresh,
                                   node.condi,
                                   tr,
                                   stable_table,
                                   sub_para)
  
  linkdf <- link(tr,
                 stable_table,
                 sub_para,
                 node.condi)
  
  Link[[i]] <-  data.frame(r=linkdf$weight*linkdf$sign)
  
}

saveRDS(Net,paste0(res_dir,'/Networkdata/figure3_net.rds'),compress = T)

saveRDS(Link %>% purrr::reduce(.,rbind),paste0(res_dir,'/Networkdata/figure3_link.rds'),compress = T)

# Fig 4 & S4 preap-------------------------------------------------------------------------

fnam <- list.files(fig4_dir,pattern='.rds')
sampled_dat <- purrr::map_dfr(fnam,~{
  res <-  readr::read_rds(paste0(fig4_dir,"/",.x))
  res$source <-.x %>% strsplit(.,'_') %>% unlist() %>% .[length(.)] %>% sub('.rds','',.)
  return(res)
})
gseed1<- paste('100',1:100,sep='_')
source.vec <- c('even','top',"random")
node.condi <- rlang::quo(G!='PP'&G!='TT')

Link.comp <- purrr::map_dfr(gseed1,~{
  gs <- as.numeric(strsplit(.x,'_')[[1]][1])
  gsl <- as.numeric(strsplit(.x,'_')[[1]][2])
  res <- purrr::map_dfr(source.vec,function(z){
    sub <- dplyr::filter(sampled_dat,Gen_seed==gs,
                         Gen_seed_label==gsl,
                         source==z)
    
    geno <- unique(sub$Genotype)
    sub_data <- dplyr::filter(raw_data,genotype%in%geno)
    sub_para <- dplyr::filter(para,genotype%in%geno)
    
    stable_table <- toolStability::genotypic_superiority_measure(sub_data,trait,
                                                                 'genotype','Environment',unit.correct = T) %>% 
      rename_with(tolower,starts_with("Genotype")) %>% 
      rename_with(~sub("Mean.","",.x),starts_with("Mean")) %>% 
      rename_with(~sub("genotypic.superiority.measure",paste(SIshort,trait),.x),"genotypic.superiority.measure") 
    
    link <- link(           
      trait,
      stable_table,
      sub_para,
      node.condi) %>% 
      mutate(Net=.x,Source=z) %>% 
      data.frame
    
    
    return(link)
  })
  return(res)
})
saveRDS(Link.comp,paste0(res_dir,'/Networkdata/Fig4_method_compare_link.rds'),compress = T)

# the name is the same for all seed, just take the first one 
nodenam<-purrr::map_df(gseed[1],~{
  gs <- as.numeric(strsplit(.x,'_')[[1]][1])
  gsl <- as.numeric(strsplit(.x,'_')[[1]][2])
  sub <- dplyr::filter(sampled_dat,Gen_seed==gs,Gen_seed_label==gsl)
  geno <- unique(sub$Genotype)
  sub_data <- dplyr::filter(raw_data,genotype%in%geno)
  sub_para <- dplyr::filter(para,genotype%in%geno)
  
  stable_table <- toolStability::genotypic_superiority_measure(sub_data,trait,
                                                               'genotype','Environment',unit.correct = T)%>% 
    rename_with(tolower,starts_with("Genotype")) %>% 
    rename_with(~sub("Mean.","",.x),starts_with("Mean")) %>% 
    rename_with(~sub("genotypic.superiority.measure",paste(SIshort,trait),.x),
                "genotypic.superiority.measure") 
  
  res<-node.nam(trait,
                stable_table,
                sub_para)
  return(res)
})
saveRDS(nodenam,paste0(res_dir,'/Networkdata/Fig4_method_compare_nodenam.rds'))

# six physiological parameters under three different sampling methods-------------------------------------------------------------------------

sampled_dat_sub <- dplyr::filter(sampled_dat,Env_seed_label==1) %>% 
  dplyr::select(c("Genotype","Gen_seed_label","source")) %>% 
  merge(.,para,by.x="Genotype",by.y = "genotype") %>% 
  tidyr::gather(.,"PP","PP.value",-c(1:3))


source.list <- split(sampled_dat[,c("Genotype","Gen_seed_label","source")],
                     list(sampled_dat$source,sampled_dat$Gen_seed_label))


geno.mean <- purrr::imap_dfr(source.list,~{
  geno <-.x$Genotype
  sub_data <- dplyr::filter(raw_data,genotype%in%geno)
  sub_para <- dplyr::filter(para,genotype%in%geno)
  
  stable_table <- toolStability::genotypic_superiority_measure(sub_data,
                                                               trait,'genotype','Environment',
                                                               unit.correct = T)
  stable_table$seed <- .y
  return(stable_table)
})
geno.mean <- tidyr::separate(geno.mean,seed,c('source','seed_number'),'[.]')
names(geno.mean)[2:3] <-c('yield','pi.yield') 
geno.mean <- merge(geno.mean,para,by.x="Genotype",by.y = "genotype") %>% 
  tidyr::gather(.,"PP","PP.value",-c(1:5))

saveRDS(geno.mean,paste0(res_dir,'/Networkdata/figure4_pp_geno.rds'),compress = T)

# Fig6 --------------------------------------------------------------------

gen_seed <- seq(98888,98897,1)
names(gen_seed)<-10
gen_numb <- 1000
# Environment 
environment.vector <- c('year','sites','nitrogen','co2','sowing')
# paste together in one column
raw_data %<>% mutate(env =interaction(year,sites,nitrogen,co2,sowing))

trait.vector <-c('flowering','grain_number','grain_protein',
                 'grain_size','lai','maturity','yield','straw')


g.l<-split(raw_data,raw_data$genotype)
GEN<-1:length(g.l)

geno_type <- purrr::map(gen_seed[[1]],~{
  set.seed(.x)
  return(sample(GEN,gen_numb))}
)
names(geno_type)<-letters[1:length(geno_type)]

thresh <- 0
rep.numb <- 1:3


for (network_type in c('Trait_network','SI_network')){
  
  if (network_type=='SI_network'){
    #for SI network, filter for only SS, SP
    node.condi <- rlang::quo(G!='PP'& G!='TT'& G!='TS'& G!='TP')
  }else{
    #for trait network, filter for only TT, TP
    node.condi <- rlang::quo(G!='PP'& G!='TS'& G!='SP'& G!='SS')
  }
  
  Net <- list()
  Link <- list()
  
  
  for(repn in rep.numb){
    geno <-  geno_type[repn]
    system.time(selected_data <- data.table::rbindlist(g.l[geno]))#4
    
    sub_para <- dplyr::filter(para,genotype%in%(names(g.l)[geno]))
    
    stable_table <- purrr::map_dfc(trait_vector,~{
      res <- toolStability::genotypic_superiority_measure(selected_data,.x,
                                                          'genotype','env',unit.correct = T)%>% 
        rename_with(~sub("genotypic.superiority.measure",paste(SIshort,trait),.x),"genotypic.superiority.measure") 
      
      return(res)
    })
    
    # remove duplicated genotype columns
    duplicate_columns <- grep('Genotype',names(stable_table)) %>% .[2:length(.)]
    stable_table <- stable_table[,-duplicate_columns]
    # reorder the column based on the order of first trait then SI 
    stable_table <- stable_table[,c(1,seq(2,ncol(stable_table)-1,2),seq(3,ncol(stable_table),2))]%>% 
      rename_with(tolower,starts_with("Genotype")) %>% 
      rename_with(~gsub("Mean.","",.x),starts_with("Mean"))
    
    Net[[i]] <- multi_trait_net_prep(thresh,
                                     node.condi,
                                     trait.vector,
                                     stable_table,
                                     sub_para,
                                     node_colr,
                                     thresh.additional=0)
    
    linkdf <- link(trait_vector,
                   stable_table,
                   sub_para,
                   node.condi,
                   thresh.additional=0)
    
    Link[[i]] <-  data.frame(r=linkdf$weight*linkdf$sign,repn=repn)
    
    saveRDS(Net[[i]],paste0(res_dir,"/Networkdata/Net_",network_type,"_fig6.rds"),compress = T)
    saveRDS(Link[[i]],paste0(res_dir,"/Networkdata/Link_",network_type,"_fig6.rds"),compress = T)
  }
}
