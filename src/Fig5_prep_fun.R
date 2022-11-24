Fig5_fun <- function(gen_seed,gen_numb,GEN,env_seed,env_numb,ENV,trait,Df,para,hdir,sel_seq){
  # this function calculate r stripe based on genotypic replicates for same environments
  # r stripe: linear form of correlation matrix 
  
  # steps:
  # For given genotype and environment number,
  # and given genotype and environment replicates:
  # 
  # First, correlation between traits, stability index and physiological parameters
  # was calculated and turned into linear forms (r stripe) for each replicate.
  #
  # Second, for one environment replicate, correlation of r stripe from genotype replicate
  # under that environment replicate was calculated and squared to get R2 and SD.
  
  # input:
  # gen_seed : a numeric vector that used for random seed sampling, 
  ## length of gen_seed stand for genotype repetition number
  # gen_numb : single value for sampling of genotype
  # GEN: a str vector of name of all genotypes
  # env_seed :a numeric vector that used for random seed sampling,
  ## length of env_seed stand for environmental repetition number
  # env_numb : single value for sampling of environment
  # ENV : a single value, number of unique environments in the data
  # trait : str for specifying the column name to extract
  # Df: list of dataframe splited by genotype
  # para: dataframe of physiological parameter
  # sel_seq: numeric vector to filter the unwantted correlation coefficient
  # (like correlation between physiological parameters)
  
  # output: dataframe containing 
  # gen_numb
  # r2
  # sd
  # gen_seed
  # env_numb
  # env_seed
  # row number equals to (sum of repetion of environment)
  
  # -------------------------------------------------------------------------
  trait.si.physio.combine <- function(Df,para,x,y){
    dplyr::left_join(
      # retain only the sampled genotypes
      # calculation of SI 
      toolStability::genotypic_superiority_measure(
        
        purrr::map_dfr(
          Df[x],
          ~{dplyr::filter(.x,Environment%in%y)}
        ),
        trait,
        "Genotype","Environment",unit.correct = T),
      # merged with the filtered physiological parameters
      para[x,],
      by=c('Genotype'='genotype'))
    
  }
  
  # start -------------------------------------------------------------------
  # generate the list of random selected genotypes name 
  # number of list equals to the number of seed
  # each list element contain the random selected Genotypes names equal to gen_numb
  geno_type <- purrr::map(gen_seed,~{
    set.seed(.x)
    return(sample(GEN,gen_numb))}
  )
  # generate the list of random selected row position for sampled environment
  # number of list equals to the number of seed
  # each list element contain the random selected Environment names equal to env_numb
  
  environment <- purrr::map(env_seed,~{
    set.seed(.x)
    return(sample(ENV,env_numb))}
  )
  
  Res <- purrr::imap_dfr(environment,function(y,y1){
    # for each environment sample repetition, rsult will be one row data 
    cor_df <- purrr::imap_dfc(geno_type,function(x,x1){
      # for each given environment 
      # the correlation stripe from each repetition of genotype will be binded by column

      table <- trait.si.physio.combine(Df,para,x,y)
      
      # because sometimes the simulated parameters sometimes are too similar
      # which can generate the correlation matrix with NA
      # so before it generate the error 
      # we must fix it by resampling
      
      # first we generate a variable for clearance of later expression 
      uniform <- table[,-which(names(table) =='Genotype')]
      # testit::has_warning(cor(uniform)) ## this can check of error when generating the correlation matrix with NA
      if(testit::has_warning(cor(uniform))){print(x)}
      while(testit::has_warning(cor(uniform))){
        
        x <- x+1 # then we change it
        # and repeat the process until it works
        table <- trait.si.physio.combine(Df,para,x,y)
        uniform <- table[,-which(names(table) =='Genotype')]
        if(!testit::has_warning(cor(uniform))){print(x)}
      }
      
      # then we can generate the correlation matrix 
      cormatrx <-cor(uniform) 
      # generate the r stripe by
      # replace the lower triangle to NA
      
      cormatrx[lower.tri(cormatrx,diag = T)] <- NA
      # transform the upper triangle into linear by row
      
      line <- data.frame(V=na.omit(as.vector(t(cormatrx)))) #V for arbitrary col name, or it will become na.omit(as.vector(t(up))) instead.
      return(line[sel_seq,])
    })
    
    # after we bind the correlation stripe from different set of sampled genotypes,
    # then we can create the correlation matrix of each correlation 
    mat <- cor(cor_df)
    # remove the replicated part in correlation matrix
    up <- mat
    up[lower.tri(up,diag = T)] <- NA
    
    # calculate the average and sd of r2
    av_r2 <- mean(up^2,na.rm = T)
    sd_r2 <- sd(up^2,na.rm = T)
    
    res <- data.frame(r2=av_r2,
                      sd=sd_r2,
                      Gen_numb=gen_numb,
                      Gen_seed=length(gen_seed),
                      Env_numb=env_numb,
                      Env_seed=length(env_seed),
                      Env_seed_label=y1)
    return(res)
  })
  
  file_nam <- paste0('GS',length(gen_seed),'_GN',gen_numb,'_ES',length(env_seed),'_EN',env_numb,'.rds')
  file_path <- paste0(hdir,file_nam)
  
  saveRDS(Res,file_path,compress = T)
  
}
