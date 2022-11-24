
Fig2_prep_fun <- function(gen_seed,gen_numb,env_seed,env_numb,ENV,trait,Df,hdir,sampling,pi_table){
  
  # This function is used for the effet of environmental number on trait and stability indices
  # especially for different sampling method(i.e., random, even and top20)
  
  # input:
  # gen_seed: gen random sampling seed
  # gen_numb: number of genotype to select
  # env_seed: env random sampling seed
  # env_numb: number of environment to select
  # ENV: vector, name of environment 
  # trait: character, column name of a numeric column
  # DF: dataframe to be sampled
  # hdir: dir to save the result
  # sampling: character, means prefix, indicating the sampling method going to use.
  # pi_table:logical, if TRUE, then the table_stability() is in used, other wise, genotypic_superiority_measure will be used 
  
  # output
  # data frame with columns,  Trait, Genotype, environment SI Gen_seed,  
  # Gen_seed_label,  Env_numb,  Env_seed,  Env_seed_label
  
  
  
  #list of random selected genotypes name
  
  environment <- purrr::map(env_seed,~{
    set.seed(.x)
    return(sample(ENV,env_numb))}
  )
  
  
  res <- purrr::imap_dfr(environment,function(y,y1){
    purrr::imap_dfr(gen_seed,function(x,x1){
      
      temp <-purrr::map_dfr(
        Df[x],
        ~{dplyr::filter(.x,Environment%in%y)}
      )
      
      if(pi_table){
        SI_table<- toolStability::table_stability(data=temp,
                                                  trait, "Genotype", "Environment",lambda = quantile(temp$yield,.95),
                                                  unit.correct = T)
      }else{
        SI_table<- toolStability::genotypic_superiority_measure(data=temp,trait, "Genotype", "Environment")
      }
      
      res <- dplyr::mutate(data.frame(SI_table),
                           Gen_numb=gen_numb,
                           Gen_seed=length(gen_seed),
                           Gen_seed_label=x1,
                           Env_numb=env_numb,
                           Env_seed=length(env_seed),
                           Env_seed_label=y1)
      return(res)
    })
  })
  file_nam <- paste0('GS',length(gen_seed),'_GN',gen_numb,'_ES',length(env_seed),'_EN',env_numb,'_',sampling,'.rds')
  file_path <- paste0(hdir,file_nam)
  saveRDS(res,file_path,compress = T)
}


sumfun <- function(df,group){
  # turn the input data frame into the summarised dataframe 
  
  # input: df, a datafreame
  # output result_df 
  # calculated the mean statistic value for each replication
  varnam <- rlang::sym(paste0('Mean.','yield')) # non standard evaluation, dynamic columns
  sort.vector <- c('Genotype','Env_numb')
  sinam <- ifelse( grepl("SI",group),rlang::sym("SI.value"), rlang::sym("pi"))
  
  
  result_df <- dplyr::summarise(
    dplyr::group_by_at(
      df,
      c('Genotype','Gen_seed','Gen_seed_label','Env_numb','Env_seed','Gen_numb')),
    MSI=mean(!!sinam),
    SDSI=sd(!!sinam),
    MTrait=mean(!!varnam),
    SDTrait=sd(!!varnam),
    CVTrait=sd(!!varnam)/MTrait,
    CVSI=sd(!!sinam)*100/MSI,
    RMSDSI=SDSI*sqrt((length(!!varnam)-1)/length(!!varnam)),
    ACCSI=(1-RMSDSI/MSI)*100
  ) %>% 
    mutate(environment.number = as.numeric(Env_numb)) %>% 
    # # sort by genotype and environmental number
    dplyr::arrange(.,!!!dplyr::syms(sort.vector))
  return(result_df)
}


difcv <- function(x){
  # calculate the difference of given vector
  res <- diff(x)
  return(c(res[1],res))
}

exec_fun <- function(sampling.vec,pi_table=FALSE){
  #executation function wrapper of Fig2 rep
  # input 
  # sampling.vec = character vector containing name of sampling
  # pi_table:logical, if TRUE, then the table_stability() is in used, other wise, genotypic_superiority_measure will be used 
  
  #output: write result in the assigned folder Env?number
  
  purrr::walk(sampling.vec,function(sampling){# for each sampling method
    
    # generate the list of sampled genotypes
    gen_seed <- purrr::map(gen_seed_len,~{
      if(sampling=='top'){
        # subset the top 20
        sub.M.y<-dplyr::filter(M.y,My>quantile(My,.8))
        # series of seed number
        top.seed <- 7564:(7563+gen_seed_len)
        # generate slice number
        top.gen <- purrr::map(top.seed,~{
          set.seed(.x)
          top.gen<-sample(sub.M.y$Genotype,gen_numb)
          res <- match(top.gen,GEN)
          return(res)
        })
        res <- top.gen
      }else if(sampling=="even"){# even
        
        rang.y<-range(M.y$My)
        step.y<- diff(rang.y)/10
        M.y$groups <- cut(M.y$My,
                          breaks=c(rang.y[1]-1,
                                   seq(rang.y[1]+step.y,
                                       rang.y[2]-step.y,step.y),
                                   rang.y[2]+1))
        numb.each.group<-as.vector(table(M.y$groups))
        geno.group<-dplyr::group_split(dplyr::group_by(M.y,groups))
        if(!identical(sapply(geno.group,nrow),numb.each.group)) 
          warning("split group result is not as planned")
        
        even.seed <- 966:(965+gen_seed_len)
        even.gen <- purrr::map(even.seed,~{
          set.seed(.x)
          seed.vec<-sample(89999,10)
          even.gen<-purrr::map_dfr(1:10,~{
            set.seed(seed.vec[.x])
            pos<-sample(numb.each.group[.x],gen_numb/10)
            sub<-dplyr::slice(geno.group[[.x]],pos)
            return(sub[,"Genotype"])
          })$Genotype
          res <- match(even.gen,GEN)
          return(res)
        })
        res <- even.gen
      }else if(sampling=="random"){
        random.seed <- 16888:(16887+gen_seed_len)
        # generate slice number
        res <- purrr::map(random.seed,~{
          set.seed(.x)
          random.gen<-sample(GEN,gen_numb)
          res <- match(random.gen,GEN)
          return(res)
        })
      }
      return(res)
    })
    names(gen_seed) <- gen_seed_len
    
    purrr::iwalk(env_seed,function(k,k1){
      purrr::iwalk(gen_seed,function(y,y1){
        purrr::walk(env_numb,function(w){
          purrr::walk(gen_numb,function(x){
            
            file_nam <- paste0('GS',names(gen_seed),'_GN',x,'_ES',length(k),'_EN',w,"_",sampling,'.rds')
            file_path <- paste0(file_dir,file_nam)
            print(file_nam)
            
            if(file.exists(file_path)){
            }else{
              
              Fig2_prep_fun(gen_seed = y,gen_numb = x,
                            env_seed = k,env_numb = w,ENV=ENV,
                            trait = 'yield',Df =df.list,
                            hdir =file_dir, sampling=sampling,pi_table=pi_table)
              print(paste(sampling,' GS',length(y),'_GN',x,'_ES',length(k),'_EN',w,format(Sys.time(), "%b %d %X ")))
            }
          })
        })
      })
    })
  })
}
