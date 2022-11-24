library(magrittr)
# read file ---------------------------------------------------------------
src_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
dat_dir <- sub('src.*','data',src_dir)
res_dir <- sub('src.*','result',src_dir)

data_trait <- readr::read_rds(paste0(dat_dir,"/nona_combine.rds"))
para <- readr::read_rds(paste0(dat_dir,"/nona_para.rds"))

# random sampling 100 environment
set.seed(1881)
ENV<-sample(unique(data_trait$Environment),100)

# random sampling for 100 genotype ------------------------------------------------------------------

set.seed(16902)
rdm.gen<-sample(unique(data_trait$Genotype),gen_numb)

# even sampling for 100 genotype based on mean yield --------------------------------------------------------------------
#mean.yield
M.y<-dplyr::summarise(dplyr::group_by(data_trait,Genotype),My=mean(yield))

rang.y<-range(M.y$My)
# divide in to 10 portion 
step.y<- diff(rang.y)/10
M.y$groups <- cut(M.y$My,
                  breaks=c(rang.y[1]-1,seq(rang.y[1]+step.y,rang.y[2]-step.y,step.y),rang.y[2]+1))
numb.each.group<-as.vector(table(M.y$groups))
geno.group<-dplyr::group_split(dplyr::group_by(M.y,groups))
if(!identical(sapply(geno.group,nrow),numb.each.group)) warning("split group result is not as planned")
# randomly select 10 seed number
set.seed(966)
seed.vec<-sample(89999,10)

even.gen<-purrr::map_dfr(1:10,~{
  # for each seed number randomly selected 10 genotypes
  set.seed(seed.vec[.x])
  pos<-sample(numb.each.group[.x],10)
  sub<-dplyr::slice(geno.group[[.x]],pos)
  return(sub[,"Genotype"])
})$Genotype

# top 20 sampling for 100 genotypes ------------------------------------------------------------------
# subset top 20% of genotype based on the mean yield 
sub.M.y<-dplyr::filter(M.y,My>quantile(My,.8))
set.seed(7564)
top.gen<-sample(sub.M.y$Genotype,100)

# -------------------------------------------------------------------------
# combine three sampling methods result into single list
gen.list<-list(random=rdm.gen,even=even.gen,top=top.gen)
gen.list.label<-c("random",'even','top20')

stable<-purrr::map_dfr(1:3,~{
  sub.dat<-dplyr::filter(data_trait,Genotype%in%gen.list[[.x]],Environment%in%ENV) %>% 
    mutate(yield=yield/1000) # change kg to ton 
  res<-toolStability::table_stability(sub.dat,"yield","Genotype","Environment",
                                      quantile(sub.dat$yield,.95),unit.correct=T) %>% 
    mutate(label=gen.list.label[.x])# sampling method 
  return(res)
}) %>% 
  dplyr::select(-Normality) %>% 
  tidyr::gather(.,"SI","SI_value",
                names(.)[!grepl("(Genotype|Mean.yield|label)",names(.))])

write.csv(stb,paste0(res_dir,"/Fig1_S1.csv"))
