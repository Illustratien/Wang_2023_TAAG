pacman::p_load(purrr,dplyr,tidyr,magrittr,toolStability,igraph,ggplot2)
src_dir <-dirname(rstudioapi::getSourceEditorContext()$path)
dat_dir <- sub('src.*','data',src_dir)
res_dir <- sub('src.*','result',src_dir)
dir.create(file.path(res_dir,'Genotype_number'), showWarnings = FALSE)
gen_res <- paste0(res_dir,'/Gen_number/')

# read data -------------------------------------------------------------------------

fnam <- list.files(gen_res,pattern='.rds')
nam <-sub('.rds','',fnam) %>% 
  strsplit(.,'_') %>%
  purrr::invoke('rbind',.) %>%
  data.frame(.,fn=fnam)

dat <- purrr::map_dfr(nam[,'fn'],~{readr::read_rds(file.path(gen_res,.x))})

res<-dat %>% 
  dplyr::filter(
    Env_seed_label==5,
    !Env_numb==10,
    !Gen_numb==10) %>% 
  mutate(across(c(Gen_seed,Env_numb),factor))


# presaved title, theme settings ------------------------------------------------

unit.pi <-quote('ton  ha'^'-1')
pi.txt <-bquote(bolditalic(P)[bold('i, yield')]~bold('('*.(unit.pi)*')'))
mt <- 'Mean of'
perc <- '(%) '
m <- 'x'
color.lab<-bquote(bold(bolditalic(N)[env]))
x.lab<-bquote(bold(Number~of~genotype~'('*bolditalic(N)[gen]*')'))
color.lab<-bquote(bold(bolditalic(N)[env]))
tc_theme1 <- TC_theme2(ax.txt.siz=10,
                       ax.tit.siz=12,
                       lgd.txt.siz=8,
                       lgd.tit.siz=9,
                       t=1,r=1,b=1,l=1)

# new single graph --------------------------------------------------------


fig5<-ggplot2::ggplot(filter(res,Gen_seed==100),
                      ggplot2::aes(x=Gen_numb,y=r2,
                                   color=Env_numb,
                                   group=Env_numb))+
  ggplot2::geom_point(size=3,alpha=.4)+
  ggplot2::geom_errorbar(ggplot2::aes(ymin=r2-sd, ymax=r2+sd), 
                         width=.2,
                         size=1,alpha=.4)+
  ggplot2::geom_line(size=1,alpha=.4,show.legend = F)+
  ggplot2::xlab(x.lab)+
  ggplot2::ylab(bquote(bolditalic(S)))+
  labs(color=color.lab)+
  ggplot2::scale_x_continuous(limits = c(0,1210),
                              breaks = c(5,seq(100,1100,200)),
                              labels = c(5,seq(100,1100,200)))+
  ggplot2::scale_y_continuous(limits = c(-.05,1.01),
                              breaks = seq(0,1,.2),
                              labels = seq(0,1,.2))+
  tc_theme1+
  ggplot2::theme(legend.position = c(.85,.3),
                 legend.background = element_rect(fill=NA),
                 axis.line = element_line(size = .5),
                 axis.ticks  =element_line(size = .5))


tiff(filename=paste0(res_dir,'/Fig.5.tiff'),
     # type="cairo",
     units="cm",
     width=14,
     height=10,
     compression = "lzw",
     pointsize=12,
     res=300,# dpi,
     family="Arial")
fig5
dev.off()
