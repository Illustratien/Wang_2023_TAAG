rm(list=ls())
pacman::p_load(purrr,dplyr,ggplot2,cowplot)
options(dplyr.summarise.inform = FALSE)
# function-------------------------------------------------------------------------
src_dir <-dirname(rstudioapi::getSourceEditorContext()$path)
dat_dir <- sub('src.*','data',src_dir)
res_dir <- sub('src.*','result',src_dir)
file_dir <- paste0(res_dir,'/Env_number/Figs3/')

# theme------------------------------------------------------------------------
source(paste0(src_dir,'TC_theme2.R'))
tc_theme2 <- TC_theme2(ax.txt.siz=6,
                       ax.tit.siz=6,
                       t=2,r=2,b=4,l=2)# axis line)
# dir-------------------------------------------------------------------------
g.vec <- c('Genotype','Gen_seed','Gen_seed_label','Env_numb','Env_seed','Gen_numb',"SI","source")
# go to root folder
fnam <- list.files(file_dir,pattern='.rds') %>% .[grep("^GS",.)]
nam <-data.frame(purrr::invoke('rbind',strsplit(sub('.rds','',fnam),'_')),fn=fnam)
data <- purrr::map2_dfr(nam[,"fn"],
                        nam[,"X5"],
                        ~{readr::read_rds(file.path(dir_file,.x)) %>%
                            filter (Gen_seed_label==2)%>% 
                            mutate(source=.y)
                        }) %>% 
  dplyr::select(-'Stability.variance') %>% 
  mutate(Genotype=factor(Genotype),
         across(c("Environmental.variance","Ecovalence",
                  "Ecovalence.modified","Genotypic.stability",
                  "Variance.of.rank","Deviation.mean.squares",
                  "Genotypic.superiority.measure"),
                function(x)x/1000)) %>% #kg to ton
  tidyr::pivot_longer(names_to = "SI",values_to = "SI.value",
                      cols = Safety.first.index:Ecovalence.modified) %>% 
  mutate(SI=gsub('[.]'," ",SI),
         SI.value=case_when(SI=="Adjusted coefficient of variation"~SI.value/100,# adjust unit to %
                            T~SI.value),
         # set the order of display
         SI=factor(SI,levels = c("Environmental variance",
                                 "Coefficient of regression","Genotypic stability",
                                 "Genotypic superiority measure","Safety first index","Coefficient of determination",
                                 "Adjusted coefficient of variation","Deviation mean squares",
                                 "Ecovalence","Ecovalence modified","Variance of rank"))) %>% 
  arrange(SI) %>% 
  sumfun(.,group=g.vec)

stblist<- data %>% 
  group_by(source,Env_numb,SI) %>% 
  summarise(across(c(MSI,CVSI),list(mean,sd))) %>%
  group_by(SI) %>% group_split()
# labeled version-------------------------------------------------------------------------

plist <- imap(stblist,function(sub,id){
  y.range <- sub$CVSI_1 %>% range()
  p <-ggplot(sub ,aes(x=Env_numb,label=SI,
                      y=CVSI_1,color=source, shape=source))+
    scale_y_continuous(limits = c(y.range[1],y.range[2]*1.2))+
    scale_x_continuous(limits = c(-5,605))+
    geom_line(size=.5,alpha=0.2,show.legend = F)+
    geom_point(size=1,alpha=0.4)+
    tc_theme2+
    labs(color="Sampling methods",shape="Sampling methods")+
    ggplot2::theme(
      strip.text = element_text(size=1),
      strip.background =element_rect(fill=NA),
      legend.position = "none",
      axis.text = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.margin = margin(t = 2, r = 2, b = 5, l = 5, unit = "pt"),
      axis.line = element_blank())+
    ggplot2::scale_x_continuous(breaks = seq(0,600,200),
                                labels = seq(0,600,200),limits = c(0,620))+
    guides(shape=guide_legend(ncol=2,override.aes = list(size = 3) ),
           color= guide_legend(override.aes = list(size = 3)))  +
    annotate("text",label=letters[id],x=8,y=y.range[2]*1.09) 
  
})

# plot legend
legend.df <- data.frame(x=c(1,1,1),y=c(1,2,3),si_name=c("even","random","top20"))
plot.legend <- ggplot(legend.df,aes(x=x,y=y,color=si_name,shape=si_name,label=si_name))+
  geom_point(size=2,alpha=.8)+
  theme_classic()+
  scale_x_continuous(limits = c(0.5,2))+
  scale_y_continuous(limits = c(0,4))+
  geom_text(aes(x=c(1.6,1.6,1.6)),size=4)+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks   =element_blank(),# tick thickness
        axis.text.x =element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        axis.line = element_blank())
plist[[11]] <- plot.legend

# text legend
text.legend.df <- data.frame(x=rep(1,10),y=seq(10,1,-1),
                             si_name=paste(letters[1:10],
                                           c("Environmental variance",
                                             "Coefficient of regression","Genotypic stability",
                                             "Genotypic superiority measure","Safety first index",
                                             "Coefficient of determination",
                                             "Adjusted coefficient of variation","Deviation mean squares",
                                             "Ecovalence","Variance of rank"),sep=" : "))
text.legend <- ggplot(text.legend.df,aes(x=x,y=y,label=si_name))+
  theme_classic()+
  scale_x_continuous(limits = c(0.9,5.3))+
  scale_y_continuous(limits = c(0,11))+
  geom_text(hjust = 0,size=2)+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks   =element_blank(),# tick thickness
        axis.text.x =element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(t = 0, r = 1, b = 0, l = 1, unit = "pt"),
        axis.line = element_blank())
plist[[12]] <- text.legend

p1ot.merge <- cowplot::plot_grid(plotlist = plist,ncol = 4,align="hv")

# shared x and y axis title
xlable_text <- grid::textGrob(bquote(bold(Number~of~environment~'('*bolditalic(N)[env]*')')), 
                              gp=grid::gpar(fontface="bold", fontsize=10),
                              vjust = .4,hjust = .2)
ylable_text <- grid::textGrob(bquote(bold(CV)[bolditalic(SI)[bold('i, yield')]]~"(%)"), 
                              gp=grid::gpar(fontface="bold", fontsize=10),
                              vjust = 0.3,hjust = .05,rot=90)
figs3 <- cowplot::plot_grid(gridExtra::grid.arrange(
  cowplot::plot_grid(ylable_text,p1ot.merge,rel_widths = c(1,20)),
  bottom=xlable_text))


tiff(filename=paste0(dir_result,'/Paper/Fig.S3.tiff'),
     units="cm",
     width=17.4,
     height=12,
     compression = "lzw",
     pointsize=12,
     res=330)# dpi)
figs3
dev.off()

# CV threshold and table for overall mean-------------------------------------------------------------------------
coln.vec <- c("a","coef","thr_0.2","thr_0.1","thr_0.05")

output <- data %>% 
  mutate(CVSI=CVSI/100) %>% 
  group_by(source,Env_numb,SI) %>% 
  summarise(across(c(MSI,CVSI),list(mean,sd))) %>% 
  ungroup() %>% 
  dplyr::select(source,Env_numb,CVSI_1,SI) %>% 
  droplevels()%>% 
  mutate(across(Env_numb:CVSI_1,log)) %>% 
  filter(is.na(CVSI_1)) %>% 
  na.omit() %>% 
  group_by(source,SI) %>%
  group_split(.) %>% 
  map_dfr(.,~{
    out <- tryCatch({
      l <- lm(CVSI_1 ~ Env_numb, data = .x )
      coef <- coefficients(l)
      .x %>% group_by(source,SI) %>% summarise() %>%
        mutate(a=coef[2],b=coef[1])
    }, 
    error=function(cond){})
    return(out)
  }) %>% 
  mutate("thr_0.2":= exp((log(.2)-b)/a),
         "thr_0.1":= exp((log(.1)-b)/a),
         "thr_0.05":= exp((log(.05)-b)/a),
         coef=exp(b)) %>%
  dplyr::select(-b)


form <- map_dfr(coln.vec,~{
  output %>% 
    dplyr::select(-setdiff(coln.vec,.x)) %>%
    tidyr::pivot_wider(names_from = SI,values_from = !!.x) %>% 
    mutate(Value=.x)
}) %>% 
  mutate(across(where(is.numeric),~{round(.x, digits = 2)})) %>% 
  as.data.frame()

xlsx::write.xlsx(form,
                 paste0(res_dir,'/FigS3.xlsx'),row.names = F,showNA = F)
