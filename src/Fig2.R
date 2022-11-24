rm(list=ls())
pacman::p_load(purrr,dplyr,tidyr,ggplot2,cowplot)
src_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# functions-------------------------------------------------------------------------

source(paste0(src_dir,'/TC_theme2.R'))
source(paste0(src_dir,'/Fig2_prep_fun.R'))

# dir-------------------------------------------------------------------------

dat_dir <- sub('src.*','data',src_dir)
res_dir <- sub('src.*','result',src_dir)
# read result from Fig2
file_dir <- paste0(res_dir,'/Env_number/Fig2/')
fnam <- list.files(file_dir,pattern='.rds')
nam <-data.frame(purrr::invoke('rbind',strsplit(sub('.rds','',fnam),'_')),fn=fnam) %>% 
  dplyr::filter(X3=='ES100',X1=='GS100',X2=='GN100')

res <- purrr::map_dfr(nam[,'fn'],~{readr::read_rds(file.path(file_dir,.x))}) %>% 
  mutate(genotypic.superiority.measure =genotypic.superiority.measure/1000000,#  kg^2 -> ton^2 
         pi =sqrt(genotypic.superiority.measure),
         genotype =factor(Genotype),
         across(c(Gen_seed_label,Gen_numb,Env_numb),as.numeric)) %>% 
  dplyr::filter(Genotype %in% c(4743,2396),# select two contrasting genotypes
                Gen_seed_label==1)# take first replicate as example 
# summary-------------------------------------------------------------------------

ES100 <- res %>% 
  dplyr::group_by(Genotype,Gen_seed_label,Env_numb,Env_seed) %>% 
  dplyr::mutate(mpi=mean(pi),fill='a') %>%
  droplevels()

Es100_rep1<- res %>% 
  dplyr::filter(Env_seed_label==100) 

g.vec <-  c('Genotype','Gen_seed','Gen_seed_label','Env_numb','Env_seed','Gen_numb')

cvsi_df <- res %>% sumfun(.,group=g.vec)%>% 
  dplyr::group_by(.,Genotype,Gen_seed,Gen_seed_label) %>% 
  dplyr::mutate(Dif=difcv(CVSI)) %>% 
  dplyr::arrange(Gen_seed_label) %>% 
  # reduce the density of points on the graph in plot p3
  dplyr::filter(Env_numb%in%c(1:10,15,20,seq(25,600,25)))


# text for plotting-------------------------------------------------------------------------
unit.pi <-quote('t . ha'^'-1')
pi.txt <-bquote(bolditalic(P)[bold('i, yield')]~bold('('*.(unit.pi)*')'))
mt <- 'Mean of'
perc <- '(%) '
m <- 'x'
x.lab<-bquote(bold(Number~of~environment~'('*bolditalic(N)[env]*')'))
x.lab2<-bquote(Number~of~environment~'('*bolditalic(N)[env]*')')

# Themes-------------------------------------------------------------------------

#for main plot
tc_theme2 <- TC_theme2(ax.txt.siz=6,
                       ax.tit.siz=6,
                       lgd.txt.siz=6,
                       lgd.tit.siz=6,
                       t=3,r=1,b=2,l=1)+
  ggplot2::theme(legend.position = "none",
                 axis.title.x =element_blank(),
                 strip.text.x = element_blank())
# for subplot
tc_theme3 <-
  theme_classic()+
  ggplot2::theme(
    axis.text =element_text(size=6),
    axis.title.y=element_text(size=6),
    axis.line = element_line(size = .2),
    axis.ticks  =element_line(size = .1),# tick thickness
    axis.ticks.length=unit(.005, "cm"),
    legend.position = "none",
    axis.title.x =element_blank(),
    strip.text.x = element_blank())

# Poly plot ---------------------------------------------------------------

p1 <- ggplot(ES100_rep1,aes(x=Env_numb,y=pi,color=genotype, shape=genotype))+
  geom_point(size=2,alpha=0.4)+#size=8
  geom_line(size=1,alpha=0.4)+
  ggplot2::scale_x_continuous(breaks = seq(0,600,200),
                              labels = seq(0,600,200),limits = c(0,620))+
  ggplot2::scale_y_continuous(breaks = seq(0,5,1),labels = seq(0,5,1),limits = c(-.5,5.5))+
  tc_theme2+
  ggplot2::ylab(pi.txt)+
  annotate("text", x = 400, y = 3,size=1,label = x.lab2)+
  guides(color = guide_legend(override.aes = list(size = 5)))# adjust only the legend

p1.21 <- ggplot(dplyr::filter(ES100_rep1,Env_numb%in%3:20,genotype==2396),
                aes(x=Env_numb,y=pi))+
  geom_point(size=1,alpha=0.4,color='#F8766D')+
  geom_line(size=.5,alpha=0.4,color='#F8766D')+
  ggplot2::scale_x_continuous(breaks = seq(0,20,5),
                              labels = seq(0,20,5),limits = c(0,21))+
  ggplot2::scale_y_continuous(breaks = seq(2,5,1),labels = seq(2,5,1),limits = c(2,5.5))+
  ggplot2::ylab(pi.txt)+
  tc_theme3+
  theme(axis.text. =element_text(size=8),
        axis.title.y=element_text(size=10))
# p1.21

p1.22 <- ggplot(dplyr::filter(ES100_rep1,Env_numb%in%3:20,genotype==4743),
                aes(x=Env_numb,y=pi))+
  geom_point(size=1,alpha=0.4,shape=17,color='#00BFC4')+
  geom_line(size=.5,alpha=0.4,color='#00BFC4')+
  ggplot2::scale_x_continuous(breaks = seq(0,20,5),
                              labels = seq(0,20,5),limits = c(0,21))+
  ggplot2::scale_y_continuous(breaks = seq(0.3,.7,.2),labels = seq(0.3,.7,.2),limits = c(0.2,.8))+
  tc_theme3+
  ggplot2::theme(
    axis.title.y = element_blank(),
  )

p1.5 <- p1+ annotation_custom(ggplotGrob(p1.21), xmin = 50, xmax = 385, 
                              ymin = 3.2, ymax = 6)+
  annotation_custom(ggplotGrob(p1.22), xmin = 365, xmax = 640, 
                    ymin = 3.2, ymax = 6)

p2 <- ggplot(ES100,aes(x=Env_numb,y=pi))+
  geom_jitter(alpha=0.4,aes(color=genotype,shape=genotype),size=.5)+
  geom_point(aes(x=Env_numb,y=mpi,fill=fill), colour = "black",size=.5)+
  ggplot2::scale_y_continuous(breaks = seq(0,5,1),labels = seq(0,5,1),limits = c(-.5,5.5))+
  tc_theme2+
  ggplot2::ylab(pi.txt)+
  ggplot2::scale_x_continuous(breaks = seq(0,600,200),
                              labels = seq(0,600,200),limits = c(0,620))+
  # notice to use the lsit or c in front of bquote
  scale_fill_manual(name = "",values="black", labels=c(bquote(bold(bar(bolditalic(P)["i, yield"])))))+# add the additional legend
  annotate("text", x = 400, y = 3,size=1,
           label = x.lab2)

p2.21 <- ggplot(filter(ES100,Env_numb%in%3:20,genotype==2396),
                aes(x=Env_numb,y=pi))+
  geom_jitter(alpha=0.2,color='#F8766D',size=.2)+
  geom_point(aes(x=Env_numb,y=mpi,fill=fill), colour = "black",size=.2)+
  ggplot2::ylab(pi.txt)+
  ggplot2::scale_x_continuous(breaks = seq(0,20,10),
                              labels = seq(0,20,10),limits = c(0,21))+
  ggplot2::scale_y_continuous(breaks = seq(0,6,2),labels = seq(0,6,2),limits = c(0,6))+
  tc_theme3

p2.22 <-  ggplot(filter(ES100,Env_numb%in%3:20,genotype==4743),
                 aes(x=Env_numb,y=pi))+
  geom_jitter(alpha=0.2,shape=17,color='#00BFC4',size=.2)+
  geom_point(aes(x=Env_numb,y=mpi,fill=fill), colour = "black",size=.2)+
  ggplot2::ylab(pi.txt)+
  ggplot2::scale_x_continuous(breaks = seq(0,20,10),
                              labels = seq(0,20,10),limits = c(0,21))+
  ggplot2::scale_y_continuous(breaks = seq(0,2,1),labels = seq(0,2,1),limits = c(0,2.1))+
  tc_theme3+  
  ggplot2::theme(
    axis.title.y = element_blank())

p2.5 <- p2+ annotation_custom(ggplotGrob(p2.21), xmin = 50, xmax = 385, 
                              ymin = 3.2, ymax = 6)+
  annotation_custom(ggplotGrob(p2.22), xmin = 360, xmax = 640, 
                    ymin = 3.2, ymax = 6)

legend_b <- cowplot::get_legend(
  p2 + 
    ggplot2::theme(legend.direction='horizontal',
                   legend.position=c(.84,.86),
                   legend.background = element_rect(fill = "transparent",colour = NA))+
    ggplot2::guides(
      # for big points
      color = guide_legend(override.aes = list(size = 3),
                           nrow=1,
                           label.hjust = -1,
                           order = 1),
      shape = guide_legend(override.aes = list(size = 3),
                           nrow=1,
                           label.hjust = -1,
                           order = 1),
      # for mean value
      fill = guide_legend(override.aes = list(size = 3),
                          nrow=1,
                          label.hjust = -2,
                          order=2)) +
    ggplot2::theme(legend.position='top', 
                   legend.justification=c(0.43,.5),
                   legend.direction='horizontal',
                   legend.text.align = 0))

p3 <- ggplot(cvsi_df,aes(x=Env_numb,y=CVSI,color=genotype, shape=genotype))+
  geom_point(size=2,alpha=0.4)+
  geom_line(size=1,alpha=0.4)+
  ggplot2::ylab(bquote(bold(CV)[bolditalic(P)[bold('i, yield')]]~"(%)"))+
  tc_theme2+
  ggplot2::scale_x_continuous(breaks = seq(0,600,200),
                              labels = seq(0,600,200),limits = c(0,620))


x.grob <- grid::textGrob(x.lab, 
                         gp=grid::gpar(fontface="bold", fontsize=8),vjust = 2,hjust = .4)


prow <- cowplot::plot_grid(
  p1.5,p2.5,p3,
  labels = letters[1:3],
  label_size=15,
  hjust = -1,
  nrow = 1) %>% #add shared x title
  gridExtra::arrangeGrob(., bottom = x.grob) %>% 
  gridExtra::grid.arrange()



fig2 <- cowplot::plot_grid(prow1,
                               legend_b+geom_blank(),
                               legend_b, 
                               ncol = 1, 
                               rel_heights = c(1.2,.1,.1))


tiff(filename=paste0(res_dir,'/Fig2.tiff'),
     # type="cairo",
     units="cm",
     width=17.4,
     height=6,
     compression = "lzw",
     pointsize=12,
     res=300,# dpi,
     family="Arial")


fig2
dev.off()

