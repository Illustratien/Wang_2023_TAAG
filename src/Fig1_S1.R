rm(list=ls())
pacman::p_load(ggplot2,dplyr,purrr,magrittr)

src_dir <-dirname(rstudioapi::getSourceEditorContext()$path)
dat_dir <- sub('src.*','data',src_dir)
res_dir <- sub('src.*','result',src_dir)

windowsFonts("Arial"=windowsFont("Arial"))

# unit
unit.pi <-quote('t ．ha'^'-1') 
xlab1<-bquote(bold(Mean~yield)~bold('('*.(unit.pi)*')'))

source(paste0(src_dir,"/TC_theme2.R"))
tc_theme2 <- TC_theme2(ax.txt.siz=6,
                       ax.tit.siz=6,
                       t=2,r=2,b=4,l=2)# axis line)
# fig1 -------------------------------------------------------------------------
stable_table <- read.csv(paste0(res_dir,"/Fig1_S1.csv.csv")) %>%
  # skip two SI for display
  dplyr::filter(!SI%in%c("Stability.variance","Ecovalence.modified")) %>% 
  mutate(SI=gsub("\\."," ",SI),# replace . with space for display
         # set display order of SI　by their correlation type with mean yield
         SIf=factor(SI,levels = c("Environmental variance",
                                  "Coefficient of regression","Genotypic stability",
                                  "Genotypic superiority measure","Safety first index","Coefficient of determination",
                                  "Adjusted coefficient of variation","Deviation mean squares",
                                  "Ecovalence","Variance of rank"))) %>% arrange(SIf)

stable_tablelist<- stable_table %>% group_by(SIf) %>% group_split()



fig1.list <- imap(stable_tablelist,function(sub,id){
  
  y.range <- sub$SI_value %>% range()
  y.m <- y.range[1]+diff(y.range)/2
  x.range <- sub$Mean.yield %>% range()
  
  p.scatter <-ggplot(sub,aes_string(x="Mean.yield",y="SI_value",color="label"))+
    geom_point(size=.5,alpha=.3,aes(shape=label))+
    labs(x=xlab1, y=sub[["SI"]][1]) +
    scale_y_continuous(limits = c(y.range[1],y.range[2]*1.02))+
    tc_theme2+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks   =element_line(size = .1),# tick thickness
          axis.ticks.length=unit(.1, "cm"),# tick length
          axis.line.x.top = element_line(size=.1),# axis line
          axis.line.x.bottom = element_line(size =.1),# axis line
          axis.line.y.left = element_line(size=.1))
  
  p.box <-  ggplot(sub,aes(x=label,y=SI_value, color = label,group=label)) +
    theme_classic()+
    geom_jitter(position=position_jitter(width=0.3),alpha=0.5,size=.06,aes(shape=label)) +
    geom_boxplot(fill=NA,alpha=0.6, color="gray",outlier.size=0,size=.05) +
    theme(legend.position = "none",
          axis.text = element_text(size=3.5),
          axis.ticks   =element_line(size = .05),# tick thickness
          axis.ticks.length=unit(.05, "cm"),# tick length,
          axis.text.x =element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
          axis.line.x.bottom = element_line(size =.1),# axis line
          axis.line.y.left = element_line(size=.1))
  
  if(id==5){ # to avoid the equation placed overlaping with the points
    p.scatter <-  p.scatter+ggpmisc::stat_poly_eq(label.x= .7,label.y = c(.3 ,.2,.1),show.legend = FALSE,size = 2)
  }else{
    p.scatter <-  p.scatter+ggpmisc::stat_poly_eq(label.x= .95,label.y = c(.9,.8,.7),show.legend = FALSE,size = 2)
  }
  
  if(id%in%c(4,5)){
    res_plot <- p.scatter+
      annotate("text",label=letters[id],x=1,y=y.range[2]*1.01)+ 
      annotation_custom(ggplotGrob(p.box), 
                        xmin =  x.range[1]*.99,
                        xmax =  x.range[2]*.5, 
                        ymin =y.range[1]*1.01, ymax = y.range[1]+diff(y.range)*.5)
  }else{
    res_plot <- p.scatter+
      scale_x_continuous(limits = c(x.range[1],x.range[2]*1.4),breaks = c(1:4))+
      annotate("text",label=letters[id],x=1,y=y.range[2])+
      annotation_custom(ggplotGrob(p.box), 
                        xmin =  x.range[2]*.99,
                        xmax =  x.range[2]*1.43, 
                        ymin =y.range[1]*1.01, ymax = y.range[1]+diff(y.range)*.5)
  }
  
  return(res_plot)
  
})

# add big legend plot 
df <- data.frame(x=c(1,1,1),y=c(1,2,3),z=c("even","random","top20"))
legend.plot <- ggplot(df,aes(x=x,y=y,color=z,label=z))+
  geom_point(size=2,alpha=.8,aes(shape=z))+
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

fig1.list[[11]] <- legend.plot

# add full name corresponding to abbreviation letters
text.legend.df <- data.frame(x=rep(1,10),y=seq(10,1,-1),
                  z=paste(letters[1:10],c("Environmental variance",
                                          "Coefficient of regression","Genotypic stability",
                                          "Genotypic superiority measure","Safety first index",
                                          "Coefficient of determination",
                                          "Adjusted coefficient of variation","Deviation mean squares",
                                          "Ecovalence","Variance of rank"),sep=" : "))
text.legend <- ggplot(text.legend.df,aes(x=x,y=y,label=z))+
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
        plot.margin = margin(t = 5, r = 1, b = 0, l = 5, unit = "pt"),
        axis.line = element_blank())

fig1.list[[12]] <- text.legend

mergeplot <- cowplot::plot_grid(plotlist = fig1.list,ncol = 4,align="hv")

# shared x and y axis title
xlabel_text <- grid::textGrob(xlab1, gp=grid::gpar(fontface="bold", fontsize=10),
                        vjust = .4,hjust = .2)
ylabel_text <- grid::textGrob("SI", gp=grid::gpar(fontface="bold", fontsize=10),
                        vjust = 0.3,hjust = .2,rot=90)
# merge 
fig1 <- cowplot::plot_grid(gridExtra::grid.arrange(
  cowplot::plot_grid(ylabel_text,mergeplot,rel_widths = c(1,20)),
  bottom=xlabel_text))


tiff(filename=paste0(res_dir,'/Fig.1.tiff'),
     # type="cairo",
     units="cm",
     width=17.4,
     height=12,
     compression = "lzw",
     pointsize=12,
     res=330,# dpi,
     family="Arial")
fig1
dev.off()

# figs1 distribution ------------------------------------------------------------

# first ten plots
si_distribution <- imap(stable_tablelist,function(sub,id){
  
  y.range <- sub$SI_value %>% range()
  
  si.distribution<-ggplot(sub)+
    xlab(' ')+
    stat_density(aes(x=SI_value,color=label,group=label), position = "identity",
                 geom = "line", alpha = 0.4,size=.5) +
    labs(x=sub[["SI"]][1],color = "Sampling methods") +
    tc_theme2+
    annotate("text",-Inf,Inf,label=letters[id], hjust = -1, vjust =2,size=3)+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks   =element_line(size = .1),# tick thickness
          axis.ticks.length=unit(.1, "cm"),# tick length
          axis.line.x.top = element_line(size=.1),# axis line
          axis.line.x.bottom = element_line(size =.1),# axis line
          axis.line.y.left = element_line(size=.1))
  
  if (id ==1){
    si.distribution <- si.distribution+
      ggplot2::guides(
        color = ggplot2::guide_legend(override.aes = list(size = 1),
                                      direction = 'vertical',
                                      keyheight=0.3,
                                      keywidth=.3,
                                      default.unit="cm"))+
      theme(legend.position = c(.35,.6),
            legend.text = element_text(size=5),
            legend.title = element_text(size=5),
            # legend.spacing.y = unit(.001, 'cm'),
            legend.key.width = unit(.5,"cm") )
  }
  
  return(si.distribution) 
  
})

# the 11th  plot
mean_yield_distribution <-ggplot(stable_tablelist[[1]])+
  xlab(xlab1)+
  stat_density(aes(x=Mean.yield,color=label,group=label), position = "identity",
               geom = "line", alpha = 0.4,size=.5) +
  labs(color = "Sampling methods")+tc_theme2+
  theme(legend.position = "none",
        legend.text = element_text(size=3),
        legend.title = element_text(size=3),
        legend.key.width = unit(.5,"cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks   =element_line(size = .1),# tick thickness
        axis.ticks.length=unit(.1, "cm"),# tick length
        axis.line.x.top = element_line(size=.1),# axis line
        axis.line.x.bottom = element_line(size =.1),# axis line
        axis.line.y.left = element_line(size=.1))+
  ggplot2::guides(
    color = ggplot2::guide_legend(override.aes = list(size = 1),
                                  direction = 'vertical',
                                  keyheight=0.3,
                                  keywidth=.3,
                                  default.unit="cm"))+
  annotate("text",label=letters[11],x=-Inf,y=Inf,size=4, hjust = -1, vjust = 2)

# merge the first elenvth plots
figs1.list<- c(si_distribution[1:10],list(mean_yield_distribution))

# corresponding full name of label letters
text.legend.df <- data.frame(x=rep(1,11),y=seq(11,1,-1),
                  z=paste(letters[1:11],c("Environmental variance",
                                          "Coefficient of regression","Genotypic stability",
                                          "Genotypic superiority measure","Safety first index",
                                          "Coefficient of determination",
                                          "Adjusted coefficient of variation","Deviation mean squares",
                                          "Ecovalence","Variance of rank","Mean yield"),sep=" : "))

corres.text <- ggplot(text.legend.df,aes(x=x,y=y,label=z))+
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
        plot.margin = margin(t = 0, r = 1, b = 0, l = 0, unit = "pt"),
        axis.line = element_blank())

figs1.list[[12]] <- corres.text

p1 <- cowplot::plot_grid(plotlist = figs1.list,ncol = 4,align = "hv")

# shared x and y legends
xlabel_text <- grid::textGrob("Value of SI/Trait", gp=grid::gpar(fontface="bold", fontsize=10),
                        vjust = .4,hjust = .2)
ylabel_text <- grid::textGrob("Density", gp=grid::gpar(fontface="bold", fontsize=10),
                        vjust = 0.3,hjust = .2,rot=90)
# final merge
figs1 <- cowplot::plot_grid(gridExtra::grid.arrange(
  cowplot::plot_grid(ylabel_text,p1,rel_widths = c(1,20)),
  bottom=xlabel_text))

tiff(filename=paste0(res_dir,'/Fig.S1.tiff'),
     units="cm",
     width=17.4,
     height=12,
     compression = "lzw",
     pointsize=12,
     res=330,# dpi,
     family="Arial")
figs1
dev.off()
