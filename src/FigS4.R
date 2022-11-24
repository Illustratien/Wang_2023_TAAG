rm(list = ls())
pacman::p_load(scales,ggplot2,magrittr,dplyr)

# dir ---------------------------------------------------------------------
src_dir <-dirname(rstudioapi::getSourceEditorContext()$path)
dat_dir <- sub('src.*','data',src_dir)
res_dir <- sub('src.*','result',src_dir)

source(paste0(src_dir,"/TC_theme.R"))
source(paste0(src_dir,"/TC_theme2.R"))

# trait vector -------------------------------------------------------------------------
pp.vec <- c("y_rue","y_sla","ll_modifier",
            "potential_grain_filling_rate","node_no_correction","tfac_slope")

geno.mean <- readRDS(paste0(res_dir,'/Networkdata/figure4_pp_geno.rds')) %>% 
  dplyr::filter(.,PP%in%pp.vec) %>% 
  mutate(pp.value=as.numeric(PP.value),
         PP=factor(PP,levels = pp.vec),
         source=sub("top","top 20",source),
         G = paste0(seed_number,source),
         G1 = paste0(PP.value,source,PP),
         across(c('yield','pi.yield'),function(x)x/1000),
         source = factor(source,levels = c("even","random","top 20")))

# modification for the display 
display <- geno.mean %>% 
  tidyr::gather(.,"trait","trait.value",2:3) 
display$trait <-as.factor(display$trait) 
levels(display$trait) <-  c(expression(bolditalic("P")["i, yield"]),"yield")
display$source <-as.factor(display$source)
levels(display$source) <-  c("even","random","top~20")
# figs4------------------------------------------------------------------------
#left part pp value
labelleft <- data.frame(l=letters[1:18],PP.value=0.82,density=7.8,
                        seed_number=1,
                        expand.grid(PP=geno.mean$PP %>% levels(),
                                    source=geno.mean$source %>% levels()))

pleft <- ggplot(geno.mean,aes(x=PP.value,color=source,group=seed_number))+
  stat_density(position = "identity", aes(),
               geom = "line", alpha = 0.1,size=.3)+
  facet_grid(source~PP)+
  TC_theme2(legend.position=c(.09,.9),
            ax.tit.siz = 5,strp.txt.siz = 4,ax.txt.siz = 4,
            lgd.tit.siz = 3,lgd.txt.siz = 3,r=0,
            b=.5)+
  theme(strip.text.y = element_blank(),
        strip.text.x = element_text( face = "italic"),
        axis.line = element_line(size = .5),
        axis.ticks  =element_line(size = .5))+
  xlab("Physiological parameter")+
  ggplot2::guides(
    color = ggplot2::guide_legend(override.aes = list(size = .8,alpha=.7),
                                  direction = 'vertical',
                                  keyheight=0.3,
                                  keywidth=.3,
                                  default.unit="cm",
                                  # ncol=1,
                                  title = "sampling method" ))+
  geom_text( data=labelleft,
             aes(label = l,x=PP.value,y=density),show.legend = F,size=3)

# right part mean yield and SI distribution
labelright <- data.frame(l=letters[19:24],
                         trait.value=0.52,
                         density=4.5,
                         seed_number=1,
                         expand.grid(trait=display$trait %>% levels(),
                                     source=display$source %>% levels()))

pright <- ggplot(display,aes(x=trait.value,color=source,group=seed_number))+
  stat_density(position = "identity", aes(),
               geom = "line", alpha = 0.1,size=.3)+
  facet_grid(source~trait,
             labeller =label_parsed)+
  TC_theme2( ax.tit.siz = 5,strp.txt.siz = 4,ax.txt.siz = 4,
             lgd.tit.siz = 8,lgd.txt.siz = 8,
             b=.2,l=2,legend.position="none")+
  xlab(bquote(bold('t ? ha'^'-1')))+
  theme(axis.title.y = element_blank(),
        axis.line = element_line(size = .5),
        axis.ticks  =element_line(size = .5))+# tick thickness
  geom_text(data=labelright,
            aes(label = l,x=trait.value,y=density),
            parse = T,show.legend = F,size=3)

figs4 <- cowplot::plot_grid(pleft,pright, 
                            nrow = 1, 
                            rel_widths = c(3,1.2))



tiff(filename=paste0(res_dir,'/Fig.S4.tiff'),
     units="cm",
     width=17.4,
     height=8,
     compression = "lzw",
     pointsize=12,
     res=500)

figs4 
dev.off()
