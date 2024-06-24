# Code for figures 4A-D in Hartmann et al 2024 
#Partitioning the influence of host specificity in amphibian populations threatened by multiple emerging infectious diseases#
# in Biological Conservation


# Load library
library(ggplot2)
library(tibble)
library(tidyverse)
library(MetBrewer)
library(ggpubr)
library(MASS)
library(gridExtra)
library(RColorBrewer)
library(ggtext)
library(patchwork)
library(zoo)

# read in data
# summary data for figs4A-C
dat4abc <- read.csv("Host_contribution_fig4A-C.csv")

# summary data for figs4D
dat4d <- read.csv("host_cont_full_data.csv")

#Host contribution figures 4A, 4B, 4C
Figure4A <- ggplot(dat4abc, aes(Infection_bd, pi_bd)) 

fig4a <- Figure4A + geom_point(aes(fill=Bd_shedding, size=sppco), shape=21) + 
  scale_size(range=c(0,10),name=expression(theta[A]), guide = guide_legend(override.aes =list(fill = "black"), order=1))+
  scale_fill_gradient(low="#7BCCC4", high="#C51B7D", 
                      name=expression(theta[S]), limits=c(0,4),breaks=c(0,1,2,3,4),
                      labels=c(0,1,2,3,4)) + 
  labs(x =expression(theta[I]), y="\u03c0", title="Batrachochytrium dendrobatidis")+
  scale_y_continuous(breaks=c(0, .1, 0.2, 0.3, 0.4, 0.5), limits=c(0, 0.55))+
  scale_x_continuous(limits=c(0, 3.15), breaks=c(0, 1, 2, 3))+
  theme_bw(base_size = 20)+
  theme(strip.background = element_rect(fill="white"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=20, color="black"),
        axis.title = element_text(size=26, color="black"),
        plot.title = element_text(size=22, hjust=0.5, face="italic"),
        legend.title = element_text(size=26, color="black"))+
  geom_richtext(x = 0.7, y=0.49, label = "<i>N. perstriatus</i>", fill=NA, label.colour=NA)+
  geom_richtext(x = 1.95, y=0.09, label = "<i>A. gryllus</i>", fill=NA, label.colour=NA)+
  geom_richtext(x = 2.9, y=0.28, label = "<i>N. viridescens</i>", fill=NA, label.colour=NA)

fig4a

Figure4B <- ggplot(dat4abc, aes(Infection_pk, pi_pk))

fig4b <- Figure4B + geom_point(aes(fill=Pk_shedding, size=sppco), shape=21) + 
  scale_size(range=c(0,10),name=expression(theta[A]),guide = guide_legend(override.aes =list(fill = "black"),order=1))+
  scale_fill_gradient(low="#7BCCC4", high="#C51B7D", 
                      name=expression(theta[S]))+#,limits=c(0,3),breaks=c(0,1,2,3),
  #labels=c(0,1,2,3#)) + 
  labs(x =expression(theta[I]), y="\u03c0", title="Perkinsea")+
  scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6), limits=c(0, 0.6))+
  scale_x_continuous(breaks=c(0, 1, 2, 3, 4, 5), limits=c(0, 5.5))+
  theme_bw(base_size = 20)+
  theme(strip.background = element_rect(fill="white"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=20, color="black"),
        axis.title = element_text(size=26, color="black"),
        plot.title = element_text(size=24, hjust = 0.5),
        legend.title = element_text(size=26, color="black"))+
  geom_richtext(x = 3.55, y=0.49, label = "<i>L. sphenocephalus</i>", fill=NA, label.colour=NA)+
  geom_richtext(x = 5.1, y=0.4, label = "<i>A. tigrinum</i>", fill=NA, label.colour=NA)


fig4b

Figure4C <- ggplot(dat4abc, aes(Infection_rv, pi_rv)) 

fig4c <- Figure4C + geom_point(aes(fill=log(Rv_shedding+1), size=sppco), shape=21) + 
  scale_size(range=c(0,10),name=expression(theta[A]), guide = guide_legend(override.aes =list(fill = "black"),order=1))+
  scale_fill_gradient(low="#7BCCC4", high="#C51B7D", name=expression(theta[S]),
                      breaks=c(0, 1.0, 2.0, 3), limits=c(0,3), labels=c(0,1,2,3)) +
  scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1.0), limits=c(0, 1.05))+
  labs(x =expression(theta[I]), y="\u03c0", title="Ranavirus")+
  theme_bw(base_size = 20)+
  theme(strip.background = element_rect(fill="white"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=20, color="black"),
        axis.title = element_text(size=26, color="black"),
        plot.title = element_text(size=24, hjust = 0.5, face="italic"),
        legend.title = element_text(size=26))+
  geom_richtext(x = 0.48, y=0.66, label = "<i>N. viridescens</i>", fill=NA, label.colour=NA)

fig4c

# composite figure for Fig 2 version 1
figcomp4 <- (fig4a + fig4b + fig4c) + 
  plot_layout(ncol=3, nrow = 1,
              widths = c(3,3))+
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size=24))#add figure labels

figcomp4 #view multi-panel figure

ggsave("Fig4_asymmestries_v2.tiff", plot=figcomp4, units="in", width=26, height=8, dpi=300, compression = 'lzw')



Figure4D <- ggplot(dat4d, aes(rel_preb, pi)) 

fig4d <- Figure4D + geom_point(aes(fill=rel_load, size=Density), alpha=0.8, shape=21) + 
  scale_size(range=c(0,10),name=expression(theta[A]), guide = guide_legend(override.aes =list(fill = "black"), order=1))+
  scale_fill_gradient(low="#7BCCC4", high="#C51B7D", 
                      name=expression(theta[S]), limits=c(0,3), breaks=c(0, 1, 2, 3),
                      labels=c(0,1,2,3))+ 
  labs(x =expression(theta[I]), y="\u03c0", title="Total Pathogen Asymmetry")+
  scale_y_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), limits=c(0, 0.52))+
  theme_bw(base_size = 20)+
  theme(strip.background = element_rect(fill="white"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=20, color="black"),
        axis.title = element_text(size=26, color="black"),
        plot.title = element_text(size=22, hjust=0.5),
        legend.title = element_text(size=26, color="black"))+
  geom_richtext(x = 1.8, y=0.18, label = "<i>N. viridescens</i>", fill=NA, label.colour=NA, size=5)+
  geom_richtext(x = 1.24, y=0.3, label = "<i>L. sphenocephalus</i>", fill=NA, label.colour=NA, size=5)+
  geom_richtext(x = 4.5, y=0.06, label = "<i>A. gryllus</i>", fill=NA, label.colour=NA, size=5)+
  geom_richtext(x = 0.48, y=0.092, label = "<i>N. perstriatus</i>", fill=NA, label.colour=NA, size=5)+
  geom_richtext(x = 2.35, y=0.25, label = "<i>A. tigrinum</i>", fill=NA, label.colour=NA, size=5)


fig4d


ggsave("Fig4D.tiff", plot=fig4d, units="in", width=8, height=6, dpi=300, compression = 'lzw')



# composite figure for Fig 2 version 1
figcomp4full <- (fig4a + fig4b + fig4c + fig4d) + 
  plot_layout(ncol=4, nrow = 1,
              widths = c(3,3))+
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size=24))#add figure labels

figcomp4full #view multi-panel figure

ggsave("Fig4_full_combined.tiff", plot=figcomp4full, units="in", width=28.5, height=6, dpi=300, compression = 'lzw')


# composite figure 2

layout <- "
AABBCC
AABBCC
AABBCC
DDD###
DDD###
DDD###
DDD###
DDD###
DDD###
"

# arrange
figcomp4full2<-  fig4a + fig4b + fig4c + free(fig4d) + 
  plot_layout(design=layout)+
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size=26),
        legend.key.size = unit(0.55,"cm"))#add figure labels

figcomp4full2 #view multi-panel figure

ggsave("Fig4_full_V2.tiff", plot=figcomp4full2, units="in", width=22, height=14, dpi=300, compression = 'lzw')

