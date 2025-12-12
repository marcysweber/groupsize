#setup
##########

library(nlrx)
library(sensitivity)
library(ggplot2)
library(future)
library(reshape2)
library(ggrepel)
library(data.table)
library(ggpubr)
library(dplyr)

#library(tidyverse)

musigma_processing <- function(data) {
  return(dcast(data, metric + parameter ~ index))
}

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

heatmaptheme <- 
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),)

setwd("./figuredata")
load("figuredata.RData")
##########


#Figure 3 - group size itself
###########


mean(resultsGS$`mean-group-size`)
median(resultsGS$`mean-group-size`)



a <- ggplot(resultsGS, aes(`mean-group-size`)) +
  geom_histogram(bins = 20, aes(y=..count../sum(..count..)), fill="grey25", col="white") +
  scale_x_log10() +
  labs(title = "A.") +
  xlab("population mean group size")+
  ylab("proportion of total") +
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

newclumpabun <- GSheatmapdata[[1]]
newclumpenergypercap <- GSheatmapdata[[2]]
newenergypercapabun <- GSheatmapdata[[3]]

groupsizebreaks <- c(2, 5, 10, 25, 50, 100)
b <- ggplot(newenergypercapabun, aes(x = abundance/1000, y = energy.per.capita, fill = mean.group.size)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1, trans = "log", breaks = groupsizebreaks) +
  labs(title = "B.", fill = "group\nsize") +
  xlab("abundance") +  
  ylab("energy per capita")+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

c <- ggplot(newclumpenergypercap, aes(x = clump.size, y = energy.per.capita, fill = mean.group.size)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1, trans = "log", breaks = groupsizebreaks) +
  labs(title = "C.", fill = "group\nsize") +
  xlab("clump size")+
  ylab("energy per capita") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

d <- ggplot(newclumpabun, aes(x = clump.size, y = abundance/1000, fill = mean.group.size)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1, trans = "log", breaks = groupsizebreaks) +
  labs(title = "D.", fill = "group\nsize") +
  xlab("clump size") +  
  ylab("abundance")+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

multiplot(a, c, b, d, cols = 2)

############


#Figure 4 intake ~ clump x group size
############

bigexp_smallclump <- p1.2data[p1.2data$clump.size==1,]
bigexp_medsmallclump <- p1.2data[p1.2data$clump.size==126,]
bigexp_medmedclump <- p1.2data[p1.2data$clump.size==251,]
bigexp_medbigclump <- p1.2data[p1.2data$clump.size==376,]
bigexp_bigbigclump <- p1.2data[p1.2data$clump.size==501,]
newsmallclump <- p1.2data[p1.2data$clump.size==63,]

bigexp_smallclump_lm <- lm(foraging.efficiency.dist~mean.group.size, data = bigexp_smallclump)
bigexp_medsmallclump_lm <- lm(foraging.efficiency.dist~mean.group.size, data = bigexp_medsmallclump)
bigexp_medmedclump_lm <- lm(foraging.efficiency.dist~mean.group.size, data = bigexp_medmedclump)
bigexp_medbigclump_lm <- lm(foraging.efficiency.dist~mean.group.size, data = bigexp_medbigclump)
bigexp_bigbigclump_lm <- lm(foraging.efficiency.dist~mean.group.size, data = bigexp_bigbigclump)

newsmallclump_lm <- lm(foraging.efficiency.dist~log(mean.group.size), data = newsmallclump)


a <- ggplot(p1.2data, aes(x=mean.group.size, y=foraging.efficiency.time)) +
  labs(title = "A. all clump sizes") +
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ ylim(0, 5) +
  ylab("energy intake rate") +
  xlab("population mean group size") +
  geom_smooth(method = "lm", color = "black", se = FALSE)+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +
  stat_regline_equation(label.y = 0, label.x = 1.2, aes(label = after_stat(eq.label))) 

b <- ggplot(p1.2data[p1.2data$clump.size==1,], aes(x=mean.group.size, y=foraging.efficiency.time)) +
  labs(title = "B. clump size = 1") +
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ ylim(0, 5) +
  ylab("energy intake rate") +
  xlab("population mean group size") +
  geom_smooth(method = "lm", color = "black", se = FALSE)+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +
  stat_regline_equation(label.y = 0, label.x = 1.2, aes(label = after_stat(eq.label))) 

c <- ggplot(rbind(bigexp_medsmallclump), aes(x=mean.group.size, y=foraging.efficiency.time)) +
  labs(title = "c. clump size = 126") +
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ ylim(0, 5) +
  ylab("energy intake rate") +
  xlab("population mean group size") +
  geom_smooth(method = "lm", color = "black", se = FALSE)+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +
  stat_regline_equation(label.y = 0, label.x = 1.2, aes(label = after_stat(eq.label))) 

d <- ggplot(bigexp_bigbigclump, aes(x=mean.group.size, y=foraging.efficiency.time)) +
  labs(title = "d. clump size = 501") +
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ ylim(0, 5) +
  ylab("energy intake rate") +
  xlab("population mean group size") +
  geom_smooth(method = "lm", color = "black", se = FALSE)+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +
  stat_regline_equation(label.y = 0, label.x = 1.2, aes(label = after_stat(eq.label))) 


multiplot(a, b, c, d, cols = 4)



############

#Figure 5 distance ~ clump x group size
#############
a <- ggplot(p1.2data, aes(x=mean.group.size, y=mean.distance.traveled / 17.9)) +
  labs(title = "A. all clump sizes") + 
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ scale_y_log10(limits=c(75, 2250)) +
  ylab("daily distance traveled") +
  xlab("population mean group size") +
  geom_smooth(method = "lm", color = "black", se = FALSE)+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +
  stat_regline_equation(label.y = log10(80), label.x = log10(11), aes(label = after_stat(eq.label)))  



b <- ggplot(p1.2data[p1.2data$clump.size==1,], aes(x=mean.group.size, y=mean.distance.traveled / 17.9)) +
  labs(title = "B. clump size = 1") + 
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ scale_y_log10(limits=c(75, 2250)) +
  ylab("daily distance traveled") +
  xlab("population mean group size") +
  geom_smooth(method = "lm", color = "black", se = FALSE)+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +
  stat_regline_equation(label.y = log10(80), label.x = log10(11), aes(label = after_stat(eq.label)))  



c <- ggplot(p1.2data[p1.2data$clump.size==126,], aes(x=mean.group.size, y=mean.distance.traveled / 17.9)) +
  labs(title = "C. clump size = 126") + 
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ scale_y_log10(limits=c(75, 2250)) +
  ylab("daily distance traveled") +
  xlab("population mean group size") +
  geom_smooth(method = "lm", color = "black", se = FALSE)+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +
  stat_regline_equation(label.y = log10(80), label.x = log10(11), aes(label = after_stat(eq.label)))  

d <- ggplot(p1.2data[p1.2data$clump.size==501,], aes(x=mean.group.size, y=mean.distance.traveled / 17.9)) +
  labs(title = "D. clump size = 501") + 
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ scale_y_log10(limits=c(75, 2250)) +
  ylab("daily distance traveled") +
  xlab("population mean group size") +
  geom_smooth(method = "lm", color = "black", se = FALSE)+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +
  stat_regline_equation(label.y = log10(80), label.x = log10(11), aes(label = after_stat(eq.label)))  

multiplot(a, b, c, d, cols = 4)


############


#Figure 6 - target distance modulates foraging ~ group size relationship
##############
a<-ggplot(tgtneighbor_tgtdistv2[tgtneighbor_tgtdistv2$tgt.dist < 20,], aes(x = mean.group.size, y = foraging.efficiency.time)) +
  geom_point(color = "gray25") +
  ylim(0, 5) + xlim(1, 35) +
  labs(title = "A. target distance < 20", x = "mean group size", y = "mean intake rate")+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "black")+  
  stat_regline_equation(label.y.npc = "top", label.x.npc = "left", aes(label = after_stat(eq.label)))  

#annotate("text", label = shorttgtdist.lm$coefficients[2], x = 25, y = 2.9)

longtgtdist.lm <- lm(formula = foraging.efficiency.dist~mean.group.size, data = tgtneighbor_tgtdistv2[tgtneighbor_tgtdistv2$tgt.dist > 20,])
b<-ggplot(tgtneighbor_tgtdistv2[tgtneighbor_tgtdistv2$tgt.dist > 20,], aes(x = mean.group.size, y = foraging.efficiency.time)) +
  geom_point(color = "gray25") +
  ylim(0, 5) + xlim(1, 35) +
  labs(title = "B. target distance > 20", x = "mean group size", y = "mean intake rate")+
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +  
  stat_regline_equation(label.y.npc = "bottom", label.x.npc = "left", aes(label = after_stat(eq.label)))  

#annotate("text", label = longtgtdist.lm$coefficients[2], x = 20, y = 0.75)


multiplot(a, b, cols = 2)

c<-ggplot(tgtneighbor_tgtdistv2[tgtneighbor_tgtdistv2$tgt.dist < 20,], aes(x = mean.group.size, y = mean.distance.traveled)) +
  geom_point(color = "gray25") +
  #ylim(0, 5) + xlim(1, 35) +
  labs(title = "C. target distance < 20", x = "mean group size", y = "mean distance traveled")+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "black")+  
  stat_regline_equation(label.y.npc = "top", label.x.npc = "left", aes(label = after_stat(eq.label)))  

#annotate("text", label = shorttgtdist.lm$coefficients[2], x = 25, y = 2.9)

longtgtdist.lm <- lm(formula = foraging.efficiency.dist~mean.group.size, data = tgtneighbor_tgtdistv2[tgtneighbor_tgtdistv2$tgt.dist > 20,])
d<-ggplot(tgtneighbor_tgtdistv2[tgtneighbor_tgtdistv2$tgt.dist > 20,], aes(x = mean.group.size, y = mean.distance.traveled)) +
  geom_point(color = "gray25") +
  #ylim(0, 5) + xlim(1, 35) +
  labs(title = "D. target distance > 20", x = "mean group size", y = "mean distance traveled")+
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +  
  stat_regline_equation(label.y.npc = "bottom", label.x.npc = "left", aes(label = after_stat(eq.label)))  

#annotate("text", label = longtgtdist.lm$coefficients[2], x = 20, y = 0.75)


multiplot(a, b, c, d, cols = 2)

##############


#Appendix: Pattern-matching figures
##############

#daily path length, comparison Vidal-Cordasco data and model output
a<- ggplot(VidalCardaso, aes(x = `DMD (km/day)`)) +
  geom_histogram(aes(y=..count../sum(..count..))) +
  xlim(0, 10) +
  ylim(0, 0.2) +
  labs(title = "A.") + 
  xlab("mean daily movment distance\n(km/day)\nVidal-Cordasco et al. 2020") +
  ylab("proportion of species")+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), 
    panel.background = element_rect(fill = "white", color = "gray50"),
  )

days <- 4300 / 24
resultsGS$distinkms <- (resultsGS$`mean-distance-traveled`* 10) / days 
resultsGS$distinkms <- resultsGS$distinkms / 1000

b<- ggplot(resultsGS, aes(x = distinkms)) +
  geom_histogram(aes(y=..count../sum(..count..))) +
  xlim(0, 10) +
  ylim(0, 0.2) +
  labs(title = "B.") +
  xlab("mean daily movement distance\n(converted)\n") +
  ylab("proportion of simulations")+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), 
    panel.background = element_rect(fill = "white", color = "gray50"),
  )


multiplot(a, b, cols = 2)

trimmedKamilarCooper <- KamilarCooperactivitybudgetdata[complete.cases(KamilarCooperactivitybudgetdata[,37:38]),]
trimmedKamilarCooper$`move/move + feed` <- as.numeric(trimmedKamilarCooper$`move/move + feed`)

a<- ggplot(trimmedKamilarCooper, aes(x = `move/move + feed`)) +
  geom_histogram(aes(y=..count../sum(..count..))) +
  xlim(0, 1.0) +
  ylim(0, 0.2) +
  labs(title = "A.") + 
  xlab("time moving out of\ntime moving or feeding\n(Kamilar & Cooper 2013)") +
  ylab("proportion of species")+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), 
    panel.background = element_rect(fill = "white", color = "gray50"),
  )



b<- ggplot(resultsGS, aes(x = `mean-percent-time-moving`)) +
  geom_histogram(aes(y=..count../sum(..count..))) +
  xlim(0, 1.0) +
  ylim(0, 0.2) +
  labs(title = "B.") +
  xlab("time moving of all timesteps\n\n") +
  ylab("proportion of simulations")+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), 
    panel.background = element_rect(fill = "white", color = "gray50"),
  )


multiplot(a, b, cols = 2)



#######

#% belonging to group

#Elementary effects for population mean group size.

#Elementary effects for population mean total energy intake 

#Elementary effects for population mean distance traveled. 

#Elementary effects for population mean inter-individual distance 

#Heatmaps with the interactive effects of the most influential parameters on population mean group size

#Heatmaps with the interactive effects of the most influential parameters on intake

#Heatmaps with the interactive effects of the most influential parameters on daily distance traveled.

#Energy intake rate as a product of mean group size, for all clump sizes (A) and for clumps of different sizes (B-G)

#Daily distance traveled as a product of mean group size, for all clump sizes (A) and for clumps of different sizes (B-G).

#within-pop/group-level results

#Heatmaps with the interactive effects of the most influential parameters on interindividual distance. 