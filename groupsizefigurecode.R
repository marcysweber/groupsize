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
  return(dcast(as.data.table(data), metric + parameter ~ index))
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
load("supplementalfiguredata.RData")
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

#Appendix/Supplement: Pattern-matching figures
##############

#daily path length, comparison Vidal-Cordasco data and model output
#data: VidalCardaso, KamilarCooper, resultsGS
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




#% belonging to group
#data: resultsGS

ggplot(resultsGS, aes(`percent-grouped`)) +
  geom_histogram(binwidth = 0.1, aes(y=..count../sum(..count..)), fill="grey25", col="white")+
  xlab("percent of popoulation belonging to a group")+
  ylab("proportion of all simulations") +
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

 #######

#Appendix/Supplement: Morris Elementary Effects figures
#########
#data: analysisGS

analysisGSscatter <- data.frame()
for (df in analysisGS) {
  analysisGSscatter <- rbind(analysisGSscatter, (musigma_processing(as.data.frame(df))))
}

eetheme <- theme(
  axis.title = element_text(size = 20, color = "black"),
  axis.text = element_text(size = 14, color = "black"),
  
  panel.grid = element_line(color = "black"),
  panel.grid.major = element_line(color = "gray75"),
  panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
)

eelabelsize <- 3

#Elementary effects for population mean group size.

meanGSMEEdata <- analysisGSscatter[analysisGSscatter$metric == "mean-group-size_mean",]
meanGSMEEdata$number <- 1:15
meanGSMEEdata$readable.params <- c("abundance", 
                                   "clump size", 
                                   "energy per capita", 
                                   "extraction rate (mean)", 
                                   "extraction rate (SD)", 
                                   "movement speed", 
                                   "movement noise",
                                   "sensory range, other primates",
                                   "patch regrowth interval",
                                   "mean patch quality",
                                   "SD patch quality",
                                   "% regrowth",
                                   "sensory range, resources",
                                   "target distance", 
                                   "target neighbors") 

a<-ggplot(meanGSMEEdata, aes(x = mustar, y = sigma, label = parameter)) +
  geom_point() + geom_text_repel(aes(label=ifelse(sigma>20, as.character(readable.params),number)), size = eelabelsize) +
  labs(title = "A. Sensitivity analysis, Mean group size") +
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  scale_x_continuous(expand = c(0.1,0.1))+
  eetheme


#Elementary effects for population mean total energy intake
intakeMEEdata <- analysisGSscatter[analysisGSscatter$metric == "foraging-efficiency-time_mean",]
intakeMEEdata$number <- 1:15
intakeMEEdata$readable.params <- c("abundance", 
                                   "clump size", 
                                   "energy per capita", 
                                   "extraction rate (mean)", 
                                   "extraction rate (SD)", 
                                   "movement speed", 
                                   "movement noise",
                                   "sensory range, other primates",
                                   "patch regrowth interval",
                                   "mean patch quality",
                                   "SD patch quality",
                                   "% regrowth",
                                   "sensory range, resources",
                                   "target distance", 
                                   "target neighbors") 

b<-ggplot(intakeMEEdata, aes(x = mustar, y = sigma, label = parameter)) +
  geom_point() + geom_text_repel(aes(label=ifelse(mustar>0.4, as.character(readable.params),number)), size = eelabelsize) +
  labs(title = "B. Sensitivity analysis, energy intake") +
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  scale_x_continuous(expand = c(0.1,0.1))+
  eetheme


#Elementary effects for population mean distance traveled. 
distGSMEEdata <- analysisGSscatter[analysisGSscatter$metric == "mean-distance-traveled_mean",]
distGSMEEdata$number <- 1:15
distGSMEEdata$readable.params <- c("abundance", 
                                   "clump size", 
                                   "energy per capita", 
                                   "extraction rate (mean)", 
                                   "extraction rate (SD)", 
                                   "movement speed", 
                                   "movement noise",
                                   "sensory range, other primates",
                                   "patch regrowth interval",
                                   "mean patch quality",
                                   "SD patch quality",
                                   "% regrowth",
                                   "sensory range, resources",
                                   "target distance", 
                                   "target neighbors") 

c<-ggplot(distGSMEEdata, aes(x = mustar, y = sigma, label = parameter)) +
  geom_point() + geom_text_repel(aes(label=ifelse(mustar>10000, as.character(readable.params),number)), size = eelabelsize) +
  labs(title = "C. Sensitivity analysis, Distance traveled") +
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  scale_x_continuous(expand = c(0.1,10))+
  eetheme


#Elementary effects for population mean inter-individual distance 
iidGSMEEdata <- analysisGSscatter[analysisGSscatter$metric == "mean-inter-indiv-dist_mean",]
iidGSMEEdata$number <- 1:15
iidGSMEEdata$readable.params <- c("abundance", 
                                  "clump size", 
                                  "energy per capita", 
                                  "extraction rate (mean)", 
                                  "extraction rate (SD)", 
                                  "movement speed", 
                                  "movement noise",
                                  "sensory range, other primates",
                                  "patch regrowth interval",
                                  "mean patch quality",
                                  "SD patch quality",
                                  "% regrowth",
                                  "sensory range, resources",
                                  "target distance", 
                                  "target neighbors") 

d<-ggplot(iidGSMEEdata, aes(x = mustar, y = sigma, label = parameter)) +
  geom_point() + geom_text_repel(aes(label=ifelse(mustar>30, as.character(readable.params),number)), size = eelabelsize) +
  labs(title = "D. Sensitivity analysis, Inter-Individual Distance") +
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  scale_x_continuous(expand = c(0.1,1))+
  scale_y_continuous(expand = c(0.1, 2))+
  eetheme

multiplot(a, b, c, d, cols=2)

#########

#Appendix/Supplement: heatmaps for all factorial experiments
##########
#data: pop.level
#####
#1
clump_maxmove <- pop.level[[1]]

#2
clump_abundance<- pop.level[[2]]

#3
clump_energypercap<- pop.level[[3]]

#4
energypercap_abundance<- pop.level[[4]]

#5
qualmean_clump<- pop.level[[5]]

#6
qualmean_maxmove<- pop.level[[6]]

#7
regrowth_clump<- pop.level[[7]]

#8
regrowth_qualmean<- pop.level[[8]]

#9
regrowth_maxmove<- pop.level[[9]]

#10
tgtneighbor_abundance<- pop.level[[10]]

#11
tgtneighbor_clump<- pop.level[[11]]

#12
tgtneighbor_energypercap<- pop.level[[12]]

#13
tgtneighbor_tgtdist<- pop.level[[13]]
#####

#Heatmaps with the interactive effects of the most influential parameters on population mean group size
#####
a <- ggplot(tgtneighbor_tgtdist, aes(x = tgt.neighbor, y = tgt.dist, fill = log(mean.group.size))) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "behaviors", fill = "log group size") +
  xlab("target neighbors")+
  ylab("target distance") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )


b <- ggplot(tgtneighbor_energypercap, aes(x = tgt.neighbor, y = energy.per.capita, fill = log(mean.group.size))) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "", fill = "log group size") +
  xlab("target neighbors")+
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

c <- ggplot(tgtneighbor_clump, aes(x = tgt.neighbor, y = clump.size, fill = log(mean.group.size))) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "", fill = "log group size") +
  xlab("target neighbors")+
  ylab("clump size") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

d <- ggplot(tgtneighbor_abundance, aes(x = tgt.neighbor, y = abundance, fill = log(mean.group.size))) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "", fill = "log group size") +
  xlab("target neighbors")+
  ylab("abundance") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

e <- ggplot(energypercap_abundance, aes(x = energy.per.capita, y = abundance, fill = log(mean.group.size))) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "", fill = "log group size") +
  xlab("energy per capita")+
  ylab("abundance") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

f <- ggplot(clump_energypercap, aes(x = clump.size, y = energy.per.capita, fill = log(mean.group.size))) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "", fill = "log group size") +
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

g <- ggplot(clump_abundance, aes(x = clump.size, y = abundance, fill = log(mean.group.size))) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "", fill = "log group size") +
  xlab("clump size")+
  ylab("abundance") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

multiplot(a, b, c, d, e, f, g, cols = 2)
#####

#Heatmaps with the interactive effects of the most influential parameters on intake
#####

a <- ggplot(extraction_regrowth, aes(x = extraction.rate.mean, y = patch.regrowth.interval, fill = foraging.efficiency.time)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "", fill = "energy \nintake \nrate") +
  xlab("extraction rate")+
  ylab("regrowth interval") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

b <- ggplot(extraction_energypercap, aes(x = extraction.rate.mean, y = energy.per.capita, fill = foraging.efficiency.time)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "", fill = "energy \nintake \nrate") +
  xlab("extraction rate")+
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

c <- ggplot(extraction_clump, aes(x = extraction.rate.mean, y = clump.size, fill = foraging.efficiency.time)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "", fill = "energy \nintake \nrate") +
  xlab("extraction rate")+
  ylab("clump size") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

d <- ggplot(clump_energypercap, aes(x = clump.size, y = energy.per.capita, fill = foraging.efficiency.time)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "", fill = "energy \nintake \nrate") +
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



e <- ggplot(regrowth_clump, aes(x = clump.size, y = patch.regrowth.interval, fill = foraging.efficiency.time)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "", fill = "energy \nintake \nrate") +
  xlab("clump size")+
  ylab("regrowth interval") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )



multiplot(a, b, d, c, e, cols = 2)

#####


#Heatmaps with the interactive effects of the most influential parameters on daily distance traveled.
#####
#clumpsize and regrwoth
a <- ggplot(regrowth_clump, aes(x = patch.regrowth.interval, y = clump.size, fill = mean.distance.traveled/17.9)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "a.", fill = "daily travel\ndistance") +
  xlab("regrowth interval")+
  ylab("clump size") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

#clump size and speed
b <- ggplot(clump_maxmove, aes(x = max.move, y = clump.size, fill = mean.distance.traveled/17.9)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "b.", fill = "daily travel\ndistance") +
  xlab("movement speed")+
  ylab("clump size") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

#clump size and energy per capita
c <- ggplot(clump_energypercap, aes(x = energy.per.capita, y = clump.size, fill = mean.distance.traveled/17.9)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "c.", fill = "daily travel\ndistance") +
  xlab("energy per capita")+
  ylab("clump size") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

#extraction rate mean and sp3eed
d <- ggplot(extraction_maxmove, aes(x = extraction.rate.mean, y = max.move, fill = mean.distance.traveled/17.9)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "d.", fill = "daily travel\ndistance") +
  xlab("extraciton rate mean")+
  ylab("movement speed") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

#extraction rate mean and clump size
e <- ggplot(extraction_clump, aes(x = extraction.rate.mean, y = clump.size, fill = mean.distance.traveled/17.9)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "e.", fill = "daily travel\ndistance") +
  xlab("extraciton rate mean")+
  ylab("clump size") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

#extraction rate mena and energy per capita 
f <- ggplot(extraction_energypercap, aes(x = extraction.rate.mean, y = energy.per.capita, fill = mean.distance.traveled/17.9)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "f.", fill = "daily travel\ndistance") +
  xlab("extraciton rate mean")+
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

#extraction rate mean and regrwoth
g <- ggplot(extraction_regrowth, aes(x = extraction.rate.mean, y = patch.regrowth.interval, fill = mean.distance.traveled/17.9)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "g.", fill = "daily travel\ndistance") +
  xlab("extraciton rate mean")+
  ylab("regrowth interval") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

multiplot(a, b, c, d, e, f, g, cols = 2)
#####

##########

#Figures 4 and 5 with even more clump sizes
######

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


#Energy intake rate as a product of mean group size, for all clump sizes (A) and for clumps of different sizes (B-G)


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
c <- ggplot(p1.2data[p1.2data$clump.size==63,], aes(x=mean.group.size, y=foraging.efficiency.time)) +
  labs(title = "C. clump size = 63") +
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
d <- ggplot(rbind(bigexp_medsmallclump), aes(x=mean.group.size, y=foraging.efficiency.time)) +
  labs(title = "D. clump size = 126") +
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

e <- ggplot(p1.2data[p1.2data$clump.size==251,], aes(x=mean.group.size, y=foraging.efficiency.time)) +
  labs(title = "E. clump size = 251") +
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
f <- ggplot(p1.2data[p1.2data$clump.size==376,], aes(x=mean.group.size, y=foraging.efficiency.time)) +
  labs(title = "F. clump size = 376") +
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




g <- ggplot(bigexp_bigbigclump, aes(x=mean.group.size, y=foraging.efficiency.time)) +
  labs(title = "G. clump size = 501") +
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


multiplot(a, b, c, d, e, f, g, cols = 2)


#Daily distance traveled as a product of mean group size, for all clump sizes (A) and for clumps of different sizes (B-G).
a0 <- ggplot(p1.2data, aes(x=mean.group.size, y=mean.distance.traveled / 17.9)) +
  labs(title = "A. all clump sizes") + 
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ 
  scale_y_log10(limits=c(50, 2250)) +
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
  stat_regline_equation(label.y = log10(60), label.x = log10(8), aes(label = after_stat(eq.label)))  



a <- ggplot(p1.2data[p1.2data$clump.size==1,], aes(x=mean.group.size, y=mean.distance.traveled / 17.9)) +
  labs(title = "B. clump size = 1") + 
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ 
  scale_y_log10(limits=c(50, 2250)) +
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
  stat_regline_equation(label.y = log10(75), label.x = log10(8), aes(label = after_stat(eq.label)))  




aa <- ggplot(p1.2data[p1.2data$clump.size==63,], aes(x=mean.group.size, y=mean.distance.traveled / 17.9)) +
  labs(title = "C. clump size = 63") + 
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ scale_y_log10(limits=c(50, 2250)) +
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
  stat_regline_equation(label.y.npc = "bottom", label.x = log10(8), aes(label = after_stat(eq.label)))  


b <- ggplot(p1.2data[p1.2data$clump.size==126,], aes(x=mean.group.size, y=mean.distance.traveled / 17.9)) +
  labs(title = "D. clump size = 126") + 
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ scale_y_log10(limits=c(50, 2250)) +
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
  stat_regline_equation(label.y = log10(75), label.x = log10(8), aes(label = after_stat(eq.label)))  



c <- ggplot(p1.2data[p1.2data$clump.size==251,], aes(x=mean.group.size, y=mean.distance.traveled / 17.9)) +
  labs(title = "E. clump size = 251") + 
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ scale_y_log10(limits=c(50, 2250)) +
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
  stat_regline_equation(label.y = log10(75), label.x = log10(11), aes(label = after_stat(eq.label)))  



d <- ggplot(p1.2data[p1.2data$clump.size==376,], aes(x=mean.group.size, y=mean.distance.traveled / 17.9)) +
  geom_point(size = 0.5) +
  labs(title = "F. clump size = 376") +
  scale_x_log10(limits=c(2, 150))+ scale_y_log10(limits=c(50, 2250)) +
  ylab("daily distance traveled") +
  xlab("population mean group size") +
  geom_smooth(method = "lm", color = "black", se = FALSE)+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )+
  stat_regline_equation(label.y = log10(75), label.x.npc = "center", aes(label = after_stat(eq.label)))  


e <- ggplot(p1.2data[p1.2data$clump.size==501,], aes(x=mean.group.size, y=mean.distance.traveled / 17.9)) +
  labs(title = "G. clump size = 501") + 
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ scale_y_log10(limits=c(50, 2250)) +
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
  stat_regline_equation(label.y = log10(75), label.x.npc = "center", aes(label = after_stat(eq.label)))  


#1200x260 pxls
multiplot(a0, a, aa, b, c, d, e, cols = 2)

##########


#Within-pop/group-level results
##############




###############

#Heatmaps with the interactive effects of the most influential parameters on interindividual distance. 
#####
#tgt dist and tgt neighbors
a <- ggplot(tgtneighbor_tgtdist, aes(x = tgt.neighbor, y = tgt.dist, fill = mean.inter.indiv.dist)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "a.", fill = "interindividual\ndistance") +
  xlab("target neighbors")+
  ylab("target distance") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

#tgt neighbors and clump size
b <- ggplot(tgtneighbor_clump, aes(x = tgt.neighbor, y = clump.size, fill = mean.inter.indiv.dist)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "b.", fill = "interindividual\ndistance") +
  xlab("target neighbors")+
  ylab("clump size") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

#energy per cap and abundance
c <- ggplot(energypercap_abundance, aes(x = energy.per.capita, y = abundance, fill = mean.inter.indiv.dist)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "c.", fill = "interindividual\ndistance") +
  xlab("energy per capita")+
  ylab("abundance") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

#energy per cap and clump 
d <- ggplot(clump_energypercap, aes(x = energy.per.capita, y = clump.size, fill = mean.inter.indiv.dist)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "d.", fill = "interindividual\ndistance") +
  xlab("energy per capita")+
  ylab("clump size") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )



#extraction rate mean and clump size
e <- ggplot(extraction_clump, aes(x = extraction.rate.mean, y = clump.size, fill = mean.inter.indiv.dist)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "e.", fill = "interindividual\ndistance") +
  xlab("extraction rate mean")+
  ylab("clump size") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )



#tgtdist_clump <- read.delim("basemodelprimatesocialgroups group size target distance x clump size-table.csv", skip = 6, header = TRUE, sep = ",")
f <- ggplot(tgtdist_clump, aes(x = tgt.dist, y = clump.size, fill = mean.inter.indiv.dist)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "f.", fill = "interindividual\ndistance") +
  xlab("target distance")+
  ylab("clump size") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

#tgtdist_abundance <- read.delim("basemodelprimatesocialgroups group size target distance x abundance-table.csv", skip = 6, header = TRUE, sep = ",")
g <- ggplot(tgtdist_abundance, aes(x = tgt.dist, y = abundance, fill = mean.inter.indiv.dist)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "g.", fill = "interindividual\ndistance") +
  xlab("target distance")+
  ylab("abundance") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )
#tgtdist_extraction <- read.delim("basemodelprimatesocialgroups group size target distance x extraction rate mean-table.csv", skip = 6, header = TRUE, sep = ",")
h <- ggplot(tgtdist_extraction, aes(x = tgt.dist, y = extraction.rate.mean, fill = mean.inter.indiv.dist)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "h.", fill = "interindividual\ndistance") +
  xlab("target distance")+
  ylab("extraction rate mean") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )


multiplot(a, b, c, d, e, f, g, h, cols = 2)

#####