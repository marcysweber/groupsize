#packages, functions, paths
################

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


netlogopath <- file.path("/Program Files/NetLogo 6.2.2")
modelpath <- file.path("C:\\Users\\Marcy\\Documents\\GitHub\\primate-social-groups-model\\basemodelprimatesocialgroups.nlogo")
outpath <- file.path("C:\\Users\\Marcy\\Desktop\\")
#####

#visualization vars
####################

heatmaptheme <- 
  theme(
  axis.title = element_text(size = 10, color = "black"),
  axis.text = element_text(size = 8, color = "black"),
  legend.text = element_text(size = 8),
  legend.title = element_text(size = 10),
  panel.grid = element_line(color = "black"),
  panel.grid.major = element_line(color = "gray75"),
  panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),)


#####

#H0 setup & run 
################

#metrics for H0, group size chapter
#these are the pattern-matching metrics and metrics that ensure
#that we're getting groups of variable size

h0metrics <- c(
                "mean-group-size",
                "var-group-size",
                "percent-grouped", 
               "mean-distance-traveled",
               "mean-percent-time-moving")

#constants for H0, group size chapter
#group recog turned ON

h0constants <- list("group-recog-module?" = "true",
                       "go-tests-on?" = "false",
                       "resource-tests-on?" = "false",
                       "move-tests-on?" = "false") 

#parameters for H0, group size chapter
#all parameters from H0 of gregariousness will be included

h0vars <- list(
  "abundance" = list(min = 200000, max = 1200000, qfun = "qunif"),
  "clump-size" = list(min = 1, max = 500, qfun = "qunif"), 
  "energy-per-capita" = list(min = 4000, max = 9000, qfun = "qunif"),
  "qual-mean" = list(min = 25, max = 150, qfun = "qunif"),
  "qual-sd" = list(min = 1, max = 20, qfun = "qunif"),
  "extraction-rate-mean" = list(min = 2, max = 7, qfun = "qunif"),
  "extraction-rate-sd" = list(min = 1, max = 3, qfun = "qunif"),
  
  "tgt-dist" = list(min = 5, max = 40, qfun = "qunif"),
  "tgt-neighbor" = list(min = 0, max = 11, qfun = "qunif"),
  
  "other-primate-detection-radius" = list(min = 50, max = 100, qfun = "qunif"),
  "resource-detection-radius" = list(min = 50, max = 100, qfun = "qunif"),
  "regrowth-rate" = list(min = 0.5, max = 1.0, qfun = "qunif"),
  "patch-regrowth-interval" = list(min = 500, max = 3000, qfun = "qunif"),
  "movement-noise" = list(min = 10, max = 45, qfun = "qunif"),
  "max-move" = list(min = 10, max = 50, qfun = "qunif"))

#set up NLRX experiment for H0 (pattern-matching and group verification)

nlh0 <- nl(nlversion = "6.2.2",
              nlpath = netlogopath,
              modelpath = modelpath,
              jvmmem = 12000)


nlh0@experiment <- experiment(expname = "groupsizeh0MEE",
                                 outpath = outpath,
                                 repetition = 1,
                                 tickmetrics = "false",
                                 idsetup = "setup",
                                 idgo = "go",
                                 runtime = 4300,
                                 #stopcond = "(ticks > 5000)",
                                 metrics = h0metrics,
                                 variables = h0vars,
                                 constants = h0constants)

nlh0@simdesign <- simdesign_morris(nl = nlh0,
                                      morristype = "oat",
                                      morrislevels = 12,
                                      morrisr = 72,
                                      morrisgridjump = 6, 
                                      nseeds = 1)

#run NLRX experiment for H0

progressr::handlers("progress")
plan(multisession)
resultsGSh0 <- progressr::with_progress(run_nl_all(nlh0))
setsim(nlh0, "simoutput")<-resultsGSh0
analysisGSh0 <- analyze_nl(nlh0)
analysisGSh0 <- split(analysisGSh0, analysisGSh0$metric)
#####

#H0 processing and visualization
################
#basic pattern-matching histograms
hist(resultsGSh0$`mean-percent-time-moving`)
hist(resultsGSh0$`mean-distance-traveled` * 10 / (4300 / 24))


#find the area of gregariousness space that makes >90% grouped
hist(resultsGSh0$`percent-grouped`)
hist(resultsGSh0$`mean-group-size`)

#dcasts the data so that each parameter has a row with mu* and sigma
analysisGSh0scatter <- data.frame()
for (df in analysisGSh0) {
  analysisGSh0scatter <- rbind(analysisGSh0scatter, (musigma_processing(as.data.frame(df))))
}

#makes a mu*-sigma plot for each of the metrics generated by the MEE
metrics <- unique(analysisGSh0scatter$metric)

for (metric in metrics) {
  print(ggplot(analysisGSh0scatter[analysisGSh0scatter$metric == metric,], aes(x = mustar, y = sigma, label = parameter)) +
          geom_point() + geom_label_repel(size = 3) +
          labs(title = paste0("MEE, Group Size H0 ", metric)))
}


##heatmaps based on strongest params
a<-ggplot(resultsGSh0, aes(x = `tgt-neighbor`, y = `other-primate-detection-radius`, fill = `percent-grouped`)) +
  geom_tile() +  #scale_y_discrete(labels = tgtdist.scale, breaks = everyother) +
  #scale_x_discrete(breaks = tgtneighbor.breaks)+
  labs(title = "gregariousness and grouping", fill = "% grouped") +
  xlab("target neighbors")+
  ylab("other primate detection radius") +  

  heatmaptheme  + 
  scale_fill_distiller(palette = "YlGn", direction = 1) 


b<-ggplot(resultsGSh0, aes(x = `tgt-dist`, y = `other-primate-detection-radius`, fill = `percent-grouped`)) +
  geom_tile() +  #scale_y_discrete(labels = tgtdist.scale, breaks = everyother) +
  #scale_x_discrete(breaks = tgtneighbor.breaks)+
  labs(title = "gregariousness and grouping", fill = "% grouped") +
  xlab("target distance")+
  ylab("other primate detection radius") +  
  
  heatmaptheme  + 
  scale_fill_distiller(palette = "YlGn", direction = 1) 

c<-ggplot(resultsGSh0, aes(x = `tgt-neighbor`, y = `tgt-dist`, fill = `percent-grouped`)) +
  geom_tile() +  #scale_y_discrete(labels = tgtdist.scale, breaks = everyother) +
  #scale_x_discrete(breaks = tgtneighbor.breaks)+
  labs(title = "gregariousness and grouping", fill = "% grouped") +
  xlab("target neighbors")+
  ylab("target distance") +  
  
  heatmaptheme  + 
  scale_fill_distiller(palette = "YlGn", direction = 1) 

multiplot(a, b, c, cols=1)

d<-ggplot(resultsGSh0, aes(x = `qual-mean`, y = `clump-size`, fill = `percent-grouped`)) +
  geom_tile() +  #scale_y_discrete(labels = tgtdist.scale, breaks = everyother) +
  #scale_x_discrete(breaks = tgtneighbor.breaks)+
  labs(title = "gregariousness and grouping", fill = "% grouped") +
  xlab("patch quality")+
  ylab("clump size") +  
  
  heatmaptheme  + 
  scale_fill_distiller(palette = "YlGn", direction = 1) 
d
#####

#refining the pattern-matching after another MEE round - 2/3/23
####################

#patch-regrowth-interval
ggplot(resultsGS, aes(x = `patch-regrowth-interval`, y = `mean-distance-traveled`* 10 / (4300 / 24))) +
  geom_jitter()

ggplot(resultsGS, aes(x = `patch-regrowth-interval`, y = `mean-percent-time-moving`)) +
  geom_jitter()

#extraction-rate-mean
ggplot(resultsGS, aes(x = `extraction-rate-mean`, y = `mean-distance-traveled`* 10 / (4300 / 24))) +
  geom_jitter()

ggplot(resultsGS, aes(x = `extraction-rate-mean`, y = `mean-percent-time-moving`)) +
  geom_jitter()
ggplot(resultsGSh0, aes(x = `extraction-rate-mean`, y = `mean-percent-time-moving`)) +
  geom_jitter()

ggplot(resultsGS, aes(x = `extraction-rate-mean`, y = `patch-regrowth-interval`, fill = `mean-distance-traveled`* 10 / (4300 / 24))) +
  geom_tile() +  #scale_y_discrete(labels = tgtdist.scale, breaks = everyother) +
  #scale_x_discrete(breaks = tgtneighbor.breaks)+
  labs(title = "", fill = "distance") +
  xlab("extraction rate")+
  ylab("regrowth interval") +  
  heatmaptheme  + 
  scale_fill_distiller(palette = "YlGn", direction = 1) 

ggplot(resultsGS, aes(x = `extraction-rate-mean`, y = `patch-regrowth-interval`, fill = `mean-percent-time-moving`)) +
  geom_tile() +  #scale_y_discrete(labels = tgtdist.scale, breaks = everyother) +
  #scale_x_discrete(breaks = tgtneighbor.breaks)+
  labs(title = "", fill = "% moving") +
  xlab("extraction rate")+
  ylab("regrowth interval") +  
  heatmaptheme  + 
  scale_fill_distiller(palette = "YlGn", direction = 1) 




#max-move
ggplot(resultsGS, aes(x = `max-move`, y = `mean-distance-traveled`* 10 / (4300 / 24))) +
  geom_jitter()

######

#group size hypothesis test - third attempt, setup and run
####################
GSmetrics <- c(
  "mean-group-size",
  "var-group-size",
  "percent-grouped", 
  "foraging-efficiency-time", 
  "foraging-efficiency-dist", 
  "mean-distance-traveled",
  "mean-percent-time-moving",
  "var-energy-intake-monthly",
  "mean-inter-indiv-dist")

GSconstants <- list("group-recog-module?" = "true",
                    "go-tests-on?" = "false",
                    "resource-tests-on?" = "false",
                    "move-tests-on?" = "false") 

GSvars <- list(
  "abundance" = list(min = 200000, max = 1200000, qfun = "qunif"),
  "clump-size" = list(min = 1, max = 500, qfun = "qunif"), 
  "energy-per-capita" = list(min = 4000, max = 9000, qfun = "qunif"),
  "qual-mean" = list(min = 25, max = 150, qfun = "qunif"),
  "qual-sd" = list(min = 1, max = 20, qfun = "qunif"),
  "extraction-rate-mean" = list(min = 2, max = 8, qfun = "qunif"),
  "extraction-rate-sd" = list(min = 1, max = 5, qfun = "qunif"),
  
  "tgt-dist" = list(min = 1, max = 40, qfun = "qunif"),
  "tgt-neighbor" = list(min = 3, max = 11, qfun = "qunif"),
  
  "other-primate-detection-radius" = list(min = 70, max = 100, qfun = "qunif"),
  "resource-detection-radius" = list(min = 50, max = 100, qfun = "qunif"),
  "regrowth-rate" = list(min = 0.5, max = 1.0, qfun = "qunif"),
  "patch-regrowth-interval" = list(min = 500, max = 2500, qfun = "qunif"),
  "movement-noise" = list(min = 10, max = 45, qfun = "qunif"),
  "max-move" = list(min = 10, max = 30, qfun = "qunif"))

nlGS <- nl(nlversion = "6.2.2",
           nlpath = netlogopath,
           modelpath = modelpath,
           jvmmem = 12000)


nlGS@experiment <- experiment(expname = "groupsizeMEE",
                              outpath = outpath,
                              repetition = 1,
                              tickmetrics = "false",
                              idsetup = "setup",
                              idgo = "go",
                              runtime = 4300,
                              #stopcond = "(ticks > 5000)",
                              metrics = GSmetrics,
                              variables = GSvars,
                              constants = GSconstants)

nlGS@simdesign <- simdesign_morris(nl = nlGS,
                                   morristype = "oat",
                                   morrislevels = 12,
                                   morrisr = 72,
                                   morrisgridjump = 6, 
                                   nseeds = 1)

#run NLRX experiment 

progressr::handlers("progress")
plan(multisession)
resultsGS <- progressr::with_progress(run_nl_all(nlGS))
setsim(nlGS, "simoutput")<-resultsGS
analysisGS <- analyze_nl(nlGS)
analysisGS <- split(analysisGS, analysisGS$metric)
#####

#group size hypothesis test processing and visualization
#########################

#verify basic pattern-matching thru histograms
hist(resultsGS$`mean-distance-traveled`* 10 / (4300 / 24))
hist(resultsGS$`mean-percent-time-moving`)
hist(resultsGS$`percent-grouped`)



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




#activity budget, comparison Kamilar & Cooper data and model output
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





hist(resultsGS$`mean-group-size`)



#GS figure 3A
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





mean(resultsGS$`mean-group-size`)
median(resultsGS$`mean-group-size`)

#dcasts the data so that each parameter has a row with mu* and sigma
analysisGSscatter <- data.frame()
for (df in analysisGS) {
  analysisGSscatter <- rbind(analysisGSscatter, (musigma_processing(as.data.frame(df))))
}

#makes a mu*-sigma plot for each of the metrics generated by the MEE
metrics <- unique(analysisGSscatter$metric)
for (metric in metrics) {
  print(ggplot(analysisGSscatter[analysisGSscatter$metric == metric,], aes(x = mustar, y = sigma, label = parameter)) +
          geom_point() + geom_label_repel(size = 3) +
          labs(title = paste0("MEE, Group Size ", metric)))
}

#prelim heatmaps based on strongest params
#the first things to examine are mean group size 

#first, better mu* x sigma plot
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

ggplot(meanGSMEEdata, aes(x = mustar, y = sigma, label = parameter)) +
  geom_point() + geom_text_repel(aes(label=ifelse(sigma>20, as.character(readable.params),number)), size = 5) +
  labs(title = "Sensitivity analysis, Mean group size") +
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  scale_x_continuous(expand = c(0.1,0.1))+
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 14, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )


#according to attempt #3, most influential are:
#tgt-neighbor
#tgt-dist (mu*)
#energy-per-capita
#clump-size
#abundance

a <- ggplot(resultsGS, aes(x = `tgt-neighbor`, y = `tgt-dist`, fill = log(`mean-group-size`))) +
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

b <- ggplot(resultsGS, aes(x = `tgt-neighbor`, y = `energy-per-capita`, fill = log(`mean-group-size`))) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "tgt-neighbors & energy per cap", fill = "log group size") +
  xlab("target neighbor")+
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

c <- ggplot(resultsGS, aes(x = `tgt-neighbor`, y = `clump-size`, fill = log(`mean-group-size`))) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "tgt-neighbors & clump", fill = "log group size") +
  xlab("target neighbor")+
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



d <- ggplot(resultsGS, aes(x = `tgt-neighbor`, y = `abundance`, fill = log(`mean-group-size`))) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "tgt-neighbors & abundance", fill = "log group size") +
  xlab("target neighbor")+
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




#WRONGFigure 3 b, c, d
groupsizebreaks <- c(2, 5, 10, 25, 50, 100)
b <- ggplot(resultsGS, aes(x = `abundance`/1000, y = `energy-per-capita`, fill = `mean-group-size`)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1, trans = "log", breaks = groupsizebreaks) +
  labs(title = "b.", fill = "group\nsize") +
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

c <- ggplot(resultsGS, aes(x = `clump-size`, y = `energy-per-capita`, fill = `mean-group-size`)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1, trans = "log", breaks = groupsizebreaks) +
  labs(title = "c.", fill = "group\nsize") +
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

d <- ggplot(resultsGS, aes(x = `clump-size`, y = `abundance`/1000, fill = `mean-group-size`)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1, trans = "log", breaks = groupsizebreaks) +
  labs(title = "d.", fill = "group\nsize") +
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

multiplot(a, b, c, d, e, f, g, cols = 2)
#multiplot(f, g, h, i, j, cols = 2)

#########

#foraging efficiency, so that we can control for everything except group size
######
foreffMEEdata <- analysisGSscatter[analysisGSscatter$metric == "foraging-efficiency-dist_mean",]
foreffMEEdata$number <- 1:15
foreffMEEdata$readable.params <- c("abundance", 
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

ggplot(foreffMEEdata, aes(x = mustar, y = sigma, label = parameter)) +
  geom_point() + geom_text_repel(aes(label=ifelse(mustar>0.4, as.character(readable.params),number)), size = 5) +
  labs(title = "Sensitivity analysis, foraging efficiency") +
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  scale_x_continuous(expand = c(0.1,0.1))+
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 14, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

ggplot(resultsGS, aes(x = (`mean-group-size`), y = `foraging-efficiency-dist`)) +
  geom_point()

ggplot(resultsGS, aes(x = log(`mean-group-size`), y = `foraging-efficiency-dist`, color = `max-move`)) +
  geom_point(size = 0.5)

ggplot(resultsGS, aes(x = log(`mean-group-size`), y = log(`foraging-efficiency-dist`), color = `max-move`)) +
  geom_point(size = 0.5)

ggplot(resultsGS, aes(x = log(`mean-group-size`), y = `mean-distance-traveled`, color = `max-move`)) +
  geom_point(size = 0.5)

ggplot(resultsGS, aes(x = log(`mean-group-size`), y = `mean-inter-indiv-dist`, color = `max-move`)) +
  geom_point(size = 0.5)


a<- ggplot(resultsGS, aes(x = `patch-regrowth-interval`, y = `max-move`, fill = log(`foraging-efficiency-dist`))) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "regrowth and speed", fill = "foraging eff") +
  xlab("regrowth interval") +   
  ylab("movement speed")+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

b<- ggplot(resultsGS, aes(x = `qual-mean`, y = `max-move`, fill = log(`foraging-efficiency-dist`))) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "patch qual and speed", fill = "foraging eff") +
  xlab("patch quality")+
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

c<- ggplot(resultsGS, aes(x = `clump-size`, y = `max-move`, fill = log(`foraging-efficiency-dist`))) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "clump and movement speed", fill = "foraging eff") +
  xlab("clump size")+
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

d<- ggplot(resultsGS, aes(x = `patch-regrowth-interval`, y = `qual-mean`, fill = log(`foraging-efficiency-dist`))) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "regrowth and patch qual", fill = "foraging eff") +
  xlab("regrowth interval")+
  ylab("patch qual") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

e<- ggplot(resultsGS, aes(x = `patch-regrowth-interval`, y = `clump-size`, fill = log(`foraging-efficiency-dist`))) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "regrowth and clump", fill = "foraging eff") +
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

f<- ggplot(resultsGS, aes(x = `qual-mean`, y = `clump-size`, fill = log(`foraging-efficiency-dist`))) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "patch qual and clump", fill = "foraging eff") +
  xlab("patch quality")+
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

multiplot(a, b, c, d, e, f, cols = 2)

#########

#intake
##############
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

ggplot(intakeMEEdata, aes(x = mustar, y = sigma, label = parameter)) +
  geom_point() + geom_text_repel(aes(label=ifelse(mustar>0.4, as.character(readable.params),number)), size = 5) +
  labs(title = "Sensitivity analysis, energy intake") +
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  scale_x_continuous(expand = c(0.1,0.1))+
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 14, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )


################

#pop-level group size and foraging eff factorial exp results (heatmaps)
##############
setwd("C:/Users/Marcy/Desktop/group size heatmap analysis")
earlyfebfiles <- list.files(path = "/Users/Marcy/Desktop/group size heatmap analysis", pattern = "*.csv")
pop.level <- lapply(earlyfebfiles, read.delim, skip = 6, header = TRUE, sep = ",")

#pop.level does not indicate the experiment - but earlyfebfiles can be used to index

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




#first, group size 


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
multiplot(e, f, g)

#oldFigure 3 b, c, d
groupsizebreaks <- c(2, 5, 10, 25, 50, 100)
b <- ggplot(energypercap_abundance, aes(x = abundance/1000, y = energy.per.capita, fill = mean.group.size)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1, trans = "log", breaks = groupsizebreaks) +
  labs(title = "b.", fill = "group\nsize") +
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

c <- ggplot(clump_energypercap, aes(x = clump.size, y = energy.per.capita, fill = mean.group.size)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1, trans = "log", breaks = groupsizebreaks) +
  labs(title = "c.", fill = "group\nsize") +
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

d <- ggplot(clump_abundance, aes(x = clump.size, y = abundance/1000, fill = mean.group.size)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1, trans = "log", breaks = groupsizebreaks) +
  labs(title = "d.", fill = "group\nsize") +
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


#NEW Figure 3 b, c, d
setwd("C:/Users/Marcy/Desktop/group size heatmap analysis/extra reps")
earlymarfiles <- list.files(path = "/Users/Marcy/Desktop/Marcy dissertatin stuff/group size heatmap analysis/extra reps", pattern = "*.csv")
GSheatmapdata <- lapply(earlymarfiles, read.delim, skip = 6, header = TRUE, sep = ",")

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



#foraging efficiency

a <- ggplot(regrowth_maxmove, aes(x = patch.regrowth.interval, y = max.move, fill = foraging.efficiency.dist)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "", fill = "foraging eff") +
  xlab("regrowth interval")+
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



b <- ggplot(qualmean_maxmove, aes(x = qual.mean, y = max.move, fill = foraging.efficiency.dist)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "", fill = "foraging eff") +
  xlab("patch quality")+
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


c <- ggplot(clump_maxmove, aes(x = clump.size, y = max.move, fill = foraging.efficiency.dist)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "", fill = "foraging eff") +
  xlab("clump size")+
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



d <- ggplot(regrowth_qualmean, aes(x = patch.regrowth.interval, y = qual.mean, fill = foraging.efficiency.dist)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "", fill = "foraging eff") +
  xlab("regrowth interval")+
  ylab("patch quality") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )


e <- ggplot(regrowth_clump, aes(x = clump.size, y = patch.regrowth.interval, fill = foraging.efficiency.dist)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "", fill = "foraging eff") +
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


f <- ggplot(qualmean_clump, aes(x = clump.size, y = qual.mean, fill = foraging.efficiency.dist)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "", fill = "foraging eff") +
  xlab("clump size")+
  ylab("patch quality") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )



multiplot(a, b, d, c, e, f, cols = 2)








##########

#group-level group size and foraging eff factorial exp results
##########
setwd("C:/Users/Marcy/Desktop/groupsizegrouplevel")

files <- list.files(path = "/", pattern = "*.csv")
group.level <- lapply(files, read.delim, skip = 6, header = TRUE, sep = ",")


######

#foraging efficiency ~ group size, p1.2 and p1.3 analysis first attempt
######################
setwd("C:/Users/Marcy/Desktop/groupsizep1-2")
p1.2data <- read.delim("basemodelprimatesocialgroups group size p1-2 p1-3-table.csv", skip = 6, header = TRUE, sep = ",")

ggplot(p1.2data, aes(mean.group.size)) +
  geom_histogram(bins = 20) +
  scale_x_log10()
mean(p1.2data$mean.group.size)
median(p1.2data$mean.group.size)

ggplot(p1.2data, aes(x=clump.size, y=mean.group.size)) +
  geom_point()

ggplot(p1.2data, aes(x=clump.size, y=foraging.efficiency.dist)) +
  geom_point()


ggplot(p1.2data, aes(x=mean.group.size, y=foraging.efficiency.dist, color = clump.size)) +
  geom_point() +
  scale_x_log10()



setwd("C:/Users/Marcy/Desktop/groupsizep1-3/first attempt")
p1.3filesattempt1 <- list.files(pattern = "*.csv")
 lapply(p1.3filesattempt1, read.csv, header = TRUE, sep = ",")
p1.3dataattempt1 <- do.call(rbind, lapply(p1.3filesattempt1, function(x) read.csv(x, stringsAsFactors = FALSE)))

p1.3dataattempt1$foraging.efficiency.dist <- p1.3dataattempt1$group.mean.intake / p1.3dataattempt1$group.mean.distance.travelled

ggplot(p1.3dataattempt1, aes(x=clump, y=group.size)) +
  geom_point()

ggplot(p1.3dataattempt1, aes(x=clump, y=foraging.efficiency.dist)) +
  geom_point()

ggplot(p1.3dataattempt1, aes(x=group.size, y=foraging.efficiency.dist, color = clump)) +
  geom_point() +
  scale_x_log10()



######

#foraging efficiency ~ group size p1.2 and 0p1.3 analysis second attempt
##############
setwd("C:/Users/Marcy/Desktop/Marcy dissertatin stuff/groupsizep1-2")
p1.2data <- read.delim("basemodelprimatesocialgroups group size p1-2 p1-3 2nd attempt-table.csv", skip = 6, header = TRUE, sep = ",")
p1.2data63 <- read.delim("basemodelprimatesocialgroups group size p1-2 clump 63-table.csv", skip = 6, header = TRUE, sep = ",")
p1.2data63$slopes <- rep(0, 625)
p1.2data63$z <- rep(0, 625)
p1.2data63$distslopes <- rep(0, 625)

p1.2data <- rbind(p1.2data, p1.2data63)


p1.2data <- p1.2data[p1.2data$mean.group.size>=2,]

ggplot(p1.2data, aes(mean.group.size)) +
  geom_histogram(bins = 20) +
  scale_x_log10()
mean(p1.2data$mean.group.size)
median(p1.2data$mean.group.size)

ggplot(p1.2data, aes(x=clump.size, y=mean.group.size)) +
  geom_point()

ggplot(p1.2data, aes(x=clump.size, y=foraging.efficiency.dist)) +
  geom_point()


ggplot(p1.2data, aes(x=mean.group.size, y=foraging.efficiency.dist, color = clump.size)) +
  geom_point() +
  scale_x_log10()

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



# export at 1200 x 260 pixels
a0 <- ggplot(p1.2data, aes(x=mean.group.size, y=foraging.efficiency.dist)) +
  labs(title = "A. all clump sizes") +
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ ylim(0, 15) +
  ylab("foraging efficiency") +
  xlab("population mean group size") +
  geom_smooth(method = "lm", color = "black", se = FALSE)+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +
  stat_regline_equation(label.y = 14, label.x = 1.2, aes(label = after_stat(eq.label))) 

a <- ggplot(bigexp_smallclump, aes(x=mean.group.size, y=foraging.efficiency.dist)) +
  labs(title = "B. clump size = 1") + 
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ ylim(0, 15) +
  ylab("foraging efficiency") +
  xlab("population mean group size") +
  geom_smooth(method = "lm", color = "black", se = FALSE)+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +
  stat_regline_equation(label.y = 14, label.x = 1.2, aes(label = after_stat(eq.label))) 

aa <- ggplot(p1.2data[p1.2data$clump.size==63,], aes(x=mean.group.size, y=foraging.efficiency.dist)) +
  labs(title = "C. clump size = 63") +
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ ylim(0, 15) +
  ylab("foraging efficiency") +
  xlab("population mean group size") +
  geom_smooth(method = "lm", color = "black", se = FALSE)+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +
  stat_regline_equation(label.y = 14, label.x = 1.2, aes(label = after_stat(eq.label))) 

b <- ggplot(bigexp_medsmallclump, aes(x=mean.group.size, y=foraging.efficiency.dist)) +
  labs(title = "D. clump size = 126") + 
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ ylim(0, 15) +
  ylab("foraging efficiency") +
  xlab("population mean group size") +
  geom_smooth(method = "lm", color = "black", se = FALSE)+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +
  stat_regline_equation(label.y = 14, label.x = 1.2, aes(label = after_stat(eq.label))) 
  

c <- ggplot(p1.2data[p1.2data$clump.size==251,], aes(x=mean.group.size, y=foraging.efficiency.dist)) +
  labs(title = "E. clump size = 251") + 
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ ylim(0, 15) +
  ylab("foraging efficiency") +
  xlab("population mean group size") +
  geom_smooth(method = "lm", color = "black", se = FALSE)+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +
  stat_regline_equation(label.y = 14, label.x = 1.2, aes(label = after_stat(eq.label))) 

 
d <- ggplot(p1.2data[p1.2data$clump.size==376,], aes(x=mean.group.size, y=foraging.efficiency.dist)) +
  geom_point(size = 0.5) +
  labs(title = "F. clump size = 376") +
  scale_x_log10(limits=c(2, 150))+ ylim(0, 15) +
  ylab("foraging efficiency") +
  xlab("population mean group size")+
  geom_smooth(method = "lm", color = "black", se = FALSE)+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),

    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )


e <- ggplot(p1.2data[p1.2data$clump.size==501,], aes(x=mean.group.size, y=foraging.efficiency.dist)) +
  labs(title = "G. clump size = 501") +
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ ylim(0, 15) +
  ylab("foraging efficiency") +
  xlab("population mean group size") +
  geom_smooth(method = "lm", color = "black", se = FALSE)+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +
  stat_regline_equation(label.y = 14, label.x = 1.2, aes(label = after_stat(eq.label)))  



multiplot(a0, a, aa, b, c, d, e, cols = 2)

p1.2data.0.rmv <- p1.2data
p1.2data.0.rmv[p1.2data.0.rmv$mean.group.size==0,] <- NA
p1.2data.0.rmv <- p1.2data.0.rmv[complete.cases(p1.2data.0.rmv),]

p1.2lm <- lm(foraging.efficiency.dist~log10(mean.group.size), data = p1.2data.0.rmv)
summary(p1.2lm)




#alt fig 4

a <- ggplot(p1.2data, aes(x=mean.group.size, y=foraging.efficiency.dist)) +
  labs(title = "A. all clump sizes") +
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ ylim(0, 15) +
  ylab("foraging efficiency") +
  xlab("population mean group size") +
  geom_smooth(method = "lm", color = "black", se = FALSE)+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +
  stat_regline_equation(label.y = 14, label.x = 1.2, aes(label = after_stat(eq.label))) 

b <- ggplot(p1.2data[p1.2data$clump.size==1,], aes(x=mean.group.size, y=foraging.efficiency.dist)) +
  labs(title = "B. clump size = 1") +
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ ylim(0, 15) +
  ylab("foraging efficiency") +
  xlab("population mean group size") +
  geom_smooth(method = "lm", color = "black", se = FALSE)+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +
  stat_regline_equation(label.y = 14, label.x = 1.2, aes(label = after_stat(eq.label))) 

c <- ggplot(rbind(bigexp_medsmallclump), aes(x=mean.group.size, y=foraging.efficiency.dist)) +
  labs(title = "c. clump size = 126") +
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ ylim(0, 15) +
  ylab("foraging efficiency") +
  xlab("population mean group size") +
  geom_smooth(method = "lm", color = "black", se = FALSE)+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +
  stat_regline_equation(label.y = 14, label.x = 1.2, aes(label = after_stat(eq.label))) 

d <- ggplot(bigexp_bigbigclump, aes(x=mean.group.size, y=foraging.efficiency.dist)) +
  labs(title = "d. clump size = 501") +
  geom_point(size = 0.5, color = "gray25") +
  scale_x_log10(limits=c(2, 150))+ ylim(0, 15) +
  ylab("foraging efficiency") +
  xlab("population mean group size") +
  geom_smooth(method = "lm", color = "black", se = FALSE)+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"), panel.background = element_rect(fill = "white", color = "gray50"),
  ) +
  stat_regline_equation(label.y = 14, label.x = 1.2, aes(label = after_stat(eq.label))) 


multiplot(a, b, c, d, cols = 4)






#fig 4 with energy intake 

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









setwd("C:/Users/Marcy/Desktop/groupsizep1-3/")
p1.3filesattempt2 <- list.files(pattern = "*.csv")
#lapply(p1.3filesattempt2, read.csv, header = TRUE, sep = ",")
p1.3dataattempt2 <- do.call(rbind, lapply(p1.3filesattempt2, function(x) read.csv(x, stringsAsFactors = FALSE)))

p1.3dataattempt2$foraging.efficiency.dist <- p1.3dataattempt2$group.mean.intake / p1.3dataattempt2$group.mean.distance.travelled


ggplot(p1.3dataattempt2, aes(x=clump, y=group.size)) +
  geom_point()

ggplot(p1.3dataattempt2, aes(x=clump, y=foraging.efficiency.dist)) +
  geom_point()

ggplot(p1.3dataattempt2, aes(x=group.size, y=foraging.efficiency.dist, color = clump)) +
  geom_point() +
  scale_x_log10()



a <- ggplot(p1.3dataattempt2[p1.3dataattempt2$clump==1,], aes(x=group.size, y=foraging.efficiency.dist)) +
  geom_point(size = 0.5) +
  labs(title = "a. clump size = 1") + 
  scale_x_log10(limits=c(2, 150))+ ylim(0, 15) +
  ylab("foraging efficiency") +
  xlab("group size")

b <- ggplot(p1.3dataattempt2[p1.3dataattempt2$clump==126,], aes(x=group.size, y=foraging.efficiency.dist)) +
  geom_point(size = 0.5) +
  labs(title = "b. clump size = 126") + 
  scale_x_log10(limits=c(2, 150))+ ylim(0, 15) +
  ylab("foraging efficiency") +
  xlab("group size")

c <- ggplot(p1.3dataattempt2[p1.3dataattempt2$clump==251,], aes(x=group.size, y=foraging.efficiency.dist)) +
  geom_point(size = 0.5) +
  labs(title = "c. clump size = 251") + 
  scale_x_log10(limits=c(2, 150))+ ylim(0, 15) +
  ylab("foraging efficiency") +
  xlab("group size")
d <- ggplot(p1.3dataattempt2[p1.3dataattempt2$clump==376,], aes(x=group.size, y=foraging.efficiency.dist)) +
  geom_point(size = 0.5) +
  labs(title = "d. clump size = 376") + 
  scale_x_log10(limits=c(2, 150))+ ylim(0, 15) +
  ylab("foraging efficiency") +
  xlab("group size")

e <- ggplot(p1.3dataattempt2[p1.3dataattempt2$clump==501,], aes(x=group.size, y=foraging.efficiency.dist)) +
  geom_point(size = 0.5) +
  labs(title = "e. clump size = 501") + 
  scale_x_log10(limits=c(2, 150))+ ylim(0, 15) +
  ylab("foraging efficiency") +
  xlab("group size")
multiplot(a, b, c, d, e, cols = 5)



p1.3highforeffresults <- p1.3dataattempt2[p1.3dataattempt2$foraging.efficiency.dist>10,]

mean(p1.3highforeffresults$clump)
mean(p1.3highforeffresults$tgt.neighbor)
mean(p1.3highforeffresults$tgt.dist)
mean(p1.3highforeffresults$abundance) #appears low
mean(p1.3highforeffresults$energy.per.capita)

ggplot(p1.3highforeffresults, aes(x=group.size, y=foraging.efficiency.dist)) +
  geom_point(size = 0.5) +
  labs(title = "results above 10") + 
  scale_x_log10() +
  ylab("foraging efficiency") +
  xlab("group size")



p1.3lowA <- p1.3dataattempt2[p1.3dataattempt2$abundance<450000,]
ggplot(p1.3lowA, aes(x=group.size, y=foraging.efficiency.dist, color = tgt.dist)) +
  geom_point() +
  scale_x_log10()


# it's not just the resource configuration that is producing groups with very high foraging
# efficiency; could it be that these groups are in populations consisting of mostly larger gorups,
# or something like that?

#get the population data for groups in this subset

subset <- p1.2data[p1.2data$X.run.number. %in% unique(p1.3highforeffresults$run.number),]
hist(subset$mean.group.size)
hist(subset$var.group.size)
hist(subset$mean.distance.traveled/17.9)
hist(p1.2data$mean.distance.traveled/17.9)

a <- ggplot(p1.2data, aes(mean.group.size)) + geom_histogram() + xlim(0,50)
b <- ggplot(subset, aes(mean.group.size)) + geom_histogram()+ xlim(0,50)
c <- ggplot(p1.2data, aes(var.group.size)) + geom_histogram()+ xlim(0,500)
d <- ggplot(subset, aes(var.group.size)) + geom_histogram()+ xlim(0,500)

multiplot(a, b, c, d, cols = 2)

ggplot(p1.3dataattempt2, aes(group.size)) +geom_histogram()+
  scale_x_log10()
max(p1.3dataattempt2$group.size)
length(p1.3dataattempt2[p1.3dataattempt2$group.size > 5,])

length(p1.3dataattempt2$group.size)

#how many of all the groups were larger than...
length(which(p1.3dataattempt2$group.size > 10)) / length(p1.3dataattempt2$group.size)
length(which(p1.3dataattempt2$group.size > 30)) / length(p1.3dataattempt2$group.size)
length(which(p1.3dataattempt2$group.size > 50)) / length(p1.3dataattempt2$group.size)
length(which(p1.3dataattempt2$group.size > 100)) / length(p1.3dataattempt2$group.size)



#how many of the populations had groups larger than...



#making regression lines for each population
p1.3split <- split(p1.3dataattempt2, p1.3dataattempt2$run.number)

slopes <- c()
for (df in p1.3split) {
  poplm <- lm(foraging.efficiency.dist~group.size, data = df)
  slopes <- c(slopes, poplm$coefficients[2])
}

p1.2data$slopes <- slopes[p1.2data$X.run.number.]

ggplot() + geom_histogram(p1.2data, mapping = aes(slopes))

mean(p1.2data$slopes, na.rm = TRUE)
mean(subset$slopes, na.rm = TRUE)

p1.2data$z <- scale(p1.2data$slopes)
ggplot(p1.2data, aes(z)) + geom_histogram()

a <- ggplot(p1.2data[p1.2data$clump.size==1,], aes(slopes)) +
  geom_histogram() +
  labs(title = "a. clump size = 1")+
  xlim(-5, 5) + ylim(0, 601)
b <- ggplot(p1.2data[p1.2data$clump.size==126,], aes(slopes)) +
  geom_histogram() +
  labs(title = "b. clump size = 126")+
  xlim(-5, 5) + ylim(0, 601)
c <- ggplot(p1.2data[p1.2data$clump.size==251,], aes(slopes)) +
  geom_histogram() +
  labs(title = "c. clump size = 251")+
  xlim(-5, 5) + ylim(0, 601)
d <- ggplot(p1.2data[p1.2data$clump.size==376,], aes(slopes)) +
  geom_histogram() +
  labs(title = "d. clump size = 376")+
  xlim(-5, 5) + ylim(0, 601)
e <- ggplot(p1.2data[p1.2data$clump.size==501,], aes(slopes)) +
  geom_histogram() +
  labs(title = "e. clump size = 501")+
  xlim(-5, 5) + ylim(0, 601)
multiplot(a, b, d, d, e, cols=5)

subset2 <- p1.2data[p1.2data$clump.size==376,]
subset2 <- subset2[subset2$abundance < 400000,]
#subset2 <- p1.2data[p1.2data$energy.per.capita < 5000,]
ggplot(subset2, aes(slopes)) +
  geom_histogram() +
  labs(title = "clump size = 376 and low abundance")+
  xlim(-5, 5) + ylim(0, 601)

#############


#hypothesis 2

#daily distance traveled
#########
#first, better mu* x sigma plot
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

ggplot(distGSMEEdata, aes(x = mustar, y = sigma, label = parameter)) +
  geom_point() + geom_text_repel(aes(label=ifelse(mustar>10000, as.character(readable.params),number)), size = 5) +
  labs(title = "Sensitivity analysis, Distance traveled") +
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  scale_x_continuous(expand = c(0.1,0.1))+
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 14, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

#movement speed, extraction rate mean, clump size

a <- ggplot(resultsGS, aes(x = `max-move`, y = `extraction-rate-mean`, fill = `mean-distance-traveled`/17.9)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "a.", fill = "daily travel\ndistance") +
  xlab("movement speed")+
  ylab("extraction rate (mean)") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

b <- ggplot(resultsGS, aes(x = `max-move`, y = `clump-size`, fill = `mean-distance-traveled`/17.9)) +
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

c <- ggplot(resultsGS, aes(x = `clump-size`, y = `extraction-rate-mean`, fill = `mean-distance-traveled`/17.9)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "c.", fill = "daily travel\ndistance") +
  xlab("clump size")+
  ylab("extraction rate (mean)") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

multiplot(a, b, c)

#energy per capita, regrowth interval, target distance
d <- ggplot(resultsGS, aes(x = `clump-size`, y = `energy-per-capita`, fill = `mean-distance-traveled`/17.9)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "d.", fill = "daily travel\ndistance") +
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
e <- ggplot(resultsGS, aes(x = `clump-size`, y = `patch-regrowth-interval`, fill = `mean-distance-traveled`/17.9)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "e.", fill = "daily travel\ndistance") +
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
f <- ggplot(resultsGS, aes(x = `energy-per-capita`, y = `extraction-rate-mean`, fill = `mean-distance-traveled`/17.9)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "f.", fill = "daily travel\ndistance") +
  xlab("energy per capita")+
  ylab("extraction rate (mean)") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )
g <- ggplot(resultsGS, aes(x = `patch-regrowth-interval`, y = `extraction-rate-mean`, fill = `mean-distance-traveled`/17.9)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "g.", fill = "daily travel\ndistance") +
  xlab("regrowth interval")+
  ylab("extraction rate (mean)") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

h <- ggplot(resultsGS, aes(x = `patch-regrowth-interval`, y = `energy-per-capita`, fill = `mean-distance-traveled`/17.9)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "h.", fill = "daily travel\ndistance") +
  xlab("regrowth interval")+
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

multiplot(d, e, f, g, cols = 2)




#factorial experiment results for daily travel distance

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
setwd("C:/Users/Marcy/Desktop/group size heatmap analysis")
extraction_maxmove <- read.delim("basemodelprimatesocialgroups group size extraction rate mean x speed-table.csv", skip = 6, header = TRUE, sep = ",")
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
extraction_clump <- read.delim("basemodelprimatesocialgroups group size extraction rate mean x clump size-table.csv", skip = 6, header = TRUE, sep = ",")
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
extraction_energypercap <- read.delim("basemodelprimatesocialgroups group size extraction rate mean x energy per capita-table.csv", skip = 6, header = TRUE, sep = ",")
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
extraction_regrowth <- read.delim("basemodelprimatesocialgroups group size extraction rate mean x regrowth-table.csv", skip = 6, header = TRUE, sep = ",")
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





hist(p1.2data$mean.distance.traveled / 17.9)

DDTlm <- lm(log10(mean.distance.traveled)~log10(mean.group.size), data = p1.2data.0.rmv)
summary(DDTlm)

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






#alt fig 5
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


distslopes <- c()
for (df in p1.3split) {
  poplm <- lm(group.mean.distance.travelled~group.size, data = df)
  distslopes <- c(distslopes, poplm$coefficients[2])
}

p1.2data$distslopes <- distslopes[p1.2data$X.run.number.]

a<-ggplot(p1.2data[p1.2data$clump.size == 1,], 
       aes(distslopes)) + 
  geom_histogram() +
  xlim(-5000, 5000) + ylim(0, 550)
b<-ggplot(p1.2data[p1.2data$clump.size == 126,], 
          aes(distslopes)) + 
  geom_histogram()+
  xlim(-5000, 5000) + ylim(0, 550)
c<-ggplot(p1.2data[p1.2data$clump.size == 251,], 
          aes(distslopes)) + 
  geom_histogram()+
  xlim(-5000, 5000) + ylim(0, 550)
d<-ggplot(p1.2data[p1.2data$clump.size == 376,], 
          aes(distslopes)) + 
  geom_histogram()+
  xlim(-5000, 5000) + ylim(0, 550)
e<-ggplot(p1.2data[p1.2data$clump.size == 501,], 
          aes(distslopes)) + 
  geom_histogram()+
  xlim(-5000, 5000) + ylim(0, 550)
multiplot(a, b, c, d, e, cols = 5)


#####

#interindividual distance
############
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

ggplot(iidGSMEEdata, aes(x = mustar, y = sigma, label = parameter)) +
  geom_point() + geom_text_repel(aes(label=ifelse(mustar>30, as.character(readable.params),number)), size = 5) +
  labs(title = "Sensitivity analysis, Inter-Individual Distance") +
  xlab(paste("\u03BC", "*"))+
  ylab("\u03C3 ") +
  scale_x_continuous(expand = c(0.1,0.1))+
  theme(
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 14, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

#target distance, target neighbors, clump size, abundance, extraction rate mean, energy per capita
a <- ggplot(resultsGS, aes(x = `tgt-dist`, y = `tgt-neighbor`, fill = `mean-inter-indiv-dist`)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "a.", fill = "IID") +
  xlab("target distance")+
  ylab("target neighbors") +  
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

b <- ggplot(resultsGS, aes(x = `tgt-dist`, y = `clump-size`, fill = `mean-inter-indiv-dist`)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "b.", fill = "IID") +
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

c <- ggplot(resultsGS, aes(x = `tgt-neighbor`, y = `clump-size`, fill = `mean-inter-indiv-dist`)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "c.", fill = "IID") +
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

d <- ggplot(resultsGS, aes(x = `extraction-rate-mean`, y = `clump-size`, fill = `mean-inter-indiv-dist`)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "d.", fill = "IID") +
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

e <- ggplot(resultsGS, aes(x = `abundance`, y = `clump-size`, fill = `mean-inter-indiv-dist`)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "e.", fill = "IID") +
  xlab("abundance")+
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


f <- ggplot(resultsGS, aes(x = `abundance`, y = `extraction-rate-mean`, fill = `mean-inter-indiv-dist`)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "f.", fill = "IID") +
  xlab("abundance")+
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





multiplot(a, b, c, d, e, f, cols = 2)




#interindividual distance factorial heatmaps

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



tgtdist_clump <- read.delim("basemodelprimatesocialgroups group size target distance x clump size-table.csv", skip = 6, header = TRUE, sep = ",")
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

tgtdist_abundance <- read.delim("basemodelprimatesocialgroups group size target distance x abundance-table.csv", skip = 6, header = TRUE, sep = ",")
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
tgtdist_extraction <- read.delim("basemodelprimatesocialgroups group size target distance x extraction rate mean-table.csv", skip = 6, header = TRUE, sep = ",")
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





#prelim analysis for p2.6 using factorial exp

ggplot(tgtneighbor_tgtdist, aes(x = mean.group.size, y = foraging.efficiency.dist, color = mean.inter.indiv.dist)) +
  geom_point()

ggplot(tgtneighbor_tgtdist, aes(x = mean.group.size, y = foraging.efficiency.dist, color = tgt.dist)) +
  geom_point()


tgtneighbor_tgtdistv2 <- read.delim("basemodelprimatesocialgroups group size tgtneighbor x tgtdist-tablev2.csv", skip = 6, header = TRUE, sep = ",")

ggplot(tgtneighbor_tgtdistv2) + geom_point(aes(x = tgt.dist, y = mean.inter.indiv.dist))

shorttgtdist.lm <- lm(formula = foraging.efficiency.dist~mean.group.size, data = tgtneighbor_tgtdistv2[tgtneighbor_tgtdistv2$tgt.dist < 20,])
a<-ggplot(tgtneighbor_tgtdistv2[tgtneighbor_tgtdistv2$tgt.dist < 20,], aes(x = mean.group.size, y = foraging.efficiency.dist)) +
  geom_point(color = "gray25") +
  ylim(0, 3.5) + xlim(1, 35) +
  labs(title = "A. target distance < 20", x = "mean group size", y = "mean foraging efficiency")+
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
b<-ggplot(tgtneighbor_tgtdistv2[tgtneighbor_tgtdistv2$tgt.dist > 20,], aes(x = mean.group.size, y = foraging.efficiency.dist)) +
  geom_point(color = "gray25") +
  ylim(0, 3.5) + xlim(1, 35) +
  labs(title = "B. target distance > 20", x = "mean group size", y = "mean foraging efficiency")+
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



a<-ggplot(tgtneighbor_tgtdistv2[tgtneighbor_tgtdistv2$tgt.neighbor < 8,], aes(x = mean.group.size, y = foraging.efficiency.dist)) +
  geom_point() +
  ylim(0, 3.5) + xlim(1, 35) +
  labs(title = "a. target neighbors < 8")+
  stat_smooth(method = "lm")

b<-ggplot(tgtneighbor_tgtdistv2[tgtneighbor_tgtdistv2$tgt.neighbor > 7,], aes(x = mean.group.size, y = foraging.efficiency.dist)) +
  geom_point() +
  ylim(0, 3.5) + xlim(1, 35) +
  labs(title = "b. target neighbors >= 8")+
  stat_smooth(method = "lm")

multiplot(a, b, cols = 2)





setwd("C:/Users/Marcy/Desktop/groupsizegrouplevel/GStgtneighbortgtdist")
tgtneighbor_tgtdist_grouplevel_files <- list.files(pattern = "*.csv")
tgtneighbor_tgtdist_grouplevel <- do.call(rbind, lapply(tgtneighbor_tgtdist_grouplevel_files, function(x) read.csv(x, stringsAsFactors = FALSE)))

tgtneighbor_tgtdist_grouplevel$foraging.efficiency.dist <- tgtneighbor_tgtdist_grouplevel$group.mean.intake / tgtneighbor_tgtdist_grouplevel$group.mean.distance.travelled


a<-ggplot(tgtneighbor_tgtdist_grouplevel[tgtneighbor_tgtdist_grouplevel$tgt.dist < 20,], 
          aes(x = group.size, 
              y = foraging.efficiency.dist)) +
  geom_point() +
  ylim(0, 4) + scale_x_log10(limits = c(1, 160)) +
  labs(title = "a. target distance < 20")+
  stat_smooth(method = "lm")+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

b<-ggplot(tgtneighbor_tgtdist_grouplevel[tgtneighbor_tgtdist_grouplevel$tgt.dist > 20,], 
          aes(x = group.size, 
                                                 y = foraging.efficiency.dist)) +
  geom_point() +
  ylim(0, 4) + scale_x_log10(limits = c(1, 160)) +
  labs(title = "b. target distance > 20")+
  stat_smooth(method = "lm")+
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

multiplot(a, b, cols = 2)



a<-ggplot(tgtneighbor_tgtdist_grouplevel[tgtneighbor_tgtdist_grouplevel$tgt.neighbor < 8,], aes(x = group.size, y = foraging.efficiency.dist)) +
  geom_point() +
  ylim(0, 3.5) + xlim(1, 35) +
  labs(title = "a. target neighbors < 8")

b<-ggplot(tgtneighbor_tgtdist_grouplevel[tgtneighbor_tgtdist_grouplevel$tgt.neighbor > 7,], aes(x = group.size, y = foraging.efficiency.dist)) +
  geom_point() +
  ylim(0, 3.5) + xlim(1, 35) +
  labs(title = "b. target neighbors >= 8")

multiplot(a, b, cols = 2)


tgtneighbor_tgtdist_grouplevelsplit <- split(tgtneighbor_tgtdist_grouplevel, 
                                             tgtneighbor_tgtdist_grouplevel$run.number)

slopes <- c()
for (df in tgtneighbor_tgtdist_grouplevelsplit) {
  poplm <- lm(foraging.efficiency.dist~group.size, data = df)
  slopes <- c(slopes, poplm$coefficients[2])
}

tgtneighbor_tgtdistv2$slopes <- slopes[tgtneighbor_tgtdistv2$X.run.number.]

hist(tgtneighbor_tgtdistv2$slopes)

a<-ggplot(tgtneighbor_tgtdistv2[tgtneighbor_tgtdistv2$tgt.dist < 20,], 
          aes(x = slopes)) +
  geom_histogram() +
  labs(title = "a. target distance < 20")

b<-ggplot(tgtneighbor_tgtdistv2[tgtneighbor_tgtdistv2$tgt.dist > 20,], 
          aes(x = slopes)) +
  geom_histogram() +
  labs(title = "b. target distance > 20")

multiplot(a, b, cols = 2)



a<-ggplot(tgtneighbor_tgtdistv2[tgtneighbor_tgtdistv2$tgt.neighbor < 8,], 
          aes(x = slopes)) +
  geom_histogram() +
  labs(title = "a. target neighbors < 8")

b<-ggplot(tgtneighbor_tgtdistv2[tgtneighbor_tgtdistv2$tgt.neighbor > 7,], 
          aes(x = slopes)) +
  geom_histogram() +
  labs(title = "b. target neighbors >= 8")

multiplot(a, b, cols = 2)


###############




#var in group size
##############

a <- ggplot(resultsGS, aes(x = `abundance`, y = `clump-size`, fill = log(`var-group-size`))) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "behaviors", fill = "var group size") +
  xlab("abundance")+
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



###############


#April 2025
###########
setwd("/Users/me597/Documents/group_size_output")

files <- list.files(path = "/Users/me597/Documents/group_size_output", pattern = "*.csv")
group.level <- lapply(files, read.delim, header = TRUE, sep = ",")

groupLevelDF <- bind_rows(group.level, .id = "column_label")
groupLevelDF$clump <- as.factor(groupLevelDF$clump)


colvar <- groupLevelDF$abundance

refinedgrouplevelDF <- groupLevelDF[groupLevelDF$abundance==200000,]
refinedgrouplevelDF <- refinedgrouplevelDF[refinedgrouplevelDF$energy.per.capita ==4000,]


a <- ggplot(refinedgrouplevelDF[refinedgrouplevelDF$clump==1,], aes(x = log10(group.size), y = group.mean.weekly.intake))+
  geom_point(alpha = 0.5, size=0.2) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")  + 
  labs(title = "Clump size = 1")
b <- ggplot(refinedgrouplevelDF[refinedgrouplevelDF$clump==126,], aes(x = log10(group.size),y = group.mean.weekly.intake))+
  geom_point(alpha = 0.5, size=0.2)+ stat_smooth(method = "lm", 
                            formula = y ~ x, 
                            geom = "smooth")  + 
  labs(title = "Clump size = 126")

c <- ggplot(refinedgrouplevelDF[refinedgrouplevelDF$clump==251,], aes(x = log10(group.size),y = group.mean.weekly.intake))+
  geom_point(alpha = 0.5, size=0.2)+ 
  labs(title = "Clump size = 251")+ stat_smooth(method = "lm", 
                                                formula = y ~ x, 
                                                geom = "smooth")

d <- ggplot(refinedgrouplevelDF[refinedgrouplevelDF$clump==501,], aes(x = log10(group.size), y = group.mean.weekly.intake))+
  geom_point(alpha = 0.5, size=0.2)+ 
  labs(title = "Clump size = 501")+ stat_smooth(method = "lm", 
                                                formula = y ~ x, 
                                                geom = "smooth")

multiplot(a,b,c,d, cols=2)


a <- ggplot(refinedgrouplevelDF[refinedgrouplevelDF$clump==1,], aes(x = log10(group.size), y = group.mean.weekly.distance.travelled))+
  geom_point(alpha = 0.5, size=0.2) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")  + 
  labs(title = "Clump size = 1")
b <- ggplot(refinedgrouplevelDF[refinedgrouplevelDF$clump==126,], aes(x = log10(group.size),y = group.mean.weekly.distance.travelled))+
  geom_point(alpha = 0.5, size=0.2)+ stat_smooth(method = "lm", 
                                                 formula = y ~ x, 
                                                 geom = "smooth")  + 
  labs(title = "Clump size = 126")

c <- ggplot(refinedgrouplevelDF[refinedgrouplevelDF$clump==251,], aes(x = log10(group.size),y = group.mean.weekly.distance.travelled))+
  geom_point(alpha = 0.5, size=0.2)+ 
  labs(title = "Clump size = 251")+ stat_smooth(method = "lm", 
                                                formula = y ~ x, 
                                                geom = "smooth")

d <- ggplot(refinedgrouplevelDF[refinedgrouplevelDF$clump==501,], aes(x = log10(group.size), y = group.mean.weekly.distance.travelled))+
  geom_point(alpha = 0.5, size=0.2)+ 
  labs(title = "Clump size = 501")+ stat_smooth(method = "lm", 
                                                formula = y ~ x, 
                                                geom = "smooth")

multiplot(a,b,c,d, cols=2)



a<-ggplot(groupLevelDF[groupLevelDF$run.number==11,], aes(x = group.size, y = group.mean.weekly.distance.travelled))+
  geom_point() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")  + 
  labs(title = "Run Number 11")



b<-ggplot(groupLevelDF[groupLevelDF$run.number==12,], aes(x = group.size, y = group.mean.weekly.distance.travelled))+
  geom_point() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")  + 
  labs(title = "Run Number 12")



c<-ggplot(groupLevelDF[groupLevelDF$run.number==13,], aes(x = group.size, y = group.mean.weekly.distance.travelled))+
  geom_point() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")  + 
  labs(title = "Run Number 13")



d<-ggplot(groupLevelDF[groupLevelDF$run.number==14,], aes(x = group.size, y = group.mean.weekly.distance.travelled))+
  geom_point() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth")  + 
  labs(title = "Run Number 14")

multiplot(a,b,c,d, cols=2)



slopedf <- data.frame(matrix(ncol = 12, nrow = length(group.level)))
colnames(slopedf) <- c("run.number", "tgt.neighbor", "tgt.dist", "abundance", "energy.per.capita", "clump", "patch.size", "extraction", "patch.regrowth.interval", "max.move", "intercept", "slope")

for (i in 1:length(group.level)) {
  df <- group.level[i][[1]]
  looplm <- lm((group.mean.weekly.distance.travelled~group.size), data = df)
  
  
  slopedf[i,]<- c(df[1,]$run.number, df[1,]$tgt.neighbor, df[1,]$tgt.dist, df[1,]$abundance, df[1,]$energy.per.capita, df[1,]$clump, df[1,]$patch.size, df[1,]$extraction, df[1,]$patch.regrowth.interval, df[1,]$max.move,looplm$coefficients[1], looplm$coefficients[2])

}

plot(slopedf$energy.per.capita, slopedf$slope)






refinedSlopeDF <- slopedf[slopedf$abundance==200000,]

ggplot(refinedSlopeDF, aes(x = clump, y = intercept, color = energy.per.capita)) +
  geom_point()

ggplot(slopedf, aes(x = energy.per.capita, y = clump, fill = slope)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

ggplot(slopedf, aes(x = energy.per.capita, y = abundance, fill = slope)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

ggplot(slopedf, aes(x = abundance, y = clump, fill = slope)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

ggplot(slopedf, aes(x = tgt.dist, y = tgt.neighbor, fill = slope)) +
  geom_tile()+
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    
    panel.grid = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray75"),
    panel.grid.minor = element_line(color = "gray90"), panel.background = element_rect(fill = "white", color = "gray50"),
  )

############
