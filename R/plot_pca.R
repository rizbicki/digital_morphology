multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL,title=rep("",length(list(...)))) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]]+
              ggtitle(title[i])+
              theme(plot.title = element_text(hjust = 0)), 
            vp = viewport(layout.pos.row = matchidx$row,
                          layout.pos.col = matchidx$col))
    }
  }
}
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


library(grid)
library(cluster)
library(dplyr)
library(tidyr)
library(psych)
library(magrittr)
library(ggplot2)
library(scales)
library(ggforce)

repetitions=50
components=4

file_names=list.files(path="../Data_missing/")
file_names=grep("xls",file_names) %>% file_names[.]
file_names= ((file_names!="image-variable-check.xlsx")) %>% 
  as.logical() %>% which() %>%  file_names[.] 
file_names=(regexpr(".", file_names, fixed = TRUE) - 1) %>% stringr::str_sub(file_names,start = rep(1,length(file_names)),end =.)

read_datasets=gsub("-","",gsub("_","",file_names))


table=readRDS("../RDS/table_pca_noise_nomask.RDS")
table[,grep("comp",names(table))]=
  abs(table[,grep("comp",names(table))])
plot_data=matrix(0,length(file_names),2*(components))
plot_data=as.data.frame(plot_data)
for (i in 1:length(file_names))
{
  dataset=table[((i-1)*repetitions+1):(i*repetitions),c(1,grep("comp",names(table)))]
  plot_data[i,]=c(apply(dataset[,-1], 2, mean,na.rm=T),apply(dataset[,-1], 2, function(x) sd(x,na.rm=T)/sqrt(repetitions-sum(is.na(x)))))
}

names(plot_data)=c(paste(rep("mean comp",components),1:components),
                   paste(rep("error comp",components),1:components))

df <- data.frame(
  dataset = factor(rep(read_datasets,length(grep("mean",names(plot_data))))),
  mean = as.vector(as.matrix(plot_data[,grep("mean",names(plot_data))])),
  group= factor(rep(c(paste(rep("Component:",components),1:components)),each=length(file_names))),
  upper = as.vector(as.matrix(plot_data[,grep("mean",names(plot_data))]))+2*as.vector(as.matrix(plot_data[,grep("error",names(plot_data))])),
  lower = as.vector(as.matrix(plot_data[,grep("mean",names(plot_data))]))-2*as.vector(as.matrix(plot_data[,grep("error",names(plot_data))]))
)


table=readRDS("../RDS/table_pca_nonoise_mask.RDS")
table[,grep("comp",names(table))]=
  abs(table[,grep("comp",names(table))])
plot_data=matrix(0,length(file_names),2*(components))
plot_data=as.data.frame(plot_data)
for (i in 1:length(file_names))
{
  dataset=table[((i-1)*repetitions+1):(i*repetitions),c(1,grep("comp",names(table)))]
  plot_data[i,]=c(apply(dataset[,-1], 2, mean,na.rm=T),apply(dataset[,-1], 2, function(x) sd(x,na.rm=T)/sqrt(repetitions-sum(is.na(x)))))
}

names(plot_data)=c(paste(rep("mean comp",components),1:components),
                   paste(rep("error comp",components),1:components))

df2 <- data.frame(
  dataset = factor(rep(read_datasets,length(grep("mean",names(plot_data))))),
  mean = as.vector(as.matrix(plot_data[,grep("mean",names(plot_data))])),
  group= factor(rep(c(paste(rep("Component:",components),1:components)),each=length(file_names))),
  upper = as.vector(as.matrix(plot_data[,grep("mean",names(plot_data))]))+2*as.vector(as.matrix(plot_data[,grep("error",names(plot_data))])),
  lower = as.vector(as.matrix(plot_data[,grep("mean",names(plot_data))]))-2*as.vector(as.matrix(plot_data[,grep("error",names(plot_data))]))
)

dff=rbind(df,df2)

table=readRDS("../RDS/table_pca_noise_mask.RDS")
table[,grep("comp",names(table))]=
  abs(table[,grep("comp",names(table))])
plot_data=matrix(0,length(file_names),2*(components))
plot_data=as.data.frame(plot_data)
for (i in 1:length(file_names))
{
  dataset=table[((i-1)*repetitions+1):(i*repetitions),c(1,grep("comp",names(table)))]
  plot_data[i,]=c(apply(dataset[,-1], 2, mean,na.rm=T),apply(dataset[,-1], 2, function(x) sd(x,na.rm=T)/sqrt(repetitions-sum(is.na(x)))))
}

names(plot_data)=c(paste(rep("mean comp",components),1:components),
                   paste(rep("error comp",components),1:components))

df3 <- data.frame(
  dataset = factor(rep(read_datasets,length(grep("mean",names(plot_data))))),
  mean = as.vector(as.matrix(plot_data[,grep("mean",names(plot_data))])),
  group= factor(rep(c(paste(rep("Component:",components),1:components)),each=length(file_names))),
  upper = as.vector(as.matrix(plot_data[,grep("mean",names(plot_data))]))+2*as.vector(as.matrix(plot_data[,grep("error",names(plot_data))])),
  lower = as.vector(as.matrix(plot_data[,grep("mean",names(plot_data))]))-2*as.vector(as.matrix(plot_data[,grep("error",names(plot_data))]))
)

dff=rbind(dff,df3)


dff$type=as.factor(rep(c("Noise","Missing","Noise and Missing"),each=dim(df)[1]))
dff$type=factor(dff$type,levels(dff$type)[c(3,1,2)])

dff$type <- factor(dff$type,levels(dff$type)[c(3,2,1)])


dff$type <- as.character(dff$type)
dff$type[dff$type=="Noise"]="N"
dff$type[dff$type=="Missing"]="M"
dff$type[dff$type=="Noise and Missing"]="NM"
dff$type <- as.factor(dff$type)
dff$type=factor(dff$type,levels(dff$type)[c(3,1,2)])
dff$group <- as.character(dff$group)
dff$group[dff$group=="Component: 1"] <- "PC1"
dff$group[dff$group=="Component: 2"] <- "PC2"
dff$group[dff$group=="Component: 3"] <- "PC3"
dff$group[dff$group=="Component: 4"] <- "PC4"

levels(dff$dataset) <- c("A2013",
"A2008",
"A2009",
"B2018",
"B2015",
"E2015",
"K2006i",
"K2006p",
"M1992",
"P2005",
"R2014",
"S2012",
"T2008")

plot_corr=ggplot(dff[grep("PC",dff$group),]) + 
  xlab("")+
  ylab("Average Absolute Correlation (%)")+ 
  geom_rect(aes(x=type,ymin=1/4*100, ymax=1/2*100, xmin=0, xmax=Inf),
            color="grey", alpha=0.03)+ 
  geom_rect(aes(ymin=3/4*100, ymax=1*100, xmin=0, xmax=Inf),
            color="grey", alpha=0.03)+
  geom_errorbar(aes(x=type,ymin = lower*100, ymax = upper*100), 
                width = 0.95,size=2.2,color=gg_color_hue(4)[3])+
  geom_point(aes(x=type, y=mean*100, group=dataset),size=6)+
  theme_minimal(base_size = 25)+theme(legend.position = "none",
                                      legend.title=element_blank(),
                                      #strip.text.y=element_text(angle = 0),
                                      strip.text.y=element_text(size = 0),
                                      panel.spacing.y = unit(1.5, "lines"),
                                      panel.spacing.x = unit(2, "lines"),
                                      axis.text.x=element_text(angle=-90, 
                                                               hjust=0),
                                      panel.grid.major=element_line(color="grey",size=0.5),
                                      panel.grid.minor=element_line(color="grey",size=0.5))+ 
  scale_y_continuous(limits = c(0,100))+
  facet_grid_paginate(dataset~group)+coord_flip()
plot_corr

dff_pca <- dff

table=readRDS("../RDS/table_PCASimilarity_noise_mask.RDS")

average <- apply(table,1,mean)
error <- apply(table,1,function(x)sd(x)/sqrt(length(x)))
df <- data.frame(
  dataset = file_names,
  mean = average,
  upper = average+2*error,
  lower = average-2*error,
  type="NM"
)

table=readRDS("../RDS/table_PCASimilarity_noise_nomask.RDS")

average <- apply(table,1,mean)
error <- apply(table,1,function(x)sd(x)/sqrt(length(x)))
df2 <- data.frame(
  dataset=file_names,
  mean = average,
  upper = average+2*error,
  lower = average-2*error,
  type="N"
)
dff=rbind(df,df2)

table=readRDS("../RDS/table_PCASimilarity_nonoise_mask.RDS")

average <- apply(table,1,mean)
error <- apply(table,1,function(x)sd(x)/sqrt(length(x)))
df3 <- data.frame(
  dataset = file_names,
  mean = average,
  upper = average+2*error,
  lower = average-2*error,
  type="M"
)
dff=rbind(dff,df3)

dff$type=factor(dff$type,levels(dff$type)[c(1,3,2)])

levels(dff$dataset) <- c("A2013",
                         "A2008",
                         "A2009",
                         "B2018",
                         "B2015",
                         "E2015",
                         "K2006i",
                         "K2006p",
                         "M1992",
                         "P2005",
                         "R2014",
                         "S2012",
                         "T2008")


plot_pcasim=ggplot(dff, 
                aes(x=type, y=mean*100)) + 
  geom_line(size=3)+
  xlab("")+
  ylab("PCA Similarity (%)")+ 
  geom_rect(aes(ymin=1/4*100, ymax=1/2*100, xmin=0, xmax=Inf),
            color="grey", alpha=0.03)+ 
  geom_rect(aes(ymin=3/4*100, ymax=1*100, xmin=0, xmax=Inf),
            color="grey", alpha=0.03)+
  geom_errorbar(aes(x=type,ymin = lower*100, ymax = upper*100), width = 0.95,
                size=2.2,color=gg_color_hue(4)[1])+
  geom_point(aes(x=type, y=mean*100, group=dataset),size=6)+
  theme_minimal(base_size = 25)+theme(legend.position = "none",
                                       legend.title=element_blank(),
                                       #strip.text.y=element_text(angle = 0),
                                       strip.text.y=element_text(size = 20),
                                       panel.spacing.y = unit(1, "lines"),
                                       panel.spacing.x = unit(2, "lines"),
                                       axis.text.x=element_text(angle=-90, 
                                                                hjust=0),
                                      panel.grid.major=element_line(color="grey",size=0.5),
                                       panel.grid.minor=element_line(color="grey",size=0.5),
                                      axis.text.y=element_blank())+ 
  scale_y_continuous(limits = c(0,100))+
  facet_grid(dataset~.)+coord_flip()
plot_pcasim


pdf("../figures/pca_sim.pdf",width = 15,height=23)
grid.newpage()
pushViewport(viewport(layout = grid.layout(100, 4)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(plot_corr, vp = vplayout(1:100, 1:3))  # key is to define vplayout
print(plot_pcasim, vp = vplayout(3:100, 4))
dev.off()


### table

table_full_pca <- dff_pca[grep("PC",dff_pca$group),] %>% 
  select(dataset, group,type,mean) %>% 
  arrange(dataset,group,type)

table_full_pcasim <- dff %>% 
  select(dataset,type,mean) %>% 
  arrange(dataset,type)

table_full_pcasim$group="Sim"

table_full <- rbind(table_full_pca,table_full_pcasim)%>% 
  arrange(dataset,group,type)

table_full$type <- as.character(table_full$type)
print(xtable::xtable(table_full), 
      include.rownames=FALSE)

