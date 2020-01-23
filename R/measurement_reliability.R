# Performs the analysis regarding 'measurement reliability' of images

# neet to set directory to the root

library(tidyr)
library(readxl)
library(dplyr)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

# ggplot configuration
theme = theme_set(theme_bw(base_size = 22))
theme = theme_update(legend.position="top", legend.title=element_blank(),panel.grid.major.x=element_blank())


read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

###########################
### Read and preprocess data
###########################

mysheets <- read_excel_allsheets("Data/measurements-clean.xls")

data_all=data.frame(t(rep(NA,5)))
colnames(data_all)=c("Species","Barcode","T","identifier","measurement_value")
for(i in 1:length(mysheets))
{
  
  data=mysheets[[i]]
  colunasNomes=paste(colnames(data)[2],colnames(data)[ncol(data)-1],sep=":")
  dataAux=gather(data,identificador,medida,
                 colnames(data)[2:(ncol(data)-1)])
  dataAux=cbind(names(mysheets)[i],dataAux)
  names(dataAux)=names(data_all)
  data_all=rbind(data_all,dataAux)
}
data_all=data_all[!is.na(data_all$measurement_value),]
data_all=data_all[,!names(data_all)%in%c("T")]


split_name=strsplit(x=data_all$identifier,split = "\\.")
split_name=t(sapply(split_name, function(x)
{
  return(c(paste(x[1:2],collapse = "."),x[3],x[4]))
}))
data_all=cbind(split_name,data_all)
colnames(data_all)[1:3]=c("measurement_name","replicate","S_or_I")

id=paste(data_all$Barcode,data_all$Species,
                    data_all$measurement_name,
                    data_all$S_or_I,paste = "_")
data_all$id_unique_sample= id
data_all$measurement_value=as.numeric(data_all$measurement_value)
data_all=data_all[!is.na(data_all$replicate),]


############
## Compute average of all measurements made for each image or specimen
############

data_all_averages=data_all %>% 
  group_by(Species,Barcode,measurement_name,S_or_I) %>%   
  summarise(average_measurement=mean(measurement_value,na.rm=TRUE))

data_all_averages_S=data_all_averages %>% 
  filter(S_or_I=="S") # only specimen measurements
data_all_averages_I=data_all_averages %>% 
  filter(S_or_I=="I") # only images measurements

# identify each unique measurement
data_all_averages_S$identifier=paste(data_all_averages_S$Barcode,
                                     data_all_averages_S$Species,
                                     data_all_averages_S$measurement_name,
                                     paste = "_")
data_all_averages_I$identifier=paste(data_all_averages_I$Barcode,
                                     data_all_averages_I$Species,
                                     data_all_averages_I$measurement_name,
                                     paste = "_")


############################
# compute difference between measurements
############################

data_all_averages_difference=merge(data_all_averages_S,
                                   data_all_averages_I,
                                   by="identifier")

data_all_averages_difference$average_measurement_diff=
  data_all_averages_difference$average_measurement.x-data_all_averages_difference$average_measurement.y

data_all_averages_difference=data_all_averages_difference[,c("identifier","Species.x",
                                                             "Barcode.x",
                                                             "measurement_name.x",
                                                             "average_measurement.x", 
                                                             "average_measurement.y",
                                                             "average_measurement_diff")]

# rename columns
colnames(data_all_averages_difference)=c("Identifier","Species","Barcode",
                                         "measurement_name","average_measurement.x", 
                                         "average_measurement.y",
                                         "average_measurement_diff")

data_all_averages_difference$measured_species=paste(data_all_averages_difference$Species,
                                                  data_all_averages_difference$measurement_name,
                                                  sep = ": ")

# compute pvalues to test if average difference is the same
ps_median=data_all_averages_difference %>% group_by(Species,measurement_name) %>% 
  summarise(pval=paste("p=",round(wilcox.test(average_measurement.x,
                                              average_measurement.y)$p.value,3),sep=""),
            n=n())

# plot histograms
species <- table(data_all_averages_difference$Species)
g <- list()
for(ii in seq_along(species))
{
  g[[ii]] <- ggplot(data=data_all_averages_difference %>% filter(Species==names(species)[ii]))+
    geom_histogram(aes(x=average_measurement_diff,binwidth=diff(range(average_measurement_diff,na.rm = TRUE)) / (2 * IQR(average_measurement_diff,na.rm = TRUE) / sum(!is.na(average_measurement_diff))^(1/3))),fill="azure4",alpha=0.6)+ 
    geom_label(data=ps_median %>% filter(Species==names(species)[ii]),aes(label=pval,x=Inf,y=Inf,hjust=1,vjust=1),size=9, 
               colour = "black", fontface = "bold",fill="white",label.size=0)+
    facet_wrap(~measured_species,nrow=1, scales="free")+
    geom_vline(xintercept=0,color="blue",size=1.6,alpha=0.6)+xlab("Difference between specimen and image measurements (mm)")+ylab("Number of observations")+
    ggtitle(names(species)[ii])+
    theme_bw(base_size = 40)+
    theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
}
g2 <- marrangeGrob(g, nrow=length(species),ncol=1,top="")
# uncomment to save plot
#ggsave("figures/histogram.pdf",g2,height = 35,width = 20,
#       dpi = "print")

# plot relative error versus magnitude of each variable
ggplot(data_all_averages_difference,
       aes(x=average_measurement.y,y=
             abs(average_measurement_diff)/average_measurement.y))+
  geom_point(size=3)+
  ylab("Relative error")+
  xlab("Measurement on the specimen (mm)")+
  geom_smooth(method="loess", se = TRUE,size=3,span=1.15)+
  theme_bw(base_size = 40)+
  theme(plot.title = element_text(face = "bold"), 
        panel.grid.minor = element_blank())+ scale_y_sqrt()
# uncomment to save plot
#ggsave("figures/relative_error.pdf",height = 11.5,width = 20,dpi = "print")


# compute and plot std deviation of error versus magnitude of each variable
summary_errors <- data_all_averages_difference %>% 
  group_by(Species,measurement_name) %>% 
  summarise(mean.x=mean(average_measurement.x),
            mean.y=mean(average_measurement.y,na.rm=T),
            error=sd(average_measurement_diff,na.rm=T),
            mean_error=mean(abs(average_measurement_diff),na.rm=T))

ggplot(summary_errors,aes(x=mean.y,y=error))+
  geom_point(size=8)+
  ylab("Standard deviation of the error (mm)")+
  xlab("Average measurement on the specimen (mm)")+
  geom_smooth(method="lm", se = FALSE,size=3)+
  theme_bw(base_size = 40)+
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
# uncomment to save plot
#ggsave("../figures/standard_deviation.pdf",height = 10.5,width = 20,dpi = "print")


# compute correlation coefficient between measurements made in species vs images
ps=data_all_averages_difference %>% group_by(Species,measurement_name) %>% 
  summarise(corr=paste("r=",round(cor(average_measurement.x,
                                      average_measurement.y,use = "complete.obs"),3),sep=""))

# plot scatterplots of measurements made in species vs images  
species <- table(data_all_averages_difference$Species)
g <- list()
for(ii in seq_along(species))
{
  g[[ii]] <- ggplot(data=data_all_averages_difference %>% filter(Species==names(species)[ii]))+
    geom_point(aes(x=average_measurement.x,
                   y=average_measurement.y),size=2.6,alpha=0.7)+
    facet_wrap(~measurement_name,scales="free",nrow=1)+
    geom_abline(intercept=0,slope=1,color="blue",size=1.6)+ 
    geom_label(data=ps %>% filter(Species==names(species)[ii]),aes(label=corr),
               size=9,colour = "black", fontface = "bold",fill="white",
               x=-Inf,y=Inf,hjust=0,vjust=1,label.size=0)+
    xlab("Specimen measurement (mm)")+ylab("Image measurement (mm)")+
    ggtitle(names(species)[ii])+
    theme_bw(base_size = 40)+
    theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
}
g2 <- marrangeGrob(g, nrow=length(species),ncol=1,top="")
# uncomment to save plot
# ggsave("../figures/correlation.pdf",g2,height = 35,width = 20,dpi = "print")
