# Performs the analysis regarding 'measurement reliability' of images

# neet to set directory to the root

library(tidyr)
library(readxl)
library(dplyr)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(plotly)
library(irr)

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

###################
# Precision analyses
###################
data_all_2_replicates_I <- data_all %>% 
  filter(replicate==1|replicate==2,S_or_I=="I")
data_all_2_replicates_S <- data_all %>% 
  filter(replicate==1|replicate==2,S_or_I=="S")
data_all_2_replicates_I$identifier=paste(data_all_2_replicates_I$Barcode,
                                         data_all_2_replicates_I$Species,
                                         data_all_2_replicates_I$measurement_name,
                                         paste = "_")
data_all_2_replicates_S$identifier=paste(data_all_2_replicates_S$Barcode,
                                         data_all_2_replicates_S$Species,
                                         data_all_2_replicates_S$measurement_name,
                                         paste = "_")
diff_NA <- function(x)
{
  return_value <- diff(x)
  return(ifelse(length(return_value)==0,NA,return_value))
}

data_all_2_replicates_I_differences <- data_all_2_replicates_I %>% 
  group_by(identifier) %>% 
  summarise(diff_I=diff_NA(measurement_value),mean=mean(measurement_value,na.rm=T)) 
data_all_2_replicates_S_differences <- data_all_2_replicates_S %>% 
  group_by(identifier) %>% 
  summarise(diff_S=diff_NA(measurement_value),mean=mean(measurement_value,na.rm=T)) 

data_all_2_replicates <- merge(data_all_2_replicates_I_differences,
                               data_all_2_replicates_S_differences,
                               by="identifier")

data_all_2_summary <- data.frame(identifier=data_all_2_replicates$identifier,
                                 error_I=data_all_2_replicates$diff_I/data_all_2_replicates$mean.x,
                                 error_S=data_all_2_replicates$diff_S/data_all_2_replicates$mean.y,
                                 measurement_S=data_all_2_replicates$mean.y,
                                 measurement_I=data_all_2_replicates$mean.x)
data_all_2_summary <- data_all_2_summary %>% 
  gather("type","error",-identifier,-measurement_S,-measurement_I)

data_all_2_summary$measurement <- ifelse(data_all_2_summary$type=="error_I",
                                         data_all_2_summary$measurement_I,
                                         data_all_2_summary$measurement_S)

data_all_2_summary$type[data_all_2_summary$type=="error_I"] <- "Image"
data_all_2_summary$type[data_all_2_summary$type=="error_S"] <- "Specimen"

ggplot(data_all_2_summary,aes(x=measurement,y=
                                error,color=type,shape=type,fill=type))+
  ylab("Relative precision")+
  xlab("Measurement magnitude (mm)")+
  geom_point(method="loess", se = TRUE,
             size=2,alpha=0.7)+
  geom_smooth(method="loess", se = TRUE,
              size=1.7)+
  theme_bw(base_size = 40)+
  theme(plot.title = element_text(face = "bold"), 
        panel.grid.minor = element_blank(),
        legend.position="top",
        legend.key.width=unit(1.5,"cm"))+ 
  scale_y_sqrt(limits=c(0,0.3))+ 
  labs(color = "Precision in")+ 
  labs(fill = "Precision in")+ 
  labs(shape = "Precision in")
#ggsave("../figures/relative_precision.pdf",height = 11.5,width = 20,dpi = "print")


ggplot(data_all_2_replicates_S_differences,
       aes(x=mean,y=
             abs(diff_S)/mean))+
  geom_point(size=2,alpha=0.7)+
  ylab("Relative error")+
  xlab("Measurement on the specimen (mm)")+
  geom_smooth(method="loess", se = TRUE,size=3,span=1.15)+
  theme_bw(base_size = 40)+
  theme(plot.title = element_text(face = "bold"), 
        panel.grid.minor = element_blank())+ 
  scale_y_sqrt(limits=c(0,0.3))
#ggsave("../figures/relative_error.pdf",height = 11.5,width = 20,dpi = "print")



#g <- ggplot(data_all_2_replicates)+
#  geom_point(aes(diff_S,diff_I),alpha=0.2)+
#  geom_abline()+
#  xlab("Difference in specimem")+
#  ylab("Difference in image")
#ggplotly(g)

#mean(data_all_2_replicates$diff_S>data_all_2_replicates$diff_I,na.rm=T)
#differences <- data_all_2_replicates$diff_S>data_all_2_replicates$diff_I
#binom.test(table(differences[!is.na(differences)]))

data_all_2_replicates_complete <- data_all_2_replicates[complete.cases(data_all_2_replicates),]
t.test(abs(data_all_2_replicates_complete$diff_S),
       abs(data_all_2_replicates_complete$diff_I),
       paired = TRUE)

first_NA <- function(x)
{
  return(x[1])
}
second_NA <- function(x)
{
  return(x[2])
}

data_all_2_replicates_I_two_col <- data_all_2_replicates_I %>% 
  group_by(identifier) %>% 
  summarise(first_I=first_NA(measurement_value),
            second_I=second_NA(measurement_value)) 
data_all_2_replicates_S_two_col <- data_all_2_replicates_S %>% 
  group_by(identifier) %>% 
  summarise(first_S=first_NA(measurement_value),
            second_S=second_NA(measurement_value)) 
icc_S=icc(
  data_all_2_replicates_S_two_col[complete.cases(data_all_2_replicates_S_two_col),-1])
icc_I=icc(
  data_all_2_replicates_I_two_col[complete.cases(data_all_2_replicates_I_two_col),-1]
)
icc_S
icc_I

data_all_2_replicates_I_two_col <- data_all_2_replicates_I %>% 
  group_by(identifier) %>% 
  summarise(first_I=first_NA(measurement_value),
            second_I=second_NA(measurement_value),
            measurement_name=first_NA(measurement_name),
            Species=first_NA(Species)) 
icc_I <- data_all_2_replicates_I_two_col %>% 
  group_by(measurement_name,Species) %>% 
  summarise(icc=icc(cbind(first_I,second_I))$value) 
data_all_2_replicates_S_two_col <- data_all_2_replicates_S %>% 
  group_by(identifier) %>% 
  summarise(first_S=first_NA(measurement_value),
            second_S=second_NA(measurement_value),
            measurement_name=first_NA(measurement_name),
            Species=first_NA(Species)) 
icc_S <- data_all_2_replicates_S_two_col %>% 
  group_by(measurement_name,Species) %>% 
  summarise(icc=icc(cbind(first_S,second_S))$value) 

#CalcRepeatability2 <- function(x,y)
#{
#  if(mean(is.na(y))>1/3)
#    return(NA)
#  return(CalcRepeatability(x,y))
#}

#library(evolqg)
#rep_S <- data_all_2_replicates_S_two_col %>% 
#  dplyr::group_by(measurement_name,Species) %>% 
#  dplyr::summarise(rep=CalcRepeatability2(rep(1:length(first_S),2),
                                   as.data.frame(c(first_S,second_S))))
#rep_I <- data_all_2_replicates_I_two_col %>% 
#  dplyr::group_by(measurement_name,Species) %>% 
#  dplyr::summarise(rep=CalcRepeatability2(rep(1:length(first_I),2),
#                                          as.data.frame(c(first_I,second_I))))
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
       aes(x=average_measurement.x,y=
             abs(average_measurement_diff)/average_measurement.x))+
  geom_point(size=3)+
  ylab("Relative error")+
  xlab("Measurement on the specimen (mm)")+
  geom_smooth(method="loess", se = TRUE,size=3,span=1.15)+
  theme_bw(base_size = 40)+
  theme(plot.title = element_text(face = "bold"), 
        panel.grid.minor = element_blank())+ scale_y_sqrt()
# uncomment to save plot
ggsave("../figures/relative_error.pdf",height = 11.5,width = 20,dpi = "print")

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
  summarise(corr=paste0(paste0("icc=",round(icc(cbind(average_measurement.x,
                                                      average_measurement.y))$value,2),sep="")))



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
    theme(plot.title = element_text(face = "bold"), 
          panel.grid.minor = element_blank())
}
g2 <- marrangeGrob(g, nrow=length(species),ncol=1,top="")
# uncomment to save plot
# ggsave("../figures/correlation.pdf",g2,height = 35,width = 20,dpi = "print")
