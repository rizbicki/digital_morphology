library(readxl)
library(dplyr)

compute_missing=function(dataset,datasets,measurements,
                         units_data)
{
  print(dataset)
  data.especimen=datasets %>%  .[[which(names(.)==dataset)]]
  data.especimen=as.data.frame(apply(data.especimen,2,function(x) as.numeric(unlist(x))))
  measurements_image=measurements %>%  .[[which(names(.)==dataset)]]
  measurements_image=as.data.frame(apply(measurements_image,2,function(x) as.numeric(unlist(x))))
  intersection_variables=intersect(names(data.especimen),names(measurements_image))
  data.especimen=data.especimen[,intersection_variables]
  measurements_image=measurements_image[,intersection_variables]
  measurements_image[is.na(measurements_image)]=0
  return(mean(measurements_image!=1))
}

compute_100missing=function(dataset,datasets,measurements,
                         units_data)
{
  print(dataset)
  data.especimen=datasets %>%  .[[which(names(.)==dataset)]]
  data.especimen=as.data.frame(apply(data.especimen,2,function(x) as.numeric(unlist(x))))
  measurements_image=measurements %>%  .[[which(names(.)==dataset)]]
  measurements_image=as.data.frame(apply(measurements_image,2,function(x) as.numeric(unlist(x))))
  intersection_variables=intersect(names(data.especimen),names(measurements_image))
  data.especimen=data.especimen[,intersection_variables]
  measurements_image=measurements_image[,intersection_variables]
  measurements_image[is.na(measurements_image)]=0
  return(mean(apply(measurements_image,2,function(x)sum(x)==0)))
}

compute_averages_continuous=function(dataset,datasets,measurements,
                                  units_data)
{
  print(dataset)
  data.especimen=datasets %>%  .[[which(names(.)==dataset)]]
  data.especimen=as.data.frame(apply(data.especimen,2,function(x) as.numeric(unlist(x))))
  measurements_image=measurements %>%  .[[which(names(.)==dataset)]]
  measurements_image=as.data.frame(apply(measurements_image,2,function(x) as.numeric(unlist(x))))
  units=units_data %>%  .[[which(names(.)==dataset)]]
  intersection_variables=intersect(names(data.especimen),names(measurements_image))
  units=units[,intersection_variables]
  data.especimen=data.especimen[,intersection_variables]
  data.especimen[,units%in%c("cm")] <- data.especimen[,units%in%c("cm")]*10
  data.especimen <- data.especimen[,units%in%c("cm","mm")]
  return(apply(data.especimen,2,function(x)mean(x,na.rm=T)))
}

at_least_one_missing=function(dataset,datasets,measurements,
                             units_data)
{
  print(dataset)
  data.especimen=datasets %>%  .[[which(names(.)==dataset)]]
  data.especimen=as.data.frame(apply(data.especimen,2,function(x) as.numeric(unlist(x))))
  measurements_image=measurements %>%  .[[which(names(.)==dataset)]]
  measurements_image=as.data.frame(apply(measurements_image,2,function(x) as.numeric(unlist(x))))
  intersection_variables=intersect(names(data.especimen),names(measurements_image))
  data.especimen=data.especimen[,intersection_variables]
  measurements_image=measurements_image[,intersection_variables]
  measurements_image[is.na(measurements_image)]=0
  return(mean(apply(measurements_image,2,function(x){
    any(x!=1)})))
}

at_least_50_missing=function(dataset,datasets,measurements,
                             units_data)
{
  print(dataset)
  data.especimen=datasets %>%  .[[which(names(.)==dataset)]]
  data.especimen=as.data.frame(apply(data.especimen,2,function(x) as.numeric(unlist(x))))
  measurements_image=measurements %>%  .[[which(names(.)==dataset)]]
  measurements_image=as.data.frame(apply(measurements_image,2,function(x) as.numeric(unlist(x))))
  intersection_variables=intersect(names(data.especimen),names(measurements_image))
  data.especimen=data.especimen[,intersection_variables]
  measurements_image=measurements_image[,intersection_variables]
  measurements_image[is.na(measurements_image)]=0
  return(mean(apply(measurements_image,2,function(x){
    mean(x!=1)>0.5})))
}

summary_units=function(dataset,datasets,measurements,
                         units_data)
{
  data.especimen=datasets %>%  .[[which(names(.)==dataset)]]
  data.especimen=as.data.frame(apply(data.especimen,2,function(x) as.numeric(unlist(x))))
  measurements_image=measurements %>%  .[[which(names(.)==dataset)]]
  measurements_image=as.data.frame(apply(measurements_image,2,function(x) as.numeric(unlist(x))))
  units=units_data %>%  .[[which(names(.)==dataset)]]
  
  intersection_variables=intersect(names(data.especimen),names(measurements_image))
  units=units[,intersection_variables]
  return(table(as.matrix(units)))
}

missing_by_type=function(dataset,datasets,measurements,
                          units_data)
{
  data.especimen=datasets %>%  .[[which(names(.)==dataset)]]
  data.especimen=as.data.frame(apply(data.especimen,2,function(x) as.numeric(unlist(x))))
  measurements_image=measurements %>%  .[[which(names(.)==dataset)]]
  measurements_image=as.data.frame(apply(measurements_image,2,function(x) as.numeric(unlist(x))))
  units=units_data %>%  .[[which(names(.)==dataset)]]
  intersection_variables=intersect(names(data.especimen),names(measurements_image))
  units=units[,intersection_variables]
  units[1,units[1,]=="cm"] <- "mm"
  units[1,units[1,]=="raio"] <- "ratio"
  data.especimen=data.especimen[,intersection_variables]
  measurements_image=measurements_image[,intersection_variables]
  measurements_image[is.na(measurements_image)]=0
  
  missings <- apply(as.matrix(unique(as.character(units[1,]))),1,function(xx)
  {
    which_type <- names(units)[units[1,]==xx]
    return(list(type=xx,missing=mean(measurements_image[,colnames(measurements_image)%in%which_type]!=1)))
  })
  
  return(missings)
}


read_excel_allsheets = function(filename) {
  sheets = readxl::excel_sheets(filename)
  x =    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) = sheets
  x
}


data.measurements=read_excel_allsheets("../Data/image-variable-check (copy).xlsx")

files=list.files(path="../Data_missing/")
original.data=lapply(as.list(files), function(x) readxl::read_excel(paste("../Data_missing/",x,sep="")))

file_names=(regexpr(".", files, fixed = TRUE) - 1) %>% stringr::str_sub(files,start = rep(1,length(files)),end =.)
names(original.data)=file_names

units_data=read_excel_allsheets("../Data/variable-unit.xlsx")

missing <- rep(NA,length(file_names))
names(missing) <- file_names
names(missing) <- stringr::str_replace(names(missing),"-","")
names(missing) <- stringr::str_replace(names(missing),"_","")
names(missing) <- stringr::str_replace(names(missing),"-","")
names(missing) <- stringr::str_replace(names(missing),"_","")
quantity_one_missing <- missing
quantity_50_missing <- missing
quantity_100_missing <- missing
missing_type <- list()
units <- list()
averages_continuous <- list()
ii=1
for (name in file_names)  #for each dataset
{
  missing[ii] <- compute_missing(dataset=name,
                                 datasets=original.data,
                                 measurements=data.measurements,
                                 units_data=units_data)
  quantity_one_missing[ii] <- at_least_one_missing(dataset=name,
                                                datasets=original.data,
                                                measurements=data.measurements,
                                                units_data=units_data)
  quantity_50_missing[ii] <- at_least_50_missing(dataset=name,
                                                datasets=original.data,
                                                measurements=data.measurements,
                                                units_data=units_data)
  
  quantity_100_missing[ii] <- compute_100missing(dataset=name,
                                             datasets=original.data,
                                             measurements=data.measurements,
                                             units_data=units_data)
  units[[ii]] <- summary_units(dataset=name,
                                    datasets=original.data,
                                    measurements=data.measurements,
                                    units_data=units_data)
  averages_continuous[[ii]] <- compute_averages_continuous(dataset=name,
                                                     datasets=original.data,
                                                     measurements=data.measurements,
                                                     units_data=units_data)
  missing_type[[ii]] <- missing_by_type(dataset=name,
                                        datasets=original.data,
                                        measurements=data.measurements,
                                        units_data=units_data)
  ii=ii+1
}


table <- cbind(as.data.frame(names(missing)),
                paste0(round(missing*100,2),"%"),
                paste0(round(quantity_one_missing*100,2),"%"),
                paste0(round(quantity_50_missing*100,2),"%"),
                paste0(round(quantity_100_missing*100,2),"%"))
names(table) <- c("dataset","Percentage missing",
                   "At least 1 missing","At least 50% missings",
                   "At least 100% missings")
print(xtable::xtable(table[,-3]), include.rownames=FALSE)

table <- cbind(as.data.frame(names(missing)),
                missing,
                quantity_50_missing)


names_units <- unique(names(unlist(units)))
tab_units <- matrix(0,length(units),length(names_units))
colnames(tab_units) <- names_units
for (ii in 1:length(units))  #for each dataset
{
  tab_units[ii,names(units[[ii]])] <- units[[ii]]
  ii=ii+1
}
tab_units[,"mm"] <- tab_units[,"cm"]+tab_units[,"mm"]
tab_units[,"ratio"] <- tab_units[,"raio"]+tab_units[,"ratio"]
tab_units <- tab_units[,-which(colnames(tab_units) %in% c("cm","raio"))]
tab_units <- cbind(as.data.frame(names(missing)),tab_units)
print(xtable::xtable(tab_units[,-6],digits=0), include.rownames=FALSE)



names_units <- unique(names(unlist(units)))
names_units <- names_units[!names_units%in%c("cm","raio")]
tab_missing_type <- matrix(NA,length(units),length(names_units))
colnames(tab_missing_type) <- names_units
for (ii in 1:length(units))  #for each dataset
{
  missing_type_local <- sapply(missing_type[[ii]],matrix)
  tab_missing_type[ii,unlist(missing_type_local[1,])] <- unlist(missing_type_local[2,])
  ii=ii+1
}

tab_missing_type_d <- as.data.frame(matrix(NA,nrow(tab_missing_type),ncol(tab_missing_type)))
for(ii in 1:ncol(tab_missing_type[,-5]))
{
  tab_missing_type_d[!is.na(tab_missing_type[,ii]),ii] <- paste0(round(tab_missing_type[!is.na(tab_missing_type[,ii]),ii]*100,2),"%")
  tab_missing_type_d[is.na(tab_missing_type[,ii]),ii] <- NA
}

rownames(tab_missing_type_d) <- names(missing)
print(xtable::xtable(tab_missing_type_d[,-5], include.rownames=FALSE),NA.string="-")


table <- cbind(as.data.frame(names(missing)),
                paste0(round(missing*100,2),"%"),
                paste0(round(quantity_one_missing*100,2),"%"),
                paste0(round(quantity_50_missing*100,2),"%"))

averages <- NULL
for(ii in 1:length(averages_continuous))
{
  averages <- rbind(averages,cbind(as.data.frame(file_names[ii]),averages_continuous[[ii]]))
}
colnames(averages) <- c("dataset","averages")
library(ggplot2)

ggplot(averages, 
       aes(x=dataset, y=averages)) +
  xlab("")+
  ylab("Average image measurement (mm)")+ 
  geom_point(shape=19)+
  theme_minimal(base_size = 30)+
  theme(legend.position = "none",
        legend.title=element_blank(),
        text=element_text(size=15),
        strip.text.y=element_text(angle = 0),
        panel.spacing = unit(2, "lines"))+
  coord_flip()
