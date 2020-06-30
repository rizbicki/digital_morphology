library(readxl)

compute_missing=function(dataset,datasets,measurements,
                         dataset_units)
{
  print(dataset)
  data.specimen=datasets %>%  .[[which(names(.)==dataset)]]
  data.specimen=as.data.frame(apply(data.specimen,2,function(x) as.numeric(unlist(x))))
  measurements_image=measurements %>%  .[[which(names(.)==dataset)]]
  measurements_image=as.data.frame(apply(measurements_image,2,function(x) as.numeric(unlist(x))))
  intersection=intersect(names(data.specimen),names(measurements_image))
  data.specimen=data.specimen[,intersection]
  measurements_image=measurements_image[,intersection]
  measurements_image[is.na(measurements_image)]=0
  return(mean(measurements_image!=1))
}

compute_100missing=function(dataset,datasets,measurements,
                            dataset_units)
{
  print(dataset)
  data.specimen=datasets %>%  .[[which(names(.)==dataset)]]
  data.specimen=as.data.frame(apply(data.specimen,2,function(x) as.numeric(unlist(x))))
  measurements_image=measurements %>%  .[[which(names(.)==dataset)]]
  measurements_image=as.data.frame(apply(measurements_image,2,function(x) as.numeric(unlist(x))))
  intersection=intersect(names(data.specimen),names(measurements_image))
  data.specimen=data.specimen[,intersection]
  measurements_image=measurements_image[,intersection]
  measurements_image[is.na(measurements_image)]=0
  return(mean(apply(measurements_image,2,function(x)sum(x)==0)))
}


compute_averages_contin=function(dataset,datasets,measurements,
                                  dataset_units)
{
  print(dataset)
  data.specimen=datasets %>%  .[[which(names(.)==dataset)]]
  data.specimen=as.data.frame(apply(data.specimen,2,function(x) as.numeric(unlist(x))))
  measurements_image=measurements %>%  .[[which(names(.)==dataset)]]
  measurements_image=as.data.frame(apply(measurements_image,2,function(x) as.numeric(unlist(x))))
  units=dataset_units %>%  .[[which(names(.)==dataset)]]
  intersection=intersect(names(data.specimen),names(measurements_image))
  units=units[,intersection]
  data.specimen=data.specimen[,intersection]
  data.specimen[,units%in%c("cm")] <- data.specimen[,units%in%c("cm")]*10
  data.specimen <- data.specimen[,units%in%c("cm","mm")]
  return(apply(data.specimen,2,function(x)mean(x,na.rm=T)))
}

at_least_one_missing=function(dataset,datasets,measurements,
                             dataset_units)
{
  print(dataset)
  data.specimen=datasets %>%  .[[which(names(.)==dataset)]]
  data.specimen=as.data.frame(apply(data.specimen,2,function(x) as.numeric(unlist(x))))
  measurements_image=measurements %>%  .[[which(names(.)==dataset)]]
  measurements_image=as.data.frame(apply(measurements_image,2,function(x) as.numeric(unlist(x))))
  intersection=intersect(names(data.specimen),names(measurements_image))
  data.specimen=data.specimen[,intersection]
  measurements_image=measurements_image[,intersection]
  measurements_image[is.na(measurements_image)]=0
  return(mean(apply(measurements_image,2,function(x){
    any(x!=1)})))
}

at_least_50_missing=function(dataset,datasets,measurements,
                             dataset_units)
{
  print(dataset)
  data.specimen=datasets %>%  .[[which(names(.)==dataset)]]
  data.specimen=as.data.frame(apply(data.specimen,2,function(x) as.numeric(unlist(x))))
  measurements_image=measurements %>%  .[[which(names(.)==dataset)]]
  measurements_image=as.data.frame(apply(measurements_image,2,function(x) as.numeric(unlist(x))))
  intersection=intersect(names(data.specimen),names(measurements_image))
  data.specimen=data.specimen[,intersection]
  measurements_image=measurements_image[,intersection]
  measurements_image[is.na(measurements_image)]=0
  return(mean(apply(measurements_image,2,function(x){
    mean(x!=1)>0.5})))
}

summary_units=function(dataset,datasets,measurements,
                         dataset_units)
{
  data.specimen=datasets %>%  .[[which(names(.)==dataset)]]
  data.specimen=as.data.frame(apply(data.specimen,2,function(x) as.numeric(unlist(x))))
  measurements_image=measurements %>%  .[[which(names(.)==dataset)]]
  measurements_image=as.data.frame(apply(measurements_image,2,function(x) as.numeric(unlist(x))))
  units=dataset_units %>%  .[[which(names(.)==dataset)]]
  
  intersection=intersect(names(data.specimen),names(measurements_image))
  units=units[,intersection]
  return(table(as.matrix(units)))
}

missing_by_type=function(dataset,datasets,measurements,
                          dataset_units)
{
  data.specimen=datasets %>%  .[[which(names(.)==dataset)]]
  data.specimen=as.data.frame(apply(data.specimen,2,function(x) as.numeric(unlist(x))))
  measurements_image=measurements %>%  .[[which(names(.)==dataset)]]
  measurements_image=as.data.frame(apply(measurements_image,2,function(x) as.numeric(unlist(x))))
  units=dataset_units %>%  .[[which(names(.)==dataset)]]
  intersection=intersect(names(data.specimen),names(measurements_image))
  units=units[,intersection]
  units[1,units[1,]=="cm"] <- "mm"
  units[1,units[1,]=="raio"] <- "ratio"
  data.specimen=data.specimen[,intersection]
  measurements_image=measurements_image[,intersection]
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
data.original=lapply(as.list(files), function(x) readxl::read_excel(paste("../Data_missing/",x,sep="")))

library(dplyr)
names=(regexpr(".", files, fixed = TRUE) - 1) %>% stringr::str_sub(files,start = rep(1,length(files)),end =.)
names(data.original)=names

dataset_units=read_excel_allsheets("../Data//variable-unit.xlsx")

missing <- rep(NA,length(names))
names(missing) <- names
names(missing) <- stringr::str_replace(names(missing),"-","")
names(missing) <- stringr::str_replace(names(missing),"_","")
names(missing) <- stringr::str_replace(names(missing),"-","")
names(missing) <- stringr::str_replace(names(missing),"_","")
number_one_missing <- missing
number_50_missing <- missing
number_100_missing <- missing
missing_type <- list()
units <- list()
averages_contin <- list()
ii=1
for (name in names)  #para cada dataset
{
  missing[ii] <- compute_missing(dataset=name,
                                 datasets=data.original,
                                 measurements=data.measurements,
                                 dataset_units=dataset_units)
  number_one_missing[ii] <- at_least_one_missing(dataset=name,
                                                datasets=data.original,
                                                measurements=data.measurements,
                                                dataset_units=dataset_units)
  number_50_missing[ii] <- at_least_50_missing(dataset=name,
                                                datasets=data.original,
                                                measurements=data.measurements,
                                                dataset_units=dataset_units)
  
  number_100_missing[ii] <- compute_100missing(dataset=name,
                                                datasets=data.original,
                                                measurements=data.measurements,
                                                dataset_units=dataset_units)
  
  units[[ii]] <- summary_units(dataset=name,
                                    datasets=data.original,
                                    measurements=data.measurements,
                                    dataset_units=dataset_units)
  averages_contin[[ii]] <- compute_averages_contin(dataset=name,
                                                     datasets=data.original,
                                                     measurements=data.measurements,
                                                     dataset_units=dataset_units)
  missing_type[[ii]] <- missing_by_type(dataset=name,
                                         datasets=data.original,
                                         measurements=data.measurements,
                                         dataset_units=dataset_units)
  ii=ii+1
}


table <- cbind(as.data.frame(names(missing)),
                missing,
                number_50_missing,
                number_100_missing)

table_final=readRDS("../RDS/table_pca_nonoise_mask.RDS")

table_accuracy <- table_final %>% 
  dplyr::group_by(dataset) %>% 
  dplyr::summarise(c1=mean(abs(`comp 1`)),
            c2=mean(abs(`comp 2`)),
            c3=mean(abs(`comp 3`)),
            c4=mean(abs(`comp 4`)))%>% 
  dplyr::arrange(dataset)

tab_cor_sim=readRDS("../RDS/table_PCASimilarity_nonoise_mask.RDS")
tab_cor_sim <- rowMeans(tab_cor_sim)

tab_corr <- cor(table_accuracy[,-1],table[,-1],
    method="spearman")

tab_corr_sim <- cor(tab_cor_sim,table[,-1],
                method="spearman")

p_vals <- matrix(NA,5,3)
tab_corr_p <- rbind(as.data.frame(tab_corr),tab_corr_sim)
for(ii in 1:4)
{
  for(jj in 1:3)
  {
    p_vals[ii,jj] <-   cor.test(table_accuracy[,1+ii] %>% pull(),table[,1+jj],
                                method="spearman")$p.value
    tab_corr_p[ii,jj] <- paste0(round(100*tab_corr[ii,jj],1),"% (",round(p_vals[ii,jj],2),")")
    
  }
}
ii <- 5# PCA sim
for(jj in 1:3)
{
  p_vals[ii,jj] <-   cor.test(tab_cor_sim,table[,1+jj],
                              method="spearman")$p.value
  tab_corr_p[ii,jj] <- paste0(round(100*tab_corr_sim[jj],1),"% (",round(p_vals[ii,jj],2),")")
  
}
p_vals
tab_corr_p

print(xtable::xtable(tab_corr_p))
