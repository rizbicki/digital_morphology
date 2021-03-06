library(cluster)
library(dplyr)
library(tidyr)
library(permute)
library(psych)
library(magrittr)
library(evolqg)

pca_sim <- function(eg.x.vectors,eg.y.vectors,eg.x.values,eg.y.values) 
{
  total_var <- eg.x.values %*% eg.y.values
  return(c(sum((eg.x.values %o% eg.y.values) * 
                 ((t(eg.x.vectors) %*% (eg.y.vectors))^2))/total_var))
}

set.seed(2019)

files_=list.files(path="../Data_missing/")
files_=grep("xls",files_) %>% files_[.]

names_=(regexpr(".", files_, fixed = TRUE) - 1) %>% stringr::str_sub(files_,start = rep(1,length(files_)),end =.)

read_excel_allsheets = function(filename) {
  sheets = readxl::excel_sheets(filename)
  x =    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) = sheets
  x
}

# add noise according to linear model learned from the measurement data
add_noise <- function(x,intercept_mm=0.63970,slope_mm=0.01913)
{
  average_measurment <- mean(x,na.rm=TRUE)
  return(rnorm(length(x),
               x,
               intercept_mm+slope_mm*average_measurment))
}

compare_acp_sim=function(dataset_,datasets_,measures_,
                         acp,
                         units_data,
                         intercept_mm=0.63970,
                         slope_mm=0.01913,
                         noise_=FALSE,
                         with_missing=TRUE)
{
  print(dataset_)
  original_data=datasets_ %>%  .[[which(names(.)==dataset_)]]
  original_data=as.data.frame(apply(original_data,2,function(x) as.numeric(unlist(x))))
  digital_data=measures_ %>%  .[[which(names(.)==dataset_)]]
  digital2_data=as.data.frame(apply(digital_data,2,function(x) as.numeric(unlist(x))))
  intersection_variables=intersect(names(original_data),names(digital_data))
  original_data=original_data[,intersection_variables]
  digital_data=digital_data[,intersection_variables]
  
  original_data_noise <- original_data
  
  # add noise
  units_data2=units_data %>%  .[[which(names(.)==dataset_)]]
  units_data2=unlist(units_data2[,intersection_variables])
  if(noise_==TRUE)
  {
    for(ii in 1:ncol(original_data))
    {
      if(units_data2[ii]=="mm")
      {
        original_data_noise[,ii] <- add_noise(original_data[,ii],
                                              intercept_mm=intercept_mm,
                                              slope_mm=slope_mm)
      }
      if(units_data2[ii]=="dm")
      {
        original_data_noise[,ii] <- add_noise(10*original_data[,ii],
                                              intercept_mm=intercept_mm,
                                              slope_mm=slope_mm) 
      } 
      if(units_data2[ii]=="cm")
      {
        original_data_noise[,ii] <- add_noise(100*original_data[,ii],
                                              intercept_mm=intercept_mm,
                                              slope_mm=slope_mm) 
      }
      if(units_data2[ii]=="m")
      {
        original_data_noise[,ii] <- add_noise(1000*original_data[,ii],
                                              intercept_mm=intercept_mm,
                                              slope_mm=slope_mm) 
      }
      if(units_data2[ii]=="categorical")
      {
        original_data[,ii] <- as.factor(original_data[,ii])
        original_data_noise[,ii] <- as.factor(original_data_noise[,ii])
      }
    }
  } else {  # ransform categorical variables to factors
    for(ii in 1:ncol(original_data))
    {
      if(units_data2[ii]=="categorical")
      {
        original_data[,ii] <- as.factor(original_data[,ii])
        original_data_noise[,ii] <- as.factor(original_data_noise[,ii])
      }
    }
  }
  
  names_letters <- apply(expand.grid(letters,letters,letters),1,
                         function(x)paste0(x,collapse = ""))
  colnames(original_data) <- names_letters[1:ncol(original_data)]
  colnames(original_data_noise) <- names_letters[1:ncol(original_data_noise)]
  colnames(digital_data) <- names_letters[1:ncol(digital_data)]
  
  scale <- sqrt(apply(original_data,2,var))
  original_data <- original_data[,(abs(scale)>1e-10)|is.na(scale)]
  original_data_noise <- original_data_noise[,(abs(scale)>1e-10)|is.na(scale)]
  digital_data <- digital_data[,(abs(scale)>1e-10)|is.na(scale)]
  # transform categorical variables to dummies
  
  matrix_original_data <- model.matrix(~.-1,original_data)
  matrix_original_data_noise <- model.matrix(~.-1,original_data_noise)
  
  # apply mask 
  matrix_digital_data <- matrix(NA,nrow(digital_data),
                                ncol(matrix_original_data))
  for(ii in 1:ncol(matrix_original_data))
  {
    names_original <-gsub("[^[:alpha:]]+", "",colnames(matrix_original_data)[ii])
    matrix_digital_data[,ii] <- digital_data[,names_original,drop=T]
  }
  scale <- sqrt(apply(matrix_original_data,2,var))
  matrix_original_data <- matrix_original_data[,abs(scale)>1e-10]
  original_data=as.data.frame(scale(matrix_original_data))
  matrix_original_data_noise <- matrix_original_data_noise[,abs(scale)>1e-10]
  original_data_noise=as.data.frame(scale(matrix_original_data_noise))
  digital_data=matrix_digital_data
  colnames(digital_data) <- colnames(original_data)
  digital_data <- as.data.frame(digital_data)
  
  intersection_variables <- colnames(original_data)
  
  if(with_missing)
  {
    digital_data=digital_data %>% .[sample(1:nrow(.),nrow(original_data),replace = T),]
    digital_data[digital_data!=1]=NA
  } else {
    digital_data=as.data.frame(matrix(1,nrow(original_data),ncol(original_data)))
    colnames(digital_data)=colnames(original_data)
  }
  
  remove_variables=(apply(digital_data,2, function(x) sum(is.na(x))))==nrow(original_data)
  remove_sample_units=(apply(digital_data,1, function(x) sum(is.na(x))))==ncol(original_data)
  remove_variables2=(apply(original_data,2, function(x) sum(is.na(x))))==nrow(original_data)
  remove_sample_units2=(apply(original_data,1, function(x) sum(is.na(x))))==ncol(original_data)
  
  
  if ((sum(remove_sample_units2)==0) && (sum(remove_variables2)==0))
  {
    original_data=original_data
  }  else if((sum(remove_sample_units2)!=0) && (sum(remove_variables2)==0)){
    
    original_data=original_data[-which(remove_sample_units2),]
    original_data_noise=original_data_noise[-which(remove_sample_units2),]
  } else if((sum(remove_sample_units2)==0) && (sum(remove_variables2)!=0)){
    
    original_data=original_data[,-which(remove_variables2)]  
    original_data_noise=original_data_noise[,-which(remove_variables2)]  
  } else {
    original_data=original_data[-which(remove_sample_units2),-which(remove_variables2)]
    original_data_noise=original_data_noise[-which(remove_sample_units2),-which(remove_variables2)]
  } 
  
  intersection_variables=intersect(names(original_data),names(digital_data))
  original_data=original_data[,intersection_variables]
  original_data_noise=original_data_noise[,intersection_variables]
  
  digital_data=digital_data[,intersection_variables]
  
  remove_variables=(apply(digital_data,2, function(x) sum(is.na(x))))==nrow(digital_data)
  remove_sample_units=(apply(digital_data,1, function(x) sum(is.na(x))))==ncol(digital_data)
  
  digital_data <- apply(digital_data,2,as.numeric)
  if ((sum(remove_sample_units)==0) && (sum(remove_variables)==0))
  {
    digital_data2=original_data_noise*digital_data
  } else if((sum(remove_sample_units)!=0) && (sum(remove_variables)==0)){
    digital_data2=original_data_noise[-which(remove_sample_units),]*digital_data[-which(remove_sample_units),]
  } else if((sum(remove_sample_units)==0) && (sum(remove_variables)!=0)){
    digital_data2=original_data_noise[,-which(remove_variables)]*digital_data[,-which(remove_variables)]
  } else {
    digital_data2=original_data_noise[-which(remove_sample_units),-which(remove_variables)]*digital_data[-which(remove_sample_units),-which(remove_variables)]
  }
  remove_variables2=apply(digital_data2,2, function(x) sum(is.na(x))<(nrow(digital_data2)-1))
  
  digital_data2=digital_data2[,which(remove_variables2)]
  
  pca_digital_data=try(pcaMethods::pca(digital_data2,nPcs=min(ncol(digital_data2)-5,10), 
                                       method="ppca",center=TRUE,scale="uv"))
  pca_original_data=try(pcaMethods::pca(original_data,nPcs=min(ncol(digital_data2)-5,10), 
                                        method="ppca",center=TRUE,scale="uv"))
  
  
  if((sum(class(pca_digital_data)=="try-error")==0) && (sum(class(pca_original_data)=="try-error")==0))
  {
    eg.x.vectors = pca_original_data@loadings
    eg.y.vectors=eg.x.vectors
    eg.y.vectors[colnames(digital_data2),] <- pca_digital_data@loadings[colnames(digital_data2),]
    eg.y.vectors[!(rownames(eg.y.vectors)%in%colnames(digital_data2)),] <- 0
    
    sim_pca <- pca_sim(eg.x.vectors = eg.x.vectors,
                       eg.y.vectors=eg.y.vectors,
                       eg.x.values = pca_original_data@sDev^2,
                       eg.y.values = pca_digital_data@sDev^2)
    
    
    
  }
  
  return(sim_pca)
  
}

digital_data2=read_excel_allsheets("../Data/image-variable-check (copy).xlsx")
original_data2=lapply(as.list(files_), function(x) readxl::read_excel(paste("../Data_missing/",x,sep="")))
names(original_data2)=names_
units_data=read_excel_allsheets("../Data/variable-unit.xlsx")


repetitions_=50
components_=4
row=1
table_nonoise_missing=matrix(NA,length(names_),repetitions_)
table_noise_nomissing=matrix(NA,length(names_),repetitions_)
table_noise_missing=matrix(NA,length(names_),repetitions_)
for (name_ in names_)  #para cada banco
{
  
  for (repetition_ in 1:repetitions_)  #para cada repetition_ da amostragem
  {
    
    table_noise_missing[row,repetition_]=compare_acp_sim(dataset_=name_,
                                                         datasets_=original_data2,
                                                         measures_=digital_data2,
                                                         acp=components_,
                                                         units_data=units_data,
                                                         noise_ = TRUE,
                                                         with_missing = TRUE)
    
    table_noise_nomissing[row,repetition_]=compare_acp_sim(dataset_=name_,
                                                           datasets_=original_data2,
                                                           measures_=digital_data2,
                                                           acp=components_,
                                                           units_data=units_data,
                                                           noise_ = TRUE,
                                                           with_missing = FALSE)
    
    table_nonoise_missing[row,repetition_]=compare_acp_sim(dataset_=name_,
                                                           datasets_=original_data2,
                                                           measures_=digital_data2,
                                                           acp=components_,
                                                           units_data=units_data,
                                                           noise_ = FALSE,
                                                           with_missing = TRUE)
    
  }
  row=row+1
}

saveRDS(table_noise_missing,"../RDS/table_PCASimilarity_noise_mask.RDS")
saveRDS(table_nonoise_missing,"../RDS/table_PCASimilarity_nonoise_mask.RDS")
saveRDS(table_noise_nomissing,"../RDS/table_PCASimilarity_noise_nomask.RDS")
