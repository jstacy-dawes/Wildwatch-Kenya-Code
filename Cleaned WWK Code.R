#Download R Version 3.5.1 from https://cran.r-project.org
#The following code is written for R Studio for MacOSX

rm(list=ls()) #Clears the working space to start fresh

R.Version() #Tests the version of R that you're working in


#ONLY FOR THE FIRST TIME YOU ARE RUNNING THIS CODE: Install the following packages
install.packages("tidyjson")
install.packages("magrittr")
install.packages("jsonlite")
install.packages("dplyr")
install.packages("stringr")
install.packages("tidyr")
install.packages("reshape")
install.packages("data.table")
install.packages("curl")
install.packages("knitr")


#CONTINUE WITH RUNNING THE FOLLOWING EVERY TIME:
library(tidyjson)
library(magrittr)
library(jsonlite)
library(dplyr)
library(stringr)
library(tidyr)
library(reshape)
library(data.table)
library(curl)
library(knitr)

#Download the Classification Data and Subject Data from https://www.zooniverse.org/lab/2789/data-exports
#Download "Classification Data" for all the project's data, and download "Workflow Classifications" for a certain workflows data

#Set the path to retrieve the files downloaded from Zooniverse
#Ideal to set up a new folder containing these files
setwd("~/Desktop/R Work/Cleaned Codes/WWK")

projectID=2789

#***IMPORTANT STEP!!!***

#Are you analyzing one workflow version, or data from the entire project?
#If one workflow version, enter "TRUE" and type the workflow name into the quotation marks next to workflow_name
#If analyzing data from the entire project, enter "FALSE"

one_workflow<-TRUE #***IMPORTANT STEP!!!***

if(one_workflow=="TRUE"){
  workflow_name<-"workflow3"
} else {
  workflow_version_numbers<-as.character(c(307.77,319.78,332.79,1.0,1.1)) #IMPORTANT: Must add to the list within the c() with every new workflow version number***
  #So far, the correct workflow versions are 307.77, 319.78, 332.79, 1.0, and 1.1
}


#Read in the data files with the document name
#Documents must be saved in the folder that the directory is set to
#Make sure to delete old downloads so you're not reading in old files
subjectdata<-read.csv("wildwatch-kenya-subjects.csv",stringsAsFactors = F)

if(one_workflow=="TRUE"){
  classificationdata<-read.csv(paste0(workflow_name,"-classifications.csv"),stringsAsFactors = F)
} else {
  classificationdata<-read.csv("wildwatch-kenya-classifications.csv",stringsAsFactors = F)
}

#Make sure the number of rows is correct/logical, it should be one row for every classification made
print(nrow(classificationdata))

#Only keep retired subjects
subjectdata<-subjectdata[subjectdata$retirement_reason>"",]

#Fix some errors
subjectdata$metadata<-gsub("\"OrigName\"","\"#OrigName\"",subjectdata$metadata)
subjectdata$metadata<-gsub("\"OrigFolder\"","\"#OrigFolder\"",subjectdata$metadata)
subjectdata$metadata<-gsub("‘name","name",subjectdata$metadata)
subjectdata$metadata<-gsub("origin’","origin",subjectdata$metadata)
subjectdata$locations<- gsub("\"","",subjectdata$locations)
subjectdata$locations<- gsub("{0:","",subjectdata$locations,fixed=TRUE)
subjectdata$locations<- gsub("}","",subjectdata$locations)


#Create flat subject table with the subject ids, the folder name, the image name, and the locations
t<- subjectdata$metadata %>% spread_values(subject_id=jstring("subject_id"),OrigName=jstring("name"),
                                           OrigFolder=jstring("origin"))
subjectflat<-cbind(subjectdata[,-c(5,11)],t[,-c(1,2)])
colnames(subjectflat)[1]<-"subject_ids"

#Below only necessary if analyzing the entire WWK classifications thus far (i.e. only necessary if one_workflow is FALSE)
#WWK missing some folder and file names, so we must use the Missing_Folders_Filled_In.csv to replace missing values
#Replace the NA values in OrigFolder and OrigName
#Read in the file that has the missing folders and image names filled in
if(one_workflow=="FALSE"){
  missing_folders<-read.csv("Missing_Folders_Filled_In.csv")
  
  #If df1$x == df2$x, replace df1$y with df2$y
  #i.e. if subjectflat subject ids match the subjectflat_filled_in subject ids, replace the subjectflat folder and file names with the subjectflat_filled_in folder and file names
  df1<-data.frame(subjectflat$subject_ids,subjectflat$OrigName,subjectflat$OrigFolder)
  colnames(df1)<-c("x","y","z")
  df2<-data.frame(missing_folders$subject_id,missing_folders$OrigName,missing_folders$OrigFolder)
  colnames(df2)<-c("x","y","z")
  setDT(df1)
  setDT(df2)
  df1[df2, on = .(x), `:=` (y = i.y, z = i.z)]
  
  #Replace the column in the original data frame
  subjectflat$OrigName<-df1$y
  subjectflat$OrigFolder<-df1$z 
} 

#Deal with Date Time
subjectflat$DateTimeOriginal<-subjectflat$DateTime
subjectflat$DateTime <- as.POSIXct(subjectflat$DateTime, format='%d/%m/%Y %H:%M:%S')
subjectflat[is.na(subjectflat$DateTime),]$DateTime <- as.POSIXct(subjectflat[is.na(subjectflat$DateTime),]$DateTimeOriginal, format='%Y:%m:%d %H:%M:%S')
subjectflat$Date<-format(subjectflat$DateTime,"%Y-%m-%d")
subjectflat$Time<-format(subjectflat$DateTime,"%H:%M:%S")

#Now add in a column with the latitude and longitude data
#You will need to add in the new folder names and the lat and long of each folder (aka location) into the Lat_and_Long_Data csv 
#This info comes from the CT Inventory

Lat_long<-read.csv("Lat_and_Long_Data.csv",stringsAsFactors = F)

subjectflat$Latitude<-"Unknown"
subjectflat$Longitude<-"Unknown"

subjectflat$Latitude <- sapply(subjectflat$OrigFolder,FUN=function(x){
    for(i in 1:nrow(Lat_long)){
      if (grepl(Lat_long$OrigFolder[i],x)){
        return(Lat_long$Latitude[i])
      } 
    }
    return("Unknown")
  })
  
subjectflat$Longitude <- sapply(subjectflat$OrigFolder,FUN=function(x){
    for(i in 1:nrow(Lat_long)){
      if (grepl(Lat_long$OrigFolder[i],x)){
        return(Lat_long$Longitude[i])
      } 
    }
    return("Unknown")
  })

##############Remove Duplicate Images
#Save a version of the previous subjectflat with duplicates incase you want to go back
subjectflat_old<-subjectflat

#If you need to go back to teh old version run the following code
#subjectflat<-subjectflat_old

#For images in workflow versions 2030, 5998, and 6831, remove images that contain a "_" in the image name
subjectflat <- filter(subjectflat,
                           !grepl('_', OrigName),
                           workflow_id == 2030|5998|6831)

#Save the new subject data file
write.csv(subjectflat,file="subjectflat.csv")

#Create the flattened classification dataframe
#Each row represents an individual classification

#Manipulate the data frame to spread all the values out (i.e. make it logically readable)
basic_flat_with_values <- classificationdata %>% filter(., workflow_version >=1) %>% 
  dplyr::select(., subject_ids, classification_id,user_name,user_id, workflow_version, created_at, annotations) %>%
  as.tbl_json(json.column = "annotations") %>%
  gather_array(column.name = "task_index") %>% #important for joining later
  spread_values(task = jstring("task"), task_label = jstring("task_label"), value = jstring("value")) 

basic_flat_with_values %>% data.frame %>% head

basic_flat_with_values<-basic_flat_with_values[basic_flat_with_values$task=="T3",]

classificationsummary <-  basic_flat_with_values %>% 
  gather_keys %>%
  append_values_string()
classificationsummary %>% data.frame %>% head 

classificationchoices <- basic_flat_with_values %>%
  enter_object("value") %>% json_lengths(column.name = "total_species") %>% 
  gather_array(column.name = "species_index") %>%
  spread_values(choice = jstring("choice"), answers = jstring("answers")) 

classificationchoices %>% data.frame %>% head 

classificationanswers <- classificationchoices %>% 
  enter_object("answers") %>% 
  spread_values(how_many = jstring("HOWMANY"),behavior=jstring("WHATBEHAVIORSDOYOUSEE"),young=jstring("ARETHEREANYYOUNGPRESENT"))
classificationanswers %>% data.frame %>% head    

#Combine the above to create your final classification table
add_choices <- left_join(basic_flat_with_values, classificationchoices)
tot <- left_join(add_choices, classificationanswers)

classificationflat <- tot %>% dplyr::select(., -task_index, -task_label, -value, -answers)
classificationflat %>% data.frame %>% head

#Deal with behavior
classificationflat$behavior.moving<-sapply(classificationflat$behavior,function(x)grepl("MOVING",x)*1) 
classificationflat$behavior.standing<-sapply(classificationflat$behavior,function(x)grepl("STANDING",x)*1) 
classificationflat$behavior.eating<-sapply(classificationflat$behavior,function(x)grepl("EATING",x)*1)    
classificationflat$behavior.resting<-sapply(classificationflat$behavior,function(x)grepl("RESTING",x)*1)    
classificationflat$behavior.interacting<-sapply(classificationflat$behavior,function(x)grepl("INTERACTING",x)*1)    

#Deal with empty choices 
classificationflat[is.na(classificationflat$how_many),]$how_many<-0
classificationflat[is.na(classificationflat$young),]$young<-"NO"
classificationflat[is.na(classificationflat$choice),]$choice<-"EMPTY"

#Remove unnecessary dataframes
rm(list=c("basic_flat_with_values","classificationchoices","classificationanswers","add_choices","tot"))

#Combine classification dataframe with the subject data
final_flat<-merge(classificationflat,subjectflat,by="subject_ids")


#Save your final classification dataframe
#One row for every classification made
#If you are only analyzing one workflow, it will save two versions of the classification dataframe, one for just that workflow, and one with the new workflow added to the previous workflow classification dataframe
#This creates a dataframe of all the classifications for the entire project, saved as wwk-classifications-flattened-all.csv
if(one_workflow=="TRUE"){
  write.csv(final_flat, file = paste0("wwk-classifications-flattened-",workflow_name,".csv"))
} else {
  write.csv(final_flat,file="wwk-classifications-flattened-all.csv")
}

#Create a dataframe with the 'best answers' for each photo
#Each row represents one photo

classificationsummary<-data.frame(subject_ids=sort(unique(classificationflat$subject_ids)),
                                  number_class=tapply(classificationflat$choice,classificationflat$subject_ids,function(x)length(x)), #number of classifications
                                  number_species=tapply(classificationflat$choice,classificationflat$subject_ids,function(x)length(unique(x))), #number of species
                                  perc_species=tapply(classificationflat$choice,classificationflat$subject_ids,function(x)max(table(x))/length(x)), #aggreement on best species
                                  best_species=tapply(classificationflat$choice,classificationflat$subject_ids,function(x)names(sort(table(x),decreasing = T))[1]), #"best vote" species/ consensus on species
                                  number_count=tapply(classificationflat$how_many,classificationflat$subject_ids,function(x)length(unique(x))), #number of different counts
                                  perc_count=tapply(classificationflat$how_many,classificationflat$subject_ids,function(x)max(table(x))/length(x)), #aggreement on count
                                  best_count=unlist(tapply(classificationflat$how_many,classificationflat$subject_ids,function(x)if(dim(table(x))>0){as.numeric(names(sort(table(x),decreasing = T))[1])}else{NA})), #best answer for count/ consensus on count
                                  perc_young=tapply(classificationflat$young,classificationflat$subject_ids,function(x)max(table(x))/length(x)), #aggreement on young
                                  best_young=tapply(classificationflat$young,classificationflat$subject_ids,function(x)names(sort(table(x),decreasing = T))[1]), #consensus on young
                                  perc_b_moving=tapply(classificationflat$behavior.moving,classificationflat$subject_ids,function(x)max(table(x))/length(x)), #aggreement on behavior,
                                  best_b_moving=tapply(classificationflat$behavior.moving,classificationflat$subject_ids,function(x)names(sort(table(x),decreasing = T))[1]),
                                  perc_b_standing=tapply(classificationflat$behavior.standing,classificationflat$subject_ids,function(x)max(table(x))/length(x)), 
                                  best_b_standing=tapply(classificationflat$behavior.standing,classificationflat$subject_ids,function(x)names(sort(table(x),decreasing = T))[1]),
                                  perc_b_eating=tapply(classificationflat$behavior.eating,classificationflat$subject_ids,function(x)max(table(x))/length(x)), 
                                  best_b_eating=tapply(classificationflat$behavior.eating,classificationflat$subject_ids,function(x)names(sort(table(x),decreasing = T))[1]),
                                  perc_b_resting=tapply(classificationflat$behavior.resting,classificationflat$subject_ids,function(x)max(table(x))/length(x)),
                                  best_b_resting=tapply(classificationflat$behavior.resting,classificationflat$subject_ids,function(x)names(sort(table(x),decreasing = T))[1]),
                                  perc_b_interacting=tapply(classificationflat$behavior.interacting,classificationflat$subject_ids,function(x)max(table(x))/length(x)), 
                                  best_b_interacting=tapply(classificationflat$behavior.interacting,classificationflat$subject_ids,function(x)names(sort(table(x),decreasing = T))[1]))

final_summary<-merge(classificationsummary,subjectflat,by="subject_ids")

#Save your final summary dataframe
#If you are only analyzing one workflow, it will save two versions of the classification summary dataframe, one for just that workflow, and one with the new workflow added to the previous workflow classification summary dataframe
#This creates a summary dataframe of all the images for the entire project, saved as wwk-classifications-summary-all.csv
#If you are analyzing all workflows from the entire project, it will just save one csv as "wwk-classifications-summary-all.csv"
#WARNING: SAVING THESE FILES WILL OVERWRITE PREVIOUS FILES THAT HAVE THE SAME NAME

if(one_workflow=="TRUE"){
  write.csv(final_summary, file = paste0("wwk-classifications-summary-",workflow_name,".csv"))
  recent_workflow<-final_summary
  past_workflow<-read.csv("wwk-classifications-summary-all.csv")
  common_cols <- intersect(colnames(recent_workflow), colnames(past_workflow))
  final_summary_all<- rbind(recent_workflow[, common_cols], past_workflow[, common_cols])
  final_summary_all<- subset(final_summary_all,!duplicated(final_summary_all$subject_ids))
  write.csv(final_summary_all,"wwk-classifications-summary-all.csv")
} else {
  write.csv(final_summary,file="wwk-classifications-summary-all.csv")
}


###############################ANALYSIS#######################################
setwd("~/Desktop/R Work/Cleaned Codes/WWK")

#If you would like to analyze results from the entire project up until current, type "All"
#If you would like to analyze results only from most recent workflow type the workflow version number in quotation marks
workflow<-"All"

if(workflow=="All"){
  recent_workflow<-read.csv(file = paste0("wwk-classifications-summary-",workflow_name,".csv"))
  past_workflow<-read.csv("wwk-classifications-summary-all.csv")
  common_cols <- intersect(colnames(recent_workflow), colnames(past_workflow))
  final_summary_all<- rbind(recent_workflow[, common_cols], past_workflow[, common_cols])
  final_summary_all<- subset(final_summary_all,!duplicated(final_summary_all$subject_ids))
} else{
  final_summary_all<-final_summary
}

#Input a column for Site (Namunyak or Loisaba)  
final_summary_all$Site<-final_summary_all$OrigFolder
final_summary_all$Site<-gsub("^[AaLl].*","Loisaba",final_summary_all$Site)
final_summary_all$Site<-gsub("^[NnSs].*","Namunyak",final_summary_all$Site)
table(final_summary_all$Site)

ct_locations<-data.frame(table(final_summary_all$Latitude,final_summary_all$Longitude))
ct_locations<-subset(ct_locations,ct_locations$Freq>10)

#Number of classifications made per species
species_classifications<-data.frame(table(unlist(final_flat$choice)))
print(species_classifications) #Prints the data frame below

#Save the list as a data frame if you'd like                                
write.csv(species_occurences, paste0("Wildwatch_Kenya_Species_Classifications",workflow,".csv"))

#Number of photos for each species, as determined by the 'best vote' 
best_species<-data.frame(table(final_summary_all$best_species))
best_species<-best_species[order(-best_species$Freq),]
print(best_species) #Prints the data frame below
#Save the list as a data frame if you'd like
write.csv(best_species, paste0("Wildwatch_Kenya_Best_Species",workflow,".csv"))

#Number of photos of each species separated by side (Loisaba or Namunyak)
Species_by_site<-data.frame(table(final_summary_all$best_species,by=final_summary_all$Site))

Species_by_site<-subset(Species_by_site,Species_by_site$Var1!="EMPTY")
Species_by_site<-subset(Species_by_site,Species_by_site$Var1!="HUMAN")
Species_by_site<-subset(Species_by_site,Species_by_site$Var1!="HUMANVEHICLE")
Species_by_site<-subset(Species_by_site,Species_by_site$Var1!="LIVESTOCK")

#Total number of species photos in Namunyak
with(Species_by_site, sum(Freq[by == "Namunyak"]))
#Total number of species photos in Loisaba
with(Species_by_site, sum(Freq[by == "Loisaba"]))

#Total number of young, as determined by the "best vote"
print(nrow(final_summary_all[final_summary_all$best_young=="YES",]))

#Number of people who contributed to classifying the images
length(unique(classificationflat$user_name))

#Analyze a specific species by filling the species name within the quotation marks
species<-"GIRAFFE"
specific_species<-final_summary_all[final_summary_all$best_species==species,]

#Number of individuals of that specific species
print(sum(specific_species$number_count))

#Number of images of that specific species per camera trap site
library(plyr)
locations<-count(specific_species, vars=c("Latitude","Longitude"))
#Save the images per camera site for mapping on GIS
write.csv(locations,paste0("Images_per_Site_",species,"_",workflow,".csv"))

#Save the data frame of images with only the specific species
#For viewing on topcat
write.csv(specific_species,paste0("photos_with_",species,"_",workflow,".csv"))

#Where were the predators seen?
predators<-c("LION","LEOPARD","HYENA","BATEAREDFOX","CIVET","MONGOOSE","GENET","WILDDOG","CHEETAH","HONEYBADGER","CARACAL","ZORILLA","SERVAL")
predator_locations<-count(final_summary_all[final_summary_all$best_species %in% predators,], vars=c("Latitude","Longitude"))

#List of volunteers who participated
volunteers<-data.frame(unique(final_flat$user_id))
#Number of active volunteers
print(nrow(volunteers))
#Save list of volunteers
write.csv(volunteers,paste0)


