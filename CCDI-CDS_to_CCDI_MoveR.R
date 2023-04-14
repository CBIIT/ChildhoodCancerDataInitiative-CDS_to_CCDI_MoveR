#!/usr/bin/env Rscript

#Childhood Cancer Data Initiative - CDS_to_CCDI_MoveR.R

#This script will take a CDS metadata manifest file (flat) and move the contents to a CCDI metadata manifest (tabular).

##################
#
# USAGE
#
##################

#Run the following command in a terminal where R is installed for help.

#Rscript --vanilla CDS_to_CCDI_MoveR.R --help


##################
#
# Env. Setup
#
##################

#List of needed packages
list_of_packages=c("readr","dplyr","tidyr","knitr","openxlsx","stringi","readxl","janitor","optparse","tools")

#Based on the packages that are present, install ones that are required.
new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
suppressMessages(if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org"))

#Load libraries.
suppressMessages(library(readr,verbose = F))
suppressMessages(library(dplyr,verbose = F))
suppressMessages(library(tidyr,verbose = F))
suppressMessages(library(knitr,verbose = F))
suppressMessages(library(readxl,verbose = F))
suppressMessages(library(openxlsx, verbose = F))
suppressMessages(library(stringi,verbose = F))
suppressMessages(library(janitor,verbose = F))
suppressMessages(library(optparse,verbose = F))
suppressMessages(library(tools,verbose = F))


#remove objects that are no longer used.
rm(list_of_packages)
rm(new.packages)


##################
#
# Arg parse
#
##################

#Option list for arg parse
option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL, 
              help="dataset file, CDS_submission_metadata_template.xlsx", metavar="character"),
  make_option(c("-t", "--template"), type="character", default=NULL, 
              help="dataset template file, CCDI_submission_metadata_template.xlsx", metavar="character")
)

#create list of options and values for file input
opt_parser = OptionParser(option_list=option_list, description = "\nCCDI-CDS_to_CCDI_MoveR v1.0.1")
opt = parse_args(opt_parser)

#If no options are presented, return --help, stop and print the following message.
if (is.null(opt$file)&is.null(opt$template)){
  print_help(opt_parser)
  cat("Please supply both the input file (-f) and template file (-t), CCDI_submission_metadata_template.xlsx.\n\n")
  suppressMessages(stop(call.=FALSE))
}


#Data file pathway
file_path=file_path_as_absolute(opt$file)

#Template file pathway
template_path=file_path_as_absolute(opt$template)

cat("\nThe CDS data template is being moved to a CCDI template at this time.\n")

#Obtain template version
version=suppressMessages(read_xlsx(path = template_path,col_names = FALSE, trim_ws = TRUE, sheet = "README and INSTRUCTIONS", guess_max = 1000000, col_types = "text"))[1,3][[1]]



###########
#
# File name rework
#
###########

#Rework the file path to obtain a file extension.
file_name=stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][2])
ext=tolower(stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][1]))
path=paste(dirname(file_path),"/",sep = "")

#Output file name based on input file name and date/time stamped.
output_file=paste(file_name,
                  "_",
                  version,
                  "_MoveR",
                  stri_replace_all_fixed(
                    str = Sys.Date(),
                    pattern = "-",
                    replacement = ""),
                  sep="")


##############
#
# Read in each tab and apply to a data frame list
#
##############

#determine sheets in submission template
sheet_names_temp=readxl::excel_sheets(path = template_path)
non_template_sheets=c("README and INSTRUCTIONS","Dictionary","Terms and Value Sets")
sheet_names_temp=sheet_names_temp[!sheet_names_temp %in% non_template_sheets]

# A bank of NA terms to make sure NAs are brought in correctly
NA_bank=c("NA","na","N/A","n/a","")


if (ext == "tsv"){
  df=suppressMessages(read_tsv(file = file_path, trim_ws = TRUE, na=NA_bank, guess_max = 1000000, col_types = cols(.default = col_character())))
}else if (ext == "csv"){
  df=suppressMessages(read_csv(file = file_path, trim_ws = TRUE, na=NA_bank, guess_max = 1000000, col_types = cols(.default = col_character())))
}else if (ext == "xlsx"){
  df=suppressMessages(read_xlsx(path = file_path, trim_ws = TRUE, na=NA_bank, sheet = "Metadata", guess_max = 1000000, col_types = "text"))
}else{
  stop("\n\nERROR: Please submit a data file that is in either xlsx, tsv or csv format.\n\n")
}


#Establish the list for the template file
workbook_list_temp=list()

#create a list of all node pages with data
for (node in sheet_names_temp){
  #read the sheet
  df_temp=suppressMessages(read_xlsx(path = template_path, trim_ws = TRUE, na=NA_bank, sheet = node, guess_max = 1000000, col_types = "text"))
  #if there are at least one row in the resulting data frame, add it
  if (dim(df_temp)[1]>0){
      #add the data frame to the workbook
      workbook_list_temp=append(x = workbook_list_temp,values = list(df_temp))
      names(workbook_list_temp)[length(workbook_list_temp)]<-node
  }
}

nodes_present_temp=names(workbook_list_temp)

temp_node_prop=names(unlist(x = workbook_list_temp,recursive = TRUE))
temp_node_prop_df=tibble(temp_node_prop=temp_node_prop)

temp_node_prop_df=temp_node_prop_df%>%
  separate(temp_node_prop,c("temp_node","temp_prop"),"\\.",extra = "merge")

cds_props=colnames(df)

df_template_transfer_base=tibble(cds_prop="",ccdi_prop="",ccdi_node="",action="",special_action="")
df_template_transfer=df_template_transfer_base[0,]


#create empty df that notes placements that are not 1:1 between models.
prop_no_placements=c()


sink(file = paste(path,output_file,"_log.txt",sep = ""))

cat("The following output will note 'ERRORS' for a property that was not placed within the new template.\n-----------\n")

for (x in 1:length(cds_props)){
  df_template_transfer_add=df_template_transfer_base
  cds_prop=cds_props[x]
  df_template_transfer_add$cds_prop=cds_prop
  
  if(any(!is.na(df[cds_prop]))){
    
    #SPECIAL PROPERTY CHANGES
    #The mappings between CDS and CCDI require that certain properties obtain different names. This section of if/else will check to see if that is the property and then change its value to CCDI values.
    cds_prop_check=FALSE
    if (cds_prop=='primary_investigator_name'){
      cds_prop="personnel_name"
      cds_prop_check=TRUE
      df_template_transfer_add$special_action="hard_coded_translation"
    }else if(cds_prop=='primary_investigator_email'){
      cds_prop="email_address"
      cds_prop_check=TRUE
      df_template_transfer_add$special_action="hard_coded_translation"
    }else if(cds_prop=='sample_anatomic_site'){
      cds_prop="anatomic_site"
      cds_prop_check=TRUE
      df_template_transfer_add$special_action="hard_coded_translation"
    }else if(cds_prop=='bases'){
      cds_prop="number_of_bp"
      cds_prop_check=TRUE
      df_template_transfer_add$special_action="hard_coded_translation"
    }else if(cds_prop=='primary_diagnosis'){
      cds_prop="diagnosis_finer_resolution"
      cds_prop_check=TRUE
      df_template_transfer_add$special_action="hard_coded_translation"
    }
    
    if (cds_prop %in% temp_node_prop_df$temp_prop){
      ccdi_node_pos=grep(pattern = TRUE, x = temp_node_prop_df$temp_prop %in% cds_prop)
      ccdi_node=temp_node_prop_df$temp_node[ccdi_node_pos]
      ccdi_prop=cds_prop
      df_template_transfer_add$ccdi_prop=ccdi_prop
      
      
      #revert to old cds_prop value now that it has gotten past the check of same value in template.
      if (cds_prop_check){
        cds_prop=cds_props[x]
      }
      
      #specific add to make sure that sample_anatomic_site is placed in the sample node
      if (cds_prop_check & cds_prop=="sample_anatomic_site"){
        ccdi_node="sample"
        df_template_transfer_add$special_action=paste(c(df_template_transfer_add$special_action,"assumed_node"), collapse = ";")
      }
      
      if (length(ccdi_node)>1){
        if ("sequencing_file" %in% ccdi_node){
          ccdi_node="sequencing_file"
          #add to the special_action column
          if (df_template_transfer_add$special_action==""){
            df_template_transfer_add$special_action="assumed_node"
          }else{
            df_template_transfer_add$special_action=paste(c(df_template_transfer_add$special_action,"assumed_node"), collapse = ";")
          }
        }else if("diagnosis" %in% ccdi_node){
          ccdi_node="diagnosis"
          if (df_template_transfer_add$special_action==""){
            df_template_transfer_add$special_action="assumed_node"
          }else{
            df_template_transfer_add$special_action=paste(c(df_template_transfer_add$special_action,"assumed_node"), collapse = ";")
          }
        }else{
          prop_no_placements=c(prop_no_placements,cds_prop)
        }
      }
      
      df_template_transfer_add$ccdi_node=ccdi_node
      
      #check to make sure the data frames are the same length
      df_length=length(df[cds_prop][[1]])
      template_length=dim(workbook_list_temp[ccdi_node][[1]])[1]
      if(df_length!=template_length){
        #create a stand in matrix to replace with the correct amount if the lengths are different, usually happens for the initial data frame creation
        replace_df=as.data.frame(matrix(nrow=df_length,ncol = dim(workbook_list_temp[ccdi_node][[1]])[2]))
        colnames(replace_df)<-names(workbook_list_temp[ccdi_node][[1]])
        replace_df$type=ccdi_node
        workbook_list_temp[ccdi_node][[1]]=replace_df
      }
      
      
      workbook_list_temp[ccdi_node][[1]][ccdi_prop]=df[cds_prop]
      
      df_template_transfer_add$action='transfered'
      
      df_template_transfer=rbind(df_template_transfer,df_template_transfer_add)
      
    }else{
      
      #Make of list of not transferred properties
      prop_no_placements=c(prop_no_placements,cds_prop)
      df_template_transfer_add$action='not_transfered'
      df_template_transfer=rbind(df_template_transfer,df_template_transfer_add)
      
    }
  }
}


cat(paste("\nERROR: The following property, ",prop_no_placements,", did not find a placement in the new template.", sep=""))



#Write out of data frame to explain what was placed 
log_table_out=kable(df_template_transfer)

cat("\n\nThe following table is an output of a CCDI template workbook nodes and properties that were moved from the CDS data file properties or did not find a place in the new template.",
    "-----------\n",
    log_table_out,
    sep = "\n")


cat("\nSpecial Actions:\n-----------\nHard_coded_translation: a special placement of a value that is based on a priori knowledge of both the CDS and CCDI template and interactions.\nAssumed_node: a special placement when a property might occur in multiple tabs, but has been moved to the shown tab.\n")

file_nodes_transfered=unique(df_template_transfer$ccdi_node[grep(pattern = "_file", x = df_template_transfer$ccdi_node)])

if (length(file_nodes_transfered>1)){
  cat("\n######################################################################################\nThe following transfer of data has file metadata that occur in two different file tabs.\nPlease ensure that the correct file rows have been placed in their respective tabs.\n######################################################################################\n")
}

for (node in sheet_names_temp){
  #df=readWorkbook(xlsxFile = file_path,sheet = node, na.strings = NA_bank)
  #create an emptier version that removes the type and makes everything a character
  workbook_list_df=workbook_list_temp[node][[1]]
  df_empty_test=workbook_list_df%>%
    select(-type)%>%
    mutate(across(everything(), as.character))
  #remove empty rows and columns
  df_empty_test=remove_empty(df_empty_test,c("rows","cols"))
  
  #if there are at least one row in the resulting data frame, add it
  if (dim(df_empty_test)[1]>0){
    linking_props=colnames(workbook_list_df)[grepl(pattern = "\\.",x = colnames(workbook_list_df))]
    
    
    #for each linking property
    for (linking_prop in linking_props){
      #if that property is completely empty
      if (all(is.na(workbook_list_df[linking_prop]))){
        #determine the equivilent property in the CDS file
        df_prop=stri_split_fixed(str = linking_prop, pattern = ".")[[1]][2]
        #if that CDS prop exist, then apply those rows to the CCDI df
        if (df_prop %in% colnames(df)){
          workbook_list_df[linking_prop]=df[df_prop]
        }
      }
    }
    #Write out the df back to the workbook.
    workbook_list_temp[node][[1]]=workbook_list_df
  }
}



sink()

#Since the properties came from a flattened file, it is likely that splitting out information into tabs will create a decent number of redundant rows. Doing a unique will reduce the size and redundancy of the information.

for (node in sheet_names_temp){
  
  workbook_list_temp[node][[1]]=unique(workbook_list_temp[node][[1]])
  
}


###############
#
# Write out
#
###############

#Write out file

wb=openxlsx::loadWorkbook(file = template_path)

cat("\n\nWriting out the MoveR file.\n")

#progress bar
pb=txtProgressBar(min=0,max=length(sheet_names_temp),style = 3)
x=0

for (node in sheet_names_temp){
  x=x+1
  setTxtProgressBar(pb,x)
  df=workbook_list_temp[node][[1]]
  openxlsx::deleteData(wb, sheet = node,rows = 1:(dim(df)[1]+1),cols=1:(dim(df)[2]+1),gridExpand = TRUE)
  openxlsx::writeData(wb=wb, sheet=node, df)
  openxlsx::saveWorkbook(wb = wb,file = paste(path,output_file,".xlsx",sep = ""), overwrite = T)
}



cat(paste("\n\nProcess Complete.\n\nThe output file can be found here: ",path,"\n\n",sep = "")) 
