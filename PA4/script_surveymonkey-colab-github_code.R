################################ INFO ############################################################
# ~ Need to save in UTF-8 encoding and File/"Reopen with Encoding" to preserve Romanian characters.
# ~ Regarding the above mentioned, the script should be sourced as follows:
#     source_encod <- function(f, encoding = 'UTF-8') {
#       l <- readLines(f, encoding = encoding)
#       eval(parse(text = l), envir = .GlobalEnv)
#     }    # source(x, encoding = 'UTF-8') # doesn't work every time
#
#     source_github_encod <- function(url, encoding = 'UTF-8') {
#       raw <- paste(url, "?raw=TRUE", sep = "", collapse = "")
#       l <- readLines(raw, encoding = encoding)
#       eval(parse(text = l), envir = .GlobalEnv)
#     }       #devtools::source_url(x, encoding = 'UTF-8')   #  doesn't work every time
#
#  
# ~ 
# ~ 
##################################################################################################


########################### SCORING INFO #########################################################

# SIG
# “no” = 0, “?” = 1.5, “yes” = 3
# this is SIG-r 2011 ... unidimensioal scale

# MBI-GS - use primarly Exhaustion & Cynicism
# Likert 7 trepte unde: 0 = Niciodată și 6 = Zilnic
# all negative worded items were rephrased ... no reverse coded items
# emotional exhaustion (MBI-EX - Epuizare: 1, 3, 5, 11, 14
# cynicism (MBI-CY) - Cinism: 2, 7, 8, 13, 15
# professional efficacy (MBI-PE) - Ineficienta profesionala: 4, 6, 9, 10, 12, 16
# TOTAL: toti itemii
#  The MBI-GS cut-off scores for severe burnout are mean sum scores ≥ 2.20 on the MBI-EX and ≥ 2.00 on the MBI-CY, or ≤ 3.66 on the MBI-PE

################################ SETTINGS ########################################################

cutoffPCL <- 32   # literature: 31-33 or 38 
algPCL <- data.frame(B = 1, C = 1, D = 2, E = 2)
algPCL_subclin <- data.frame(B = 1, C = 1, D = 1, E = 1)

cutoffMBI_Ex <- 2.20  
cutoffMBI_Cy <- 2

##################################################################################################

################################ PROCESS DATA ####################################################

# Define column index:  index = col index; itemindex = index of item in questionnaire
indexSIG <- 39:46
indexMBI <- 47:62
indexPCL <- 138:157
itemindexMBI_Ex <- c(1, 3, 5, 11, 14) 
itemindexMBI_Cy <- c(2, 7, 8, 13, 15) 
itemindexMBI_Pe <- c(4, 6, 9, 10, 12, 16) 

# Rename columns
names(Data)[1:12] <- stringr::str_replace_all(names(Data)[1:12], "[[:blank:]]", "_")
names(Data)[13] <- "Real_Email"
names(Data)[14] <- "Real_Tel"
names(Data)[15] <- "Real_Name"
names(Data)[30] <- "Real_Age_categ"
names(Data)[31] <- "Real_Sex"

names(Data)[names(Data) %in% names(Data[, indexSIG])]  <- c(sprintf("SIG_%01d", seq(1, 8)))
names(Data)[names(Data) %in% names(Data[, indexMBI])]  <- c(sprintf("MBI_%01d", seq(1, 16)))
names(Data)[names(Data) %in% names(Data[, indexPCL])]  <- c(sprintf("PCL_%01d", seq(1, 20)))


Data <-
  Data %>%
  dplyr::filter(Response_Status == "completed")                          # select only complete cases
 

# Recode 
## Define function that recodes to numeric, but watches out to coercion to not introduce NAs
colstonumeric <- function(df){
  tryCatch({
    df_num <- as.data.frame(
      lapply(df,
             function(x) { as.numeric(as.character(x))})) 
  },warning = function(stop_on_warning) {
    message("Stoped the execution of numeric conversion: ", conditionMessage(stop_on_warning))
  }) 
}
##
## Define function that reverse codes items
ReverseCode <- function(df, tonumeric = FALSE, min = NULL, max = NULL) {
  if(tonumeric) df <- colstonumeric(df)
  df <- (max + min) - df
}
##

# lapply(Data[,indexPCL], unique)
Data[ ,indexPCL][ Data[ ,indexPCL] == "deloc"] <- "0"
Data[ ,indexPCL][ Data[ ,indexPCL] == "puțin"] <- "1"
Data[ ,indexPCL][ Data[ ,indexPCL] == "moderat"] <- "2"
Data[ ,indexPCL][ Data[ ,indexPCL] == "mult"] <- "3"
Data[ ,indexPCL][ Data[ ,indexPCL] == "foarte mult"] <- "4"
Data[ ,indexPCL][ Data[ ,indexPCL] == "Not Answered"] <- NA

Data[ ,indexSIG][ Data[ ,indexSIG] == "NU"] <- "0"
Data[ ,indexSIG][ Data[ ,indexSIG] == "?"] <- "1.5"
Data[ ,indexSIG][ Data[ ,indexSIG] == "DA"] <- "3"
Data[ ,indexSIG][ Data[ ,indexSIG] == "Not Answered"] <- NA

Data[ ,indexMBI] <- data.frame(lapply(Data[ ,indexMBI], 
                                      function(x) {gsub(".*Niciodat.*", "0", x)}), stringsAsFactors = FALSE)
Data[ ,indexMBI] <- data.frame(lapply(Data[ ,indexMBI], 
                                      function(x) {gsub(".*Zilnic.*", "6", x)}), stringsAsFactors = FALSE)
Data[ ,indexMBI][ Data[ ,indexMBI] == "Not Answered"] <- NA


Data[, indexSIG] <- colstonumeric(Data[, indexSIG])
Data[, indexMBI] <- colstonumeric(Data[, indexMBI])
Data[, indexPCL] <- colstonumeric(Data[, indexPCL])



# Scores
## Define function that scores only rows with less than 10% NAs (returns NA if all or above threshold percentage of rows are NA); can reverse code if vector of column indexes and min, max are provided.
ScoreLikert <- function(df, stat = "sum", natozero = FALSE, napercent = .1, tonumeric = FALSE, reversecols = NULL, min = NULL, max = NULL) {
  reverse_list <- list(reversecols = reversecols, min = min, max = max)
  reverse_check <- !sapply(reverse_list, is.null)
  
  # Recode to numeric, but watch out to coercion to not introduce NAs
  colstonumeric <- function(df){
    tryCatch({
      df_num <- as.data.frame(
        lapply(df,
               function(x) { as.numeric(as.character(x))})) 
    },warning = function(stop_on_warning) {
      message("Stoped the execution of numeric conversion: ", conditionMessage(stop_on_warning))
    }) 
  }
  
  if(tonumeric) df <- colstonumeric(df)
  
  if(all(reverse_check)){
    df[ ,reversecols] <- (max + min) - df[ ,reversecols]
  }else if(any(reverse_check)){
    stop("Insuficient info for reversing. Please provide: ", paste(names(reverse_list)[!reverse_check], collapse = ", "))
  }
  
  if(tonumeric) df <- colstonumeric(df)                           
  
  if(natozero) df[is.na(df)] <- 0                                    # NAs to 0 can help when stat = "mean" with na.rm = T because it keeps denominator constant
  
  if(stat == "sum"){
  df_res <- ifelse(rowSums(is.na(df)) > ncol(df) * napercent,
                   NA,
                   rowSums(df, na.rm = TRUE) * NA ^ (rowSums(!is.na(df)) == 0))
  }
  if(stat == "mean"){
    df_res <- ifelse(rowSums(is.na(df)) > ncol(df) * napercent,
                     NA,
                     rowMeans(df, na.rm = TRUE) * NA ^ (rowSums(!is.na(df)) == 0))
  }
  return(df_res)
}
##


# Score PCL
Data$PCL_Total <- ScoreLikert(Data[, indexPCL], napercent = .3)               # NA if NA threshold is exceeded 
Data$PCL_B <- ScoreLikert(Data[, c(sprintf("PCL_%01d", 1:5))], napercent = 1)    # do nothing if NA threshold is exceeded
Data$PCL_C <- ScoreLikert(Data[, c(sprintf("PCL_%01d", 6:7))], napercent = 1) 
Data$PCL_D <- ScoreLikert(Data[, c(sprintf("PCL_%01d", 8:14))], napercent = 1)  
Data$PCL_E <- ScoreLikert(Data[, c(sprintf("PCL_%01d", 15:20))], napercent = 1)

# Score SIG
Data$SIG_Total <- ScoreLikert(Data[, indexSIG], napercent = .3)

# Score MBI
Data$MBI_Total <- ScoreLikert(Data[, indexMBI], napercent = .3, stat = "mean", natozero = TRUE)
Data$MBI_Ex <- ScoreLikert(Data[, c(sprintf("MBI_%01d", itemindexMBI_Ex))], napercent = 1, stat = "mean", natozero = TRUE)
Data$MBI_Cy <- ScoreLikert(Data[, c(sprintf("MBI_%01d", itemindexMBI_Cy))], napercent = 1, stat = "mean", natozero = TRUE)
Data$MBI_Pe <- ScoreLikert(Data[, c(sprintf("MBI_%01d", itemindexMBI_Pe))], napercent = 1, stat = "mean", natozero = TRUE)


# PCL Diagnostic Algorithm
itemsPCL_B <- c(sprintf("PCL_%01d", 1:5))
itemsPCL_C <- c(sprintf("PCL_%01d", 6:7))
itemsPCL_D <- c(sprintf("PCL_%01d", 8:14))
itemsPCL_E <- c(sprintf("PCL_%01d", 15:20))

DataPCLAlg <-  
  Data %>% 
  dplyr::select(tidyselect::all_of(indexPCL)) %>% 
  dplyr::mutate_all(
    ~(case_when(
      . >=2 ~ 1,
      # . <2 ~ 0,
      is.na(.) ~ 0,
      TRUE  ~  0))) %>% 
  
  mutate(PCL_CritB = case_when(rowSums(.[,itemsPCL_B], na.rm = TRUE) >= algPCL$B ~ 1,      # algPCL <- data.frame(B = 1, C = 1, D = 2, E = 2)
                               # rowSums(.[,itemsPCL_B], na.rm = TRUE) <1 ~ 0,
                               TRUE  ~  0)) %>% 
  mutate(PCL_CritC = case_when(rowSums(.[,itemsPCL_C], na.rm = TRUE) >= algPCL$C ~ 1,    
                               # rowSums(.[,itemsPCL_C], na.rm = TRUE) <1 ~ 0,
                               TRUE  ~  0)) %>% 
  mutate(PCL_CritD = case_when(rowSums(.[,itemsPCL_D], na.rm = TRUE) >= algPCL$D ~ 1,   
                               # rowSums(.[,itemsPCL_D], na.rm = TRUE) <1 ~ 0,
                               TRUE  ~  0)) %>% 
  mutate(PCL_CritE = case_when(rowSums(.[,itemsPCL_E], na.rm = TRUE) >= algPCL$E ~ 1,    
                               # rowSums(.[,itemsPCL_E], na.rm = TRUE) <1 ~ 0,
                               TRUE  ~  0)) %>% 
  mutate(PCL_Alg = case_when(PCL_CritB == 1 & PCL_CritC == 1 & PCL_CritD == 1 & PCL_CritE == 1 ~ 1,
                             TRUE  ~  0)) 

Data$PCLAlg <- DataPCLAlg$PCL_Alg

DataPCLAlg_subclin <-  
  Data %>% 
  dplyr::select(tidyselect::all_of(indexPCL)) %>% 
  dplyr::mutate_all(
    ~(case_when(
      . >=2 ~ 1,
      # . <2 ~ 0,
      is.na(.) ~ 0,
      TRUE  ~  0))) %>% 
  
  mutate(PCL_CritB = case_when(rowSums(.[,itemsPCL_B], na.rm = TRUE) >= algPCL_subclin$B ~ 1,  
                               # rowSums(.[,itemsPCL_B], na.rm = TRUE) <1 ~ 0,
                               TRUE  ~  0)) %>% 
  mutate(PCL_CritC = case_when(rowSums(.[,itemsPCL_C], na.rm = TRUE) >= algPCL_subclin$C ~ 1,    
                               # rowSums(.[,itemsPCL_C], na.rm = TRUE) <1 ~ 0,
                               TRUE  ~  0)) %>% 
  mutate(PCL_CritD = case_when(rowSums(.[,itemsPCL_D], na.rm = TRUE) >= algPCL_subclin$D ~ 1,   
                               # rowSums(.[,itemsPCL_D], na.rm = TRUE) <1 ~ 0,
                               TRUE  ~  0)) %>% 
  mutate(PCL_CritE = case_when(rowSums(.[,itemsPCL_E], na.rm = TRUE) >= algPCL_subclin$E ~ 1,    
                               # rowSums(.[,itemsPCL_E], na.rm = TRUE) <1 ~ 0,
                               TRUE  ~  0)) %>% 
  mutate(PCL_Alg_subclin = case_when(PCL_CritB == 1 & PCL_CritC == 1 & PCL_CritD == 1 & PCL_CritE == 1 ~ 1,
                                     TRUE  ~  0)) 

Data$PCLAlg_subclin <- DataPCLAlg_subclin$PCL_Alg_subclin

Data$MBI_Ex_cut <- ifelse(Data$MBI_Ex >= cutoffMBI_Ex, 1, 0)
Data$MBI_Cy_cut <- ifelse(Data$MBI_Cy >= cutoffMBI_Cy, 1, 0)


# global
df_screening <- Data[,c("Response_ID", "Real_Name", "Real_Tel", "Real_Email", "Collector_ID", "Real_Age_categ",
                        "SIG_Total", "MBI_Ex_cut", "MBI_Cy_cut",
                        "PCL_Total", "PCL_B", "PCL_C", "PCL_D", "PCL_E", "PCLAlg", "PCLAlg_subclin"
)]

