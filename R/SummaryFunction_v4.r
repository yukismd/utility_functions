#############################################################################
## Utility function to return column summary depending on column data type ##
## input: data.frame, output: data.frame                                   ##
## v1: 2018/01/13                                                          ##
## v2: 2018/01/14                                                          ##
##    - Fix the error when input data's data type has sigle column         ##
## v3: 2018/01/15                                                          ##
##    - Data column summary is implemented                                 ##
## v3_1: 2018/01/15                                                        ##
##    - POSIXct POSIXt column summary is implemented                       ##
## v4: 2018/01/16                                                          ##
##    - N Blank count for "character" type is added                        ##
#############################################################################

library(dplyr)
library(lubridate)

# All column(vector) summary function 
AllVectorSummary <- function(col_all, n_row){
  n_null <- sum(is.na(col_all))
  if(class(col_all) == "character"){
    n_blank <- sum(col_all=="")
  }else{
    n_blank <- 0
  }
  num <- n_row-n_null-n_blank
  #return( data.frame(N=num, N_Null=n_null) )
  return( c(num, n_null, n_blank) )
}

# Categorical column(vector) summary function 
charVectorSummary <- function(col_c, n_row){
  n_category <- n_distinct(col_c)
  category_per <- n_category/n_row
  #return( data.frame(N_Category=n_category, Pct_Uniq_category=Category_per) )
  return( c(n_category, category_per) )
}

# Numeric column(vector) summary function 
numVectorSummary <- function(col_n){
  minimum <- min(col_n, na.rm=TRUE)
  qtl <- quantile(col_n, probs=c(.01, .05, .1, .25, .4, .5, .6, .75, .9, .95, .99), na.rm=TRUE)
  q1 <- qtl[1]
  q5 <- qtl[2]
  q10 <- qtl[3]
  q25 <- qtl[4]
  q40 <- qtl[5]
  med <- qtl[6]
  q60 <- qtl[7]
  q75 <- qtl[8]
  q90 <- qtl[9]
  q95 <- qtl[10]
  q99 <- qtl[11]
  maximum <- max(col_n, na.rm=TRUE)
  avg <- mean(col_n, na.rm=TRUE)
  std <- sd(col_n, na.rm=TRUE)
  return( c(minimum, q1, q5, q10, q25, q40, med, q60, q75, q90, q95, q99, maximum, avg, std) )
}

# Date column(vector) summary function 
dateVectorSummary <- function(col_d){
  t_min <- as.character(min(col_d, na.rm=TRUE))
  t_med <- as.character(median(col_d, na.rm=TRUE))
  t_max <-  as.character(max(col_d, na.rm=TRUE))
  return( c(t_min, t_med, t_max) )
}

ColumnSummary <- function(dataframe){
  col_name <- names(dataframe)
  n_row <- nrow(dataframe)
  
  # get data type
  c_type <- sapply(dataframe, class)
  col_type = c()
  for(clmn in c_type){
    #print(clmn)
    if(length(clmn)==1){
      col_type <- append(col_type, clmn)
    }else if(length(clmn)>1){
      col_type <- append(col_type, paste(clmn[1],clmn[2]))
    }else{
      col_type <- append(col_type, "")
    }
  }
  
  ## All data set
  # Apply AllVectorSummary function
  df_all_summary <- sapply(dataframe, AllVectorSummary, n_row=n_row)
  df_all_summary <- data.frame(t(df_all_summary))
  names(df_all_summary) <- c("N", "N_Null", "N_Blank")
  df_all_summary["Col_Name"] <- names(dataframe)
  
  ## Categorical data set
  char_type_col <- col_type %in% c("character","logical","factor")
  if(sum(char_type_col)>=1){
    df_char <- dataframe[char_type_col]
    
    # Apply charVectorSummary function
    df_char_summary <- sapply(df_char, charVectorSummary, n_row=n_row)
    df_char_summary <- data.frame(t(df_char_summary))
    names(df_char_summary) <- c("N_Category", "Pct_Uniq_category")
    df_char_summary["Col_Name"] <- names(df_char)
  }
  else{  # Define when categorical column does not exist
    df_char_summary <- data.frame(Col_Name=col_name, stringsAsFactors=FALSE)
  }
  
  ## Numerical data set
  num_type_col <- col_type %in% c("integer","numeric")
  if(sum(num_type_col)>=1){
    df_num <- dataframe[num_type_col]
    
    # Apply numVectorSummary function
    df_num_summary <- sapply(df_num, numVectorSummary)
    df_num_summary <- data.frame(t(df_num_summary))
    names(df_num_summary) <- c("Min", "Q1", "Q5", "Q10", "Q25", "Q40", "Med", "Q60", "Q75", "Q90", 
                               "Q95", "Q99", "Max", "Mean", "SD")
    df_num_summary["Col_Name"] <- names(df_num)
  }
  else{  # Define when numerical column does not exist
    df_num_summary <- data.frame(Col_Name=col_name, stringsAsFactors=FALSE)
  }
  
  ## Date data set
  # for date type summary
  date_type_col <- col_type %in% c("Date", "POSIXct POSIXt")
  if(sum(date_type_col)>=1){
    df_date <- dataframe[date_type_col]
    
    # Apply dateVectorSummary function
    df_date_summary <- sapply(df_date, dateVectorSummary)
    df_date_summary <- data.frame(t(df_date_summary))
    names(df_date_summary) <- c("T_Start", "T_Median", "T_End")
    df_date_summary["Col_Name"] <- names(df_date)
  }
  else{  # Define when numerical column does not exist
    df_date_summary <- data.frame(Col_Name=col_name, stringsAsFactors=FALSE)
  }
  
  # Join all data sets
  df_final <- data.frame(Col_Name=col_name, Data_Type=col_type, stringsAsFactors=FALSE)
  df_final <- left_join(df_final, df_all_summary, by="Col_Name")
  df_final <- left_join(df_final, df_char_summary, by="Col_Name")
  df_final <- left_join(df_final, df_num_summary, by="Col_Name")
  df_final <- left_join(df_final, df_date_summary, by="Col_Name")
  
  return( df_final )
}


# Example
# ColumnSummary(data.frame)


