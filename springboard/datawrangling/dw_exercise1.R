library(readxl)
library(dplyr)
library(xlsx)

#refine <- read_excel("C:/DataScience/Code/refine.csv")
# str(refine)

df <- read.csv("C:/DataScience/Code/refine.csv")
#str(df)

fix_company_name <- function (para_df)
{
  #para_df$company <- paste(para_df$company , " TEST")  ---- Test
  #args(sub) 
  #function (pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) 
  para_df$company <- tolower(sub(".*ps$", "philips", para_df$company, ignore.case = TRUE))
  para_df$company <- tolower(sub("^ak.*", "akzo", para_df$company, ignore.case = TRUE))
  para_df$company <- tolower(sub("^uni.*", "unilever", para_df$company, ignore.case = TRUE))
  para_df$company <- tolower(sub("^van.*", "van houten", para_df$company, ignore.case = TRUE))
  
  return(para_df)
}

fixed_company_df <- fix_company_name(df)
View(df)
View(fixed_company_df)


parse_product_code <- function (para_df)
{ 
  #para_df <- mutate(para_df, product_code="test", product_number="test1")
  #words <- strsplit("wer-304", "-")[[1]]
  #words <- strsplit(para_df$`Product code \ number`, "-")[[1]]
  #words
  #words[1]
  #words[2]
  View(para_df)
  para_df <- mutate(para_df, ProductCode = substr(para_df$`Product code / number`,1,1)) 
  para_df <- mutate(para_df, ProductNumber = substr(para_df$`Product code / number`, 3, 5)) 
  
  return(para_df)
}

updated_dataset <- parse_product_code(fixed_company_df)
View(updated_dataset)




