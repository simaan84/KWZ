library(xts) # for time series manipulation
library(lubridate) # for date manipulation
library(readxl) # reading excel files to load data

# MOMETUM -  January 1927 to December 2018
FF_file <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/10_Portfolios_Prior_12_2_CSV.zip"
temp <- tempfile()
download.file(FF_file,temp)
unz_files <- unzip(temp)
ds <- read.csv(unz_files,skip = 10,stringsAsFactors = F)
flag_obs <- grep("Average Equal",ds[,1],ignore.case = T)
ds <- ds[1:(flag_obs-1),]
ds <- data.frame(apply(ds,2,as.numeric))
names(ds)[1] <- "date"
ds$date <- ds$date*100  + 1
ds$date <- ymd(ds$date)
ds$date <- ceiling_date(ds$date,"m") - 1
ds[,-1] <- ds[,-1]/100
ds <- ds[ds$date >= "1927-01-01",]
ds <- ds[ds$date <= "2018-12-31",]
ds1 <- ds

# 25BM - January 1927 to December 2018
FF_file <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/25_Portfolios_5x5_CSV.zip"
temp <- tempfile()
download.file(FF_file,temp)
unz_files <- unzip(temp)
ds <- read.csv(unz_files,skip = 15,stringsAsFactors = F)
flag_obs <- grep("Average Equal",ds[,1],ignore.case = T)
ds <- ds[1:(flag_obs-1),]
ds <- data.frame(apply(ds,2,as.numeric))
names(ds)[1] <- "date"
ds$date <- ds$date*100  + 1
ds$date <- ymd(ds$date)
ds$date <- ceiling_date(ds$date,"m") - 1
ds[,-1] <- ds[,-1]/100
ds <- ds[ds$date >= "1927-01-01",]
ds <- ds[ds$date <= "2018-12-31",]
ds2 <- ds

# 25OP-Inv July 1963 to December 2018
FF_file <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/25_Portfolios_OP_INV_5x5_CSV.zip"
temp <- tempfile()
download.file(FF_file,temp)
unz_files <- unzip(temp)
ds <- read.csv(unz_files,skip = 24,stringsAsFactors = F)
flag_obs <- grep("Average Equal",ds[,1],ignore.case = T)
ds <- ds[1:(flag_obs-1),]
ds <- data.frame(apply(ds,2,as.numeric))
names(ds)[1] <- "date"
ds$date <- ds$date*100  + 1
ds$date <- ymd(ds$date)
ds$date <- ceiling_date(ds$date,"m") - 1
ds[,-1] <- ds[,-1]/100
ds <- ds[ds$date >= "1963-07-01",]
ds <- ds[ds$date <= "2018-12-31",]
ds3 <- ds

# 49 Industry -  July 1969 to December 2018
FF_file <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/49_Industry_Portfolios_CSV.zip"
temp <- tempfile()
download.file(FF_file,temp)
unz_files <- unzip(temp)
ds <- read.csv(unz_files,skip = 11,stringsAsFactors = F)
flag_obs <- grep("Average Equal",ds[,1],ignore.case = T)
ds <- ds[1:(flag_obs-1),]
ds <- data.frame(apply(ds,2,as.numeric))
names(ds)[1] <- "date"
ds$date <- ds$date*100  + 1
ds$date <- ymd(ds$date)
ds$date <- ceiling_date(ds$date,"m") - 1
ds[,-1] <- ds[,-1]/100
ds <- ds[ds$date >= "1969-07-01",]
ds <- ds[ds$date <= "2018-12-31",]
ds4 <- ds


## --------------------------------------------------------
low_to_strat <- c("Size","Gross Profitability",
                  "Value","ValProf",
                  "Accruals","Asset Growth",
                  "Investment","Piotroski's F-score")

low_to_strat


## ----message=FALSE,warning=FALSE-------------------------
file.i <- "http://rnm.simon.rochester.edu/data_lib/ToAatTC/Simple_Strategies_Returns.xlsx"
temp <- tempfile()
download.file(file.i,temp)
ds_sheets_all <- sort(excel_sheets(temp))
ds_sheets <- ds_sheets_all[ds_sheets_all %in% low_to_strat]
NM_data_list <- list()
for (sheet_i in ds_sheets) {
  ds_i <- data.frame(read_xlsx(temp,sheet = sheet_i))
  ds_i <- ds_i[,c(1,2,ncol(ds_i))]
  names(ds_i)[-1] <- paste(sheet_i,names(ds_i)[-1],sep = "_")
  ds_i$date <- ds_i$Month
  ds_i$date <- ymd(ds_i$date*100 + 1)
  ds_i$date <- ceiling_date(ds_i$date,"m") - 1
  ds_i$Month <- NULL
  ds_i[,1:2] <- ds_i[,1:2]/100
  NM_data_list <- c(NM_data_list,list(ds_i))
}

NM_data_LT <- Reduce(merge,NM_data_list)
NM_data_LT <- na.omit(NM_data_LT)


## --------------------------------------------------------
dim(NM_data_LT)


## --------------------------------------------------------
mid_to_strat <- c("Net Issuance (rebal.-A)","Return-on-book equity","Failure Probability",
                  "ValMomProf","ValMom","Idiosyncratic Volatility",
                  "Momentum","PEAD (SUE)","PEAD (CAR3)")
high_to_start <- c("Industry Momentum","Industry Relative Reversals","High-frequency Combo",
                   "Short-run Reversals","Seasonality","IRR (Low Vol)")
all_to_start <- unique(sort(c(low_to_strat,mid_to_strat,high_to_start)))
length(all_to_start)


## --------------------------------------------------------
ds_sheets <- ds_sheets_all[ds_sheets_all %in% all_to_start]
NM_data_list <- list()
for (sheet_i in ds_sheets) {
  ds_i <- data.frame(read_xlsx(temp,sheet = sheet_i))
  ds_i <- ds_i[,c(1,2,ncol(ds_i))]
  names(ds_i)[-1] <- paste(sheet_i,names(ds_i)[-1],sep = "_")
  ds_i$date <- ds_i$Month
  ds_i$date <- ymd(ds_i$date*100 + 1)
  ds_i$date <- ceiling_date(ds_i$date,"m") - 1
  ds_i$Month <- NULL
  ds_i[,1:2] <- ds_i[,1:2]/100
  NM_data_list <- c(NM_data_list,list(ds_i))
}

NM_data_ALL <- Reduce(merge,NM_data_list)
NM_data_ALL <- na.omit(NM_data_ALL)
dim(NM_data_ALL)


## --------------------------------------------------------
ds_list <- list(ds1,ds2,ds3,ds4,NM_data_LT,NM_data_ALL)
names(ds_list) <- c("MOM10","BM25","OPIN25","IND49","NMV16","NMV46")
sapply(ds_list,nrow)


## --------------------------------------------------------
ds_list <- ds_list[c("MOM10","BM25","OPIN25","NMV16","NMV46","IND49")]
sapply(ds_list,ncol) - 1


## --------------------------------------------------------
FF_file <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip"
temp <- tempfile()
download.file(FF_file,temp)
unz_files <- unzip(temp)
ds <- read.csv(unz_files,skip = 3)
flag_obs <- grep("Annual",ds[,1],ignore.case = T)
ds <- ds[1:(flag_obs-1),]
names(ds)[1] <- "date"
ds <- data.frame(apply(ds, 2, as.numeric))
ds$date <- ceiling_date(ymd(ds$date*100+ 01),"m")-1
ds <- ds[,c("date","RF")]
ds$RF <- ds$RF/100
ds_rf <- ds
rm(ds)


## --------------------------------------------------------
adjust_rf <- function(ds_i) {
  ds_i <- merge(ds_i,ds_rf, by = "date")
  RF <- ds_i$RF
  ds_i[,-1] <- ds_i[,-1] - RF
  ds_i$RF <- NULL
  return(ds_i)
}


## --------------------------------------------------------
ds_list <- lapply(ds_list,adjust_rf)

