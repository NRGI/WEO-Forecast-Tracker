library(data.table); library(readxl)


#THIS SCRIPT DOWNLOADS AND FORMATS IMF WORLD ECONOMIC OUTLOOK DATA (SPRING VERSIONS) FROM THE IMF WEBSITE
#ALL DATA IS AVAILABLE AT https://www.imf.org/external/ns/cs.aspx?id=28

#THE DATA DOWNLOADED IN THIS SCRIPT IS FOR THE COUNTRY LEVEL


############################
#Download WEO files for years 2010 to 2018, read into data.frames and combine
####

l <- list()
for(i in 0:8) {
  print(i)
  temp <- tempfile("file", fileext = ".tsv")
  download.file(paste0("https://www.imf.org/external/pubs/ft/weo/201",i,"/01/weodata/WEOApr201",i,"all.xls"), temp)
  temp2 <- gsub("TMORRI~1", "TMORRISON", temp)
  z <- read.delim(temp2)
  file.remove(temp2)
  setDT(z)
  z[, vintage:=2010+i]
  l[[i+1]] <- z
}; rm(temp, temp2, i)
x <- rbindlist(l, fill=TRUE, id=FALSE)
x[x=="n/a"] <- NA



############################
#Transform data.table from wide to long format and reformat columns
####

dat <- melt(x, measure.vars = paste0("X", 1980:2023), variable.name = "year", value.name = "value")
dat[, year:=as.numeric(gsub("X", "", year))][, value:=gsub(",", "", value)]
dat[, value:=as.numeric(value)]
dat <- dat[, lapply(.SD, as.character), by=.(Estimates.Start.After, year, value, vintage)]

#Remove certain columns from dataset
dat <- dat[, -c("Estimates.Start.After", "Subject.Notes", "Country.Series.specific.Notes","WEO.Country.Code")]


############################
#Layer dataset: 
#     - For each WEO vintage, there are five years of projections, plus a number of years of historical data
#     - This step limits each vintage to only the projection data, as there are revisions to the historical data in subsequent vintages
#     - All historical data is appended from the most recent vintage, being the most accurate data available
####

dt <- copy(dat)
dt <- dat[year>=vintage]
dt <- unique(rbind(dt, dat[vintage==2018]))

samp <- merge(
  dat[year==vintage-1, -'value'],
  dat[vintage==2018, -'vintage'],
  by=c('year','WEO.Subject.Code','ISO',"Subject.Descriptor","Units","Scale","Country")
)

dt <- rbind(samp, dt)








