library(data.table); library(readxl)


#THIS SCRIPT DOWNLOADS AND FORMATS IMF WORLD ECONOMIC OUTLOOK DATA (SPRING VERSIONS) FROM THE IMF WEBSITE
#ALL DATA IS AVAILABLE AT https://www.imf.org/external/ns/cs.aspx?id=28

#THE DATA DOWNLOADED IN THIS SCRIPT IS FOR THE COUNTRY-GROUP LEVEL


############################
#Download WEO files for years 2010 to 2018, read into data.frames and combine
####

l2 <- list()
for(i in 0:10) {
  print(i)
  temp <- tempfile("file", fileext = ".tsv")
  # download.file(paste0("https://www.imf.org/external/pubs/ft/weo/201",i,"/01/weodata/WEOApr201",i,"alla.xls"), temp)
  download.file(paste0("https://www.imf.org/external/pubs/ft/weo/", 2008+i, "/01/weodata/WEOApr", 2008+i, "alla.xls"), temp)
  temp2 <- gsub("TMORRI~1", "TMORRISON", temp)
  z <- read.delim(temp2)
  file.remove(temp2)
  setDT(z)
  z[, vintage:=2010+i]
  l2[[i+1]] <- z
}; rm(temp,temp2,i)
z <- rbindlist(l2, fill=TRUE, id=FALSE)
z[z=="n/a"] <- NA



############################
#Transform data.table from wide to long format and reformat columns
####

dat2 <- melt(z, measure.vars = paste0("X", 1980:2023), variable.name = "year", value.name = "value")
dat2[, year:=as.numeric(gsub("X", "", year))][, value:=gsub(",", "", value)]
dat2[, value:=as.numeric(value)]
dat2 <- dat2[, lapply(.SD, as.character), by=.(Estimates.Start.After, year, value, vintage)]

#Remove certain columns from dataset
dat2 <- dat2[, -c("Estimates.Start.After", "Subject.Notes", "Series.specific.Notes","WEO.Country.Group.Code")]


############################
#Layer dataset: 
#     - For each WEO vintage, there are five years of projections, plus a number of years of historical data
#     - This step limits each vintage to only the projection data, as there are revisions to the historical data in subsequent vintages
#     - All historical data is appended from the most recent vintage, being the most accurate data available
####

dt <- copy(dat2)
dt <- dat2[year>=vintage]
dt <- unique(rbind(dt, dat2[vintage==2018]))

samp <- merge(
  dat2[year==vintage-1, -'value'],
  dat2[vintage==2018, -'vintage'],
  by=c('year','WEO.Subject.Code','Country.Group.Name',"Units","Scale","Subject.Descriptor")
)

dt <- rbind(samp, dt)








