library("readxl")
library("xlsx")
library(reshape2)

# Reading all xlsx files
cus <- read_excel("Put\ updated\ data\ here/1C.\ Dashboard_data_Customs\ St.xlsx")
gle <- read_excel("Put\ updated\ data\ here/1C.\ Dashboard_data_Glen Eden.xlsx")
hen <- read_excel("Put\ updated\ data\ here/1C.\ Dashboard_data_Henderson.xlsx")
kpr <- read_excel("Put\ updated\ data\ here/1C.\ Dashboard_data_Khyber\ Pass.xlsx")
pak <- read_excel("Put\ updated\ data\ here/1C.\ Dashboard_Data_Pakuranga.xlsx")
pap <- read_excel("Put\ updated\ data\ here/1C.\ Dashboard_data_Papatoetoe.xlsx")
pat <- read_excel("Put\ updated\ data\ here/1C.\ Dashboard_data_Patumahoe.xlsx")
pen <- read_excel("Put\ updated\ data\ here/1C.\ Dashboard_data_Penrose.xlsx")
qus <- read_excel("Put\ updated\ data\ here/1C.\ Dashboard_data_Queen\ Street.xlsx")
tak <- read_excel("Put\ updated\ data\ here/1C.\ Dashboard_data_Takapuna.xlsx")
gle2 <- read_excel("Put\ updated\ data\ here/2C.\ Dashboard_data_Glen Eden.xlsx")
hen2 <- read_excel("Put\ updated\ data\ here/2C.\ Dashboard_data_Henderson.xlsx")
pen2 <- read_excel("Put\ updated\ data\ here/2C.\ Dashboard_data_Penrose.xlsx")
tak2 <- read_excel("Put\ updated\ data\ here/2C.\ Dashboard_data_Takapuna.xlsx")
pak2 <- read_excel("Put\ updated\ data\ here/2C.\ Dashboard_Data_Pakuranga.xlsx")
qus2 <- read_excel("Put\ updated\ data\ here/2C.\ Dashboard_data_Queen\ Street.xlsx")


#combine data with more then one file
gle <- rbind(gle, gle2)
hen <- rbind(hen, hen2)
pen <- rbind(pen, pen2)
tak <- rbind(tak, tak2)
pak <- rbind(pak, pak2)
qus <- rbind(qus, qus2)

auk <- rbind(cus, gle, hen, kpr, pak, pap, pat, pen, qus, tak)
rm(gle2)
rm(hen2)
rm(pen2)
rm(tak2)
rm(pak2)
rm(qus2)

#Format auk Data
auk$Parameter <- tolower(auk$Parameter)
auk$Date <- format(as.POSIXct(auk$Date,format='%Y/%d/%m %H:%M:%S'),format='%m/%d/%Y')
auk$Time <- format(as.POSIXct(auk$Time,format='%Y/%d/%m %H:%M:%S'),format='%H:%M:%S')
auk$date <- paste(auk$Date, auk$Time)
auk$date <- as.POSIXct(auk$date, format="%m/%d/%Y %H:%M:%S")
auk$Value <- as.numeric(auk$Value)

#Format Customs Street Data
cus$Parameter <- tolower(cus$Parameter)
cus$Date <- format(as.POSIXct(cus$Date,format='%Y/%d/%m %H:%M:%S'),format='%m/%d/%Y')
cus$Time <- format(as.POSIXct(cus$Time,format='%Y/%d/%m %H:%M:%S'),format='%H:%M:%S')
cus$date <- paste(cus$Date, cus$Time)
cus$date <- as.POSIXct(cus$date, format="%m/%d/%Y %H:%M:%S")
cus$Value <- as.numeric(cus$Value)

#Format Glen Eden Data
gle$Parameter <- tolower(gle$Parameter)
gle$Date <- format(as.POSIXct(gle$Date,format='%Y/%d/%m %H:%M:%S'),format='%m/%d/%Y')
gle$Time <- format(as.POSIXct(gle$Time,format='%Y/%d/%m %H:%M:%S'),format='%H:%M:%S')
gle$date <- paste(gle$Date, gle$Time)
gle$date <- as.POSIXct(gle$date, format="%m/%d/%Y %H:%M:%S")
gle$Value <- as.numeric(gle$Value)

#Format Henderson Data
hen$Parameter <- tolower(hen$Parameter)
hen$Date <- format(as.POSIXct(hen$Date,format='%Y/%d/%m %H:%M:%S'),format='%m/%d/%Y')
hen$Time <- format(as.POSIXct(hen$Time,format='%Y/%d/%m %H:%M:%S'),format='%H:%M:%S')
hen$date <- paste(hen$Date, hen$Time)
hen$date <- as.POSIXct(hen$date, format="%m/%d/%Y %H:%M:%S")
hen$Value <- as.numeric(hen$Value)

#Format Khyber Pass Data
kpr$Parameter <- tolower(kpr$Parameter)
kpr$Date <- format(as.POSIXct(kpr$Date,format='%Y/%d/%m %H:%M:%S'),format='%m/%d/%Y')
kpr$Time <- format(as.POSIXct(kpr$Time,format='%Y/%d/%m %H:%M:%S'),format='%H:%M:%S')
kpr$date <- paste(kpr$Date, kpr$Time)
kpr$date <- as.POSIXct(kpr$date, format="%m/%d/%Y %H:%M:%S")
kpr$Value <- as.numeric(kpr$Value)

#Format Pakurange Data
pak$Parameter <- tolower(pak$Parameter)
pak$Date <- format(as.POSIXct(pak$Date,format='%Y/%d/%m %H:%M:%S'),format='%m/%d/%Y')
pak$Time <- format(as.POSIXct(pak$Time,format='%Y/%d/%m %H:%M:%S'),format='%H:%M:%S')
pak$date <- paste(pak$Date, pak$Time)
pak$date <- as.POSIXct(pak$date, format="%m/%d/%Y %H:%M:%S")
pak$Value <- as.numeric(pak$Value)

#Format Papatoetoe Data
pap$Parameter <- tolower(pap$Parameter)
pap$Date <- format(as.POSIXct(pap$Date,format='%Y/%d/%m %H:%M:%S'),format='%m/%d/%Y')
pap$Time <- format(as.POSIXct(pap$Time,format='%Y/%d/%m %H:%M:%S'),format='%H:%M:%S')
pap$date <- paste(pap$Date, pap$Time)
pap$date <- as.POSIXct(pap$date, format="%m/%d/%Y %H:%M:%S")
pap$Value <- as.numeric(pap$Value)

#Format Patumahoe Data
pat$Parameter <- tolower(pat$Parameter)
pat$Date <- format(as.POSIXct(pat$Date,format='%Y/%d/%m %H:%M:%S'),format='%m/%d/%Y')
pat$Time <- format(as.POSIXct(pat$Time,format='%Y/%d/%m %H:%M:%S'),format='%H:%M:%S')
pat$date <- paste(pat$Date, pat$Time)
pat$date <- as.POSIXct(pat$date, format="%m/%d/%Y %H:%M:%S")
pat$Value <- as.numeric(pat$Value)

#Format Penrose Data
pen$Parameter <- tolower(pen$Parameter)
pen$Date <- format(as.POSIXct(pen$Date,format='%Y/%d/%m %H:%M:%S'),format='%m/%d/%Y')
pen$Time <- format(as.POSIXct(pen$Time,format='%Y/%d/%m %H:%M:%S'),format='%H:%M:%S')
pen$date <- paste(pen$Date, pen$Time)
pen$date <- as.POSIXct(pen$date, format="%m/%d/%Y %H:%M:%S")
pen$Value <- as.numeric(pen$Value)

#Format Queen Street Data
qus$Parameter <- tolower(qus$Parameter)
qus$Date <- format(as.POSIXct(qus$Date,format='%Y/%d/%m %H:%M:%S'),format='%m/%d/%Y')
qus$Time <- format(as.POSIXct(qus$Time,format='%Y/%d/%m %H:%M:%S'),format='%H:%M:%S')
qus$date <- paste(qus$Date, qus$Time)
qus$date <- as.POSIXct(qus$date, format="%m/%d/%Y %H:%M:%S")
qus$Value <- as.numeric(qus$Value)

#Format Takapuna data
tak$Parameter <- tolower(tak$Parameter)
tak$Date <- format(as.POSIXct(tak$Date,format='%Y/%d/%m %H:%M:%S'),format='%m/%d/%Y')
tak$Time <- format(as.POSIXct(tak$Time,format='%Y/%d/%m %H:%M:%S'),format='%H:%M:%S')
tak$date <- paste(tak$Date, tak$Time)
tak$date <- as.POSIXct(tak$date, format="%m/%d/%Y %H:%M:%S")
tak$Value <- as.numeric(tak$Value)


#Cast data to OpenAir format
auk <- dcast(auk, date~Parameter , fun.aggregate = mean, value.var = "Value")
cus <- dcast(cus, date~Parameter , fun.aggregate = mean, value.var = "Value")
gle <- dcast(gle, date~Parameter , fun.aggregate = mean, value.var = "Value")
hen <- dcast(hen, date~Parameter , fun.aggregate = mean, value.var = "Value")
kpr <- dcast(kpr, date~Parameter , fun.aggregate = mean, value.var = "Value")
pak <- dcast(pak, date~Parameter , fun.aggregate = mean, value.var = "Value")
pap <- dcast(pap, date~Parameter , fun.aggregate = mean, value.var = "Value")
pat <- dcast(pat, date~Parameter , fun.aggregate = mean, value.var = "Value")
pen <- dcast(pen, date~Parameter , fun.aggregate = mean, value.var = "Value")
qus <- dcast(qus, date~Parameter , fun.aggregate = mean, value.var = "Value")
tak <- dcast(tak, date~Parameter , fun.aggregate = mean, value.var = "Value")

#Export to csv
write.csv(auk,"data/auckland_data.csv", row.names = FALSE)
write.csv(cus,"data/customs_st_data.csv", row.names = FALSE)
write.csv(gle,"data/glen_eden_data.csv", row.names = FALSE)
write.csv(hen,"data/henderson_data.csv", row.names = FALSE)
write.csv(kpr,"data/khyber_pass_data.csv", row.names = FALSE)
write.csv(pak,"data/pakuranga_data.csv", row.names = FALSE)
write.csv(pap,"data/papatoetoe_data.csv", row.names = FALSE)
write.csv(pat,"data/patumahoe_data.csv", row.names = FALSE)
write.csv(pen,"data/penrose_data.csv", row.names = FALSE)
write.csv(qus,"data/queen_st_data.csv", row.names = FALSE)
write.csv(tak,"data/takapuna_data.csv", row.names = FALSE)

print("Data conversion is completed. Dashboard data have been updated") 