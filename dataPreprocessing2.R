library("readxl")
library(data.table)
library(lubridate) 

setwd("~/Programming/erasmusCourses/DS/Final Project/proyecto_ds")

# d04 <- read_excel("data/uncleaned/2004/2004_todos.xls", sheet = 1)
# # There are two columns here that have the exact same data; drop the extra
# different_values <- d04[["FechaCaptura...7"]] != d04[["FechaCaptura...8"]]
# print(which(different_values))
# d04 <- subset(d04, select = -FechaCaptura...7)
# names(d04)[names(d04) == "FechaCaptura...8"] <- "FechaCaptura"
d04 <- fread("data/uncleaned/2004/2004_todos_csv.csv")
d04 <- d04[,-8]
d04$FechaCaptura <- as.Date(d04$FechaCaptura, format = "%d/%m/%Y")


# There's an issue with the date encoding, this file needed to be converted to csv
# Reading it as an excel always tried to parse an entire column the same way
# Exporting it as a csv allowed each row to be written with its own date format
# Then the lubridate in R can take care of parsing each row however it should be parsed
# d05 <- read_excel("data/uncleaned/2005/2005_todos.xls", sheet = 1)
# # There are two columns here that have the exact same data
# different_values <- d05[["FechaCaptura...7"]] != d05[["FechaCaptura...8"]]
# print(which(different_values))
# d05 <- subset(d05, select = -FechaCaptura...7)
# names(d05)[names(d05) == "FechaCaptura...8"] <- "FechaCaptura"

d05 <- fread("data/uncleaned/2005/2005_todos_csv.csv")
d05 <- d05[, -7]
d05$FechaCaptura <- dmy(d05$FechaCaptura)
# length(subset(d05, is.na(FechaCaptura))$d05)

d06 <- read_excel("data/uncleaned/2006/2006_todos.xls", sheet = 1, col_types = "text")
d06$NombreAnillador <- "MANUEL VAZQUEZ CASTRO"
d06$FechaCaptura <- as.Date(d06$FechaCaptura, format = "%d-%m-%y")

# There are some columns that are not shared; these should be dropped later 
# if they are unimportant.

# There was an issue with the HoraCaptura encoding in this sheet

d07 <- fread("data/uncleaned/2007/2007_todos_csv.csv")
d07$FechaCaptura <- as.Date(d07$FechaCaptura, format = "%d/%m/%y")

# Same; the hora is text. 
d08 <- read_excel("data/uncleaned/2008/2008_todos.xlsx", sheet = 1, col_types = "text")
d08$CodigoGrupo <-660019
d08$NombreGrupo <- "MANUEL VAZQUEZ CASTRO"
d08$FechaCaptura <- as.Date(d08$FechaCaptura, format = "%d-%m-%y")

# There was also an issue with the dates encoding in this file. Had to change the 
# custom format of the date in excel to all be dmy, then export it to a csv (because there 
# were still a bunch of rows as text). Then coerce them using dmy
# colTypes <- rep("text", 59)
# colTypes[8] <- "date"
# d09 <- read_excel("data/uncleaned/2009/2009_todos.xlsx", sheet = "Hoja1", col_types = colTypes)
d09 <-fread("data/uncleaned/2009/2009_todos_csv.csv")
d09$FechaCaptura <- dmy(d09$FechaCaptura)


# 2010 was spread out over many files
d10_01 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(1).csv", sep=";")
d10_02 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(2).csv", sep=";")
d10_03 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(3).csv", sep=";")
d10_04 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(4).csv", sep=";")
d10_05 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(5).csv", sep=";")
d10_06 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(6).csv", sep=";")
d10_07 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(7).csv", sep=";")
d10_08 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(8).csv", sep=";")
d10_09 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(9).csv", sep=";")
d10_10 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(10).csv", sep=";")
d10_11 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(11).csv", sep=";")
d10_12 <- read.csv("data/uncleaned/2010/Datos enviados/Datos a 2010/2010Zam-MV(12).csv", sep=";")

dfs_2010 <- list(d10_01, d10_02, d10_03, d10_04, d10_05, d10_06, d10_07, d10_08, d10_09, d10_10, d10_11, d10_12)

# Combine dataframes into one
d10 <- do.call(rbind, dfs_2010)
d10 <- subset(d10, CodigoGrupo != "")

# Some of the dates were missing delimiters (e.g. 13/052010)
# d10$FechaCaptura <- as.Date(d10$FechaCaptura, format = "%d/%m/%Y")
d10$FechaCaptura <- dmy(d10$FechaCaptura)

colTypes <- rep("text", 59)
colTypes[8] <- "date"
d11 <- read_excel("data/uncleaned/2011/2011_todos.xls", sheet = "DATOS", col_types = colTypes)

colTypes <- rep("text", 19)
colTypes[8] <- "date"
d12 <- read_excel("data/uncleaned/2012/2012_todos.xls", sheet = "DATOS", col_types = colTypes)

colTypes <- rep("text", 59)
colTypes[8] <- "date"
d13 <- read_excel("data/uncleaned/2013/2013_todos.xls", sheet = "DATOS", col_types = colTypes)
colTypes <- rep("text", 63)
colTypes[8] <- "date"
d13_extra <- read_excel("data/uncleaned/2013/2013_falta.xls", sheet = "DATOS", col_types = colTypes)
not_shared1 <- setdiff(names(d13_extra), names(d13))
not_shared1
not_shared2 <- setdiff(names(d13), names(d13_extra))
not_shared2
names(d13_extra)[which(names(d13_extra) == "Anilla METAL")] <- "Anilla"
d13_extra <- d13_extra[, !names(d13_extra) %in% not_shared1]
d13 <- rbind(d13, d13_extra)

colTypes <- rep("text", 63)
colTypes[8] <- "date"
d14 <- read_excel("data/uncleaned/2014/2014_todos.xls", sheet = "DATOS", col_types = colTypes)
d14_extra <- read_excel("data/uncleaned/2014/2014_falta.xls", sheet = "DATOS", col_types = colTypes)
names(d14)[which(names(d14) == "Anilla METAL")] <- "Anilla"
d14 <- rbind(d14, d14_extra)

d15 <- read_excel("data/uncleaned/2015/2015_todos.xls", sheet = "DATOS", col_types = colTypes)
names(d15)[which(names(d15) == "Anilla METAL")] <- "Anilla"

combine_if_no_common_anillas <- function(df1, df2) {
  # Check if any values in "Anilla" column of df1 are the same as df2
  x <- df2$Anilla %in% df1$Anilla
  if (!any(x)) {
    combined_df <- rbind(df1, df2)
    return(combined_df)
  } else {
    print("DUPLICATED ANILLA")
    print(which(x))
    return(df1)
  }
}

colTypes <- rep("text", 59)
d16_1 <- read_excel("data/uncleaned/2016/Enviados/anillamientos2016EGR1.xls", col_types = colTypes)
d16_2 <- read_excel("data/uncleaned/2016/Enviados/anillamientos2016EGR2.xls", col_types = colTypes)
d16 <- combine_if_no_common_anillas(d16_1, d16_2)
d16_3 <- read_excel("data/uncleaned/2016/Enviados/anillamientos2016EGR3.xls", col_types = colTypes)
d16 <- combine_if_no_common_anillas(d16, d16_3)
d16_4 <- read_excel("data/uncleaned/2016/Enviados/anillamientos2016EGR4.xls", col_types = colTypes)
d16 <- combine_if_no_common_anillas(d16, d16_4)
d16_5 <- read_excel("data/uncleaned/2016/Enviados/anillamientos2016EGR5.xls", col_types = colTypes)
d16 <- combine_if_no_common_anillas(d16, d16_5)
d16_6 <- read_excel("data/uncleaned/2016/Enviados/anillamientos2016EGR6.xls", col_types = colTypes)
d16 <- combine_if_no_common_anillas(d16, d16_6)
names(d16)[names(d16) == "Fecha"] <- "FechaCaptura"
d16$NombreGrupo <- "EUSEBIO GOMEZ REINA"


prep_for_combo_and_combine <- function(df16_, d16) {
  names(df16_)[names(df16_) == "Anilla METAL"] <- "Anilla"
  not_shared <- setdiff(names(df16_), names(d16))
  print("The following columns will be removed: ")
  print(not_shared)
  if(length(not_shared)>0){
    print("Removed unshared")
    df16_ <- df16_[, !names(df16_) %in% not_shared]
  } else{
    print("All columns the same")
  }
  d16 <- combine_if_no_common_anillas(d16, df16_)
  return(d16)
}

d16_7 <- read_excel("data/uncleaned/2016/Enviados/2016ZamSayago.xls", col_types = "text")
d16_7$NombreGrupo <- "JOSE MANUEL SAYAGO ROBLES"
d16 <- prep_for_combo_and_combine(d16_7, d16)
d16_8 <- read_excel("data/uncleaned/2016/Enviados/2016Zam1.xls", col_types = "text")
d16 <- prep_for_combo_and_combine(d16_8, d16)
d16_9 <- read_excel("data/uncleaned/2016/Enviados/2016Zam2.xls", col_types = "text")
d16 <- prep_for_combo_and_combine(d16_9, d16)

# Doesn't work because some of the dates were stored as excel numbers.
# d16$FechaCaptura1 <- as.Date(d16$FechaCaptura, format = "%d/%m/%Y")

# Function to convert Excel serial numbers to dates
excel_to_date <- function(x) {
  # Check if the value is numeric (Excel serial number)
  if(grepl("^-?\\d*\\.?\\d+$", x)) {
    # Convert Excel serial number to date
    as.Date(as.numeric(x) - 1, origin = "1899-12-30",)
  } else {
    # Return as is (text date)
    as.Date(x, format = "%d/%m/%Y")
  }
}

# Apply the function to the column
converted_dates <- lapply(d16$FechaCaptura, excel_to_date)
converted_dates <- t(data.frame(converted_dates))
d16$FechaCaptura <- converted_dates
d16$FechaCaptura <- as.Date(d16$FechaCaptura, format = "%Y-%m-%d")

colTypes <- rep("text", 63)
d17 <- read_excel("data/uncleaned/2017/2017_todos_fixed.xlsx", sheet = "DATOS", col_types = colTypes)
# d17$FechaCaptura <- as.Date(d17$FechaCaptura, format = "%d/%m/%Y")

# TODO: Add this extra sheet of data in
# colTypes[8] <- "date"
# d17_extra <- read_excel("data/uncleaned/2017/2017_extra.xls", sheet = "DATOS", col_types = colTypes)
# any(d17_extra[["Anilla METAL"]] %in% d17[["Anilla METAL"]])
# d17_extra$FechaCaptura <-ymd(d17_extra$FechaCaptura)
# d17 <- rbind(d17, d17_extra)
names(d17)[which(names(d17) == "Anilla METAL")] <- "Anilla"
converted_dates <- lapply(d17$FechaCaptura, excel_to_date)
converted_dates <- t(data.frame(converted_dates))
d17$FechaCaptura <- converted_dates
d17$FechaCaptura <- as.Date(d17$FechaCaptura, format = "%Y-%m-%d")


colTypes <- rep("text", 63)
colTypes[8] <- "date"
d18_1 <- read_excel("data/uncleaned/2018/2018_1.xls", col_types = colTypes)
d18_1$NombreGrupo <- "JUAN MANUEL SAEZ MUNOZ"
d18_2 <- read_excel("data/uncleaned/2018/2018_EGR1.xls", col_types = "text")
d18_2$NombreGrupo <- "EUSEBIO GOMEZ REINA"
names(d18_2)[which(names(d18_2) == "Fecha")] <- "FechaCaptura"
d18_2$FechaCaptura <- as.Date(d18_2$FechaCaptura, format = "%d/%m/%Y")
d18 <- prep_for_combo_and_combine(d18_1, d18_2)
d18_3 <- read_excel("data/uncleaned/2018/2018_EGR2.xls", col_types = "text")
d18_3$NombreGrupo <- "EUSEBIO GOMEZ REINA"
names(d18_3)[which(names(d18_3) == "Fecha")] <- "FechaCaptura"
d18_3$FechaCaptura <- as.Date(d18_3$FechaCaptura, format = "%d/%m/%Y")
d18 <- combine_if_no_common_anillas(d18, d18_3)
d18_4 <- read_excel("data/uncleaned/2018/2018_EGR3.xls", col_types = "text")
d18_4$NombreGrupo <- "EUSEBIO GOMEZ REINA"
names(d18_4)[which(names(d18_4) == "Fecha")] <- "FechaCaptura"
d18_4$FechaCaptura <- as.Date(d18_4$FechaCaptura, format = "%d/%m/%Y")
d18 <- combine_if_no_common_anillas(d18, d18_4)
colTypes <- rep("text", 63)
colTypes[8] <- "date"
d18_5 <- read_excel("data/uncleaned/2018/ZamPVC-2018MV1.xls", col_types = colTypes)
d18_5$NombreGrupo <-"MANUEL VAZQUEZ CASTRO"
d18 <- prep_for_combo_and_combine(d18_5, d18)
d18_6 <- read_excel("data/uncleaned/2018/ZamPVC-2018MV2.xls", col_types = colTypes)
d18_6$NombreGrupo <-"MANUEL VAZQUEZ CASTRO"
d18 <- prep_for_combo_and_combine(d18_6, d18)
d18_7 <- read_excel("data/uncleaned/2018/ZamPVC-2018MV3.xls", col_types = colTypes)
d18_7$NombreGrupo <-"MANUEL VAZQUEZ CASTRO"
d18 <- prep_for_combo_and_combine(d18_7, d18)
d18_8 <- read_excel("data/uncleaned/2018/ZamPVC-2018MV4.xls", col_types = colTypes)
d18_8$NombreGrupo <-"MANUEL VAZQUEZ CASTRO"
d18 <- prep_for_combo_and_combine(d18_8, d18)
d18_9 <- read_excel("data/uncleaned/2018/ZamPVC-2018MV5.xls", col_types = colTypes)
d18_9$NombreGrupo <-"MANUEL VAZQUEZ CASTRO"
d18 <- prep_for_combo_and_combine(d18_9, d18)
d18_10 <- read_excel("data/uncleaned/2018/ZamPVC-2018MV6.xls", col_types = colTypes)
d18_10$NombreGrupo <-"MANUEL VAZQUEZ CASTRO"
d18 <- prep_for_combo_and_combine(d18_10, d18)
d18_11 <- read_excel("data/uncleaned/2018/ZamPVC-2018MV7.xls", col_types = colTypes)
d18_11$NombreGrupo <-"MANUEL VAZQUEZ CASTRO"
d18 <- prep_for_combo_and_combine(d18_11, d18)

d19_1 <- read_excel("data/uncleaned/2019/ZamPVC-MV2019.xls", sheet="DATOS", col_types = colTypes)
d19_1$NombreGrupo <-"MANUEL VAZQUEZ CASTRO"
d19_2 <- read_excel("data/uncleaned/2019/ZamEGR2019.xls", col_types = "text")
d19_2$NombreGrupo <-"EUSEBIO GOMEZ REINA"
names(d19_2)[which(names(d19_2) == "Fecha")] <- "FechaCaptura"
d19_2$FechaCaptura <- as.Date(d19_2$FechaCaptura, format = "%d/%m/%Y")
d19 <- prep_for_combo_and_combine(d19_1, d19_2)
d19_3 <- read_excel("data/uncleaned/2019/ZamJuanmaPVC2019.xls", col_types = colTypes)
d19_3$NombreGrupo <-"JUAN MANUEL SAEZ MUNOZ"
d19 <- prep_for_combo_and_combine(d19_3, d19)
d19_4 <- read_excel("data/uncleaned/2019/ZamPVC-Sayago2019.xls", col_types = colTypes)
d19_4$NombreGrupo <-"JOSE MANUEL SAYAGO ROBLES"
d19 <- prep_for_combo_and_combine(d19_4, d19)

d20 <- read_excel("data/uncleaned/2020/2020_todos.xls", sheet="DATOS", col_types = "text")
names(d20)[which(names(d20) == "Anilla METAL")] <- "Anilla"
converted_dates <- lapply(d20$FechaCaptura, excel_to_date)
converted_dates <- t(data.frame(converted_dates))
d20$FechaCaptura <- converted_dates
d20$FechaCaptura <- as.Date(d20$FechaCaptura, format = "%Y-%m-%d")


d21_1 <- read_excel("data/uncleaned/2021/Zam-EGR2021.xls", col_types = "text")
names(d21_1)[which(names(d21_1) == "Fecha")] <- "FechaCaptura"
d21_1$FechaCaptura <- as.Date(d21_1$FechaCaptura, format = "%d/%m/%Y")
d21_1$NombreGrupo <-"EUSEBIO GOMEZ REINA"
colTypes <- rep("text", 63)
colTypes[8] <- "date"
d21_2 <- read_excel("data/uncleaned/2021/ZamPVC-MV2021.xls", sheet="DATOS", col_types = colTypes)
d21_2$NombreGrupo <-"MANUEL VAZQUEZ CASTRO"
d21 <- prep_for_combo_and_combine(d21_2, d21_1)
colTypes <- rep("text", 59)
colTypes[8] <- "date"
d21_3 <- read_excel("data/uncleaned/2021/Zam-FJPM2021.xls", sheet="DATOS", col_types = colTypes)
d21_3$NombreGrupo <-"FRANCISCO JOSE PARRADO MARQUEZ"
d21 <- combine_if_no_common_anillas(d21, d21_3)
colTypes <- rep("text", 63)
colTypes[8] <- "date"
d21_4 <- read_excel("data/uncleaned/2021/Zam-JCAM2021.xls", sheet="DATOS", col_types = colTypes)
d21_4$NombreGrupo <-"JUAN CARLOS ADAME MEJIAS"
d21 <- prep_for_combo_and_combine(d21_4, d21)
colTypes <- rep("text", 59)
colTypes[8] <- "date"
d21_5 <- read_excel("data/uncleaned/2021/Zam-MHH2021.xls", sheet="DATOS", col_types = colTypes)
d21_5$NombreGrupo <-"MARIA HARANA HERRERA"
d21 <- combine_if_no_common_anillas(d21_5, d21)
colTypes <- rep("text", 63)
colTypes[8] <- "date"
d21_6 <- read_excel("data/uncleaned/2021/ZamPVC-JMSM2021.xls", sheet="DATOS", col_types = colTypes)
d21_6$NombreGrupo <-"JUAN MANUEL SAEZ MUNOZ"
d21 <- prep_for_combo_and_combine(d21_6, d21)
d21_7 <- read_excel("data/uncleaned/2021/ZamPVC-JMSR2021.xls", sheet="DATOS", col_types = "text")
d21_7$NombreGrupo <-"JOSE MANUEL SAYAGO ROBLES"
d21 <- prep_for_combo_and_combine(d21_7, d21)


d22_1 <- read_excel("data/uncleaned/2022/ZamEGR2022.xls", col_types = "text")
names(d22_1)[which(names(d22_1) == "Fecha")] <- "FechaCaptura"
d22_1$FechaCaptura <- as.Date(d22_1$FechaCaptura, format = "%d/%m/%Y")
d22_1$NombreGrupo <-"EUSEBIO GOMEZ REINA"
colTypes <- rep("text", 63)
colTypes[8] <- "date"
d22_2 <- read_excel("data/uncleaned/2022/ZamPVC-MV2022.xls", sheet="DATOS", col_types = colTypes)
d22_2$NombreGrupo <-"MANUEL VAZQUEZ CASTRO"
d22 <- prep_for_combo_and_combine(d22_2, d22_1)
colTypes <- rep("text", 59)
colTypes[8] <- "date"
d22_3 <- read_excel("data/uncleaned/2022/ZamFrancisco Javier Perez Mata2022.xls", sheet="DATOS", col_types = colTypes)
d22_2$NombreGrupo <-"FRANCISCO JOSE PARRADO MARQUEZ"
d22 <- combine_if_no_common_anillas(d22_3, d22)
d22_4 <- read_excel("data/uncleaned/2022/ZamJuan Carlos Adame Mejias2022.xls", sheet="DATOS", col_types = colTypes)
d22_4$NombreGrupo <-"JUAN CARLOS ADAME MEJIAS"
d22 <- combine_if_no_common_anillas(d22_4, d22)
d22_5 <- read_excel("data/uncleaned/2022/ZamMaria Harana Herrera2022.xls", sheet="DATOS", col_types = colTypes)
d22_5$NombreGrupo <-"MARIA HARANA HERRERA"
d22 <- combine_if_no_common_anillas(d22_5, d22)
colTypes <- rep("text", 63)
colTypes[8] <- "date"
d22_6 <- read_excel("data/uncleaned/2022/ZamPVCJuan Manuel Saenz Muñoz2022.xls", sheet="DATOS", col_types = colTypes)
d22_6$NombreGrupo <-"JUAN MANUEL SAEZ MUNOZ"
d22 <- prep_for_combo_and_combine(d22_6, d22)
d22_7 <- read_excel("data/uncleaned/2022/ZamPVCJose Manuel Sayago Robles2022.xls", sheet="DATOS", col_types = colTypes)
d22_7$NombreGrupo <-"JOSE MANUEL SAYAGO ROBLES"
d22 <- prep_for_combo_and_combine(d22_7, d22)

d23_1 <- read_excel("data/uncleaned/2023/Zam-EusebioGomez2023.xls", col_types = "text")
names(d23_1)[which(names(d23_1) == "Fecha")] <- "FechaCaptura"
d23_1$FechaCaptura <- as.Date(d23_1$FechaCaptura, format = "%d/%m/%Y")
d23_1$NombreGrupo <-"EUSEBIO GOMEZ REINA"
colTypes <- rep("text", 64)
colTypes[8] <- "date"
d23_2 <- read_excel("data/uncleaned/2023/ZamPVC-ManuelVazquez2023.xls", sheet="DATOS", col_types = colTypes)
d23_2$NombreGrupo <-"MANUEL VAZQUEZ CASTRO"
d23 <- prep_for_combo_and_combine(d23_2, d23_1)
colTypes <- rep("text", 59)
colTypes[8] <- "date"
d23_3 <- read_excel("data/uncleaned/2023/Zam-JuanCarlosAdame2023.xls", sheet="DATOS", col_types = colTypes)
d23_3$NombreGrupo <-"JUAN CARLOS ADAME MEJIAS"
d23 <- combine_if_no_common_anillas(d23_3, d23)
d23_4 <- read_excel("data/uncleaned/2023/Zam-MariaHarana2023.xls", sheet="DATOS", col_types = colTypes)
d23_4$NombreGrupo <-"MARIA HARANA HERRERA"
d23 <- combine_if_no_common_anillas(d23_4, d23)
colTypes <- rep("text", 63)
colTypes[8] <- "date"
d23_5 <- read_excel("data/uncleaned/2023/ZamPVC-JoseManuelSayago2023.xls", sheet="DATOS", col_types = colTypes)
d23_5$NombreGrupo <-"JOSE MANUEL SAYAGO ROBLES"
d23 <- prep_for_combo_and_combine(d23_5, d23)


rm(d13_extra, d14_extra, d10_01, d10_02, d10_03, d10_04, d10_05, d10_06, d10_07, d10_08, d10_09,
   d10_10,d10_11,d10_12, dfs_2010, d16_1, d16_2, d16_3, d16_4, d16_5, d16_6, d16_7, d16_8, d16_9,
   d17_extra, d18_1, d18_2, d18_3, d18_4, d18_5, d18_6, d18_7, d18_8, d18_9, d18_10, d18_11,
   d19_1, d19_2, d19_3, d19_4, d21_1, d21_2, d21_3, d21_4, d21_5, d21_6, d21_7,
   d22_1, d22_2, d22_3, d22_4, d22_5, d22_6, d22_7,
   d23_1, d23_2, d23_3, d23_4, d23_5)

# Names of ringers:
# MANUEL VAZQUEZ CASTRO
# EUSEBIO GOMEZ REINA
# JOSE MANUEL SAYAGO ROBLES
# ROBERTO CARLOS PAYA SANCHEZ
# FRANCISCO JOSE PARRADO MARQUEZ
# JUAN MANUEL SAEZ MUNOZ
# ARACELI GARRIDO AGUILERA
# JUAN CARLOS ADAME MEJIAS
# MARIA HARANA HERRERA
# FRANCISCO JAVIER PEREZ MATA



#################### DONE READING FILES. NOW COMBINING #########################
# List of dataframe names
dataframe_names <- c("d04", "d05", "d06", "d07", "d08", "d09", "d10", "d11", "d12", 
                     "d13", "d14", "d15", "d16", "d17", "d18", "d19", "d20", "d21", 
                     "d22", "d23")
all_column_names <- c()

for (df_name in dataframe_names) {
  df_col_names <- colnames(get(df_name))
  all_column_names <- c(all_column_names, df_col_names)
}

unique_column_names <- unique(all_column_names)

# print(sort(unique_column_names))

# Name columns consistently
for (name in dataframe_names) {
  # Check if columns exist before renaming
  if ("NombreGrupo" %in% colnames(get(name))) {
    assign(name, transform(get(name), 
                           NombreAnillador = NombreGrupo))
  }
  if ("CodigoGrupo" %in% colnames(get(name))) {
    assign(name, transform(get(name), 
                           CodigoAnillador = CodigoGrupo))
  }
}

# Define the desired columns
desired_columns <- c("CodigoAnillador", "NombreAnillador", "Anilla", 
                     "NombreEspecie", "CodigoEspecie", "FechaCaptura", 
                     "CodigoEdad", "CodigoSexo", "NombreLocalidad", 
                     "MUNICIPIO", "CodigoLocalidad", "Observaciones", 
                     "CodigoHabitat", "CodigoMetodo", "NumRedes", 
                     "HoraCaptura", "HoraDesde", "HoraHasta", 
                     "TipoRegistro", "CodigoCentro", "CodigoReclamo", "DataFrameName")

# Add a new column with the dataframe name
for (df_name in dataframe_names) {
  current_df <- get(df_name)
  current_df$DataFrameName <- df_name
  assign(df_name, current_df)
}

# Combine dataframes and select desired columns
combined_df <- do.call(rbind, lapply(dataframe_names, function(x) {
  df <- get(x)
  # Add missing columns with NA values
  missing_columns <- setdiff(desired_columns, colnames(df))
  for (col in missing_columns) {
    df[[col]] <- NA
  }
  # Convert to data frame in case it is a data table
  df <- as.data.frame(df)
  # Reorder columns
  df<- df[, desired_columns]
  return(df)
}))


############# INSPECTING MISSING VALUES ######################
(missing_counts <- colSums(is.na(combined_df)))

# unique(subset(combined_df, is.na(CodigoAnillador))$DataFrameName)

# These sheets all have a lot of dates missing, most likely just because the 
# date is in the wrong format (dd/mm/yy and mm/dd/yyyy combo)
missingDate <- unique(subset(combined_df, is.na(FechaCaptura))$DataFrameName)

for (df_name in missingDate) {
  subset_df <- get(df_name)  # Assuming your dataframes are named according to the values in DataFrameName
  missing_count <- length(subset(subset_df, is.na(FechaCaptura))$DataFrameName)
  print(paste("Missing date count in", df_name, ":", missing_count))
}

# missing_rows <- subset(d17, is.na(FechaCaptura))
# head(missing_rows)

# Sheet is just missing a ton of locality data; nothing I can do about it. Maybe Manolo's data
# will be able to fill it in a bit
missingLocality <- unique(subset(combined_df, is.na(NombreLocalidad))$DataFrameName)
for (df_name in missingLocality) {
  subset_df <- get(df_name)  # Assuming your dataframes are named according to the values in DataFrameName
  missing_count <- length(subset(subset_df, is.na(NombreLocalidad))$DataFrameName)
  print(paste("Missing locality count in", df_name, ":", missing_count))
}

write.csv(combined_df, file = './data/combined_anillamiento.csv', row.names = FALSE)

###################### FILL IN MANOLOS DATA #######################

manolos_data <- fread("limpieza/limpio/anillamiento_de_Manolo.csv")

# missing_values <- manolos_data$METAL[!manolos_data$METAL %in% combined_df$Anilla]
# 76000 missing, but prob from 1987-2003

manolos_subset_2004_onwards <- manolos_data[manolos_data$AÑO > 2003, ]


# Find the values in 'manolos_subset$METAL' that are not present in 'combined_subset$Anilla'
missing_values <- manolos_subset_2004_onwards$METAL[!manolos_subset_2004_onwards$METAL %in% combined_df$Anilla]
print(length(missing_values))
# There's still another 17500 missing rings.

# Prep to combine
manolos_data$CodigoAnillador <-660019
manolos_data$NombreAnillador <- "MANUEL VAZQUEZ CASTRO"
names(manolos_data)[which(names(manolos_data) == "METAL")] <- "Anilla"
names(manolos_data)[which(names(manolos_data) == "ESPECIE")] <- "NombreEspecie"
names(manolos_data)[which(names(manolos_data) == "FECHA")] <- "FechaCaptura"
names(manolos_data)[which(names(manolos_data) == "LUGAR")] <- "NombreLocalidad"
names(manolos_data)[which(names(manolos_data) == "EDAD")] <- "CodigoEdad"
names(manolos_data)[which(names(manolos_data) == "SEXO")] <- "CodigoSexo"


# Fill in missing locations data
missingLocations <- subset(combined_df, is.na(NombreLocalidad))

# Find the common Anilla values between missingLocations and manolos_data
common_anilla <- intersect(missingLocations$Anilla, manolos_data$Anilla)
# Find the corresponding rows in manolos_data
matching_rows_man <- manolos_data$Anilla %in% common_anilla
# Find the corresponding rows in combined_df
matching_rows_comb <- combined_df$Anilla %in% common_anilla
# Update NombreLocalidad in combined_df where Anilla matches
combined_df$NombreLocalidad[matching_rows_comb] <- manolos_data$NombreLocalidad[matching_rows_man]


# Iterating over each row is much slower but produces the same result
# for (i in 1:nrow(missingLocations)) {
#   # Get the Anilla value from manolos_data
#   anilla_value <- missingLocations$Anilla[i]
#   
#   # Check if the Anilla value exists in missingLocations
#   if (anilla_value %in% manolos_data$Anilla) {
#     matching_row_comb <- combined_df$Anilla == anilla_value
#     matching_row_man <- manolos_data$Anilla == anilla_value
#     combined_df$NombreLocalidad[matching_row_comb] <- manolos_data$NombreLocalidad[matching_row_man]
#   }
# }

# That resolved basically ALL of the missing localities :D
# length(subset(combined_df, is.na(NombreLocalidad)))


# Add the missing rings from Manolo's data to the combined sheet
new_anilla <- setdiff(manolos_data$Anilla, combined_df$Anilla)

if (length(new_anilla) > 0) {
  # Create a subset of manolos_data with only the new Anilla values
  subset_manolos_data <- manolos_data[manolos_data$Anilla %in% new_anilla]
  
  # Add NA values for missing columns
  missing_columns <- setdiff(colnames(combined_df), colnames(subset_manolos_data))
  for (col in missing_columns) {
    subset_manolos_data[[col]] <- NA
  }
  subset_manolos_data[[DataFrameName]] <- "Manolo"
  subset_manolos_data <- subset(subset_manolos_data, select=colnames(combined_df))
  
  # Append subset_manolos_data to combined_df
  combined_df <- rbind(combined_df, subset_manolos_data)
}


(missing_counts <- colSums(is.na(combined_df)))

write.csv(combined_df, file = './data/combined_anillamiento.csv', row.names = FALSE)

