"
Author: Sophie Berdugo 
Emails: sophie.berdugo@anthro.ox.ac.uk
Affiliation: Institute of Human Sciences, University of Oxford
"

#clean environment
rm(list = ls())

library(tidyverse) 
library(readxl)

################################################################################################################################################

### LOAD THE DATA ###

#load data
subject_videos_1992 = read_excel("~/Documents/Oxford/DPhil/Project Components/Methods/Data/Subjects UIDs by Year/1992_subject_UIDs.xlsx")
subject_videos_1993 = read_excel("~/Documents/Oxford/DPhil/Project Components/Methods/Data/Subjects UIDs by Year/1993_subject_UIDs.xlsx")
subject_videos_1994 = read_excel("~/Documents/Oxford/DPhil/Project Components/Methods/Data/Subjects UIDs by Year/1994_subject_UIDs.xlsx")
subject_videos_1995 = read_excel("~/Documents/Oxford/DPhil/Project Components/Methods/Data/Subjects UIDs by Year/1995_subject_UIDs.xlsx")
subject_videos_1996 = read_excel("~/Documents/Oxford/DPhil/Project Components/Methods/Data/Subjects UIDs by Year/1996_subject_UIDs.xlsx")
subject_videos_1997 = read_excel("~/Documents/Oxford/DPhil/Project Components/Methods/Data/Subjects UIDs by Year/1997_subject_UIDs.xlsx")
subject_videos_1999 = read_excel("~/Documents/Oxford/DPhil/Project Components/Methods/Data/Subjects UIDs by Year/1998_subject_UIDs.xlsx")
subject_videos_1999 = read_excel("~/Documents/Oxford/DPhil/Project Components/Methods/Data/Subjects UIDs by Year/1999_subject_UIDs.xlsx")
subject_videos_2000 = read_excel("~/Documents/Oxford/DPhil/Project Components/Methods/Data/Subjects UIDs by Year/2000_subject_UIDs.xlsx")
subject_videos_2002 = read_excel("~/Documents/Oxford/DPhil/Project Components/Methods/Data/Subjects UIDs by Year/2002_subject_UIDs.xlsx")
subject_videos_2003 = read_excel("~/Documents/Oxford/DPhil/Project Components/Methods/Data/Subjects UIDs by Year/2003_subject_UIDs.xlsx")
subject_videos_2004 = read_excel("~/Documents/Oxford/DPhil/Project Components/Methods/Data/Subjects UIDs by Year/2004_subject_UIDs.xlsx")
subject_videos_2005 = read_excel("~/Documents/Oxford/DPhil/Project Components/Methods/Data/Subjects UIDs by Year/2005_subject_UIDs.xlsx")
subject_videos_2006 = read_excel("~/Documents/Oxford/DPhil/Project Components/Methods/Data/Subjects UIDs by Year/2006_subject_UIDs.xlsx")
subject_videos_2008 = read_excel("~/Documents/Oxford/DPhil/Project Components/Methods/Data/Subjects UIDs by Year/2008_subject_UIDs.xlsx")
subject_videos_2009 = read_excel("~/Documents/Oxford/DPhil/Project Components/Methods/Data/Subjects UIDs by Year/2009_subject_UIDs.xlsx")
subject_videos_2012 = read_excel("~/Documents/Oxford/DPhil/Project Components/Methods/Data/Subjects UIDs by Year/2012_subject_UIDs.xlsx")
subject_videos_2013 = read_excel("~/Documents/Oxford/DPhil/Project Components/Methods/Data/Subjects UIDs by Year/2013_subject_UIDs.xlsx")
subject_videos_2014 = read_excel("~/Documents/Oxford/DPhil/Project Components/Methods/Data/Subjects UIDs by Year/2014_subject_UIDs.xlsx")
subject_videos_2015 = read_excel("~/Documents/Oxford/DPhil/Project Components/Methods/Data/Subjects UIDs by Year/2015_subject_UIDs.xlsx")
subject_videos_2016 = read_excel("~/Documents/Oxford/DPhil/Project Components/Methods/Data/Subjects UIDs by Year/2016_subject_UIDs.xlsx")
subject_videos_2017 = read_excel("~/Documents/Oxford/DPhil/Project Components/Methods/Data/Subjects UIDs by Year/2017_subject_UIDs.xlsx")

################################################################################################################################################

### 1992 ###

## Fana
  
fana_92 <- c(1,3,9,10,11,15,16,17,18) #Create vector for the video codes which the individual is present in
set.seed(123) #set the seed to make the sequence of random numbers replicable
sample(fana_92, 9, replace = FALSE) #sample without replacement to get randomly ordered videos from which to sample

## Yo
  
yo_92 <- c(1,3,4,5,6,7,9,11,13,14,18,19)
set.seed(123)
sample(yo_92, 12, replace = FALSE)

## Velu
  
velu_92 <- c(2,6,7,9)
set.seed(123)
sample(velu_92, 4, replace = FALSE)

## Kai
  
kai_92 <- c(3,8,9,10,11,12,14,15,16,17)
set.seed(123)
sample(kai_92, 10, replace = FALSE)

## Jire
  
jire_92 <- c(2,3,4,5,6,8,9,10,11,12,13,14,16,17)
set.seed(123)
sample(jire_92, 14, replace = FALSE)

## Ja

ja_92 <- c(3,5,7,8,9,10,11,12,13,14,16,17)
set.seed(123)
sample(ja_92, 12, replace = FALSE)

## Tua

tua_92 <- c(1,3,4,7,8,9,11,12,14,15,17,18) 
set.seed(123) 
sample(tua_92, 12, replace = FALSE) 

## Foaf
  
foaf_92 <- c(1,3,8,9,10,11,13,15,16,17,18)
set.seed(123)
sample(foaf_92, 11, replace = FALSE)

## Vui
  
vui_92 <- c(2,6,7,9,13,18,19)
set.seed(123)
sample(vui_92, 7, replace = FALSE)

## Na**
  
na_92 <- c(2,3,4,8,11,12,17,19)
set.seed(123)
sample(na_92, 8, replace = FALSE)


################################################################################################################################################

### 1993 ###

## Yo

yo_93 <- c(2,3,5,6,7,8,9,10)
set.seed(123)
sample(yo_93, 7, replace = FALSE)

## Kai

kai_93 <- 1
print(kai_93)

## Jire

jire_93 <- 6
print(jire_93)

## Ja

ja_93 <- 6  
print(ja_93)

## Pili

pili_93 <- c(5,7,10) 
set.seed(123) 
sample(pili_93, 3, replace = FALSE) 

## Tua

tua_93 <- 1
print(tua_93)

## Foaf
  
foaf_93 <- c(1,2,5,7)
set.seed(123)
sample(foaf_93, 4, replace = FALSE)

## Na
  
na_93 <- c(4,7,10)
set.seed(123)
sample(na_93, 3, replace = FALSE)


################################################################################################################################################

### 1994 ###

## Fana
  
fana_94 <- c(2,3,4)
set.seed(123)
sample(fana_94, 3, replace = FALSE)

## Yo

yo_94 <- c(2,3,5)
set.seed(123)
sample(yo_94, 3, replace = FALSE)

## Velu

velu_94 <- 2
print(velu_94)

## Kai

kai_94 <- c(2,3,4,5) 
set.seed(123) 
sample(kai_94, 4, replace = FALSE) 

## Jire

jire_94 <- c(2,3,4,5) 
set.seed(123) 
sample(jire_94, 4, replace = FALSE) 

## Pili

pili_94 <- c(1,2,5,6) 
set.seed(123) 
sample(pili_94, 4, replace = FALSE) 

## Tua

tua_94 <- c(2,3,4)
set.seed(123)
sample(tua_94, 3, replace = FALSE)

## Foaf

foaf_94 <- c(2,4)
set.seed(123)
sample(foaf_94, 2, replace = FALSE)

## Vui
  
vui_94 <- c(1,2,3,5,6)
set.seed(123)
sample(vui_94, 5, replace = FALSE)

## Na

na_94 <- c(2,3,4,5,6)
set.seed(123)
sample(na_94, 5, replace = FALSE)


################################################################################################################################################

### 1995###

## Fana
  
fana_95 <- c(1,2,3,4,5,6,9,10,11,13,14,15,16,17)
set.seed(123)
sample(fana_95, 14, replace = FALSE)

## Yo

yo_95 <- c(2,3,4,5,7,8,9,10,11,14,15,16)
set.seed(123)
sample(yo_95, 12, replace = FALSE)

## Velu

velu_95 <- c(1,2,3,4,5,6,15)
set.seed(123)
sample(velu_95, 7, replace = FALSE)

## Kai

kai_95 <- c(1,2,3,4,5,6,7,8,11,12,13,15,16,17) 
set.seed(123) 
sample(kai_95, 14, replace = FALSE) 

## Jire

jire_95 <- c(1,2,3,4,5,6,7,8,9,10,11,13,14,15,16,17) 
set.seed(123) 
sample(jire_95, 16, replace = FALSE) 

## Pili

pili_95 <- c(1,3,4,5,7,10,12,14,15,16,17) 
set.seed(123) 
sample(pili_95, 11, replace = FALSE) 

## Tua

tua_95 <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
set.seed(123)
sample(tua_95, 17, replace = FALSE)

## Foaf

foaf_95 <- c(1,3,4,5,6,7,8,9,10,11,12,14,16)
set.seed(123)
sample(foaf_95, 13, replace = FALSE)

## Vui
  
vui_95 <- c(1,2,3,7,8,9,10,12,15,17)
set.seed(123)
sample(vui_95, 10, replace = FALSE)

## Na

na_95 <- c(1,3,4,5,6,7,8,10,12,13,15,17)
set.seed(123)
sample(na_95, 12, replace = FALSE)


################################################################################################################################################

### 1996 ###

## Fana

fana_96 <- c(2,3,4,7,10,11,16,17,21,22,25,26,28,31)
set.seed(123)
sample(fana_96, 14, replace = FALSE)

## Yo

yo_96 <- c(1,2,5,6,7,8,9,10,12,13,14,16,17,18,19,22,23,24,26,31,32,33)
set.seed(123)
sample(yo_96, 22, replace = FALSE)

## Velu

velu_96 <- c(1,2,3,4,9,10,20,21,22,24,26,27,28,30)
set.seed(123)
sample(velu_96, 14, replace = FALSE)

## Kai

kai_96 <- c(3,4,5,7,9,13,15,16,18,19,20,21,25,26,27,28,29,30,32) 
set.seed(123) 
sample(kai_96, 19, replace = FALSE) 

## Jire

jire_96 <- c(1,2,4,6,7,8,10,13,14,15,16,18,20,21,22,23,26,32) 
set.seed(123) 
sample(jire_96, 18, replace = FALSE) 

## Pili

pili_96 <- c(1,5,9,14,17,18,23,24,29,32,33) 
set.seed(123) 
sample(pili_96, 11, replace = FALSE) 

## Tua

tua_96 <- c(3,5,7,9,11,13,14,16,17,20,21,24,28,29,31,33)
set.seed(123)
sample(tua_96, 16, replace = FALSE)

## Foaf

foaf_96 <- c(1,2,4,6,7,8,10,12,13,15,16,19,20,22,23,25,26,28,29)
set.seed(123)
sample(foaf_96, 19, replace = FALSE)

## Vui
  
vui_96 <- c(1,2,3,4,5,6,8,9,10,11,12,14,15,16,17,18,19,20,21,22,23,24,29,31,32,33)
set.seed(123)
sample(vui_96, 26, replace = FALSE)

## Na

na_96 <- c(3,7,9,17,21,24,25,28)
set.seed(123)
sample(na_96, 8, replace = FALSE)


################################################################################################################################################

### 1997 ###

## Fana
fana_97 <- c(1,2,6,7,8,9,11)
set.seed(123)
sample(fana_97, 7, replace = FALSE)

## Yo

yo_97 <- c(3,4,6,9)
set.seed(123)
sample(yo_97, 4, replace = FALSE)

## Velu

velu_97 <- c(2,3)
set.seed(123)
sample(velu_97, 2, replace = FALSE)

## Kai

kai_97 <- c(1,4,8,11) 
set.seed(123) 
sample(kai_97, 4, replace = FALSE) 

## Jire

jire_97 <- c(2,3,5,6,7,9,10,11) 
set.seed(123) 
sample(jire_97, 8, replace = FALSE) 

## Pili

pili_97 <- c(3,6) 
set.seed(123) 
sample(pili_97, 2, replace = FALSE) 

## Tua

tua_97 <- c(1,2,6,8,10,11)
set.seed(123)
sample(tua_97, 6, replace = FALSE)

## Foaf

foaf_97 <- c(1,2,3,5,6,7,8)
set.seed(123)
sample(foaf_97, 7, replace = FALSE)

## Vui

vui_97 <- c(2,3,9,11)
set.seed(123)
sample(vui_97, 4, replace = FALSE)


################################################################################################################################################

### 1998 ###

## Fana
  
fana_98 <- c(1,3,4,5,7,9)
set.seed(123)
sample(fana_98, 6, replace = FALSE)

## Yo

yo_98 <- c(4,5,6,8,11)
set.seed(123)
sample(yo_98, 5, replace = FALSE)

## Velu

velu_98 <- c(2,5,7,11)
set.seed(123)
sample(velu_98, 4, replace = FALSE)

## Jire

jire_98 <- c(1,2,3,4,5,6,7,9) 
set.seed(123) 
sample(jire_98, 8, replace = FALSE) 

## Tua

tua_98 <- c(1,3)
set.seed(123)
sample(tua_98, 2, replace = FALSE)

## Foaf

foaf_98 <- c(3,6)
set.seed(123)
sample(foaf_98, 2, replace = FALSE)

## Vui

vui_98 <- c(3,6,10)
set.seed(123)
sample(vui_98, 3, replace = FALSE)

## Yolo

yolo_98 <- c(3,4,5,6,7,8,11)
set.seed(123)
sample(yolo_98, 7, replace = FALSE)

## Vuavua

vuavua_98 <- c(2,4,5,7,9,10)
set.seed(123)
sample(vuavua_98, 6, replace = FALSE)

## Fotaiu

fotaiu_98 <- c(1,4,11)
set.seed(123)
sample(fotaiu_98, 3, replace = FALSE)

################################################################################################################################################

### 1999 ###

## Fana
  
fana_99 <- c(3,6,7,8,9,10,11,12,13,14,16,17,18,19,21,23) 
set.seed(123) 
sample(fana_99, 16, replace = FALSE) 

## Yo

yo_99 <- c(1,2,3,7,9,10,14,20)
set.seed(123)
sample(yo_99, 8, replace = FALSE)

## Velu

velu_99 <- c(1,2,4,21)
set.seed(123)
sample(velu_99, 4, replace =FALSE)

## Jire

jire_99 <- c(3,4,5,7,8,10,11,12,14,16,18,19,20,21,23) 
set.seed(123) 
sample(jire_99, 15, replace = FALSE) 

## Kai

kai_99 <- c(1,4,6,10,13,16,17,18,19,21,23) 
set.seed(123) 
sample(kai_99, 11, replace = FALSE) 

## Pili

pili_99 <- c(10,14,18,23) 
set.seed(123) 
sample(pili_99, 4, replace = FALSE) 

## Tua

tua_99 <- c(1,2,3,5,6,7,8,9,12,15,17,19,20)
set.seed(123)
sample(tua_99, 13, replace = FALSE)

## Foaf

foaf_99 <- c(1,2,3,4,5,7,8,10,11,12,14,16,17,20,21,23)
set.seed(123)
sample(foaf_99, 16, replace = FALSE)

## Vui

vui_99 <- c(1,2,4,5,8,11,12,14,16,20,21,23)
set.seed(123)
sample(vui_99, 12, replace = FALSE)

## Yolo

yolo_99 <- c(1,2,3,5,7,8,9,10,12,13,15,21,22)
set.seed(123)
sample(yolo_99, 13, replace = FALSE)

## Vuavua

vuavua_99 <- c(1,2,4,7,8,9,11,12,13,15,19,20,21)
set.seed(123)
sample(vuavua_99, 13, replace = FALSE)

## Fotaiu

fotaiu_99 <- c(4,6,8,9,10,11,12,13,14,15,16,17,19,21,22,23)
set.seed(123)
sample(fotaiu_99, 16, replace = FALSE)

################################################################################################################################################

### 2000 ###

## Fana

fana_00 <- c(3,6,7,8,9,13,19,21) 
set.seed(123) 
sample(fana_00, 8, replace = FALSE) 

## Yo

yo_00 <- c(1,3,4,7,9,10,12,13,16,17,18,20,21,22)
set.seed(123)
sample(yo_00, 14, replace = FALSE)

## Velu

velu_00 <- c(2,3,5,8,15,16,18)
set.seed(123)
sample(velu_00, 7, replace =FALSE)

## Jire

jire_00 <- c(2,3,4,5,6,7,9,12,13,14,15,18,19,20,21,22) 
set.seed(123) 
sample(jire_00, 16, replace = FALSE) 

## Kai

kai_00 <- c(1,2,12,13,14,15,16,19,20,22) 
set.seed(123) 
sample(kai_00, 10, replace = FALSE) 

## Pili

pili_00 <- c(2,5,13,16,20,21) 
set.seed(123) 
sample(pili_00, 6, replace = FALSE) 

## Tua

tua_00 <- c(6,8,9,11,13,14,15,16,20)
set.seed(123)
sample(tua_00, 9, replace = FALSE)

## Foaf

foaf_00 <- c(2,3,4,5,6,7,8,9,12,13,16,17,18,20,22)
set.seed(123)
sample(foaf_00, 15, replace = FALSE)

## Yolo

yolo_00 <- c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,18,20,21,22)
set.seed(123)
sample(yolo_00, 20, replace = FALSE)

## Vuavua

vuavua_00 <- c(1,2,3,5,7,8,9,16,17,18,20,21,22)
set.seed(123)
sample(vuavua_00, 13, replace = FALSE)

## Fotaiu

fotaiu_00 <- c(2,3,4,5,6,7,8,9,12,15,16,17,18,19,20,21,22)
set.seed(123)
sample(fotaiu_00, 17, replace = FALSE)

## Poni

poni_00 <- 21
print(poni_00)

## Nto

nto_00 <- c(6,10,11,12,15,16,17,18,21,22)
set.seed(123)
sample(nto_00, 10, replace = FALSE)


################################################################################################################################################

### 2002 ###

## Fana
  
fana_02 <- c(1,5,8,13,17,18,29,36,37,40,41,42,46,50,53,54,56,57,59,61,72,73,76,92,103,106,107,108,114) 
set.seed(123)
sample(fana_02, 29, replace = FALSE) 

## Yo

yo_02 <- c(2,5,10,12,23,25,28,34,35,36,38,43,47,49,51,52,60,61,64,65,68,77,78,80,82,85,94,105,107,120,121)
set.seed(123)
sample(yo_02, 31, replace = FALSE)

## Velu

velu_02 <- c(1,2,5,11,17,18,19,20,25,26,27,32,33,43,45,46,52,53,58,65,68,69,71,76,79,80,81,82,85,87,91,98,100,104,110,112,113,114,118)
set.seed(123)
sample(velu_02, 39, replace =FALSE)

## Jire

jire_02 <- c(2,3,4,5,6,7,8,9,10,13,14,16,18,19,20,21,22,23,28,29,30,31,32,33,34,36,39,40,41,42,43,48,49,50,51,52,53,54,55,57,60,61,62,63,64,65,68,69,70,72,73,74,75,76,77,78,80,81,82,83,84,86,87,89,90,92,93,94,95,96,97,99,100,101,102,104,105,108,109,110,112,113,114,115,116,117,118,119,120,121) 
set.seed(123) 
sample(jire_02, 90, replace = FALSE) 

## Kai
  
kai_02 <- c(2,5,6,7,9,10,13,14,15,18,19,22,26,27,28,29,30,33,35,38,39,41,42,43,47,50,52,57,58,61,62,63,65,67,69,72,73,74,75,76,78,79,84,85,88,89,90,91,92,93,96,97,98,99,100,101,102,106,108,109,113,114,116,118,119,120)
set.seed(123)
sample(kai_02, 66, replace = FALSE)

## Tua
  
tua_02 <- c(1,5,9,13,14,17,18,27,32,33,36,42,50,54,56,59,61,72,73,75,76,83,85,89,92,103,106,108,111,112,114,117,119)
set.seed(123)
sample(tua_02, 33, replace = FALSE)

## Foaf

foaf_02 <- c(9,13,17,27,28,33,40,54,59,76,85,103,107,110,117,119)
set.seed(123)
sample(foaf_02, 16, replace = FALSE)

## Yolo

yolo_02 <- c(2,5,9,10,12,17,18,19,23,28,31,32,33,34,35,39,40,43,49,51,54,56,57,58,60,61,63,64,65,66,68,69,73,76,77,79,80,85,94,95,100,101,102,103,104,105,107,112,114,119,121)
set.seed(123)
sample(yolo_02, 51, replace = FALSE)

## Poni

poni_02 <- c(9,10,13,24,27,28,42,44,59,61,73,103,107,108,111,117,119)
set.seed(123)
sample(poni_02, 17, replace = FALSE)

## Vuavua

vuavua_02 <- c(1,2,5,9,17,18,19,20,22,24,25,26,27,32,33,40,42,43,45,46,50,52,53,54,57,58,61,65,68,72,73,76,79,80,81,82,85,87,89,91,98,100,101,103,104,108,109,110,112,113,116,117,118,121)
set.seed(123)
sample(vuavua_02, 54, replace = FALSE)

## Fotaiu
  
fotaiu_02 <- c(3,4,13,14,15,16,19,21,22,23,29,30,31,32,34,35,39,40,42,48,49,51,52,55,56,57,60,61,62,64,66,67,69,70,73,74,75,76,77,78,79,82,83,84,85,87,90,92,93,94,96,97,99,100,104,105,106,107,109,110,112,114,115,116,117,118,120)
set.seed(123)
sample(fotaiu_02, 67, replace = FALSE)


################################################################################################################################################

### 2003 ###

## Fana

fana_03 <- c(1,3,5,6,7,11,12,20,22,26,29,30,31,32,34,37,38,39,40,42,43,44,46,47,48,50,51,53,54,55,57,66,68,71,72,73,79,82,83,84,85) 
set.seed(123) 
sample(fana_03, 41, replace = FALSE) 

## Yo
  
yo_03 <- c(7,10,16,17,21,28,35,36,45,48,50,51,54,65,67,70,73,75,78,84)
set.seed(123)
sample(yo_03, 20, replace = FALSE)

## Velu

velu_03 <- c(6,7,14,22,25,33,34,38,41,44,47,49,52,53,55,62,64,65,71)
set.seed(123)
sample(velu_03, 19, replace =FALSE)

## Jire

jire_03 <- c(1,4,5,6,7,9,10,13,14,15,16,17,19,20,22,24,26,28,29,30,32,33,35,36,37,39,40,41,42,45,47,49,50,51,53,54,56,59,61,63,65,66,68,70,72,73,74,77,78,83,84) 
set.seed(123) 
sample(jire_03, 51, replace = FALSE) 

## Kai

kai_03 <- 38
print(kai_03)

## Tua

tua_03 <- c(1,3,4,5,6,10,11,12,14,15,17,22,23,25,26,31,32,33,34,35,36,38,40,43,46,47,52,55,56,59,60,62,64,65,68,69,70,71,72,73,74,77,79,80,82,83)
set.seed(123)
sample(tua_03, 46, replace = FALSE)

## Foaf

foaf_03 <- c(3,5,6,8,10,14,17,24,26,29,31,32,35,36,38,43,46,53,56,69,72,75,79,82)
set.seed(123)
sample(foaf_03, 24, replace = FALSE)

## Yolo

yolo_03 <- c(1,2,3,6,8,10,12,14,17,22,24,26,30,31,32,35,36,37,43,45,47,48,51,53,54,56,57,60,64,65,67,71,73,76,77,82,83)
set.seed(123)
sample(yolo_03, 37, replace = FALSE)

## Poni
  
poni_03 <- c(16,29,38,43,46)
set.seed(123)
sample(poni_03, 5, replace = FALSE)

## Vuavua

vuavua_03 <- c(6,7,10,13,14,16,19,22,24,25,27,30,33,36,38,41,44,45,47,49,52,55,56,59,62,63,64,66,67,71,72,73,74,75,77,79,81)
set.seed(123)
sample(vuavua_03, 37, replace = FALSE)

## Fotaiu

fotaiu_03 <- c(1,4,6,7,9,11,12,13,15,18,22,24,28,29,31,34,37,38,39,40,41,43,44,47,49,58,60,65,73,75,77,78,79,80,82,83,84)
set.seed(123)
sample(fotaiu_03, 37, replace = FALSE)


################################################################################################################################################

### 2004 ###

## Fana

fana_04 <- c(1,2,3,5,8,10,16,17,18,22,23,24,25,26,30,31,33,34) 
set.seed(123) 
sample(fana_04, 18, replace = FALSE) 


## Yo
  
yo_04 <- c(2,4,5,6,7,8,9,12,15,16,20,22,24,25,27,30,32)
set.seed(123)
sample(yo_04, 17, replace = FALSE)

## Velu
  
velu_04 <- c(2,6,7,8,9,13,15,20,23,26,29,31,34)
set.seed(123)
sample(velu_04, 13, replace =FALSE)

## Jire
  
jire_04 <- c(1,2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19,20,21,26,27,28,29,30,31,32,33,34,35,36) 
set.seed(123) 
sample(jire_04, 31, replace = FALSE) 

## Tua
  
tua_04 <- c(2,3,4,5,6,13,14,15,21,22,23,24,26,27,28,29,32,33,35,36)
set.seed(123)
sample(tua_04, 20, replace = FALSE)

## Foaf
  
foaf_04 <- c(2,3,4,5,6,7,9,13,14,15,16,20,21,23,24,26,28,30,31,32,33,34,35,36)
set.seed(123)
sample(foaf_04, 24, replace = FALSE)

## Yolo
  
yolo_04 <- c(2,3,4,5,6,7,9,10,12,13,14,15,16,17,18,21,22,24,26,28,30,31,32,33,35,36)
set.seed(123)
sample(yolo_04, 26, replace = FALSE)

## Vuavua

vuavua_04 <- c(12,22)
set.seed(123)
sample(vuavua_04, 2, replace = FALSE)

## Jeje

jeje_04 <- c(1,2,3,4,5,6,7,8,9,10,13,15,18,19,20,21,24,26,27,29,30,31,33,34,35,36)
set.seed(123)
sample(jeje_04, 26, replace = FALSE)

## Fanle

fanle_04 <- c(2,3,4,5,7,8,9,11,13,15,16,17,18,21,22,23,24,25,26,29,30,33,34,35,36)
set.seed(123)
sample(fanle_04, 25, replace = FALSE)


################################################################################################################################################

### 2005 ###

## Fana
  
fana_05 <- c(1,2,3,4,5,6,9,10,17,21,23,26,27,28,29,33,40,42,43,47,49,51,52,53,54) 
set.seed(123) 
sample(fana_05, 25, replace = FALSE) 

## Yo
  
yo_05 <- c(1,2,3,5,6,7,8,9,10,11,13,24,28,29,30,31,32,33,34,35,36,38,39,40,41,42,45,46,48,49,51,53,54)
set.seed(123)
sample(yo_05, 33, replace = FALSE)

## Velu
  
velu_05 <- c(3,5,8,9,11,16,22,24,25,27,28,29,31,33,34,40,45,48,50,52,53)
set.seed(123)
sample(velu_05, 21, replace =FALSE)

## Jire
  
jire_05 <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,15,16,17,18,19,20,21,23,24,25,27,28,29,30,31,33,34,36,38,40,41,42,43,44,45,46,47,48,49,51,52,53,54) 
set.seed(123) 
sample(jire_05, 46, replace = FALSE) 

## Tua
  
tua_05 <- c(2,5,7,10,18,21,26,28,32,33,36,40,41,42,44,47,48,51,52)
set.seed(123)
sample(tua_05, 19, replace = FALSE)

## Foaf
  
foaf_05 <- c(1,2,4,5,6,9,10,11,14,16,17,18,19,20,23,24,25,26,29,31,32,33,34,35,36,38,40,41,42,43,44,45,47,49,51,52,53,54)
set.seed(123)
sample(foaf_05, 38, replace = FALSE)

## Yolo

yolo_05 <- c(2,4,5,6,7,8,10,12,14,16,18,20,21,25,26,28,30,32,34,36,37,40,41,42,44,45,46,47,48,51,52)
set.seed(123)
sample(yolo_05, 31, replace = FALSE)

## Jeje
  
jeje_05 <- c(1,2,3,4,6,7,8,9,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,43,44,45,46,47,48,49,51,52,53,54)
set.seed(123)
sample(jeje_05, 50, replace = FALSE)

## Peley
  
peley_05 <- c(4,5,6,7,8,13,14,19,20,22,25,26,28,30,33,34,36,40,42,43,44,46,47,48,51)
set.seed(123)
sample(peley_05, 25, replace = FALSE)

## Fanle
  
fanle_05 <- c(1,2,3,4,5,6,9,10,13,17,18,20,21,23,24,26,27,28,29,32,33,36,40,41,42,43,44,45,47,49,52,53,54)
set.seed(123)
sample(fanle_05, 33, replace = FALSE)


################################################################################################################################################

### 2006 ###

## Fana

fana_06 <- c(3,5,7,10,13) 
set.seed(123)
sample(fana_06, 5, replace = FALSE) 

## Yo

yo_06 <- c(2,4,5,6,8,12,14,15,18)
set.seed(123)
sample(yo_06, 9, replace = FALSE)

## Velu
  
velu_06 <- c(2,14,15,18)
set.seed(123)
sample(velu_06, 4, replace =FALSE)

## Jire

jire_06 <- c(2,3,5,6,7,8,9,10,11,12,13,14,15,17,18,19) 
set.seed(123) 
sample(jire_06, 16, replace = FALSE) 

## Tua
  
tua_06 <- c(4,8,11,14,15,17,18)
set.seed(123)
sample(tua_06, 7, replace = FALSE)

## Foaf

foaf_06 <- c(2,4,6,8,10,11,12,13,14,15,16,17,18)
set.seed(123)
sample(foaf_06, 13, replace = FALSE)

## Yolo

yolo_06 <- c(3,4,6,7,8,9,10,11,12,13,14,15,16,17,18,19)
set.seed(123)
sample(yolo_06, 16, replace = FALSE)

## Jeje

jeje_06 <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)
set.seed(123)
sample(jeje_06, 18, replace = FALSE)

## Peley

peley_06 <- c(3,5,6,7,8,11,12,13,14,15,17,18,19)
set.seed(123)
sample(peley_06, 13, replace = FALSE)

## Fanle

fanle_06 <- c(2,3,4,5,6,7,8,11,13,14,15,17,18,19)
set.seed(123)
sample(fanle_06, 14, replace = FALSE)


################################################################################################################################################

### 2007 ###

## Fana

fana_07 <- c(3,4,5,8)
set.seed(123) 
sample(fana_07, 4, replace = FALSE) 

## Yo
  
yo_07 <- c(4,5,7)
set.seed(123)
sample(yo_07, 3, replace = FALSE)

## Velu - There are no videos in 2007 in which Velu is visible nut-cracking.

## Jire

jire_07 <- c(1,2,3,4,6,7)
set.seed(123)
sample(jire_07, 6, replace = FALSE)

## Tua

tua_07 <- c(2,3,4,6,7)
set.seed(123)
sample(tua_07, 5, replace = FALSE)

## Foaf
  
foaf_07 <- c(2,3,6)
set.seed(123)
sample(foaf_07, 3, replace = FALSE)

## Yolo

yolo_07 <- c(1,2,3,4,5,7,8)
set.seed(123)
sample(yolo_07, 7, replace = FALSE)

## Jeje
  
jeje_07 <- c(1,2,3,6,7,8)
set.seed(123)
sample(jeje_07, 6, replace = FALSE)

## Peley
  
peley_07 <- c(1,3,4,5,7)
set.seed(123)
sample(peley_07, 5, replace = FALSE)

## Fanle

fanle_07 <- c(3,4,5,7)
set.seed(123)
sample(fanle_07, 4, replace = FALSE)


################################################################################################################################################

### 2008 ###

## Fana
  
fana_08 <- c(3,4,7,9,13,16,19,20,24,25,26) 
set.seed(123) 
sample(fana_08, 11, replace = FALSE) 

## Yo

yo_08 <- c(1,2,3,5,6,9,11,12,13,14,15,17,19,20,21,23,24,25,26) 
set.seed(123) 
sample(yo_08, 19, replace = FALSE) 

## Velu

velu_08 <- c(3,5,13,17,19) 
set.seed(123) 
sample(velu_08, 5, replace = FALSE) 

## Jire
  
jire_08 <- c(1,2,3,4,5,6,7,9,16,17,19,20,21,23,24,25) 
set.seed(123) 
sample(jire_08, 16, replace = FALSE) 

## Tua

tua_08 <- c(6,7,13,16,19,20,23,24)
set.seed(123) 
sample(tua_08, 8, replace = FALSE) 

## Foaf
  
foaf_08 <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,19,20,21,23,24,25,26) 
set.seed(123) 
sample(foaf_08, 21, replace = FALSE) 

## Yolo

yolo_08 <- c(1,2,3,4,6,7,8,9,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26) 
set.seed(123) 
sample(yolo_08, 24, replace = FALSE) 

## Jeje
  
jeje_08 <- c(1,2,3,4,5,6,7,8,9,12,13,14,15,17,18,19,20,21,22,23,24,25,26) 
set.seed(123) 
sample(jeje_08, 23, replace = FALSE) 

## Peley
  
peley_08 <- c(1,2,3,4,5,6,7,8,9,12,14,15,17,18,19,20,21,22,23,24,25) 
set.seed(123)
sample(peley_08, 21, replace = FALSE) 

## Fanle
  
fanle_08 <- c(1,3,4,5,7,9,12,13,14,15,17,19,20,21,22,24,25) 
set.seed(123) 
sample(fanle_08, 17, replace = FALSE) 


################################################################################################################################################

### 2009 ###

## Fana

fana_09 <- c(3,4,6,10,14,18,19,33,40) 
set.seed(123) 
sample(fana_09, 9, replace = FALSE) 

## Yo

yo_09 <- c(3,4,5,6,7,8,9,12,13,14,16,17,18,22,24,25,26,28,29,34,35,36,37,38,40) 
set.seed(123) 
sample(yo_09, 25, replace = FALSE) 

## Velu

velu_09 <- c(9,17,40)
set.seed(123) 
sample(velu_09, 3, replace = FALSE) 

## Jire

jire_09 <- c(3,4,5,11,12,13,18,19,20,21,27,28,31,33,36,39,40) 
set.seed(123) 
sample(jire_09, 17, replace = FALSE) 

## Tua
  
tua_09 <- c(5,6,8,9,10,11,12,16,17,18,24,30,31,33,34,35,36,38,39)
set.seed(123) 
sample(tua_09,19, replace = FALSE) 

## Foaf
  
foaf_09 <- c(3,5,6,8,9,10,11,12,13,15,16,17,18,19,20,23,24,25,26,28,29,31,32,33,34,35,37,38,39,40) 
set.seed(123)
sample(foaf_09, 30, replace = FALSE) 

## Yolo

yolo_09 <- c(3,4,5,6,8,9,12,13,14,15,16,17,18,20,22,23,24,25,26,28,31,34,35,36,37,38,40) 
set.seed(123) 
sample(yolo_09, 27, replace = FALSE) 

## Jeje
  
jeje_09 <- c(2,3,4,6,7,8,9,10,11,12,13,15,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40) 
set.seed(123) 
sample(jeje_09, 36, replace = FALSE) 

## Peley

peley_09 <- c(2,3,4,5,6,7,8,9,10,12,13,15,16,17,18,19,21,22,23,24,25,26,27,28,29,30,31,32,34,35,36,37,38,40) 
set.seed(123) 
sample(peley_09, 34, replace = FALSE) 

## Fanle
  
fanle_09 <- c(2,3,4,5,6,9,10,11,14,16,18,19,22,25,26,28,29,30,31,32,33,36,38,40)
set.seed(123) 
sample(fanle_09, 24, replace = FALSE) 


################################################################################################################################################

### 2010 ###

## Fana
  
fana_10 <- c(5,11,14,20,22,27,31) 
set.seed(123) 
sample(fana_10, 7, replace = FALSE) 

## Yo

yo_10 <- c(1,15,16,23,24,30,32) 
set.seed(123) 
sample(yo_10, 7, replace = FALSE) 

## Velu
  
velu_10 <- c(6,8,12) 
set.seed(123) 
sample(velu_10, 3, replace = FALSE) 

## Jire
  
jire_10 <- c(3,5,7,8,9,11,13,15,20,22,23,26,28,30,33) 
set.seed(123) 
sample(jire_10, 15, replace = FALSE) 

## Tua

tua_10 <- c(1,2,4,5,6,7,8,9,12,13,14,15,16,17,21,23,24,25,27,29,30,31,32) 
set.seed(123) 
sample(tua_10, 23, replace = FALSE) 

## Foaf

foaf_10 <- c(1,3,4,5,7,8,9,11,12,13,14,15,16,17,20,21,22,23,24,25,26,27,28,30,31,33) 
set.seed(123) 
sample(foaf_10, 26, replace = FALSE) 

## Jeje

jeje_10 <- c(1,2,3,4,5,7,8,11,12,13,14,15,16,18,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34)
set.seed(123) 
sample(jeje_10, 29, replace = FALSE) 

## Peley
  
peley_10 <- c(1,2,3,4,5,7,8,11,12,14,15,16,20,21,22,23,24,25,26,27,28,29,30,31,32,33) 
set.seed(123) 
sample(peley_10, 26, replace = FALSE) 

## Fanle

fanle_10 <- c(1,2,4,5,8,11,12,20,21,22,25,27,31) 
set.seed(123) 
sample(fanle_10, 13, replace = FALSE) 

## Joya

joya_10 <- c(1,2,3,4,5,7,8,13,14,16,20,21,22,24,25,27,28,29,30,31,33) 
set.seed(123) 
sample(joya_10, 21, replace = FALSE) 


################################################################################################################################################

### 2012 ###

## Fana
  
fana_12 <- c(26,45) 
set.seed(123) 
sample(fana_12, 2, replace = FALSE) 

## Yo

yo_12 <- c(29,60) 
set.seed(123)
sample(yo_12, 2, replace = FALSE) 

## Velu
  
velu_12 <- c(25,34) 
set.seed(123) 
sample(velu_12, 2, replace = FALSE) 

## Jire
  
jire_12 <- c(2,3,6,7,9,13,14,16,17,26,30,38,39,40,43,45,47,48,53,55,56,61,64) 
set.seed(123) 
sample(jire_12, 23, replace = FALSE) 

## Tua
  
tua_12 <- c(2,5,6,7,8,15,17,21,24,27,34,38,39,43,45,48,53,55,63,64) 
set.seed(123) 
sample(tua_12, 20, replace = FALSE) 

## Foaf
  
foaf_12 <- c(5,6,7,9,12,13,14,15,21,24,26,27,28,31,35,37,38,41,47,48,51,53,55,58,59,62) 
set.seed(123) 
sample(foaf_12, 26, replace = FALSE) 

## Jeje

jeje_12 <- c(4,5,7,8,10,13,14,15,17,18,20,21,26,27,28,30,31,33,34,35,37,38,39,41,44,45,46,47,48,50,51,53,54,55,57,59,61,62,63,64) 
set.seed(123) 
sample(jeje_12, 40, replace = FALSE) 

## Peley
  
peley_12 <- c(7,8,11,14,15,19,23,26,27,30,31,34,35,38,39,41,42,45,47,48,50,55,59,63,64) 
set.seed(123) 
sample(peley_12, 25, replace = FALSE) 

## Fanle
  
fanle_12 <- c(1,2,3,8,15,16,24,26,27,31,32,35,36,40,41,42,43,45,47,48,55,56,59,61,63) 
set.seed(123) 
sample(fanle_12, 25, replace = FALSE) 

## Joya
  
joya_12 <- c(1,2,3,5,7,9,10,14,15,16,21,22,23,24,26,27,28,30,31,32,33,35,36,37,38,40,41,47,48,49,51,52,55,56,61) 
set.seed(123) 
sample(joya_12, 35, replace = FALSE) 


################################################################################################################################################

### 2013 ###

## Fana
  
fana_13 <- c(1,8,9,10,11,13,14,15,16,17,18,19,20,21,30,31,32,33,48,49,50,51,53,54,56,82,85,86,108,109,111,112,125,126) 
set.seed(123) 
sample(fana_13,33, replace = FALSE)

## Yo

yo_13 <- c(55,78,79,80,115,117,118) 
set.seed(123) 
sample(yo_13, 7, replace = FALSE) 

## Velu

velu_13 <- c(83,84,86,108) 
set.seed(123) 
sample(velu_13, 4, replace = FALSE) 

## Jire

jire_13 <- c(4,8,17,18,23,37,38,41,43,60,61,63,65,67,70,73,79,80,90,91,92,93,94,95,104,105,106,108,109,111,113) 
set.seed(123) 
sample(jire_13, 31, replace = FALSE) 

## Foaf

foaf_13 <- c(1,2,3,4,5,6,7,8,10,11,16,23,34,35,37,38,39,40,41,42,43,44,45,46,47,60,61,62,66,68,69,71,72,90,91,98,100,101,103,107,117,118,119,120,121) 
set.seed(123) 
sample(foaf_13, 45, replace = FALSE) 

## Jeje

jeje_13 <- c(1,3,4,6,7,8,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,28,29,30,34,35,36,37,39,48,49,51,52,56,60,61,64,72,74,94,96,97,98,99,102,107,108,110,113,114,116,119,120)
set.seed(123) 
sample(jeje_13, 54, replace = FALSE) 

## Fanle
  
fanle_13 <- c(1,8,9,10,11,12,13,14,15,53,54,56,57,58,59,75,76,77,78,81,82,85,86,87,88,89,107,109,110,111,112,122,123,124,125,126) 
set.seed(123) 
sample(fanle_13, 36, replace = FALSE) 


################################################################################################################################################

### 2014 ###

## Foaf

foaf_14 <- 7
print(foaf_14)

## Jeje

jeje_14 <- c(1,2,3,4,7,8,9,10,15) 
set.seed(123)
sample(jeje_14, 9, replace = FALSE) 

## Fanle

fanle_14 <- c(5,6,10,11,14) 
set.seed(123)
sample(fanle_14, 5, replace = FALSE) 

## Flanle

flanle_14 <- c(10,12,13,14) 
set.seed(123) 
sample(flanle_14, 4, replace = FALSE) 


################################################################################################################################################

### 2015 ###

## Fana

fana_15 <- c(6,8,20,21,22,38,42,43,44) 
set.seed(123) 
sample(fana_15, 9, replace = FALSE) 

## Yo

yo_15 <- c(12,13,42) 
set.seed(123) 
sample(yo_15, 3, replace = FALSE) 

## Velu
  
velu_15 <- c(11,12,13,27,28,29) 
set.seed(123) 
sample(velu_15, 6, replace = FALSE) 

## Jire

jire_15 <- c(2,3,4,10,14,15,16,17,18,19,20,24,26,28,29,38,39,40,42,43,44) 
set.seed(123) 
sample(jire_15, 21, replace = FALSE) 

## Foaf
  
foaf_15 <- c(1,2,3,4,7,8,23,25,26,27,28,29) 
set.seed(123) 
sample(foaf_15, 12, replace = FALSE) 

## Jeje

jeje_15 <- c(1,13,17,20,21,24,26,38) 
set.seed(123) 
sample(jeje_15, 8, replace = FALSE) 

## Fanle

fanle_15 <- c(3,5,6,7,8,9,30,31,32,33,34,35,36,37,38,40,41,42,43,44,45) 
set.seed(123)
sample(fanle_15, 21, replace = FALSE) 


################################################################################################################################################

### 2016 ###

## Fana
  
fana_16 <- c(7,10,11,12,14,23,24) 
set.seed(123) 
sample(fana_16, 7, replace = FALSE) 

## Yo
  
yo_16 <- c(6,7,9) 
set.seed(123) 
sample(yo_16, 3, replace = FALSE) 

## Velu
  
velu_16 <- c(6,10,11)
set.seed(123) 
sample(velu_16, 3, replace = FALSE) 

## Jire
  
jire_16 <- c(8,10,11,12,13,14,15,16,19,20,21,22,23,24) 
set.seed(123) 
sample(jire_16, 14, replace = FALSE) 

## Foaf

foaf_16 <- c(2,3,4,6,8,9,20,23,24) 
set.seed(123) 
sample(foaf_16, 9, replace = FALSE) 

## Jeje
  
jeje_16 <- c(2,3,4,5,6,8,9,14,15,17,18,19,20,21,22,23,24) 
set.seed(123) 
sample(jeje_16, 17, replace = FALSE) 

## Fanle
  
fanle_16 <- c(1,2,3,4,5,6,8,10,11,12,13,14,15,19,20,21,22,23,24) 
set.seed(123) 
sample(fanle_16, 19, replace = FALSE)


################################################################################################################################################

### 2017 ###

## Fana

fana_17 <- c(3,5) 
set.seed(123) 
sample(fana_17, 2, replace = FALSE) 

## Jire

jire_17 <- c(1,2,3,4,5)
set.seed(123) 
sample(jire_17, 5, replace = FALSE) 

## Foaf

foaf_17 <- c(3,5) 
set.seed(123) 
sample(foaf_17, 2, replace = FALSE) 

## Jeje

jeje_17 <- c(2,3,4,5) 
set.seed(123) 
sample(jeje_17, 4, replace = FALSE) 

## Fanle

fanle_17 <- c(1,2,3,4,6)
set.seed(123) 
sample(fanle_17, 5, replace = FALSE) 

