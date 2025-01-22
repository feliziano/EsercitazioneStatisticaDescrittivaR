getwd()

setwd("/Users/felizianocopponi/RWorkspace")
getwd()

dati <- read.csv("realestate_texas.csv")

summary(dati)
N <- dim(dati)[1]
freq_ass_city <- table(dati$city)
freq_rel_city <- freq_ass_city/N
freq_rel_city
