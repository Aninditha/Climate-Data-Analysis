#====================================================================================================
#DATA UNDERSTANDING
#This includes understanding the structure of the data and imputing the missing vaules in the data.
#The data related to each country is present in different files
#Data imputation is performed on each country separately as it gives best prdictions
#====================================================================================================

#USA DATA
usa.initial <- read.csv("usa.csv",header = TRUE,sep = ",")
names(usa.initial)<-c("year","agrland","forestarea","electricityconsum","co2emission","poptot","urbanpop")
usa <- usa.initial

#structure of data is same for all the data sets
str(usa)

#variables present are 
names(usa)

#Summary can be observed as
summary(usa)

#=============================================
#IMPUTE MISSING VALUES FOR ALL DATASET
#=============================================
#USA DATA
#=============================================

#impute agrland 55,56 
usa.mreg.out <- lm(usa$agrland ~usa$forestarea+usa$poptot+usa$urbanpop)
summary(usa.mreg.out)


usa.mreg.int <- predict(usa.mreg.out,
                    data.frame(usa$forestarea,usa$poptot,usa$urbanpop),
                    interval = "confidence");
usa$agrland[c(55,56)]<-usa.mreg.int[c(55,56),1]


#impute agrland for 1 
usa.mreg.out1 <- lm(usa$agrland ~usa$electricityconsum+usa$co2emission+usa$poptot+usa$urbanpop)
summary(usa.mreg.out1)
usa.mreg.int1 <- predict(usa.mreg.out1,
                     data.frame(usa$electricityconsum,usa$co2emission,usa$poptot,usa$urbanpop),
                     interval = "confidence");
usa$agrland[1]<-usa.mreg.int1[1,1]



#impute forest data from 1:30 
usa.mreg.out2 <- lm(usa$forestarea ~usa$agrland+usa$electricityconsum+usa$co2emission+usa$poptot+usa$urbanpop)
summary(usa.mreg.out2)

usa.mreg.int2 <- predict(usa.mreg.out2,
                     data.frame(usa$agrland, usa$electricityconsum,usa$co2emission,usa$poptot,usa$urbanpop),
                     interval = "confidence");
usa$forestarea[1:30]<-usa.mreg.int2[1:30,1]


#impute electricity consumption in 55,56
usa.mreg.out3 <- lm(usa$electricityconsum ~usa$agrland+usa$forestarea+usa$poptot+usa$urbanpop)
summary(usa.mreg.out3)


usa.mreg.int3 <- predict(usa.mreg.out3,
                     data.frame(usa$agrland,usa$forestarea, usa$poptot,usa$urbanpop),
                     interval = "confidence");
usa$electricityconsum[c(55,56)]<-usa.mreg.int3[c(55,56),1]

#impute co2emission in 53:56
usa.mreg.out4 <- lm(usa$co2emission ~usa$agrland+usa$forestarea+usa$electricityconsum+usa$poptot+usa$urbanpop)
summary(usa.mreg.out4)


usa.mreg.int4 <- predict(usa.mreg.out4,
                     data.frame(usa$agrland,usa$forestarea,usa$electricityconsum+ usa$poptot,usa$urbanpop),
                     interval = "confidence");
usa$co2emission[c(53:56)]<-usa.mreg.int4[c(53:56),1]


#=============================================
#BRAZIL DATA
#=============================================

brazil.initial <-read.csv("brazil.csv",header = TRUE,sep = ",")
names(brazil.initial)<-c("year","agrland","forestarea","electricityconsum","co2emission","poptot","urbanpop")
brazil <- brazil.initial


#impute agrland 55,56 
brazil.reg.out <- lm(brazil$agrland ~brazil$forestarea+brazil$poptot+brazil$urbanpop)
summary(brazil.reg.out)


brazil.reg.int <- predict(brazil.reg.out,
                          data.frame(brazil$forestarea,brazil$poptot,brazil$urbanpop),
                          interval = "confidence");
brazil$agrland[c(55,56)]<-brazil.reg.int[c(55,56),1]

#impute agrland for 1 
brazil.reg.out1 <- lm(brazil$agrland ~brazil$co2emission+brazil$poptot+brazil$urbanpop)
summary(brazil.reg.out1)


brazil.reg.int1 <- predict(brazil.reg.out1,
                           data.frame(brazil$co2emission,brazil$poptot,brazil$urbanpop),
                           interval = "confidence");
brazil$agrland[1]<-brazil.reg.int1[1,1]

#impute electricity consumption in 55,56
brazil.reg.out3 <- lm(brazil$electricityconsum ~brazil$agrland+brazil$forestarea+brazil$poptot+brazil$urbanpop)
summary(brazil.reg.out3)


brazil.reg.int3 <- predict(brazil.reg.out3,
                           data.frame(brazil$agrland,brazil$forestarea, brazil$poptot,brazil$urbanpop),
                           interval = "confidence");
brazil$electricityconsum[c(55,56)]<-brazil.reg.int3[c(55,56),1]

#impute electricity consumption in 1-11
brazil.reg.out4 <- lm(brazil$electricityconsum ~brazil$agrland+brazil$co2emission+brazil$poptot+brazil$urbanpop)
summary(brazil.reg.out4)


brazil.reg.int4 <- predict(brazil.reg.out4,
                           data.frame(brazil$agrland,brazil$co2emission ,brazil$poptot,brazil$urbanpop),
                           interval = "confidence");
brazil$electricityconsum[c(1:11)]<-brazil.reg.int4[c(1:11),1]


#impute co2emission in 53:56
brazil.reg.out5 <- lm(brazil$co2emission ~brazil$agrland+brazil$forestarea+brazil$electricityconsum+brazil$poptot+brazil$urbanpop)
summary(brazil.reg.out5)


brazil.reg.int5 <- predict(brazil.reg.out5,
                           data.frame(brazil$agrland,brazil$forestarea,brazil$electricityconsum+ brazil$poptot,brazil$urbanpop),
                           interval = "confidence");
brazil$co2emission[c(53:56)]<-brazil.reg.int5[c(53:56),1]


#impute forest data from 1:30 
brazil.reg.out2 <- lm(brazil$forestarea ~brazil$agrland+brazil$electricityconsum+brazil$co2emission+brazil$poptot+brazil$urbanpop)
summary(brazil.reg.out2)


brazil.reg.int2 <- predict(brazil.reg.out2,
                           data.frame(brazil$agrland, brazil$electricityconsum,brazil$co2emission,brazil$poptot,brazil$urbanpop),
                           interval = "confidence");
brazil$forestarea[1:30]<-brazil.reg.int2[1:30,1]

#=============================================
#CANADA DATA
#=============================================
canada.initial<-read.csv("canada.csv",header = TRUE,sep = ",")
names(canada.initial)<-c("year","agrland","forestarea","electricityconsum","co2emission","poptot","urbanpop")
canada <- canada.initial

#impute agrland 55,56 
canada.mreg.out <- lm(canada$agrland ~canada$forestarea+canada$poptot+canada$urbanpop)
summary(canada.mreg.out)


canada.mreg.int <- predict(canada.mreg.out,
                           data.frame(canada$forestarea,canada$poptot,canada$urbanpop),
                           interval = "confidence");
canada$agrland[c(55,56)]<-canada.mreg.int[c(55,56),1]

#impute agrland for 1 
canada.mreg.out1 <- lm(canada$agrland ~canada$electricityconsum+canada$co2emission+canada$poptot+canada$urbanpop)
summary(canada.mreg.out1)


canada.mreg.int1 <- predict(canada.mreg.out1,
                            data.frame(canada$electricityconsum,canada$co2emission,canada$poptot,canada$urbanpop),
                            interval = "confidence");
canada$agrland[1]<-canada.mreg.int1[1,1]

#impute forest data from 1:30 
canada.mreg.out2 <- lm(canada$forestarea ~canada$agrland+canada$electricityconsum+canada$co2emission+canada$poptot+canada$urbanpop)
summary(canada.mreg.out2)


canada.mreg.int2 <- predict(canada.mreg.out2,
                            data.frame(canada$agrland, canada$electricityconsum,canada$co2emission,canada$poptot,canada$urbanpop),
                            interval = "confidence");
canada$forestarea[1:30]<-canada.mreg.int2[1:30,1]


#impute electricity consumption in 55,56
canada.mreg.out3 <- lm(canada$electricityconsum ~canada$agrland+canada$forestarea+canada$poptot+canada$urbanpop)
summary(canada.mreg.out3)


canada.mreg.int3 <- predict(canada.mreg.out3,
                            data.frame(canada$agrland,canada$forestarea, canada$poptot,canada$urbanpop),
                            interval = "confidence");
canada$electricityconsum[c(55,56)]<-canada.mreg.int3[c(55,56),1]

#impute co2emission in 53:56
canada.mreg.out4 <- lm(canada$co2emission ~canada$agrland+canada$forestarea+canada$electricityconsum+canada$poptot+canada$urbanpop)
summary(canada.mreg.out4)


canada.mreg.int4 <- predict(canada.mreg.out4,
                            data.frame(canada$agrland,canada$forestarea,canada$electricityconsum+ canada$poptot,canada$urbanpop),
                            interval = "confidence");
canada$co2emission[c(53:56)]<-canada.mreg.int4[c(53:56),1]

#=============================================
#ARGENTINA DATA
#=============================================
argentina.initial<-read.csv("argentina.csv",header = TRUE,sep = ",")
names(argentina.initial)<-c("year","agrland","forestarea","electricityconsum","co2emission","poptot","urbanpop")
argentina <- argentina.initial

#impute agrland 55,56 
argentina.reg.out <- lm(argentina$agrland ~argentina$forestarea+argentina$poptot+argentina$urbanpop)
summary(argentina.reg.out)


argentina.reg.int <- predict(argentina.reg.out,
                             data.frame(argentina$forestarea,argentina$poptot,argentina$urbanpop),
                             interval = "confidence");
argentina$agrland[c(55,56)]<-argentina.reg.int[c(55,56),1]

#impute agrland for 1 
argentina.reg.out1 <- lm(argentina$agrland ~argentina$co2emission+argentina$poptot+argentina$urbanpop)
summary(argentina.reg.out1)


argentina.reg.int1 <- predict(argentina.reg.out1,
                              data.frame(argentina$co2emission,argentina$poptot,argentina$urbanpop),
                              interval = "confidence");
argentina$agrland[1]<-argentina.reg.int1[1,1]

#impute electricity consumption in 55,56
argentina.reg.out3 <- lm(argentina$electricityconsum ~argentina$agrland+argentina$forestarea+argentina$poptot+argentina$urbanpop)
summary(argentina.reg.out3)


argentina.reg.int3 <- predict(argentina.reg.out3,
                              data.frame(argentina$agrland,argentina$forestarea, argentina$poptot,argentina$urbanpop),
                              interval = "confidence");
argentina$electricityconsum[c(55,56)]<-argentina.reg.int3[c(55,56),1]

#impute electricity consumption in 1-11
argentina.reg.out4 <- lm(argentina$electricityconsum ~argentina$agrland+argentina$poptot+argentina$urbanpop)
summary(argentina.reg.out4)


argentina.reg.int4 <- predict(argentina.reg.out4,
                              data.frame(argentina$agrland,argentina$poptot,argentina$urbanpop),
                              interval = "confidence");
argentina$electricityconsum[c(1:11)]<-argentina.reg.int4[c(1:11),1]


#impute co2emission in 53:56
argentina.reg.out5 <- lm(argentina$co2emission ~argentina$agrland+argentina$forestarea+argentina$electricityconsum+argentina$poptot+argentina$urbanpop)
summary(argentina.reg.out5)


argentina.reg.int5 <- predict(argentina.reg.out5,
                              data.frame(argentina$agrland,argentina$forestarea,argentina$electricityconsum+ argentina$poptot,argentina$urbanpop),
                              interval = "confidence");
argentina$co2emission[c(53:56)]<-argentina.reg.int5[c(53:56),1]


#impute forest data from 1:30 
argentina.reg.out2 <- lm(argentina$forestarea ~argentina$agrland+argentina$electricityconsum+argentina$co2emission+argentina$poptot+argentina$urbanpop)
summary(argentina.reg.out2)


argentina.reg.int2 <- predict(argentina.reg.out2,
                              data.frame(argentina$agrland, argentina$electricityconsum,argentina$co2emission,argentina$poptot,argentina$urbanpop),
                              interval = "confidence");
argentina$forestarea[1:30]<-argentina.reg.int2[1:30,1]

#=============================================
#INDIA DATA
#=============================================
# Imputation for missing values in India Dataset
india.initial<- read.csv("india.csv",header= TRUE,sep = ",")
india<-india.initial[-56,]

#missing values for electricity consumption
india.mreg.out<- lm(india$electricityconsum~india$agrland+india$co2emission
                    +india$poptot+india$urbanpop)


india.mreg.int <- predict(india.mreg.out,
                          data.frame(india$agrland,india$co2emission,
                                     india$poptot,india$urbanpop),
                          interval = "confidence");
india$electricityconsum[2:11]<-india.mreg.int[2:11,1]


#missing value of forest area
india.mreg.out2<- lm(india$forestarea~india$agrland+india$electricityconsum+india$co2emission
                     +india$poptot+india$urbanpop)

india.mreg.int2 <- predict(india.mreg.out2,
                           data.frame(india$agrland,india$electricityconsum,india$co2emission,
                                      india$poptot,india$urbanpop),
                           interval = "confidence");

india$forestarea[2:30]<-india.mreg.int2[2:30,1]
#missing values for electricity consumption again
india.mreg.out3<- lm(india$electricityconsum~india$forestarea
                     +india$poptot+india$urbanpop)


india.mreg.int3 <- predict(india.mreg.out3,
                           data.frame(india$forestarea,
                                      india$poptot,india$urbanpop),
                           interval = "confidence");
india$electricityconsum[55]<-india.mreg.int3[55,1]


#missing for co2 emission
india.mreg.out4<- lm(india$co2emission~india$forestarea+india$electricityconsum
                     +india$poptot+india$urbanpop)


india.mreg.int4 <- predict(india.mreg.out4,
                           data.frame(india$forestarea,india$electricityconsum,
                                      india$poptot,india$urbanpop),
                           interval = "confidence");
india$co2emission[53:55]<-india.mreg.int4[53:55,1]

#missing values for forest area again 
india.mreg.out5<- lm(india$forestarea~india$co2emission
                     +india$poptot+india$urbanpop)


india.mreg.int5 <- predict(india.mreg.out5,
                           data.frame(india$forestarea,india$co2emission,
                                      india$poptot,india$urbanpop),
                           interval = "confidence");
india$forestarea[1]<-india.mreg.int5[1,1]

#missing values for electricity consumption again
india.mreg.out6<- lm(india$electricityconsum~india$forestarea+india$co2emission
                     +india$poptot+india$urbanpop)


india.mreg.int6 <- predict(india.mreg.out6,
                           data.frame(india$forestarea,india$co2emission,
                                      india$poptot,india$urbanpop),
                           interval = "confidence");
india$electricityconsum<-india.mreg.int6[,1]

#missing values for agriculture land
india.mreg.out7<- lm(india$agrland~india$forestarea+india$electricityconsum+india$co2emission
                     +india$poptot+india$urbanpop)


india.mreg.int7 <- predict(india.mreg.out7,
                           data.frame(india$forestarea,india$electricityconsum,india$co2emission,
                                      india$poptot,india$urbanpop),
                           interval = "confidence");
india$agrland[1]<-india.mreg.int7[1,1]
india$agrland[55]<-india.mreg.int7[55,1]

#=============================================
#CHINA DATA
#=============================================
china.initial<- read.csv("china.csv",header= TRUE,sep = ",")
china<-china.initial[-56,]

#missing values for electricity consumption
chn.mreg.out<- lm(china$electricityconsum~china$agrland+china$co2emission
                  +china$poptot+china$urbanpop)


chn.mreg.int <- predict(chn.mreg.out,
                        data.frame(china$agrland,china$co2emission,
                                   china$poptot,china$urbanpop),
                        interval = "confidence");
china$electricityconsum[2:11]<-chn.mreg.int[2:11,1]


#missing value of forest area
chn.mreg.out2<- lm(china$forestarea~china$agrland+china$electricityconsum+china$co2emission
                   +china$poptot+china$urbanpop)

chn.mreg.int2 <- predict(chn.mreg.out2,
                         data.frame(china$agrland,china$electricityconsum,china$co2emission,
                                    china$poptot,china$urbanpop),
                         interval = "confidence");

china$forestarea[2:30]<-chn.mreg.int2[2:30,1]
#missing values for electricity consumption again
chn.mreg.out3<- lm(china$electricityconsum~china$forestarea
                   +china$poptot+china$urbanpop)


chn.mreg.int3 <- predict(chn.mreg.out3,
                         data.frame(china$forestarea,
                                    china$poptot,china$urbanpop),
                         interval = "confidence");
china$electricityconsum[55]<-chn.mreg.int3[55,1]


#missing for co2 emission
chn.mreg.out4<- lm(china$co2emission~china$forestarea+china$electricityconsum
                   +china$poptot+china$urbanpop)


chn.mreg.int4 <- predict(chn.mreg.out4,
                         data.frame(china$forestarea,china$electricityconsum,
                                    china$poptot,china$urbanpop),
                         interval = "confidence");
china$co2emission[53:55]<-chn.mreg.int4[53:55,1]

#missing values for forest area again 
chn.mreg.out5<- lm(china$forestarea~china$co2emission
                   +china$poptot+china$urbanpop)


chn.mreg.int5 <- predict(chn.mreg.out5,
                         data.frame(china$forestarea,china$co2emission,
                                    china$poptot,china$urbanpop),
                         interval = "confidence");
china$forestarea[1]<-chn.mreg.int5[1,1]

#missing values for electricity consumption again
chn.mreg.out6<- lm(china$electricityconsum~china$forestarea+china$co2emission
                   +china$poptot+china$urbanpop)


chn.mreg.int6 <- predict(chn.mreg.out6,
                         data.frame(china$forestarea,china$co2emission,
                                    china$poptot,china$urbanpop),
                         interval = "confidence");
china$electricityconsum<-chn.mreg.int6[,1]

#missing values for agriculture land
chn.mreg.out7<- lm(china$agrland~china$forestarea+china$electricityconsum+china$co2emission
                   +china$poptot+china$urbanpop)


chn.mreg.int7 <- predict(chn.mreg.out7,
                         data.frame(china$forestarea,china$electricityconsum,china$co2emission,
                                    china$poptot,china$urbanpop),
                         interval = "confidence");
china$agrland[1]<-chn.mreg.int7[1,1]
china$agrland[55]<-chn.mreg.int7[55,1]

#=============================================
#PAKISTAN DATA
#=============================================
pak.initial<- read.csv("pak.csv",header= TRUE,sep = ",")
pak<-pak.initial[-56,]

#missing values for electricity consumption
pak.mreg.out<- lm(pak$electricityconsum~pak$agrland+pak$co2emission
                      +pak$poptot+pak$urbanpop)


pak.mreg.int <- predict(pak.mreg.out,
                            data.frame(pak$agrland,pak$co2emission,
                                       pak$poptot,pak$urbanpop),
                            interval = "confidence");
pak$electricityconsum[2:11]<-pak.mreg.int[2:11,1]


#missing value of forest area
pak.mreg.out2<- lm(pak$forestarea~pak$agrland+pak$electricityconsum+pak$co2emission
                       +pak$poptot+pak$urbanpop)

pak.mreg.int2 <- predict(pak.mreg.out2,
                         data.frame(pak$agrland,pak$electricityconsum,pak$co2emission,
                                    pak$poptot,pak$urbanpop),
                         interval = "confidence");

pak$forestarea[2:30]<-pak.mreg.int2[2:30,1]
#missing values for electricity consumption again
pak.mreg.out21<- lm(pak$electricityconsum~pak$agrland+pak$forestarea
                    +pak$poptot+pak$urbanpop)


pak.mreg.int21 <- predict(pak.mreg.out21,
                          data.frame(pak$agrland,pak$forestarea,
                                     pak$poptot,pak$urbanpop),
                          interval = "confidence");
pak$electricityconsum<-pak.mreg.int21[,1]



#missing values for electricity consumption again
pak.mreg.out3<- lm(pak$electricityconsum~pak$forestarea
                   +pak$poptot+pak$urbanpop)


pak.mreg.int3 <- predict(pak.mreg.out3,
                         data.frame(pak$forestarea,
                                    pak$poptot,pak$urbanpop),
                         interval = "confidence");
pak$electricityconsum[55]<-pak.mreg.int3[55,1]


#missing for co2 emission
pak.mreg.out4<- lm(pak$co2emission~pak$forestarea+pak$electricityconsum
                   +pak$poptot+pak$urbanpop)


pak.mreg.int4 <- predict(pak.mreg.out4,
                         data.frame(pak$forestarea,pak$electricityconsum,
                                    pak$poptot,pak$urbanpop),
                         interval = "confidence");
pak$co2emission[53:55]<-pak.mreg.int4[53:55,1]

#missing values for forest area again 
pak.mreg.out5<- lm(pak$forestarea~pak$co2emission
                   +pak$poptot+pak$urbanpop)


pak.mreg.int5 <- predict(pak.mreg.out5,
                         data.frame(pak$forestarea,pak$co2emission,
                                    pak$poptot,pak$urbanpop),
                         interval = "confidence");
pak$forestarea[1]<-pak.mreg.int5[1,1]

#missing values for electricity consumption again
pak.mreg.out6<- lm(pak$electricityconsum~pak$forestarea+pak$co2emission
                   +pak$poptot+pak$urbanpop)


pak.mreg.int6 <- predict(pak.mreg.out6,
                         data.frame(pak$forestarea,pak$co2emission,
                                    pak$poptot,pak$urbanpop),
                         interval = "confidence");
pak$electricityconsum<-pak.mreg.int6[,1]

#missing values for agriculture land
pak.mreg.out7<- lm(pak$agrland~pak$forestarea+pak$electricityconsum+pak$co2emission
                   +pak$poptot+pak$urbanpop)


pak.mreg.int7 <- predict(pak.mreg.out7,
                         data.frame(pak$forestarea,pak$electricityconsum,pak$co2emission,
                                    pak$poptot,pak$urbanpop),
                         interval = "confidence");
pak$agrland[1]<-pak.mreg.int7[1,1]

pak$agrland[55]<-pak.mreg.int7[55,1]

#=============================================
#Malaysia DATA
#=============================================
malaysia.initial<- read.csv("malaysia.csv",header= TRUE,sep = ",")
malaysia<-malaysia.initial[-56,]

#missing values for co2 emission
mys.mreg.out0<- lm(malaysia$co2emission~malaysia$agrland
                       +malaysia$poptot+malaysia$urbanpop)


mys.mreg.int0 <- predict(mys.mreg.out0,
                             data.frame(malaysia$agrland,malaysia$co2emission,
                                        malaysia$poptot,malaysia$urbanpop),
                             interval = "confidence");
malaysia$co2emission[1:10]<-mys.mreg.int0[1:10,1]

#missing values for electricity consumption
mys.mreg.out<- lm(malaysia$electricityconsum~malaysia$agrland+malaysia$co2emission
                      +malaysia$poptot+malaysia$urbanpop)


mys.mreg.int <- predict(mys.mreg.out,
                            data.frame(malaysia$agrland,malaysia$co2emission,
                                       malaysia$poptot,malaysia$urbanpop),
                            interval = "confidence");
malaysia$electricityconsum[2:11]<-mys.mreg.int[2:11,1]


#missing value of forest area
mys.mreg.out2<- lm(malaysia$forestarea~malaysia$agrland+malaysia$electricityconsum+malaysia$co2emission
                       +malaysia$poptot+malaysia$urbanpop)

mys.mreg.int2 <- predict(mys.mreg.out2,
                             data.frame(malaysia$agrland,malaysia$electricityconsum,malaysia$co2emission,
                                        malaysia$poptot,malaysia$urbanpop),
                             interval = "confidence");

malaysia$forestarea[2:30]<-mys.mreg.int2[2:30,1]
#missing values for electricity consumption again
mys.mreg.out3<- lm(malaysia$electricityconsum~malaysia$forestarea
                       +malaysia$poptot+malaysia$urbanpop)


mys.mreg.int3 <- predict(mys.mreg.out3,
                             data.frame(malaysia$forestarea,
                                        malaysia$poptot,malaysia$urbanpop),
                             interval = "confidence");
malaysia$electricityconsum[55]<-mys.mreg.int3[55,1]


#missing for co2 emission
mys.mreg.out4<- lm(malaysia$co2emission~malaysia$forestarea+malaysia$electricityconsum
                   +malaysia$poptot+malaysia$urbanpop)


mys.mreg.int4 <- predict(mys.mreg.out4,
                         data.frame(malaysia$forestarea,malaysia$electricityconsum,
                                    malaysia$poptot,malaysia$urbanpop),
                         interval = "confidence");
malaysia$co2emission[53:55]<-mys.mreg.int4[53:55,1]
#missing for co2 emission again
mys.mreg.out41<- lm(malaysia$co2emission~malaysia$poptot+malaysia$urbanpop)


mys.mreg.int41 <- predict(mys.mreg.out41,
                          data.frame(malaysia$poptot,malaysia$urbanpop),
                          interval = "confidence");
malaysia$co2emission<-mys.mreg.int41[,1]

#missing values for agr land
mys.mreg.out5<- lm(malaysia$agrland~malaysia$co2emission
                   +malaysia$poptot+malaysia$urbanpop)


mys.mreg.int5 <- predict(mys.mreg.out5,
                         data.frame(malaysia$co2emission,
                                    malaysia$poptot,malaysia$urbanpop),
                         interval = "confidence");
malaysia$agrland[1]<-mys.mreg.int5[1,2]
malaysia$agrland[55]<-mys.mreg.int5[55,1]

#missing values for forest area again
mys.mreg.out6<- lm(malaysia$forestarea~malaysia$agrland+malaysia$co2emission
                   +malaysia$poptot+malaysia$urbanpop)


mys.mreg.int6 <- predict(mys.mreg.out6,
                         data.frame(malaysia$agrland,malaysia$co2emission,
                                    malaysia$poptot,malaysia$urbanpop),
                         interval = "confidence");
malaysia$forestarea[1]<-mys.mreg.int6[1,1]

malaysia$electricityconsum[4]<-26.368
malaysia$electricityconsum[3]<-22.849
malaysia$electricityconsum[2]<-19.172
malaysia$electricityconsum[1]<-18.782

#=============================================
#South Africa DATA
#=============================================
#=============================================
southafrica.initial<- read.csv("southafrica.csv",header= TRUE,sep = ",")
southafrica<-southafrica.initial

#impute agrland 55,56 
mreg.out.s1 <- lm(southafrica$agrland ~southafrica$forestarea+southafrica$poptot+southafrica$urbanpop)
summary(mreg.out.s1)


mreg.int.s1 <- predict(mreg.out.s1,
                       data.frame(southafrica$forestarea,southafrica$poptot,southafrica$urbanpop),
                       interval = "confidence");
southafrica$agrland[c(55,56)]<-mreg.int.s1[c(55,56),1]


#impute agrland for 1 
mreg.out.s2 <- lm(southafrica$agrland ~southafrica$co2emission+southafrica$poptot+southafrica$urbanpop)
summary(mreg.out.s2)


mreg.int.s2 <- predict(mreg.out.s2,
                       data.frame(southafrica$electricityconsum,southafrica$co2emission,southafrica$poptot,southafrica$urbanpop),
                       interval = "confidence");
southafrica$agrland[1]<-mreg.int.s2[1,1]

#impute electricity for 1:12 
mreg.out.s3 <- lm(southafrica$electricityconsum ~southafrica$agrland+southafrica$co2emission+southafrica$poptot+southafrica$urbanpop)
summary(mreg.out.s3)


mreg.int.s3 <- predict(mreg.out.s3,
                       data.frame(southafrica$agrland,southafrica$co2emission,southafrica$poptot,southafrica$urbanpop),
                       interval = "confidence");
southafrica$electricityconsum[1:11]<-mreg.int.s3[1:11,1]



#impute forest data from 1:30 
mreg.out.s4 <- lm(southafrica$forestarea ~southafrica$agrland+southafrica$electricityconsum+southafrica$co2emission+southafrica$poptot+southafrica$urbanpop)
summary(mreg.out.s4)


mreg.int.s4 <- predict(mreg.out.s4,
                       data.frame(southafrica$agrland, southafrica$electricityconsum,southafrica$co2emission,southafrica$poptot,southafrica$urbanpop),
                       interval = "confidence");
southafrica$forestarea[1:30]<-mreg.int.s4[1:30,1]


#impute electricity consumption in 55,56
mreg.out.s5 <- lm(southafrica$electricityconsum ~southafrica$agrland+southafrica$forestarea+southafrica$poptot+southafrica$urbanpop)
summary(mreg.out.s5)


mreg.int.s5 <- predict(mreg.out.s5,
                       data.frame(southafrica$agrland,southafrica$forestarea, southafrica$poptot,southafrica$urbanpop),
                       interval = "confidence");
southafrica$electricityconsum[c(55,56)]<-mreg.int.s5[c(55,56),1]

#impute co2emission in 53:56
mreg.out.s6 <- lm(southafrica$co2emission ~southafrica$agrland+southafrica$forestarea+southafrica$electricityconsum+southafrica$poptot+southafrica$urbanpop)
summary(mreg.out.s6)


mreg.int.s6 <- predict(mreg.out.s6,
                       data.frame(southafrica$agrland,southafrica$forestarea,southafrica$electricityconsum+ southafrica$poptot,southafrica$urbanpop),
                       interval = "confidence");
southafrica$co2emission[c(53:56)]<-mreg.int.s6[c(53:56),1]
#=============================================
#Egypt DATA
#=============================================
egypt.initial<- read.csv("egypt.csv",header= TRUE,sep = ",")
egypt<-egypt.initial

#impute agrland 55,56 
mreg.out.e1 <- lm(egypt$agrland ~egypt$forestarea+egypt$poptot+egypt$urbanpop)
summary(mreg.out.e1)


mreg.int.e1 <- predict(mreg.out.e1,
                       data.frame(egypt$forestarea,egypt$poptot,egypt$urbanpop),
                       interval = "confidence");
egypt$agrland[c(55,56)]<-mreg.int.e1[c(55,56),1]


#impute agrland for 1 
mreg.out.e2 <- lm(egypt$agrland ~egypt$co2emission+egypt$poptot+egypt$urbanpop)
summary(mreg.out.e2)


mreg.int.e2 <- predict(mreg.out.e2,
                       data.frame(egypt$electricityconsum,egypt$co2emission,egypt$poptot,egypt$urbanpop),
                       interval = "confidence");
egypt$agrland[1]<-mreg.int.e2[1,1]

#impute electricity for 1:12 
mreg.out.e3 <- lm(egypt$electricityconsum ~egypt$agrland+egypt$co2emission+egypt$poptot+egypt$urbanpop)
summary(mreg.out.e3)


mreg.int.e3 <- predict(mreg.out.e3,
                       data.frame(egypt$agrland,egypt$co2emission,egypt$poptot,egypt$urbanpop),
                       interval = "confidence");
egypt$electricityconsum[1:11]<-mreg.int.e3[1:11,1]



#impute forest data from 1:30 
mreg.out.e4 <- lm(egypt$forestarea ~egypt$agrland+egypt$electricityconsum+egypt$co2emission+egypt$poptot+egypt$urbanpop)
summary(mreg.out.e4)


mreg.int.e4 <- predict(mreg.out.e4,
                       data.frame(egypt$agrland, egypt$electricityconsum,egypt$co2emission,egypt$poptot,egypt$urbanpop),
                       interval = "confidence");
egypt$forestarea[1:30]<-mreg.int.e4[1:30,1]


#impute electricity consumption in 55,56
mreg.out.e5 <- lm(egypt$electricityconsum ~egypt$agrland+egypt$forestarea+egypt$poptot+egypt$urbanpop)
summary(mreg.out.e5)


mreg.int.e5 <- predict(mreg.out.e5,
                       data.frame(egypt$agrland,egypt$forestarea, egypt$poptot,egypt$urbanpop),
                       interval = "confidence");
egypt$electricityconsum[c(55,56)]<-mreg.int.e5[c(55,56),1]

#impute co2emission in 53:56
mreg.out.e6 <- lm(egypt$co2emission ~egypt$agrland+egypt$forestarea+egypt$electricityconsum+egypt$poptot+egypt$urbanpop)
summary(mreg.out.e6)


mreg.int.e6 <- predict(mreg.out.e6,
                       data.frame(egypt$agrland,egypt$forestarea,egypt$electricityconsum+ egypt$poptot,egypt$urbanpop),
                       interval = "confidence");
egypt$co2emission[c(53:56)]<-mreg.int.e6[c(53:56),1]

egypt$electricityconsum[1:3] <- min(egypt$electricityconsum[4:54]) 

#=============================================
#Nigeria DATA
#=============================================
nigeria.initial<- read.csv("nigeria.csv",header= TRUE,sep = ",")
nigeria<-nigeria.initial

#impute agrland 55,56 
mreg.out.n1 <- lm(nigeria$agrland ~nigeria$forestarea+nigeria$poptot+nigeria$urbanpop)
summary(mreg.out.n1)


mreg.int.n1 <- predict(mreg.out.n1,
                       data.frame(nigeria$forestarea,nigeria$poptot,nigeria$urbanpop),
                       interval = "confidence");
nigeria$agrland[c(55,56)]<-mreg.int.n1[c(55,56),1]


#impute agrland for 1 
mreg.out.n2 <- lm(nigeria$agrland ~nigeria$co2emission+nigeria$poptot+nigeria$urbanpop)
summary(mreg.out.n2)


mreg.int.n2 <- predict(mreg.out.n2,
                       data.frame(nigeria$electricityconsum,nigeria$co2emission,nigeria$poptot,nigeria$urbanpop),
                       interval = "confidence");
nigeria$agrland[1]<-mreg.int.n2[1,1]

#impute electricity for 1:12 
mreg.out.n3 <- lm(nigeria$electricityconsum ~nigeria$agrland+nigeria$co2emission+nigeria$poptot+nigeria$urbanpop)
summary(mreg.out.n3)


mreg.int.n3 <- predict(mreg.out.n3,
                       data.frame(nigeria$agrland,nigeria$co2emission,nigeria$poptot,nigeria$urbanpop),
                       interval = "confidence");
nigeria$electricityconsum[1:11]<-mreg.int.n3[1:11,1]



#impute forest data from 1:30 
mreg.out.n4 <- lm(nigeria$forestarea ~nigeria$agrland+nigeria$electricityconsum+nigeria$co2emission+nigeria$poptot+nigeria$urbanpop)
summary(mreg.out.n4)


mreg.int.n4 <- predict(mreg.out.n4,
                       data.frame(nigeria$agrland, nigeria$electricityconsum,nigeria$co2emission,nigeria$poptot,nigeria$urbanpop),
                       interval = "confidence");
nigeria$forestarea[1:30]<-mreg.int.n4[1:30,1]


#impute electricity consumption in 55,56
mreg.out.n5 <- lm(nigeria$electricityconsum ~nigeria$agrland+nigeria$forestarea+nigeria$poptot+nigeria$urbanpop)
summary(mreg.out.n5)


mreg.int.n5 <- predict(mreg.out.n5,
                       data.frame(nigeria$agrland,nigeria$forestarea, nigeria$poptot,nigeria$urbanpop),
                       interval = "confidence");
nigeria$electricityconsum[c(55,56)]<-mreg.int.n5[c(55,56),1]

#impute co2emission in 53:56
mreg.out.n6 <- lm(nigeria$co2emission ~nigeria$agrland+nigeria$forestarea+nigeria$electricityconsum+nigeria$poptot+nigeria$urbanpop)
summary(mreg.out.n6)


mreg.int.n6 <- predict(mreg.out.n6,
                       data.frame(nigeria$agrland,nigeria$forestarea,nigeria$electricityconsum+ nigeria$poptot,nigeria$urbanpop),
                       interval = "confidence");
nigeria$co2emission[c(53:56)]<-mreg.int.n6[c(53:56),1]

#=============================================
#Algeria DATA
#=============================================
algeria.initial<- read.csv("algeria.csv",header= TRUE,sep = ",")
algeria<-algeria.initial

#impute agrland 55,56 
mreg.out.a1 <- lm(algeria$agrland ~algeria$forestarea+algeria$poptot+algeria$urbanpop)
summary(mreg.out.a1)


mreg.int.a1 <- predict(mreg.out.a1,
                       data.frame(algeria$forestarea,algeria$poptot,algeria$urbanpop),
                       interval = "confidence");
algeria$agrland[c(55,56)]<-mreg.int.a1[c(55,56),1]


#impute agrland for 1 
mreg.out.a2 <- lm(algeria$agrland ~algeria$co2emission+algeria$poptot+algeria$urbanpop)
summary(mreg.out.a2)


mreg.int.a2 <- predict(mreg.out.a2,
                       data.frame(algeria$electricityconsum,algeria$co2emission,algeria$poptot,algeria$urbanpop),
                       interval = "confidence");
algeria$agrland[1]<-mreg.int.a2[1,1]

#impute electricity for 1:12 
mreg.out.a3 <- lm(algeria$electricityconsum ~algeria$agrland+algeria$co2emission+algeria$poptot+algeria$urbanpop)
summary(mreg.out.a3)


mreg.int.a3 <- predict(mreg.out.a3,
                       data.frame(algeria$agrland,algeria$co2emission,algeria$poptot,algeria$urbanpop),
                       interval = "confidence");
algeria$electricityconsum[1:11]<-mreg.int.a3[1:11,1]



#impute forest data from 1:30 
mreg.out.a4 <- lm(algeria$forestarea ~algeria$agrland+algeria$electricityconsum+algeria$co2emission+algeria$poptot+algeria$urbanpop)
summary(mreg.out.a4)


mreg.int.a4 <- predict(mreg.out.a4,
                       data.frame(algeria$agrland, algeria$electricityconsum,algeria$co2emission,algeria$poptot,algeria$urbanpop),
                       interval = "confidence");
algeria$forestarea[1:30]<-mreg.int.a4[1:30,1]


#impute electricity consumption in 55,56
mreg.out.a5 <- lm(algeria$electricityconsum ~algeria$agrland+algeria$forestarea+algeria$poptot+algeria$urbanpop)
summary(mreg.out.a5)


mreg.int.a5 <- predict(mreg.out.a5,
                       data.frame(algeria$agrland,algeria$forestarea, algeria$poptot,algeria$urbanpop),
                       interval = "confidence");
algeria$electricityconsum[c(55,56)]<-mreg.int.a5[c(55,56),1]

#impute co2emission in 53:56
mreg.out.a6 <- lm(algeria$co2emission ~algeria$agrland+algeria$forestarea+algeria$electricityconsum+algeria$poptot+algeria$urbanpop)
summary(mreg.out.a6)


mreg.int.a6 <- predict(mreg.out.a6,
                       data.frame(algeria$agrland,algeria$forestarea,algeria$electricityconsum+ algeria$poptot,algeria$urbanpop),
                       interval = "confidence");
algeria$co2emission[c(53:56)]<-mreg.int.a6[c(53:56),1]

algeria$electricityconsum[1:3] <- min(algeria$electricityconsum[4:56])
