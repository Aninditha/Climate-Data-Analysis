#Initially the country which is non numeric value is set to zero before calculating corelation matrix
algeriatraining$country<-NULL
cor(algeriatraining)
algeriatraining$country<-"ALG"

#plotting all the plots for which correlation > |.80|
plot(algeriatraining$poptot,algeriatraining$temperature)
plot(algeriatraining$urbanpop,algeriatraining$temperature)
plot(algeriatraining$forestarea,algeriatraining$urbanpop)
plot(algeriatraining$electricityconsum,algeriatraining$urbanpop)
plot(algeriatraining$poptot,algeriatraining$urbanpop)
plot(algeriatraining$forestarea,algeriatraining$poptot)
plot(algeriatraining$electricityconsum,algeriatraining$poptot)
plot(algeriatraining$forestarea,algeriatraining$co2emission)
plot(algeriatraining$forestarea,algeriatraining$electricityconsum)
plot(algeriatraining$agrland,algeriatraining$forestarea)

#---------------------------------------------------------------------------------------------
#Initially the country which is non numeric value is set to zero before calculating corelation matrix
egypttraining$country<-NULL
cor(egypttraining)
egypttraining$country<-"EGY"

#plotting all the plots for which correlation > |.80|
plot(egypttraining$forestarea,egypttraining$urbanpop)
plot(egypttraining$electricityconsum,egypttraining$urbanpop)
plot(egypttraining$poptot,egypttraining$urbanpop)
plot(egypttraining$forestarea,egypttraining$poptot)
plot(egypttraining$electricityconsum,egypttraining$poptot)
plot(egypttraining$forestarea,egypttraining$co2emission)
plot(egypttraining$forestarea,egypttraining$electricityconsum)
plot(egypttraining$agrland,egypttraining$forestarea)

#------------------------------------------------------------------------------------------------
#Initially the country which is non numeric value is set to zero before calculating corelation matrix
nigeriatraining$country<-NULL
cor(nigeriatraining)
nigeriatraining$country<-"NGA"

#plotting all the plots for which correlation > |.80|
plot(nigeriatraining$agrland,nigeriatraining$urbanpop)
plot(nigeriatraining$forestarea,nigeriatraining$urbanpop)
plot(nigeriatraining$electricityconsum,nigeriatraining$urbanpop)
plot(nigeriatraining$poptot,nigeriatraining$urbanpop)
plot(nigeriatraining$agrland,nigeriatraining$poptot)
plot(nigeriatraining$forestarea,nigeriatraining$poptot)
plot(nigeriatraining$forestarea,nigeriatraining$electricityconsum)
plot(nigeriatraining$agrland,nigeriatraining$forestarea)

#-------------------------------------------------------------------------------------------------
#Initially the country which is non numeric value is set to zero before calculating corelation matrix
southafricatraining$country<-NULL
cor(southafricatraining)
southafricatraining$country<-"RSA"

#plotting all the plots for which correlation > |.80|
plot(southafricatraining$poptot,southafricatraining$urbanpop)
plot(southafricatraining$electricityconsum,southafricatraining$poptot)
plot(southafricatraining$forestarea,southafricatraining$co2emission)
plot(southafricatraining$electricityconsum,southafricatraining$co2emission)
plot(southafricatraining$forestarea,southafricatraining$electricityconsum)

#-----------------------------------------------------------------------------------------------------
#Initially the country which is non numeric value is set to zero before calculating corelation matrix
argTraining$country<-NULL
cor(argTraining)
argTraining$country<-"ARG"

plot(argTraining$forestarea,argTraining$urbanpop)
plot(argTraining$electricityconsum,argTraining$urbanpop)
plot(argTraining$poptot,argTraining$urbanpop)
plot(argTraining$forestarea,argTraining$poptot)
plot(argTraining$electricityconsum,argTraining$poptot)
plot(argTraining$forestarea,argTraining$electricityconsum)

-----------------------------------------------------------------------------------------------------
#Initially the country which is non numeric value is set to zero before calculating corelation matrix
braTraining$country<-NULL
cor(braTraining)
braTraining$country<-"BRA"

plot(braTraining$agrland,braTraining$urbanpop)
plot(braTraining$forestarea,braTraining$urbanpop)
plot(braTraining$electricityconsum,braTraining$urbanpop)
plot(braTraining$co2emission,braTraining$urbanpop)
plot(braTraining$poptot,braTraining$urbanpop)
plot(braTraining$agrland,braTraining$poptot)
plot(braTraining$forestarea,braTraining$poptot)
plot(braTraining$electricityconsum,braTraining$poptot)
plot(braTraining$co2emission,braTraining$poptot)
plot(braTraining$agrland,braTraining$co2emission)
plot(braTraining$forestarea,braTraining$co2emission)
plot(braTraining$electricityconsum,braTraining$co2emission)
plot(braTraining$agrland,braTraining$electricityconsum)
plot(braTraining$forestarea,braTraining$electricityconsum)
plot(braTraining$agrland,braTraining$forestarea)

----------------------------------------------------------------------------------------------------
#Initially the country which is non numeric value is set to zero before calculating corelation matrix
canTraining$country<-NULL
cor(canTraining)
canTraining$country<-"CAN"

plot(canTraining$forestarea,canTraining$urbanpop)
plot(canTraining$electricityconsum,canTraining$urbanpop)
plot(canTraining$poptot,canTraining$urbanpop)
plot(canTraining$forestarea,canTraining$poptot)
plot(canTraining$electricityconsum,canTraining$poptot)
plot(canTraining$forestarea,canTraining$electricityconsum)

#---------------------------------------------------------------------------------------------------
#Initially the country which is non numeric value is set to zero before calculating corelation matrix
usaTraining$country<-NULL
cor(usaTraining)
usaTraining$country<-"USA"

plot(usaTraining$agrland,usaTraining$urbanpop)
plot(usaTraining$forestarea,usaTraining$urbanpop)
plot(usaTraining$electricityconsum,usaTraining$urbanpop)
plot(usaTraining$poptot,usaTraining$urbanpop)
plot(usaTraining$agrland,usaTraining$poptot)
plot(usaTraining$forestarea,usaTraining$poptot)
plot(usaTraining$electricityconsum,usaTraining$poptot)
plot(usaTraining$agrland,usaTraining$electricityconsum)

#---------------------------------------------------------------------------------------------------
#Initially the country which is non numeric value is set to zero before calculating corelation matrix
pakTraining$country<-NULL
cor(pakTraining)
pakTraining$country<-"PAK"

plot(pakTraining$forestarea,pakTraining$urbanpop)
plot(pakTraining$electricityconsum,pakTraining$urbanpop)
plot(pakTraining$co2emission,pakTraining$urbanpop)
plot(pakTraining$poptot,pakTraining$urbanpop)
plot(pakTraining$forestarea,pakTraining$poptot)
plot(pakTraining$electricityconsum,pakTraining$poptot)
plot(pakTraining$co2emission,pakTraining$poptot)
plot(pakTraining$forestarea,pakTraining$co2emission)
plot(pakTraining$electricityconsum,pakTraining$co2emission)
plot(pakTraining$forestarea,pakTraining$electricityconsum)

#-------------------------------------------------------------------------------------------------------
#Initially the country which is non numeric value is set to zero before calculating corelation matrix
indiaTraining$country<-NULL
cor(indiaTraining)
indiaTraining$country<-"IND"

plot(indiaTraining$forestarea,indiaTraining$urbanpop)
plot(indiaTraining$electricityconsum,indiaTraining$urbanpop)
plot(indiaTraining$co2emission,indiaTraining$urbanpop)
plot(indiaTraining$poptot,indiaTraining$urbanpop)
plot(indiaTraining$forestarea,indiaTraining$poptot)
plot(indiaTraining$electricityconsum,indiaTraining$poptot)
plot(indiaTraining$co2emission,indiaTraining$poptot)
plot(indiaTraining$forestarea,indiaTraining$co2emission)
plot(indiaTraining$electricityconsum,indiaTraining$co2emission)
plot(indiaTraining$forestarea,indiaTraining$electricityconsum)

#-------------------------------------------------------------------------------------------------------------
#Initially the country which is non numeric value is set to zero before calculating corelation matrix
chinaTraining$country<-NULL
cor(chinaTraining)
chinaTraining$country<-"CHN"

plot(chinaTraining$agrland,chinaTraining$urbanpop)
plot(chinaTraining$forestarea,chinaTraining$urbanpop)
plot(chinaTraining$electricityconsum,chinaTraining$urbanpop)
plot(chinaTraining$co2emission,chinaTraining$urbanpop)
plot(chinaTraining$poptot,chinaTraining$urbanpop)
plot(chinaTraining$agrland,chinaTraining$poptot)
plot(chinaTraining$forestarea,chinaTraining$poptot)
plot(chinaTraining$co2emission,chinaTraining$poptot)
plot(chinaTraining$forestarea,chinaTraining$co2emission)
plot(chinaTraining$electricityconsum,chinaTraining$co2emission)
plot(chinaTraining$forestarea,chinaTraining$electricityconsum)

#-----------------------------------------------------------------------------------------------------------------------------------
#Initially the country which is non numeric value is set to zero before calculating corelation matrix
malaysiaTraining$country<-NULL
cor(malaysiaTraining)
malaysiaTraining$country<-"MYS"

plot(malaysiaTraining$agrland,malaysiaTraining$temperature)
plot(malaysiaTraining$electricityconsum,malaysiaTraining$temperature)
plot(malaysiaTraining$co2emission,malaysiaTraining$temperature)
plot(malaysiaTraining$poptot,malaysiaTraining$temperature)
plot(malaysiaTraining$urbanpop,malaysiaTraining$temperature)
plot(malaysiaTraining$agrland,malaysiaTraining$urbanpop)
plot(malaysiaTraining$electricityconsum,malaysiaTraining$urbanpop)
plot(malaysiaTraining$co2emission,malaysiaTraining$urbanpop)
plot(malaysiaTraining$poptot,malaysiaTraining$urbanpop)
plot(malaysiaTraining$agrland,malaysiaTraining$poptot)
plot(malaysiaTraining$electricityconsum,malaysiaTraining$poptot)
plot(malaysiaTraining$co2emission,malaysiaTraining$poptot)
plot(malaysiaTraining$agrland,malaysiaTraining$co2emission)
plot(malaysiaTraining$electricityconsum,malaysiaTraining$co2emission)
plot(malaysiaTraining$agrland,malaysiaTraining$electricityconsum)