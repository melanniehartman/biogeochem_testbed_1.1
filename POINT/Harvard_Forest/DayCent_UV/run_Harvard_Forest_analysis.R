###############################################################################
#
# run_Harvard_Forest_analysis.R
#
# Author: Melannie Hartman 
#         June 25, 2018 
#         July 23, 2018
#         August 20, 2018
#         October 15, 2018 - add MIMICS and CORPSE
#         October 22, 2018 - added observations
#      
# Description:
#   This R script will plot DayCent results from the Harvard Forest simulations
#   for two treatments (no N additions and N additions).
#   It compares DayCent results to CASA, MIMICS, and CORPSE
#
###############################################################################

# Uninitialize the variables in memory before rerunning the R script
rm(list=ls())

siteName <<- "harvardforest"
# Path to files
modelPath = "E:/dev/NCAR/CASACLM/POINT/Harvard_Forest/DayCent_UV"
casaPath = "E:/dev/NCAR/CASACLM/POINT/Harvard_Forest/CASACNP"
mimicsPath = "E:/dev/NCAR/CASACLM/POINT/Harvard_Forest/MIMICS"
corpsePath = "E:/dev/NCAR/CASACLM/POINT/Harvard_Forest/CORPSE"
setwd(modelPath)

# List the crop rotations to include in the graphs
mgmtOptions <- c("control","Naddition")

#-------------------------------------------------------------------------------------------------
# Function ReadDataLis
# * Read the aglivc, bglivcj, bglivcm, somsc columns from a set of DayCent .lis files 
#   and store into aglivcData, bglivcData, and somscData.
# * This function assumes that the .lis files for all mgmtOptions have the 
#   same number of lines. 
# * In the read.table call, set the number of lines to skip so the function reads   
#   past the values generated from the equilibrium simulation.
# * If the outvars.txt file is changed, the column numbers below might change also!

ReadDataLis = function ()
{
  
    # Columns to extract from .lis file. To read in a column there must be an array declared below. 
    # Not all columns listed below may actually be read. 

    timeCol <- 1      # Column in .lis file that contains simulation time 

    aglivcCol <- 2    # Column in .lis file that contains aglivc    (live above-ground shoots for crops/grasses, gC/m2)
    bglivcjCol <- 3   # Column in .lis file that contains bglivcj   (live juvenile fine roots for crops/grasses, gC/m2)
    bglivcmCol <- 4   # Column in .lis file that contains bglivcm   (live mature fine roots for crops/grasses, gC/m2)
    stdedcCol <- 5    # Column in .lis file that contains stdedc    (standing dead C for crops/grasses, gC/m2)

    agcaccCol <- 6    # Column in .lis file that contains agcacc    (above-ground NPP, gC/m2/yr)
    bgcjaccCol <- 7   # Column in .lis file that contains bgcjacc   (below-ground NPP, gC/m2/yr)
    bgcmaccCol <- 8   # Column in .lis file that contains bgcmacc   (below-ground NPP, gC/m2/yr)


    rleavcCol <- 9
    fbrchcCol <- 10
    rlwodcCol <-11
    frootcjCol <- 12
    frootcmCol <- 13
    crootcCol <- 14

    fcaccCol <- 15
    rlvaccCol <- 16
    fbraccCol <- 17
    rlwaccCol <- 18
    frtjaccCol <- 19
    frtmaccCol <- 20
    crtaccCol <- 21

    strucc1Col <- 22  # Column in .lis file that contains strucc(1) (surface structural litter C, gC/m2)
    strucc2Col <- 23  # Column in .lis file that contains strucc(2) (soil structural litter C, gC/m2)
    metabc1Col <- 24  # Column in .lis file that contains metabc(1) (surface metabolic litter C, gC/m2)
    metabc2Col <- 25  # Column in .lis file that contains metabc(2) (soil metabolic litter C, gC/m2)

    som1c1Col <- 26   # Column in .lis file that contains som1c(1)  (active surface organic matter C, gC/m2)
    som1c2Col <- 27   # Column in .lis file that contains som1c(2)  (active soil organic matter C, gC/m2)
    som2c1Col <- 28   # Column in .lis file that contains som2c(1)  (slow surface organic matter C, gC/m2)
    som2c2Col <- 29   # Column in .lis file that contains som2c(2)  (slow soil organic matter C, gC/m2)
    som3cCol <- 30    # Column in .lis file that contains som3c)    (passive soil organic matter C, gC/m2)
    somscCol <- 31    # Column in .lis file that contains somsc     (soil organic matter C, gC/m2)
    somtcCol <- 32    # Column in .lis file that contains somtc     (soil organic matter C + below-ground litter C, gC/m2)

    tnetmnCol <- 36   # Column in .lis file that contains tnetmn(1) (accumulator for net N mineralization, gN/m2/yr)
    tminrlCol <- 37   # Column in .lis file that contains tminrl(1) (total mineral N, gN/m2)

    annetCol <- 38    # Column in .lis file that contains annet     (accumulator for actual evapotranspiration, cm/yr)
    petanCol <- 39    # Column in .lis file that contains petann    (accumulator for potential evapotranspiration, cm/yr)


    doAlloc <- 1  # Set to 0 after array somscData has been allocated
    colCnt <- 1   # Column in somscData to store somsc values

    for (mgmt in mgmtOptions)
    {
        somName <- paste(siteName, mgmt, sep="_")
        somName <- paste(somName, ".lis", sep="")
        lisFileName <<- paste(modelPath,somName,sep="/")

        colCnt <- colCnt + 1
        lisFileName
        if (file.exists(lisFileName))
        {        
            # sep="" refers to any length of white space as being the delimiter.  Don't use " ".
            dataLis <<- read.table(file=lisFileName,skip=4,header=FALSE,sep="",dec=".",fill=TRUE)

            if (doAlloc == 1)
            {
                nTimes <<- length(dataLis[,timeCol])
                nCols <<- length(mgmtOptions) + 1

                aglivcData <<- array(data = 0.0, dim = c(nTimes,nCols))
                bglivcData <<- array(data = 0.0, dim = c(nTimes,nCols))

                rleavcData <<- array(data = 0.0, dim = c(nTimes,nCols))
                fbrchcData <<- array(data = 0.0, dim = c(nTimes,nCols))
                rlwodcData <<- array(data = 0.0, dim = c(nTimes,nCols))
                frootcData <<- array(data = 0.0, dim = c(nTimes,nCols))
                crootcData <<- array(data = 0.0, dim = c(nTimes,nCols))
                frstcData <<- array(data = 0.0, dim = c(nTimes,nCols))
  
                rlvaccData <<- array(data = 0.0, dim = c(nTimes,nCols))
                fbraccData <<- array(data = 0.0, dim = c(nTimes,nCols))
                rlwaccData <<- array(data = 0.0, dim = c(nTimes,nCols))
                frtaccData <<- array(data = 0.0, dim = c(nTimes,nCols))
                crtaccData <<- array(data = 0.0, dim = c(nTimes,nCols))
                fcaccData <<- array(data = 0.0, dim = c(nTimes,nCols))
                
                metc1Data <<- array(data = 0.0, dim = c(nTimes,nCols))
                metc2Data <<- array(data = 0.0, dim = c(nTimes,nCols))
                strucc1Data <<- array(data = 0.0, dim = c(nTimes,nCols))
                strucc2Data <<- array(data = 0.0, dim = c(nTimes,nCols))
                
                som1c1Data <<- array(data = 0.0, dim = c(nTimes,nCols))
                som2c1Data <<- array(data = 0.0, dim = c(nTimes,nCols))
                som1c2Data <<- array(data = 0.0, dim = c(nTimes,nCols))
                som2c2Data <<- array(data = 0.0, dim = c(nTimes,nCols))
                som3cData <<- array(data = 0.0, dim = c(nTimes,nCols))
                somscData <<- array(data = 0.0, dim = c(nTimes,nCols))
                somtcData <<- array(data = 0.0, dim = c(nTimes,nCols))

                # Set the first column in each array to the time variable

                aglivcData[,1] <<- dataLis[,timeCol]
                bglivcData[,1] <<- dataLis[,timeCol]

                rleavcData[,1] <<- dataLis[,timeCol]
                fbrchcData[,1] <<- dataLis[,timeCol]
                rlwodcData[,1] <<- dataLis[,timeCol]
                frootcData[,1] <<- dataLis[,timeCol]
                crootcData[,1] <<- dataLis[,timeCol]
                frstcData[,1] <<- dataLis[,timeCol]

                rlvaccData[,1] <<- dataLis[,timeCol]
                fbraccData[,1] <<- dataLis[,timeCol]
                rlwaccData[,1] <<- dataLis[,timeCol]
                frtaccData[,1] <<- dataLis[,timeCol]
                crtaccData[,1] <<- dataLis[,timeCol]
                fcaccData[,1] <<- dataLis[,timeCol]

                metc1Data[,1] <<- dataLis[,timeCol]
                metc2Data[,1] <<- dataLis[,timeCol]
                strucc1Data[,1] <<- dataLis[,timeCol]
                strucc2Data[,1] <<- dataLis[,timeCol]
                
                som1c1Data[,1] <<- dataLis[,timeCol]
                som2c1Data[,1] <<- dataLis[,timeCol]
                som1c2Data[,1] <<- dataLis[,timeCol]
                som2c2Data[,1] <<- dataLis[,timeCol]
                som3cData[,1] <<- dataLis[,timeCol]
                somscData[,1] <<- dataLis[,timeCol]
                somtcData[,1] <<- dataLis[,timeCol]

                doAlloc <- 0
            }

            aglivcData[,colCnt] <<- dataLis[,aglivcCol] 
            bglivcData[,colCnt] <<- dataLis[,bglivcjCol] + dataLis[,bglivcmCol]

            # Tree carbon
            rleavcData[,colCnt] <<- dataLis[,rleavcCol] 
            fbrchcData[,colCnt] <<- dataLis[,fbrchcCol] 
            rlwodcData[,colCnt] <<- dataLis[,rlwodcCol] 
            frootcData[,colCnt] <<- dataLis[,frootcjCol] + dataLis[,frootcmCol]
            crootcData[,colCnt] <<- dataLis[,crootcCol] 
            frstcData[,colCnt] <<- dataLis[,rleavcCol] + dataLis[,fbrchcCol] + dataLis[,rlwodcCol] + dataLis[,frootcjCol] + dataLis[,frootcmCol] + dataLis[,crootcCol] 
            
            #NPP
            rlvaccData[,colCnt] <<- dataLis[,rlvaccCol] 
            fbraccData[,colCnt] <<- dataLis[,fbraccCol] 
            rlwaccData[,colCnt] <<- dataLis[,rlwaccCol] 
            frtaccData[,colCnt] <<- dataLis[,frtjaccCol] + dataLis[,frtmaccCol]
            crtaccData[,colCnt] <<- dataLis[,crtaccCol] 
            fcaccData[,colCnt] <<- dataLis[,fcaccCol] 

            #LITTER
            metc1Data[,colCnt] <<- dataLis[,metabc1Col]
            metc2Data[,colCnt] <<- dataLis[,metabc2Col]
            strucc1Data[,colCnt] <<- dataLis[,strucc1Col]
            strucc2Data[,colCnt] <<- dataLis[,strucc2Col]
            
            #SOM
            som1c1Data[,colCnt] <<- dataLis[,som1c1Col]
            som2c1Data[,colCnt] <<- dataLis[,som2c1Col]
            som1c2Data[,colCnt] <<- dataLis[,som1c2Col]
            som2c2Data[,colCnt] <<- dataLis[,som2c2Col]
            som3cData[,colCnt] <<- dataLis[,som3cCol]
            somscData[,colCnt] <<- dataLis[,somscCol]
            somtcData[,colCnt] <<- dataLis[,somtcCol]

        }
        else
        {
           msg <<- paste(lisFileName, " does not exist.")
           print(msg)
        }
    }

} #End function ReadDataLis()

#-------------------------------------------------------------------------------------------------
# Function ReadPsynOut 
# Read DayCent psyn.out file. Assign global data structures gppCrpData and gppForData.

ReadPsynOut = function ()
{
  
  colCnt <- 1
  timeCol <<- 1
  dayCol <<- 2
  gppCrpCol <- 19
  gppForCol <- 26
  doAlloc <- 1
  
  for (mgmt in mgmtOptions)
  {

    psynFileName <- paste("psyn", siteName, mgmt, sep="_")
    psynFileName <- paste(psynFileName, ".out", sep="")
    psynFileName <- paste(modelPath,psynFileName,sep="/")
    
    if (file.exists(psynFileName))
    { 
      # sep="" refers to any length of white space as being the delimiter.  Don't use " ".
      dataPsyn <<- read.table(file=psynFileName,header=TRUE,sep="",dec=".",fill=TRUE)
      nTimes <<- length(dataPsyn[,1])
      nCols <<- length(mgmtOptions) + 1
      nYrs <<- as.integer(nTimes/365)
      minYr <<- as.integer(dataPsyn[1,timeCol])
      maxYr <<- as.integer(dataPsyn[nTimes,timeCol])
      if (doAlloc == 1)
      {
        gppCrpData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
        gppForData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
        gppCrpAnData <<- array(data = 0.0, dim = c(nYrs,nCols)) # Annual
        gppForAnData <<- array(data = 0.0, dim = c(nYrs,nCols)) # Annual
        gppCrpData[,1] <<- as.integer(dataPsyn[,timeCol]) + dataPsyn[,dayCol]/366
        gppForData[,1] <<- as.integer(dataPsyn[,timeCol]) + dataPsyn[,dayCol]/366
        doAlloc <- 0
      }
      colCnt <- colCnt + 1
      gppCrpData[,colCnt] <<- dataPsyn[,gppCrpCol] 
      gppForData[,colCnt] <<- dataPsyn[,gppForCol] 
      for (iyr in minYr:maxYr)
      {
        i <- iyr - minYr + 1
        gppCrpAnData[i,1] <<- iyr
        gppForAnData[i,1] <<- iyr
        gppCrpAnData[i,colCnt] <<- sum(gppCrpData[gppCrpData[,1]>=iyr & gppCrpData[,1]<iyr+1,colCnt])
        gppForAnData[i,colCnt] <<- sum(gppForData[gppForData[,1]>=iyr & gppForData[,1]<iyr+1,colCnt])
      }
    }
    else
    {
      msg <<- paste(psynFileName, " does not exist.")
      print(msg)
    }
  }
    
} # End function ReadPsynOut()

#-------------------------------------------------------------------------------------------------
# Function ReadDailyOut() 
# Read DayCent daily.out file. Assign global data structures agdefacData and bgdefacData.

ReadDailyOut = function ()
{
  
  colCnt <- 1
  timeCol <<- 1
  dayCol <<- 2
  agdefacCol <- 4
  bgdefacCol <- 5
  doAlloc <- 1
  
  for (mgmt in mgmtOptions)
  {
    
    dailyFileName <- paste("daily", siteName, mgmt, sep="_")
    dailyFileName <- paste(dailyFileName, ".out", sep="")
    dailyFileName <- paste(modelPath,dailyFileName,sep="/")
    
    if (file.exists(dailyFileName))
    { 
      # sep="" refers to any length of white space as being the delimiter.  Don't use " ".
      dataDailyOut <<- read.table(file=dailyFileName,header=TRUE,sep="",dec=".",fill=TRUE)
      nTimes <<- length(dataDailyOut[,1])
      nCols <<- length(mgmtOptions) + 1
      if (doAlloc == 1)
      {
        agdefacData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
        bgdefacData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
        agdefacData[,1] <<- as.integer(dataDailyOut[,timeCol]) + dataDailyOut[,dayCol]/366
        bgdefacData[,1] <<- as.integer(dataDailyOut[,timeCol]) + dataDailyOut[,dayCol]/366
        doAlloc <- 0
      }
      colCnt <- colCnt + 1
      agdefacData[,colCnt] <<- dataDailyOut[,agdefacCol] 
      bgdefacData[,colCnt] <<- dataDailyOut[,bgdefacCol] 

    }
    else
    {
      msg <<- paste(dailyFileName, " does not exist.")
      print(msg)
    }
  }
  
} # End function ReadDailyOut()

#-------------------------------------------------------------------------------------------------
# Function Read_dcsipcsv
# Read dc_sip.csv file. Assign global data structures nppCrpData and nppForData.

Read_dcsipcsv = function ()
{
  
  colCnt <- 1
  timeCol <- 1
  dayCol <- 2
  
  RhCol <- 22
  nppCrpCol1 <- 23
  nppCrpCol2 <- 24
  nppCrpCol3 <- 25

  nppForCol1 <- 26
  nppForCol2 <- 27
  nppForCol6 <- 28
  nppForCol3 <- 29
  nppForCol4 <- 29
  nppForCol5 <- 30

  doAlloc <- 1
  
  for (mgmt in mgmtOptions)
  {

    dcsipFileName <- paste("dc_sip", siteName, mgmt, sep="_")
    dcsipFileName <- paste(dcsipFileName , ".csv", sep="")
    dcsipFileName <- paste(modelPath,dcsipFileName ,sep="/")
    
    if (file.exists(dcsipFileName))
    { 
      dataDCSIP <<- read.table(file=dcsipFileName,header=TRUE,sep=",",dec=".",fill=TRUE)
      nTimes <<- length(dataPsyn[,1])
      nCols <<- length(mgmtOptions) + 1
      nYrs <<- as.integer(nTimes/365)
      minYr <<- as.integer(dataDCSIP[1,timeCol])
      maxYr <<- as.integer(dataDCSIP[nTimes,timeCol])
      if (doAlloc == 1)
      {
        RhData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
        nppCrpData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
        nppForData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
        nppCrpAnData <<- array(data = 0.0, dim = c(nYrs,nCols)) # Annual
        nppForAnData <<- array(data = 0.0, dim = c(nYrs,nCols)) # Annual
        RhData[,1] <<- as.integer(dataDCSIP[,timeCol]) + dataDCSIP[,dayCol]/366
        nppCrpData[,1] <<- as.integer(dataDCSIP[,timeCol]) + dataDCSIP[,dayCol]/366
        nppForData[,1] <<- as.integer(dataDCSIP[,timeCol]) + dataDCSIP[,dayCol]/366
        doAlloc <- 0
      }
      colCnt <- colCnt + 1
      RhData[,colCnt] <<- dataDCSIP[,RhCol] 
      nppCrpData[,colCnt] <<- dataDCSIP[,nppCrpCol1] + dataDCSIP[,nppCrpCol2] + dataDCSIP[,nppCrpCol3]
      nppForData[,colCnt] <<- dataDCSIP[,nppForCol1] + dataDCSIP[,nppForCol2] + dataDCSIP[,nppForCol3] + dataDCSIP[,nppForCol4] + dataDCSIP[,nppForCol5] + dataDCSIP[,nppForCol6]
      for (iyr in minYr:maxYr)
      {
        i <- iyr - minYr + 1
        nppCrpAnData[i,1] <<- iyr
        nppForAnData[i,1] <<- iyr
        nppCrpAnData[i,colCnt] <<- sum(nppCrpData[nppCrpData[,1]>=iyr & nppCrpData[,1]<iyr+1,colCnt])
        nppForAnData[i,colCnt] <<- sum(nppForData[nppForData[,1]>=iyr & nppForData[,1]<iyr+1,colCnt])
      }
    }
    else
    {
      msg <<- paste(dcsipFileName, " does not exist.")
      print(msg)
    }
  }
    
} # End function Read_dcsipcsv()


#-------------------------------------------------------------------------------------------------
# Function PlotDataSOM 
# * Plot graphs of simulated soil C and include som1c(2), som2c(2), and som3c
#   for all mgmtOptions.
# * Set yrange if necessary to insure that all values show up in the graphs.

PlotDataSOM = function ()
{

    startYr <- 1992
    endYr <- 2006
    colvec <<- c("brown","blue","darkorange","darkgreen","cyan","red")
    xrange <- c(startYr,endYr)
    yrange <- c(0,5000)

    plotTitle = "DayCent Total Soil Carbon and Individual Pools"
    units = "gC/m2"
    PlotLayout()
    
    colCnt <- 1   # Column in somscData with somsc values

    for (mgmt in mgmtOptions)
    {
        colCnt <- colCnt + 1
        plot(somtcData[,1],somtcData[,colCnt],type="l",lwd=2,col=colvec,lty=1,main=plotTitle,sub=mgmt,xlab="",ylab=units,xlim=xrange,ylim=yrange)
        lines(somscData[,1],somscData[,colCnt],type="l",lwd=2,col=colvec[2])
        lines(som1c2Data[,1],som1c2Data[,colCnt],type="l",lwd=2,col=colvec[3])
        lines(som2c2Data[,1],som2c2Data[,colCnt],type="l",lwd=2,col=colvec[4])
        lines(som3cData[,1],som3cData[,colCnt],type="l",lwd=2,col=colvec[5])
        lines(som2c1Data[,1],som2c1Data[,colCnt],type="l",lwd=2,col=colvec[6])
        points(obsSOC[,2],obsSOC[,3], pch="O") # O-horizon
        points(obsSOC[,2],obsSOC[,4], pch="2") # 20cm
        points(obsSOC[,2],obsSOC[,5], pch="3") # 30cm

    }
    legvals <<- c("somtc","somsc","som1c(2)", "som2c(2)","som3c","O-horizon")
    legend(x="top",y="bottom",lwd=2,col=colvec,lty=1,legend=legvals,ncol=1)

    #PlotLabels(plotTitle, units)

} # End function PlotDataSOM()

#-------------------------------------------------------------------------------------------------
# Function PlotForestC 


PlotForestC = function ()
{

    startYr <- 1992
    endYr <- 2006
    colvec <<- c("black","green","darkgreen","darkorange","cyan","blue")
    xrange <- c(startYr,endYr)
    yrange <- c(0,60000)

    plotTitle = "DayCent Total Tree Carbon and Individual Pools"
    units = "gC/m2"
    PlotLayout()
    
    colCnt <- 1   # Column in somscData with somsc values

    for (mgmt in mgmtOptions)
    {
        colCnt <- colCnt + 1
        plot(frstcData[,1],frstcData[,colCnt],type="l",lwd=2,col=colvec,lty=1,main=plotTitle,sub=mgmt,xlab="",ylab=units,xlim=xrange,ylim=yrange)
        lines(rleavcData[,1],rleavcData[,colCnt],type="l",lwd=2,col=colvec[2])
        lines(fbrchcData[,1],fbrchcData[,colCnt],type="l",lwd=2,col=colvec[3])
        lines(rlwodcData[,1],rlwodcData[,colCnt],type="l",lwd=2,col=colvec[4])
        lines(frootcData[,1],frootcData[,colCnt],type="l",lwd=2,col=colvec[5])
        lines(crootcData[,1],crootcData[,colCnt],type="l",lwd=2,col=colvec[6])

    }
    legvals <<- c("frstc","rleavc","fbrchc", "rlwodc","frootc","crootc")
    legend(x="top",y="bottom",lwd=2,col=colvec,lty=1,legend=legvals,ncol=1)

    #PlotLabels(plotTitle, units)

} # End function PlotForestC()


#-------------------------------------------------------------------------------------------------
# Function PlotForestNPP 

PlotForestNPP = function ()
{
  
  startYr <- 1992
  endYr <- 2006
  colvec <<- c("black","green","darkgreen","darkorange","cyan","blue")
  xrange <- c(startYr,endYr)
  yrange <- c(0,1000)
  
  plotTitle = "DayCent Total Tree Production by part"
  units = "gC/m2/yr"
  PlotLayout()
  
  colCnt <- 1   # Column in somscData with somsc values
  
  for (mgmt in mgmtOptions)
  {
    colCnt <- colCnt + 1
    plot(fcaccData[,1],fcaccData[,colCnt],type="l",lwd=2,col=colvec,lty=1,main=plotTitle,sub=mgmt,xlab="",ylab=units,xlim=xrange,ylim=yrange)
    lines(rlvaccData[,1],rlvaccData[,colCnt],type="l",lwd=2,col=colvec[2])
    lines(fbraccData[,1],fbraccData[,colCnt],type="l",lwd=2,col=colvec[3])
    lines(rlwaccData[,1],rlwaccData[,colCnt],type="l",lwd=2,col=colvec[4])
    lines(frtaccData[,1],frtaccData[,colCnt],type="l",lwd=2,col=colvec[5])
    lines(crtaccData[,1],crtaccData[,colCnt],type="l",lwd=2,col=colvec[6])
    
  }
  legvals <<- c("fcacc","rleavc","fbrchc", "rlwodc","frootc","crootc")
  legend(x="top",y="bottom",lwd=2,col=colvec,lty=1,legend=legvals,ncol=1)
  
  #PlotLabels(plotTitle, units)
  
} # End function PlotForestNPP()

# Function PlotForestNPP 

#-------------------------------------------------------------------------------------------------

PlotForestGPP = function (period)
{
  
  startYr <- 1992
  endYr <- 2006
  colvec <<- c("darkgreen")
  xrange <- c(startYr,endYr)
  PlotLayout()
  plotTitle = "DayCent Forest GPP"

  if (period == "daily") 
  {
    units = "gC/m2/day"
    yrange <- c(0,20)
  }
  else
  {
    units = "gC/m2/yr"
    yrange <- c(500,1500)
  }

  colCnt <- 1   # Column in gppForData with mgmt values
  for (mgmt in mgmtOptions)
  {
    colCnt <- colCnt + 1
    if (period == "daily") 
    {
      plot(gppForData[,1],gppForData[,colCnt],type="l",lwd=2,col=colvec,lty=1,main=plotTitle,sub=mgmt,xlab="",ylab=units,xlim=xrange,ylim=yrange)
    }
    else
    {
      plot(gppForAnData[,1],gppForAnData[,colCnt],type="l",lwd=2,col=colvec,lty=1,main=plotTitle,sub=mgmt,xlab="",ylab=units,xlim=xrange,ylim=yrange)
    }
    
  }
  legvals <<- c("GPP")
  legend(x="top",y="bottom",lwd=2,col=colvec,lty=1,legend=legvals,ncol=1)
  
  #PlotLabels(plotTitle, units)
  
} # End function PlotForestGPP()

#-------------------------------------------------------------------------------------------------

# Function PlotCropShootC 
# * Plot graphs of simulated shoot C for the specified crop for all mgmtOptions
# * Set ymin, ymax if necessary to insure that all values show up in the graphs.

PlotCropShootC = function()
{
    ymin <- 1
    ymax <- 1500
   
    startYr <- 1962
    endYr <- 2006   
    colvec <<- c("green")
    xrange <- c(startYr,endYr)
    yrange <- c(ymin,ymax)
    obsYrCol <- 4
    obsShootCol <- 10

    plotTitle <- paste("Shoot C", sep=" ")
    units = "gC/m2"
    PlotLayout()
    
    colCnt <- 1   # Column in aglivcData with aglivc values for the mgmt option

    # Observed values are in the Data folder.
    for (mgmt in mgmtOptions)
    {
        colCnt <- colCnt + 1
        matplot(aglivcData[,1],aglivcCropData[,colCnt],type="l",lwd=2,col=colvec,lty=1,main=plotTitle,sub=mgmt,xlab="",ylab=units,xlim=xrange,ylim=yrange)
    }
    #PlotLabels(plotTitle, units)

} # End function PlotCropShootC() 

#-------------------------------------------------------------------------------------------------

# Function PlotCropRootC 
# * Plot graphs of simulated vs. observed root C for the specified crop for all mgmtOptions
# * Set ymin, ymax if necessary to insure that all values show up in the graphs.
# * Make sure "crpval" below correspond to those in your harvest.csv files

PlotCropRootC = function()
{

    ymin <- 1
    ymax <- 500

    startYr <- 1992
    endYr <- 2006
    colvec <<- c("blue")
    xrange <<- c(startYr,endYr)
    yrange <<- c(ymin,ymax)

    plotTitle <- paste("Root C (sim=total, obs=20cm)", sep=" ")
    units = "gC/m2"
    PlotLayout()
    
    colCnt <- 1   # Column in bglivcData with bglivc values for the mgmt option

    # Observed values are in the Data folder.

    for (mgmt in mgmtOptions)
    {
        colCnt <- colCnt + 1
        plot(bglivcData[,1],bglivcCropData[,colCnt],type="l",lwd=2,col=colvec,lty=1,main=plotTitle,sub=mgmt,xlab="",ylab=units,xlim=xrange,ylim=yrange)
    }

    #PlotLabels(plotTitle, units)

} # End function PlotCropRootC() 

#-------------------------------------------------------------------------------------------------

PlotLayout = function()
{
    #library(gplots)

    par(mfrow=c(2,1))

} # End function PlotLayout()

#-------------------------------------------------------------------------------------------------

PlotLayout4 = function()
{
  #library(gplots)
  
  par(mfrow=c(2,2))
  
} # End function PlotLayout4()

#-------------------------------------------------------------------------------------------------

PlotLabels = function(plotTitle, units)
{
    mtext(plotTitle, side = 3, outer=TRUE, cex = 1.5)
    mtext(units, side = 2, outer=TRUE, cex = 1.5)

} #End function PlotLabels()

#-------------------------------------------------------------------------------------------------
# Function ReadCASAresults()
# Read POINT_casa_1_trans.csv and extract NPP, Rh, soil carbon, litter, fT, and fW.
# 

ReadCASAresults = function ()
{
  
  colCnt <- 1
  timeCol <- 3
  dayCol <- 4
  CplantCol <- 8
  CwoodCol <- 9
  CfrootCol <- 10
  ClitMetCol <- 14
  ClitStrucCol <- 15
  CsoilMicCol <- 20
  CsoilSlowCol <- 21
  CsoilPassCol <- 22
  RhCol <- 26
  gppCol <- 34
  nppCol <- 35
  fTCol <- 44
  fWCol <- 45

  startYr <- 1992
  doAlloc <- 1
  
  casaFileName <- paste("POINT_casa_1_trans", ".csv", sep="")
  casaFileName <- paste(casaPath, casaFileName, sep="/")
  
  if (file.exists(casaFileName))
  { 
    # sep="" refers to any length of white space as being the delimiter.  Don't use " ".
    dataCASA <<- read.table(file=casaFileName,header=FALSE,skip=1,sep=",",dec=".",fill=TRUE)
    nTimes <<- length(dataCASA[,1])
    nCols <<- 2 # first column is time, 2nd column is variable value
    nYrs <<- as.integer(nTimes/365)
    minYr <<- as.integer(dataCASA[1,timeCol]) + startYr - 1 
    maxYr <<- as.integer(dataCASA[nTimes,timeCol]) + startYr - 1
    if (doAlloc == 1)
    {
      RhCASAData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      gppCASAData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      gppCASAAnData <<- array(data = 0.0, dim = c(nYrs,nCols)) # Annual
      nppCASAData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      nppCASAAnData <<- array(data = 0.0, dim = c(nYrs,nCols)) # Annual
      
      metCASAData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      strucCASAData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      som1CASAData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      som2CASAData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      som3CASAData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      somscCASAData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      
      fTCASAData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      fWCASAData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
   
      RhCASAData[,1] <<- as.integer(dataCASA[,timeCol])+startYr-1 + dataCASA[,dayCol]/365   
      gppCASAData[,1] <<- as.integer(dataCASA[,timeCol])+startYr-1 + dataCASA[,dayCol]/365
      nppCASAData[,1] <<- as.integer(dataCASA[,timeCol])+startYr-1 + dataCASA[,dayCol]/365
      
      metCASAData[,1] <<- as.integer(dataCASA[,timeCol])+startYr-1 + dataCASA[,dayCol]/365
      strucCASAData[,1] <<- as.integer(dataCASA[,timeCol])+startYr-1 + dataCASA[,dayCol]/365
      som1CASAData[,1] <<- as.integer(dataCASA[,timeCol])+startYr-1 + dataCASA[,dayCol]/365
      som2CASAData[,1] <<- as.integer(dataCASA[,timeCol])+startYr-1 + dataCASA[,dayCol]/365
      som3CASAData[,1] <<- as.integer(dataCASA[,timeCol])+startYr-1 + dataCASA[,dayCol]/365
      somscCASAData[,1] <<- as.integer(dataCASA[,timeCol])+startYr-1 + dataCASA[,dayCol]/365
      
      fTCASAData[,1] <<- as.integer(dataCASA[,timeCol])+startYr-1 + dataCASA[,dayCol]/365
      fWCASAData[,1] <<- as.integer(dataCASA[,timeCol])+startYr-1 + dataCASA[,dayCol]/365
      
      doAlloc <- 0
    }
    
    colCnt <- 2
    
    RhCASAData[,colCnt] <<- dataCASA[,RhCol] 
    gppCASAData[,colCnt] <<- dataCASA[,gppCol] 
    nppCASAData[,colCnt] <<- dataCASA[,nppCol] 
    
    metCASAData[,colCnt] <<- dataCASA[,ClitMetCol]
    strucCASAData[,colCnt] <<- dataCASA[,ClitStrucCol]
    som1CASAData[,colCnt] <<- dataCASA[,CsoilMicCol] 
    som2CASAData[,colCnt] <<- dataCASA[,CsoilSlowCol] 
    som3CASAData[,colCnt] <<- dataCASA[,CsoilPassCol]
    somscCASAData[,colCnt] <<- dataCASA[,CsoilMicCol]+dataCASA[,CsoilSlowCol]+dataCASA[,CsoilPassCol]
    
    fTCASAData[,colCnt] <<- dataCASA[,fTCol] 
    fWCASAData[,colCnt] <<- dataCASA[,fWCol]
    
    for (iyr in minYr:maxYr)
    {
      i <- iyr - startYr + 1
      gppCASAAnData[i,1] <<- iyr
      nppCASAAnData[i,1] <<- iyr
      gppCASAAnData[i,colCnt] <<- sum(gppCASAData[gppCASAData[,1]>=iyr & gppCASAData[,1]<iyr+1,colCnt])
      nppCASAAnData[i,colCnt] <<- sum(nppCASAData[nppCASAData[,1]>=iyr & nppCASAData[,1]<iyr+1,colCnt])
    }

  } else {
    msg <<- paste(casaFileName, " does not exist.")
    print(msg)
  }

} # End function ReadCASAresults()

#-------------------------------------------------------------------------------------------------

# Read POINT_mimics_trans_1.csv and extract soil carbon and litter values
# 

ReadMIMICSresults = function ()
{
  
  colCnt <- 1
  timeCol <- 3
  dayCol <- 4
  # MIMICS carbon pools and fluxes have units mgC/cm3
  RhCol <- 7
  ClitMetCol <- 10
  ClitStrucCol <- 11
  CmicrCol <-12
  CmickCol <-13
  CsoilaCol <- 14
  CsoilcCol <- 15
  CsoilpCol <- 16

  #fTCol <- ?
  #fWCol <- ?
  
  startYr <- 1992
  doAlloc <- 1
  
  mimicsFileName <- paste("POINT_mimics_trans_1", ".csv", sep="")
  mimicsFileName <- paste(mimicsPath, mimicsFileName, sep="/")
  
  if (file.exists(mimicsFileName))
  { 
    # sep="" refers to any length of white space as being the delimiter.  Don't use " ".
    dataMIMICS <<- read.table(file=mimicsFileName,header=FALSE,skip=1,sep=",",dec=".",fill=TRUE)
    nTimes <<- length(dataMIMICS[,1])
    nCols <<- 2 # first column is time, 2nd column is variable value
    nYrs <<- as.integer(nTimes/365)
    minYr <<- as.integer(dataMIMICS[1,timeCol]) + startYr - 1 
    maxYr <<- as.integer(dataMIMICS[nTimes,timeCol]) + startYr - 1
    if (doAlloc == 1)
    {
      RhMIMICSData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      metMIMICSData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      strucMIMICSData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      micrMIMICSData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      mickMIMICSData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      somaMIMICSData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      somcMIMICSData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      sompMIMICSData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      somscMIMICSData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      
      #fTMIMICSData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      #fWMIMICSData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily

      RhMIMICSData[,1] <<- as.integer(dataMIMICS[,timeCol])+startYr-1 + dataMIMICS[,dayCol]/365
      metMIMICSData[,1] <<- as.integer(dataMIMICS[,timeCol])+startYr-1 + dataMIMICS[,dayCol]/365
      strucMIMICSData[,1] <<- as.integer(dataMIMICS[,timeCol])+startYr-1 + dataMIMICS[,dayCol]/365
      micrMIMICSData[,1] <<- as.integer(dataMIMICS[,timeCol])+startYr-1 + dataMIMICS[,dayCol]/365
      mickMIMICSData[,1] <<- as.integer(dataMIMICS[,timeCol])+startYr-1 + dataMIMICS[,dayCol]/365
      somaMIMICSData[,1] <<- as.integer(dataMIMICS[,timeCol])+startYr-1 + dataMIMICS[,dayCol]/365
      somcMIMICSData[,1] <<- as.integer(dataMIMICS[,timeCol])+startYr-1 + dataMIMICS[,dayCol]/365
      sompMIMICSData[,1] <<- as.integer(dataMIMICS[,timeCol])+startYr-1 + dataMIMICS[,dayCol]/365
      somscMIMICSData[,1] <<- as.integer(dataMIMICS[,timeCol])+startYr-1 + dataMIMICS[,dayCol]/365
      
      #fTMIMICSData[,1] <<- as.integer(dataMIMICS[,timeCol])+startYr-1 + dataMIMICS[,dayCol]/365
      #fWMIMICSData[,1] <<- as.integer(dataMIMICS[,timeCol])+startYr-1 + dataMIMICS[,dayCol]/365
      
      doAlloc <- 0
    }
    
    colCnt <- 2
    mgcm3togCm2 = 10*100 # convert mg C/cm3 to gC/m2 assuming depth of 100 cm
    
    RhMIMICSData[,colCnt] <<- dataMIMICS[,RhCol] * mgcm3togCm2
    metMIMICSData[,colCnt] <<- dataMIMICS[,ClitMetCol] * mgcm3togCm2
    strucMIMICSData[,colCnt] <<- dataMIMICS[,ClitStrucCol] * mgcm3togCm2
    micrMIMICSData[,colCnt] <<- dataMIMICS[,CmicrCol] * mgcm3togCm2
    mickMIMICSData[,colCnt] <<- dataMIMICS[,CmickCol] * mgcm3togCm2
    somaMIMICSData[,colCnt] <<- dataMIMICS[,CsoilaCol] * mgcm3togCm2
    somcMIMICSData[,colCnt] <<- dataMIMICS[,CsoilcCol] * mgcm3togCm2
    sompMIMICSData[,colCnt] <<- dataMIMICS[,CsoilpCol] * mgcm3togCm2
    somscMIMICSData[,colCnt] <<- (dataMIMICS[,CsoilaCol]+dataMIMICS[,CsoilcCol]+dataMIMICS[,CsoilpCol]) * mgcm3togCm2
    
    #fTMIMICSData[,colCnt] <<- dataMIMICS[,fTCol] 
    #fWMIMICSData[,colCnt] <<- dataMIMICS[,fWCol]
    
  } else {
    msg <<- paste(mimicsFileName, " does not exist.")
    print(msg)
  }
  
} # End function ReadMIMICSresults()

#-------------------------------------------------------------------------------------------------

# Read POINT_corpse_1_trans.csv and extract soil carbon and litter values
# 

ReadCORPSEresults = function ()
{
  
  colCnt <- 1
  timeCol <- 4
  dayCol <- 3
  # CORPSE carbon pools and fluxes have units kgC/m2
  RhCol <- 24
  CmicCol <-23
  CsoilluCol <- 17  # labile unprotected
  CsoilsuCol <- 18  # slow unprotected
  CsoilduCol <- 19  # dead microbe unprotected
  CsoillpCol <- 20  # labile protected
  CsoilspCol <- 21  # slow protected
  CsoildpCol <- 22  # dead microbe protected
  
  #fTCol <- ?
  #fWCol <- ?
  
  startYr <- 1992
  doAlloc <- 1
  
  corpseFileName <- paste("POINT_corpse_1_trans", ".csv", sep="")
  corpseFileName <- paste(corpsePath, corpseFileName, sep="/")
  
  if (file.exists(corpseFileName))
  { 
    # sep="" refers to any length of white space as being the delimiter.  Don't use " ".
    dataCORPSE <<- read.table(file=corpseFileName,header=FALSE,skip=1,sep=",",dec=".",fill=TRUE)
    nTimes <<- length(dataCORPSE[,1])
    nCols <<- 2 # first column is time, 2nd column is variable value
    nYrs <<- as.integer(nTimes/365)
    minYr <<- as.integer(dataCORPSE[1,timeCol]) + startYr
    maxYr <<- as.integer(dataCORPSE[nTimes,timeCol]) + startYr
    if (doAlloc == 1)
    {
      RhCORPSEData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      micCORPSEData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      somluCORPSEData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      somsuCORPSEData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      somduCORPSEData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      somlpCORPSEData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      somspCORPSEData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      somdpCORPSEData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      somscCORPSEData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      
      #fTCORPSEData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      #fWCORPSEData <<- array(data = 0.0, dim = c(nTimes,nCols)) # Daily
      
      RhCORPSEData[,1] <<- dataCORPSE[,timeCol] + startYr 
      micCORPSEData[,1] <<- dataCORPSE[,timeCol] + startYr 
      somluCORPSEData[,1] <<- dataCORPSE[,timeCol] + startYr 
      somsuCORPSEData[,1] <<- dataCORPSE[,timeCol] + startYr 
      somduCORPSEData[,1] <<- dataCORPSE[,timeCol] + startYr 
      somlpCORPSEData[,1] <<- dataCORPSE[,timeCol] + startYr 
      somspCORPSEData[,1] <<- dataCORPSE[,timeCol] + startYr 
      somdpCORPSEData[,1] <<- dataCORPSE[,timeCol] + startYr 
      somscCORPSEData[,1] <<- dataCORPSE[,timeCol] + startYr 
      
      #fTCORPSEData[,1] <<- dataCORPSE[,timeCol] + startYr 
      #fWCORPSEData[,1] <<- dataCORPSE[,timeCol] + startYr 
      
      doAlloc <- 0
    }
    
    colCnt <- 2
    kg2g <- 1000
    
    RhCORPSEData[1,colCnt] <<- dataCORPSE[1,RhCol] * kg2g
    for (k in 2:length(dataCORPSE[,RhCol]))
    {
      RhCORPSEData[k,colCnt] <<- (dataCORPSE[k,RhCol]-dataCORPSE[k-1,RhCol]) * kg2g   
    }
    micCORPSEData[,colCnt] <<- dataCORPSE[,CmicCol] * kg2g
    somluCORPSEData[,colCnt] <<- dataCORPSE[,CsoilluCol] * kg2g
    somsuCORPSEData[,colCnt] <<- dataCORPSE[,CsoilsuCol] * kg2g
    somduCORPSEData[,colCnt] <<- dataCORPSE[,CsoilduCol] * kg2g
    somlpCORPSEData[,colCnt] <<- dataCORPSE[,CsoillpCol] * kg2g
    somspCORPSEData[,colCnt] <<- dataCORPSE[,CsoilspCol] * kg2g
    somdpCORPSEData[,colCnt] <<- dataCORPSE[,CsoildpCol] * kg2g
    somscCORPSEData[,colCnt] <<- (dataCORPSE[,CsoilluCol]+dataCORPSE[,CsoilsuCol]+dataCORPSE[,CsoilduCol] 
                                + dataCORPSE[,CsoillpCol]+dataCORPSE[,CsoilspCol]+dataCORPSE[,CsoildpCol]) * kg2g
    
    #fTCORPSEData[,colCnt] <<- dataCORPSE[,fTCol] 
    #fWCORPSEData[,colCnt] <<- dataCORPSE[,fWCol]
    
  } else {
    msg <<- paste(corpseFileName, " does not exist.")
    print(msg)
  }
  
} # End function ReadCORPSEresults()
#-------------------------------------------------------------------------------------------------

# This function depends on which daycent run is being compared to CASA.
# Here DayCent forest results for the N-addition simulation are compared to CASA's
Plot_DC_CASA_GPP_NPP = function (period)
{
  
  startYr <- 1992
  endYr <- 2006
  colvec <<- c("darkgreen","green")
  xrange <- c(startYr,endYr)
  PlotLayout()

  if (period == "daily") 
  {
    units = "gC/m2/day"
    yrange <- c(0,20)
  }
  else
  {
    units = "gC/m2/yr"
    yrange <- c(400,1500)
  }

  colCntDC <- 3    # DayCent N-addition 
  subTitle = "DayCent N-unlimited; CASA C-only"
  colCntCASA <- 2
  if (period == "daily") 
  {
    plotTitle = "Daily Forest GPP"
    plot(gppForData[,1],gppForData[,colCntDC],type="l",lwd=2,col=colvec[1],lty=1,main=plotTitle,sub=subTitle,xlab="",ylab=units,xlim=xrange,ylim=yrange)
    lines(gppCASAData[,1],gppCASAData[,colCntCASA],type="l",lwd=2,col=colvec[2])

    plotTitle = "Daily Forest NPP"
    #plot(fcaccData[,1],fcaccData[,colCntDC],type="l",lwd=2,col=colvec,lty=1,main=plotTitle,sub=subTitle,xlab="",ylab=units,xlim=xrange,ylim=yrange)
    plot(nppForData[,1],nppForData[,colCntDC],type="l",lwd=2,col=colvec[1],lty=1,main=plotTitle,sub=subTitle,xlab="",ylab=units,xlim=xrange,ylim=yrange)
    lines(nppCASAData[,1],nppCASAData[,colCntCASA],type="l",lwd=2,col=colvec[2])

  } else {
    plotTitle = "Annual Forest GPP"
    plot(gppForAnData[,1],gppForAnData[,colCntDC],type="b",lwd=2,col=colvec,lty=1,main=plotTitle,sub=subTitle,xlab="",ylab=units,xlim=xrange,ylim=yrange)
    lines(gppCASAAnData[,1],gppCASAAnData[,colCntCASA],type="b",lwd=2,col=colvec[2])

    plotTitle = "Annual Forest NPP"
    plot(nppForAnData[,1],nppForAnData[,colCntDC],type="b",lwd=2,col=colvec[1],lty=1,main=plotTitle,sub=subTitle,xlab="",ylab=units,xlim=xrange,ylim=yrange)
    lines(nppCASAAnData[,1],nppCASAAnData[,colCntCASA],type="b",lwd=2,col=colvec[2])
  }
    
  legvals <<- c("DayCent", "CASA")
  legend(x="top",y="bottom",lwd=2,col=colvec,lty=1,legend=legvals,ncol=1)
  
  #PlotLabels(plotTitle, units)
  
} # End function Plot_DC_CASA_GPP_NPP()

#-------------------------------------------------------------------------------------------------
# Function Plot_DC_CASA_SOILC() 
# * Plot graphs of simulated soil C and include som1c(2), som2c(2), and som3c
#   for DayCent and CASA.
# * Set yrange if necessary to insure that all values show up in the graphs.

Plot_DC_CASA_SOILC = function ()
{
  
  startYr <- 1992
  endYr <- 2006
  colvec <<- c("brown","blue","darkorange","darkgreen","cyan")
  xrange <- c(startYr,endYr)
  yrange <- c(0,10000)
  
  plotTitle = "Above+Below Ground Soil Carbon and Individual Pools"
  units = "gC/m2"
  PlotLayout()
  
  colCntDC <- 3   # This is column for DayCent N-addition    
  colCntCASA <- 2
 
  subTitle = "DayCent N-unlimited"
  plot(somscData[,1],somscData[,colCntDC]+som1c1Data[,colCntDC]+som2c1Data[,colCntDC],type="l",lwd=2,col=colvec[1],lty=1,main=plotTitle,sub=subTitle,xlab="",ylab=units,xlim=xrange,ylim=yrange)
  lines(som1c2Data[,1],som1c1Data[,colCntDC]+som1c2Data[,colCntDC],type="l",lwd=2,col=colvec[2])
  lines(som2c2Data[,1],som2c2Data[,colCntDC]+som2c2Data[,colCntDC],type="l",lwd=2,col=colvec[3])
  lines(som3cData[,1],som3cData[,colCntDC],type="l",lwd=2,col=colvec[4])
  #points(obsSOC[,2],obsSOC[,3], pch="O") # O-horizon
  points(obsSOC[,2],obsSOC[,4], pch="2") # 20cm
  points(obsSOC[,2],obsSOC[,5], pch="3") # 30cm
  
  subTitle = "CASA C-only"
  plot(somscCASAData[,1],somscCASAData[,colCntCASA],type="l",lwd=2,col=colvec[1],lty=1,main=plotTitle,sub=subTitle,xlab="",ylab=units,xlim=xrange,ylim=yrange)
  lines(som1CASAData[,1],som1CASAData[,colCntCASA],type="l",lwd=2,col=colvec[2])
  lines(som2CASAData[,1],som2CASAData[,colCntCASA],type="l",lwd=2,col=colvec[3])
  lines(som3CASAData[,1],som3CASAData[,colCntCASA],type="l",lwd=2,col=colvec[4])
  #points(obsSOC[,2],obsSOC[,3], pch="O") # O-horizon
  points(obsSOC[,2],obsSOC[,4], pch="2") # 20cm
  points(obsSOC[,2],obsSOC[,5], pch="3") # 30cm
    
  legvals <<- c("total somc","som1c(1+2)","som2c(1+2)","som3c")
  legend(x="top",y="bottom",lwd=2,col=colvec,lty=1,legend=legvals,ncol=1)
  
  #PlotLabels(plotTitle, units)
  
} # End function Plot_DC_CASA_SOILC()

#-------------------------------------------------------------------------------------------------
# Function Plot_ALL_SOILC() 
# * Plot graphs of simulated soil C pools
#   for DayCent, CASA, MIMICS, and CORPSE
# * Set yrange if necessary to insure that all values show up in the graphs.

Plot_ALL_SOILC = function ()
{
  
  startYr <- 1992
  endYr <- 2006
  colvec <<- c("brown","blue","darkorange","darkgreen","cyan","black","green")
  xrange <- c(startYr,endYr)
  yrange <- c(0,10000)
  
  plotTitle = "Above+Below Ground Soil Carbon"
  units = "gC/m2"
  PlotLayout4()
  
  colCntDC <- 3   # This is column for DayCent N-addition    
  colCntCASA <- 2
  colCntMIMICS <- 2
  colCntCORPSE <- 2
  
  subTitle = "DayCent N-unlimited"
  plot(somscData[,1],somscData[,colCntDC]+som1c1Data[,colCntDC]+som2c1Data[,colCntDC],type="l",lwd=2,col=colvec[1],lty=1,main=plotTitle,sub=subTitle,xlab="",ylab=units,xlim=xrange,ylim=yrange)
  lines(som1c2Data[,1],som1c1Data[,colCntDC]+som1c2Data[,colCntDC],type="l",lwd=2,col=colvec[2])
  lines(som2c2Data[,1],som2c2Data[,colCntDC]+som2c2Data[,colCntDC],type="l",lwd=2,col=colvec[3])
  lines(som3cData[,1],som3cData[,colCntDC],type="l",lwd=2,col=colvec[4])
  #points(obsSOC[,2],obsSOC[,3], pch="O") # O-horizon
  points(obsSOC[,2],obsSOC[,4], pch="2") # 20cm
  points(obsSOC[,2],obsSOC[,5], pch="3") # 30cm
  
  legvals <<- c("total somc","som1c(1+2)","som2c(1+2)","som3c")
  legend(x="top",y="bottom",lwd=2,col=colvec,lty=1,legend=legvals,ncol=1)
  
  
  subTitle = "CASA C-only"
  plot(somscCASAData[,1],somscCASAData[,colCntCASA],type="l",lwd=2,col=colvec[1],lty=1,main=plotTitle,sub=subTitle,xlab="",ylab=units,xlim=xrange,ylim=yrange)
  lines(som1CASAData[,1],som1CASAData[,colCntCASA],type="l",lwd=2,col=colvec[2])
  lines(som2CASAData[,1],som2CASAData[,colCntCASA],type="l",lwd=2,col=colvec[3])
  lines(som3CASAData[,1],som3CASAData[,colCntCASA],type="l",lwd=2,col=colvec[4])
  #points(obsSOC[,2],obsSOC[,3], pch="O") # O-horizon
  points(obsSOC[,2],obsSOC[,4], pch="2") # 20cm
  points(obsSOC[,2],obsSOC[,5], pch="3") # 30cm
  
  legvals <<- c("total somc","active","slow","passive")
  legend(x="top",y="bottom",lwd=2,col=colvec,lty=1,legend=legvals,ncol=1)
  
  
  subTitle = "MIMICS"
  plot(somscMIMICSData[,1],somscMIMICSData[,colCntMIMICS],type="l",lwd=2,col=colvec[1],lty=1,main=plotTitle,sub=subTitle,xlab="",ylab=units,xlim=xrange,ylim=yrange)
  lines(somaMIMICSData[,1],somaMIMICSData[,colCntMIMICS],type="l",lwd=2,col=colvec[2])
  lines(somcMIMICSData[,1],somcMIMICSData[,colCntMIMICS],type="l",lwd=2,col=colvec[3])
  lines(sompMIMICSData[,1],sompMIMICSData[,colCntMIMICS],type="l",lwd=2,col=colvec[4])
  #points(obsSOC[,2],obsSOC[,3], pch="O") # O-horizon
  points(obsSOC[,2],obsSOC[,4], pch="2") # 20cm
  points(obsSOC[,2],obsSOC[,5], pch="3") # 30cm
  
  legvals <<- c("total somc","available","chemically protected","physically protected")
  legend(x="top",y="bottom",lwd=2,col=colvec,lty=1,legend=legvals,ncol=1)
  
  
  subTitle = "CORPSE"
  plot(somscCORPSEData[,1],somscCORPSEData[,colCntCORPSE],type="l",lwd=2,col=colvec[1],lty=1,main=plotTitle,sub=subTitle,xlab="",ylab=units,xlim=xrange,ylim=yrange)
  lines(somluCORPSEData[,1],somluCORPSEData[,colCntCORPSE],type="l",lwd=2,col=colvec[2])
  lines(somsuCORPSEData[,1],somsuCORPSEData[,colCntCORPSE],type="l",lwd=2,col=colvec[3])
  lines(somduCORPSEData[,1],somduCORPSEData[,colCntCORPSE],type="l",lwd=2,col=colvec[4])
  lines(somlpCORPSEData[,1],somlpCORPSEData[,colCntCORPSE],type="l",lwd=2,col=colvec[5])
  lines(somspCORPSEData[,1],somspCORPSEData[,colCntCORPSE],type="l",lwd=2,col=colvec[6])
  lines(somdpCORPSEData[,1],somdpCORPSEData[,colCntCORPSE],type="l",lwd=2,col=colvec[7])
  #points(obsSOC[,2],obsSOC[,3], pch="O") # O-horizon
  points(obsSOC[,2],obsSOC[,4], pch="2") # 20cm
  points(obsSOC[,2],obsSOC[,5], pch="3") # 30cm
  
  legvals <<- c("total somc","labile unprot","slow unprot","deadmic unprot","labile prot","slow prot","deadmic prot")
  legend(x="top",y="bottom",lwd=2,col=colvec,lty=1,legend=legvals,ncol=1)

  
  #PlotLabels(plotTitle, units)
  
} # End function Plot_ALL_SOILC()

#-------------------------------------------------------------------------------------------------
# Function Plot_ALL_Rh() 
# * Plot graphs of simulated daily heterotrophic respiration (Rh)
#   for DayCent, CASA, MIMICS, and CORPSE
# * Set yrange if necessary to insure that all values show up in the graphs.

Plot_ALL_Rh = function ()
{
  
  startYr <- 1992
  endYr <- 2006
  colvec <<- c("brown","blue","darkorange","darkgreen","cyan","black","green")
  xrange <- c(startYr,endYr)
  yrange <- c(0,15)
  
  plotTitle = "Heterotrophic Respiration"
  units = "gC/m2/day"
  PlotLayout4()
  
  colCntDC <- 3   # This is column for DayCent N-addition    
  colCntCASA <- 2
  colCntMIMICS <- 2
  colCntCORPSE <- 2
  
  subTitle = "DayCent N-unlimited"
  plot(RhData[,1],RhData[,colCntDC],type="l",lwd=2,col=colvec[1],lty=1,main=plotTitle,sub=subTitle,xlab="",ylab=units,xlim=xrange,ylim=yrange)
  
  subTitle = "CASA C-only"
  plot(RhCASAData[,1],RhCASAData[,colCntCASA],type="l",lwd=2,col=colvec[1],lty=1,main=plotTitle,sub=subTitle,xlab="",ylab=units,xlim=xrange,ylim=yrange)
  
  subTitle = "MIMICS"
  plot(RhMIMICSData[,1],RhMIMICSData[,colCntMIMICS],type="l",lwd=2,col=colvec[1],lty=1,main=plotTitle,sub=subTitle,xlab="",ylab=units,xlim=xrange,ylim=yrange)
  
  subTitle = "CORPSE"
  plot(RhCORPSEData[,1],RhCORPSEData[,colCntCORPSE],type="l",lwd=2,col=colvec[1],lty=1,main=plotTitle,sub=subTitle,xlab="",ylab=units,xlim=xrange,ylim=yrange)
  
  legvals <<- c("Rh")
  legend(x="top",y="bottom",lwd=2,col=colvec,lty=1,legend=legvals,ncol=1)
  
  #PlotLabels(plotTitle, units)
  
} # End function Plot_ALL_Rh()

#-------------------------------------------------------------------------------------------------
# Function Plot_DC_CASA_LITC() 
# * Plot graphs of simulated litter (metabolic and structural C)
#   for DayCent and CASA.
# * Set yrange if necessary to insure that all values show up in the graphs.

Plot_DC_CASA_LITC = function ()
{
  
  startYr <- 1992
  endYr <- 2006
  colvec <<- c("brown","blue","darkorange","darkgreen","cyan")
  xrange <- c(startYr,endYr)
  yrange <- c(0,1200)
  
  plotTitle = "Litter Carbon Pools"
  units = "gC/m2"
  PlotLayout()
  
  colCntDC <- 3   # This is column for DayCent N-addition    
  colCntCASA <- 2
  
  subTitle = "DayCent N-unlimited"
  plot(metc1Data[,1],metc1Data[,colCntDC]+metc2Data[,colCntDC],type="l",lwd=2,col=colvec[1],lty=1,main=plotTitle,sub=subTitle,xlab="",ylab=units,xlim=xrange,ylim=yrange)
  lines(strucc1Data[,1],strucc1Data[,colCntDC]+strucc2Data[,colCntDC],type="l",lwd=2,col=colvec[2])
  
  subTitle = "CASA C-only"
  plot(metCASAData[,1],metCASAData[,colCntCASA],type="l",lwd=2,col=colvec[1],lty=1,main=plotTitle,sub=subTitle,xlab="",ylab=units,xlim=xrange,ylim=yrange)
  lines(strucCASAData[,1],strucCASAData[,colCntCASA],type="l",lwd=2,col=colvec[2])
  
  legvals <<- c("metabolic","structural")
  legend(x="top",y="bottom",lwd=2,col=colvec,lty=1,legend=legvals,ncol=1)
  
  #PlotLabels(plotTitle, units)
  
} # End function Plot_DC_CASA_LITC()


#-------------------------------------------------------------------------------------------------
# Function Plot_DC_CASA_fT_fW() 
# * Plot graphs of simulated soil and temperature effects on decomposition
#   for DayCent and CASA.
# * Set yrange if necessary to insure that all values show up in the graphs.

Plot_DC_CASA_fT_fW = function ()
{
  
  startYr <- 1992
  endYr <- 2006
  colvec <<- c("red","blue")
  xrange <- c(startYr,endYr)
  yrange <- c(0,3)
  
  plotTitle = "Soil temperature and moisture effects on decomposition"
  units = "fraction"
  PlotLayout()
  
  colCntDC <- 3   # This is column for DayCent N-addition    
  colCntCASA <- 2
  
  subTitle = "DayCent N-unlimited"
  plot(agdefacData[,1],agdefacData[,colCntDC],type="l",lwd=2,col=colvec[1],lty=1,main=plotTitle,sub=subTitle,xlab="",ylab=units,xlim=xrange,ylim=yrange)
  lines(bgdefacData[,1],bgdefacData[,colCntDC],type="l",lwd=2,col=colvec[2])
  
  subTitle = "CASA C-only"
  plot(fTCASAData[,1],fTCASAData[,colCntCASA],type="l",lwd=2,col=colvec[1],lty=1,main=plotTitle,sub=subTitle,xlab="",ylab=units,xlim=xrange,ylim=yrange)
  lines(fWCASAData[,1],fWCASAData[,colCntCASA],type="l",lwd=2,col=colvec[2])
  
  legvals <<- c("f(T)","f(W)")
  legend(x="top",y="bottom",lwd=2,col=colvec,lty=1,legend=legvals,ncol=1)
  
  #PlotLabels(plotTitle, units)
  
} # End function Plot_DC_CASA_fT_fW()
#-------------------------------------------------------------------------------------------------
GetObsSOC = function ()
{
                g C/m2
#  BW O-horizon	  1771.5
#  BW (0-20cm)	  3453.25
#  BW (0-30cm)	  4720.75
#  PH O-horizon	  2565.166667
#  PH (0-20cm)	  5476
#  PH (0-30cm)  	7684.666667
  
colNamesObsSOC = c("siteName", "year", "O-horizon", "d20cm", "d30cm")
nSites = 2
nCols = length(colNamesObsSOC)
obsSOC <<- array(data = 0.0, dim = c(nSites,nCols))
colnames(obsSOC) = colNamesObsSOC

obsSOC[1,1] = "BW"
obsSOC[1,2] = 2005
obsSOC[1,3] = 1771.5
obsSOC[1,4] = 3453.25
obsSOC[1,5] = 4720.75
obsSOC[1,1] = "Prospect Hill"
obsSOC[2,2] = 2006
obsSOC[2,3] = 2565.166667
obsSOC[2,4] = 5476
obsSOC[2,5] = 7684.666667
  
} # End function GetObsSOC()

#-------------------------------------------------------------------------------------------------

# Read simulated soil C (somsc), live biomass C, and NPP from the .lis files
ReadDataLis()

ReadPsynOut()   # Read simulated GPP from the psyn.out files
Read_dcsipcsv() # Read simulated NPP from the dc_sip.csv files
ReadDailyOut()  # Read simulated agdefac and bgdefac from daily.out

# DayCent control and Naddition State Varaibles
PlotDataSOM() 
PlotForestC() 

# DayCent control and Naddition Fluxes
PlotForestNPP() # annual only because it is plotting acc variables by part
PlotForestGPP("daily")
PlotForestGPP("annual")

# Compare DayCent, CASA, MIMICS, and CORPSE results
ReadCASAresults()
ReadMIMICSresults()
ReadCORPSEresults()

# DayCent and CASA comparisons
Plot_DC_CASA_GPP_NPP("daily")
Plot_DC_CASA_GPP_NPP("annual")
Plot_DC_CASA_SOILC()
Plot_DC_CASA_LITC()
Plot_DC_CASA_fT_fW()  # Plot soil and temperature multipliers on decomposition rates

# DayCent, CASA, MIMICS, CORPSE comparisons
Plot_ALL_SOILC()
Plot_ALL_Rh()





