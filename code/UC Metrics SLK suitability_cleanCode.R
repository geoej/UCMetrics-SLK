
#list.of.packages <- c("raster","rgdal", "maptools","RMySQL","rjson","sp","dplyr","jsonlite", "RCurl", "tibble  ")
#pckList <- lapply(list.of.packages, require, character.only = TRUE)
f <- function (ngrid, ncrop) {
    
  
  library(sf)

  grid <- st_read("/Users/ej/CFFRC/04-Research/UC metrics/UCMetrics-SLK/in/grid/slk_gridlo543.shp")
  
  #---------------------------------------------------
  #               data load
  #---------------------------------------------------
  library(raster)
  
  # climate data
  tempdata = stack()
  
  ukmetpath = "/Users/ej/CFFRC/04-Research/UK crops/Analysis/in/ukmet/"
  for (i in c("UKtasJan", "UKtasFeb", "UKtasMar", "UKtasApr", "UKtasMay", "UKtasJun", "UKtasJul",
              "UKtasAug", "UKtasSep", "UKtasOct", "UKtasNov", "UKtasDec")) {
    temprast = raster(paste(ukmetpath,i,".tif",sep=""));
    crs(temprast) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0";
    tempdata = addLayer(tempdata, temprast);
  }
  names(tempdata) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
  
  # for soil
  phdata = stack()
  sandata = stack()
  claydata = stack()
  
  phpath ="/Users/ej/CFFRC/04-Research/Soil/climatesoilindex/cmbndsuitindx/data/soil/"
  otherpath = "/Users/ej/CFFRC/05-Projects/Bera/Activity 3/Tool/TTSR/soil/Soils/"
  for (o in 1:7) phdata = addLayer(phdata, raster(paste(phpath,"PHIHOX_M_sl",  o, "_250m_ll.tif", sep = "")))
  for (o in 1:7) claydata = addLayer(claydata, raster(paste(otherpath,"CLYPPT_M_sl",  o, "_250m_ll.tif", sep = "")))
  for (o in 1:7) sandata = addLayer(sandata, raster(paste(otherpath,"SNDPPT_M_sl",  o, "_250m_ll.tif", sep = "")))
  depthdata = raster(paste(phpath,"BDRICM_M_250m_ll.tif", sep = ""))
  
  # data from DB
  ecology <- read.csv("./in/ecology/ecology Jul20.csv")
  
  
  #---------------------------------------------------
  #               running suitability
  #---------------------------------------------------
  #library(dplyr)
  # outnames <- c("CROP_ID","Jan","Feb","Mar","Apr",            
  # "May","Jun","Jul","Aug","Sep",            
  # "Oct","Nov","Dec","AvgMonths","averagesoilsuit",
  # "name","avgclimsoil","lon","lat")            
  
  climSoil_ID_crops = data.frame(CROP_ID = NA,
                                 Jan= NA,
                                 Feb= NA, 
                                 Mar= NA, 
                                 Apr= NA, 
                                 May= NA, 
                                 Jun= NA, 
                                 Jul= NA,  
                                 Aug= NA, 
                                 Sep= NA, 
                                 Oct= NA,  
                                 Nov= NA, 
                                 Dec= NA,  
                                 AvgMonths= NA, 
                                 averagesoilsuit= NA, 
                                 name=NA, 
                                 avgclimsoil=NA, 
                                 lon=NA, 
                                 lat=NA)
  climsoil_ID_crops_gridpoints = list()
  climsoil_ID_crops_gridpoints_topcrop = data.frame()
  
  #--------------
  # getting extracting points for each coordinate
  #--------------
  
  for (i in 1:ngrid){
    print(i)
    coords = st_coordinates(grid$geometry[i])
    sp = SpatialPointsDataFrame(coords, data= as.data.frame(1))
    crs(sp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
    # Extract soil data
    
    ph = NULL
    clay = NULL
    sand = NULL
    
    for (l in 1:7){
      ph = cbind(ph, as.numeric(extract(phdata[[l]], sp)))
      clay = cbind(clay, as.numeric(extract(claydata[[l]], sp)))
      sand = cbind(sand, as.numeric(extract(sandata[[l]], sp)))
    }
    BDRICM.BDRICM_M = as.numeric(extract(depthdata, sp))
    
    colnames(ph) <- c("PHIHOX.M.sl1","PHIHOX.M.sl2","PHIHOX.M.sl3","PHIHOX.M.sl4","PHIHOX.M.sl5","PHIHOX.M.sl6", "PHIHOX.M.sl7")
    colnames(clay) <- c("CLYPPT.M.sl1","CLYPPT.M.sl2","CLYPPT.M.sl3","CLYPPT.M.sl4","CLYPPT.M.sl5","CLYPPT.M.sl6", "CLYPPT.M.sl7")
    colnames(sand) <- c("SNDPPT.M.sl1","SNDPPT.M.sl2","SNDPPT.M.sl3","SNDPPT.M.sl4","SNDPPT.M.sl5","SNDPPT.M.sl6", "SNDPPT.M.sl7")
    #names(BDRICM.M) <- "BDRICM.BDRICM_M"
    
    
    out <- cbind(ph, clay, sand, BDRICM.BDRICM_M)
    
    # extract climate data
    utemp <- as.numeric(data.frame(extract(tempdata, sp)))
    #print(utemp)
  
    
    # calculate the seasonal suitability for average seasonal temperature for n crops
    
    if (any(is.nan(utemp)) | any((is.na(out)))){
      climsoil_ID_crops_gridpoints[[i]] <- NaN
      climsoil_ID_crops_gridpoints_topcrop  =  rbind(climsoil_ID_crops_gridpoints_topcrop , data.frame(CROP_ID = NA,
                                                                                                       Jan= NA,
                                                                                                       Feb= NA, 
                                                                                                       Mar= NA, 
                                                                                                       Apr= NA, 
                                                                                                       May= NA, 
                                                                                                       Jun= NA, 
                                                                                                       Jul= NA,  
                                                                                                       Aug= NA, 
                                                                                                       Sep= NA, 
                                                                                                       Oct= NA,  
                                                                                                       Nov= NA, 
                                                                                                       Dec= NA,  
                                                                                                       AvgMonths= NA, 
                                                                                                       averagesoilsuit= NA, 
                                                                                                       name=NA, 
                                                                                                       avgclimsoil=NA, 
                                                                                                       lon=NA, 
                                                                                                       lat=NA))
    }else{
      #tecocrop = numeric()
  
      seasons = numeric()
      
      phsuits = numeric()
      depthsuits = numeric()
      txtursuits = numeric()
      
      #totalsoilsuit = numeric()
      averagesoilsuit = numeric()
      
      #TCS = data.frame()
      #TCSave = data.frame()
      TCnorains = data.frame()
      climSoil_ID_crops = data.frame()
      
      #-----------------
      # run for all crops 
      #----------------
      
      for (k in 1:ncrop){ #nrow(ecology)){
        
        # getting approximate season
        
        season = round ((ecology[k,"season_length_min"] + ecology[k,"season_length_max"])/60)
        seasons = append(seasons, season)
        
        #---------------------------------------------------
        #               climate suitability
        #---------------------------------------------------
        
        tmonsuit = numeric()
        tcropsuit = numeric()
        tcropsuitpren = numeric()
        
        #raindataggreg = numeric()
        #raindatapren = numeric()
        #rainsuit = numeric()  
        
        phsuit = numeric()  
        depthsuit = numeric()
        texturesuit = numeric()
        
        #totalsuit = numeric()
        #totalsuitave = numeric()
        #totalsoilsuit = numeric()
        totalsuitave = numeric()
        
        for (j in 1:12){
          x = utemp[j]
          tabs_min=ecology[k,"temperature_absolute_min"] 
          topt_min=ecology[k,"temperature_optimal_min"] 
          topt_max=ecology[k,"temperature_optimal_max"] 
          tabs_max=ecology[k,"temperature_absolute_max"]
          #print(paste0("i = ", i,", k = ", k, " tabs_min = ", tabs_min))
          if (x < tabs_min) {y = 0}
          if ((x >= tabs_min) & (x < topt_min)) {y = ((x - tabs_min)/(topt_min - tabs_min))}
          if ((x>= topt_min) & (x <topt_max))   {y=1}
          if ((x >= topt_max) & (x <tabs_max))   {y  = 1-( (x - topt_max)/(tabs_max - topt_max))} 
          if (x >= tabs_max) {y=0}
          tmonsuit= append(tmonsuit, as.numeric(y))  #CHAN test2
        }
        #teco = cbind (tabs_min,topt_min,topt_max,tabs_max)
        #tecocrop = rbind(tecocrop, teco)
        names(tmonsuit) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
        #assign(paste("tmonsuit",i, sep="") , tmonsuit)
        #}
        
        # control the quality
        # tmonsuits=numeric()
        # for (i in 1:nrow(ecology)){
        #   tmonsuits = rbind(tmonsuits,get(paste("tmonsuit",i,sep="")))
        # }
        # round(tmonsuits, digits =1)
        
        # GETTING THE SEASONS RIGHT 
        # average the temp for that crop to be used for temp suitability
        #for (j in 1:nrow(ecology)){
        
        #tmonsuit <- get(paste("tmonsuit",j, sep=""))
        
        if (season <= 1) {
          tcropsuit = tmonsuit
        }
        
        if ((season < 12) & (season > 1)) {
          for (m in 1:(12-(season-1))){
            tcropsuit = append(tcropsuit, min(tmonsuit[m:(m+(season-1))]))
          }
          
          #creating a stack for the months that fall over dec
          tcropsuitpren = c(tmonsuit[(12-(season-2)):12], tmonsuit[1:(season-1)])
  
          # adding the aggreages for the rest of the year
          for (n in 1:(length(tcropsuitpren)-(season-1))){
            tcropsuit = append(tcropsuit, min(tcropsuitpren[n:(n+(season-1))]))
          }
        }
        
        if (season >= 12) {                           #CHAN test2
          # to calcuate for the prennials all layers are the same
          for (n in 1:12){
            tcropsuit = append(tcropsuit, min(tmonsuit))
          }
        }
        tcropsuit = tcropsuit[1:12]
        names(tcropsuit) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
  
        #assign(paste("tcropsuit",j, sep="") , tcropsuit) 
        #}
        
        # control the quality
        # tcropsuits=numeric()
        # for (i in 1:nrow(ecology)){
        #   tcropsuits = rbind(tcropsuits,get(paste("tcropsuit",i,sep="")))
        #   #print(get(paste("tcropsuit",i,sep="")))
        # }
        # round(tcropsuits, digits = 1)
        
  
        TCnorains = rbind(TCnorains, tcropsuit*100)
        
        
        names(TCnorains) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
        
        
        #---------------------------------------------------
        #               soil suitability
        #---------------------------------------------------
        
        ########################################################
        # calcualte PH suitability
        ########################################################
        
        #for (k in 1:nrow(ecology)){
        #phsuit = numeric()
        phsuitfun = function(x) {
          y=0
          phabs_min=ecology[k,"soil_ph_absolute_min"] 
          phopt_min=ecology[k,"soil_ph_optimal_min"] 
          phopt_max=ecology[k,"soil_ph_optimal_max"] 
          phabs_max=ecology[k,"soil_ph_absolute_max"]
          if (any(x > phabs_min) & any(x < phopt_min))  {y = round(((x - phabs_min)/(phopt_min - phabs_min))*100)}
          if (any(x > phopt_min) & any(x < phopt_max))  {y  = 100}
          if (any(x > phopt_max) & any(x < phabs_max))  {y  = round((1-( (x - phopt_max)/(phabs_max - phopt_max)))*100)}
          if (any(x < phabs_min) | any(x > phabs_max))  {y = 0}
          return(y)
        }  
        
        if (ecology[k,"soil_depth_optimal_low"] == 1){
          phagg = (1/1200)*(5*(out[1,"PHIHOX.M.sl1"]+out[1,"PHIHOX.M.sl2"])+
                               10*(out[1,"PHIHOX.M.sl2"]+out[1,"PHIHOX.M.sl3"])+
                               15*(out[1,"PHIHOX.M.sl3"]+out[1,"PHIHOX.M.sl4"])+
                               30*(out[1,"PHIHOX.M.sl4"]+out[1,"PHIHOX.M.sl5"]))
        }else {
          if (ecology[k, "soil_depth_optimal_medium"] == 1) {
            phagg = (1/2000)*(5*(out[1,"PHIHOX.M.sl1"]+out[1,"PHIHOX.M.sl2"])+
                                 10*(out[1,"PHIHOX.M.sl2"]+out[1,"PHIHOX.M.sl3"])+
                                 15*(out[1,"PHIHOX.M.sl3"]+out[1,"PHIHOX.M.sl4"])+
                                 30*(out[1,"PHIHOX.M.sl4"]+out[1,"PHIHOX.M.sl5"])+
                                 40*(out[1,"PHIHOX.M.sl5"]+out[1,"PHIHOX.M.sl6"]))
          }else {
            phagg = (1/4000)*(5*(out[1,"PHIHOX.M.sl1"]+out[1,"PHIHOX.M.sl2"])+
                                 10*(out[1,"PHIHOX.M.sl2"]+out[1,"PHIHOX.M.sl3"])+
                                 15*(out[1,"PHIHOX.M.sl3"]+out[1,"PHIHOX.M.sl4"])+
                                 30*(out[1,"PHIHOX.M.sl4"]+out[1,"PHIHOX.M.sl5"])+
                                 40*(out[1,"PHIHOX.M.sl5"]+out[1,"PHIHOX.M.sl6"])+
                                 100*(out[1,"PHIHOX.M.sl6"]+out[1,"PHIHOX.M.sl7"]))
            
          }
        }
        
        phsuit <- lapply(phagg, phsuitfun )
        phsuits = append(phsuits, phsuit)
        #assign(paste("phsuit",k, sep="") , phsuit)
        #}
        
        # control the quality
        # phsuits=numeric()
        # for (i in 1:nrow(ecology)){
        #   phsuits = rbind(phsuits,get(paste("phsuit",i,sep="")))
        # }
        
        ########################################################
        # calcualte depth suitability
        ########################################################
        
        # depthsuit = numeric() 
        #for (k in 1:nrow(ecology)){
        
        depthsuitfun = function(x) {
          optz=10
          if ((x >=0) & (x< 50)) {
            if (ecology[k,"soil_depth_optimal_low"] == 1) {
              optz = 100
            } else {
              optz = 10
            }
          }
          if ((x >= 50) & (x< 150)) {
            if (ecology[k,"soil_depth_optimal_medium"] == 1) {
              optz = 100
            } else {
              optz = 10
            }
          }
          if (x >= 150) {
            if (ecology[k,"soil_depth_optimal_deep"] == 1) {
              optz = 100
            } else {
              optz = 10
            }
          }
          
          return(optz)
        }  
        
        depthsuit <- lapply(out[1,"BDRICM.BDRICM_M"], depthsuitfun )
        depthsuits = append(depthsuits, depthsuit)
        
        #}
        #depthsuit <- as.numeric(depthsuit)
        #names(depthsuit) <- c(sprintf("Depthsuit",seq(1:k)))
        
        ########################################################
        # calculate the soil texture suitability for the selected crops
        ########################################################
        
        #texturesuit = numeric() 
        #for (k in 1:nrow(ecology)){
        
        if (ecology[k,"soil_texture_optimal_heavy"] == 1) {
           if (ecology[k,"soil_depth_optimal_low"] == 1){
            sandagg = (1/120)*(5*(out[1,"SNDPPT.M.sl1"]+out[1,"SNDPPT.M.sl2"])+
                                 10*(out[1,"SNDPPT.M.sl2"]+out[1,"SNDPPT.M.sl3"])+
                                 15*(out[1,"SNDPPT.M.sl3"]+out[1,"SNDPPT.M.sl4"])+
                                 30*(out[1,"SNDPPT.M.sl4"]+out[1,"SNDPPT.M.sl5"]))
          }else {
            if (ecology[k, "soil_depth_optimal_medium"] == 1) {
              sandagg = (1/200)*(5*(out[1,"SNDPPT.M.sl1"]+out[1,"SNDPPT.M.sl2"])+
                                   10*(out[1,"SNDPPT.M.sl2"]+out[1,"SNDPPT.M.sl3"])+
                                   15*(out[1,"SNDPPT.M.sl3"]+out[1,"SNDPPT.M.sl4"])+
                                   30*(out[1,"SNDPPT.M.sl4"]+out[1,"SNDPPT.M.sl5"])+
                                   40*(out[1,"SNDPPT.M.sl5"]+out[1,"SNDPPT.M.sl6"]))
            }else {
              sandagg = (1/400)*(5*(out[1,"SNDPPT.M.sl1"]+out[1,"SNDPPT.M.sl2"])+
                                   10*(out[1,"SNDPPT.M.sl2"]+out[1,"SNDPPT.M.sl3"])+
                                   15*(out[1,"SNDPPT.M.sl3"]+out[1,"SNDPPT.M.sl4"])+
                                   30*(out[1,"SNDPPT.M.sl4"]+out[1,"SNDPPT.M.sl5"])+
                                   40*(out[1,"SNDPPT.M.sl5"]+out[1,"SNDPPT.M.sl6"])+
                                   100*(out[1,"SNDPPT.M.sl6"]+out[1,"SNDPPT.M.sl7"]))
              
            }
          }
          
          if (sandagg >= 65) {
            texturesuit = append(texturesuit, 25)
          }else {
            texturesuit = append(texturesuit, 100)
          }
          
          
        } else {
          if (ecology[k,"soil_texture_optimal_light"] == 1) {
            
            if (ecology[k,"soil_depth_optimal_low"] == 1){
              clayagg = (1/120)*(5*(out[1,"CLYPPT.M.sl1"]+out[1,"CLYPPT.M.sl2"])+
                                   10*(out[1,"CLYPPT.M.sl2"]+out[1,"CLYPPT.M.sl3"])+
                                   15*(out[1,"CLYPPT.M.sl3"]+out[1,"CLYPPT.M.sl4"])+
                                   30*(out[1,"CLYPPT.M.sl4"]+out[1,"CLYPPT.M.sl5"]))
            }else {
              if (ecology[k, "soil_depth_optimal_medium"] == 1) {
                clayagg = (1/200)*(5*(out[1,"CLYPPT.M.sl1"]+out[1,"CLYPPT.M.sl2"])+
                                     10*(out[1,"CLYPPT.M.sl2"]+out[1,"CLYPPT.M.sl3"])+
                                     15*(out[1,"CLYPPT.M.sl3"]+out[1,"CLYPPT.M.sl4"])+
                                     30*(out[1,"CLYPPT.M.sl4"]+out[1,"CLYPPT.M.sl5"])+
                                     40*(out[1,"CLYPPT.M.sl5"]+out[1,"CLYPPT.M.sl6"]))
              }else {
                clayagg = (1/400)*(5*(out[1,"CLYPPT.M.sl1"]+out[1,"CLYPPT.M.sl2"])+
                                     10*(out[1,"CLYPPT.M.sl2"]+out[1,"CLYPPT.M.sl3"])+
                                     15*(out[1,"CLYPPT.M.sl3"]+out[1,"CLYPPT.M.sl4"])+
                                     30*(out[1,"CLYPPT.M.sl4"]+out[1,"CLYPPT.M.sl5"])+
                                     40*(out[1,"CLYPPT.M.sl5"]+out[1,"CLYPPT.M.sl6"])+
                                     100*(out[1,"CLYPPT.M.sl6"]+out[1,"CLYPPT.M.sl7"]))
                
              }
            }
            
            if (clayagg >= 15) {
              texturesuit = append(texturesuit, 25)
            }else {
              texturesuit = append(texturesuit, 100)
            }
          } else {
            
            #if (ecology[k,"soil_texture_optimal_medium"] == 1) {
       
            if (ecology[k,"soil_depth_optimal_low"] == 1){
              sandagg = (1/120)*(5*(out[1,"SNDPPT.M.sl1"]+out[1,"SNDPPT.M.sl2"])+
                                   10*(out[1,"SNDPPT.M.sl2"]+out[1,"SNDPPT.M.sl3"])+
                                   15*(out[1,"SNDPPT.M.sl3"]+out[1,"SNDPPT.M.sl4"])+
                                   30*(out[1,"SNDPPT.M.sl4"]+out[1,"SNDPPT.M.sl5"]))
              
              clayagg = (1/120)*(5*(out[1,"CLYPPT.M.sl1"]+out[1,"CLYPPT.M.sl2"])+
                                   10*(out[1,"CLYPPT.M.sl2"]+out[1,"CLYPPT.M.sl3"])+
                                   15*(out[1,"CLYPPT.M.sl3"]+out[1,"CLYPPT.M.sl4"])+
                                   30*(out[1,"CLYPPT.M.sl4"]+out[1,"CLYPPT.M.sl5"]))
            }else {
              if (ecology[k, "soil_depth_optimal_medium"] == 1) {
                sandagg = (1/200)*(5*(out[1,"SNDPPT.M.sl1"]+out[1,"SNDPPT.M.sl2"])+
                                     10*(out[1,"SNDPPT.M.sl2"]+out[1,"SNDPPT.M.sl3"])+
                                     15*(out[1,"SNDPPT.M.sl3"]+out[1,"SNDPPT.M.sl4"])+
                                     30*(out[1,"SNDPPT.M.sl4"]+out[1,"SNDPPT.M.sl5"])+
                                     40*(out[1,"SNDPPT.M.sl5"]+out[1,"SNDPPT.M.sl6"]))
                
                clayagg = (1/200)*(5*(out[1,"CLYPPT.M.sl1"]+out[1,"CLYPPT.M.sl2"])+
                                     10*(out[1,"CLYPPT.M.sl2"]+out[1,"CLYPPT.M.sl3"])+
                                     15*(out[1,"CLYPPT.M.sl3"]+out[1,"CLYPPT.M.sl4"])+
                                     30*(out[1,"CLYPPT.M.sl4"]+out[1,"CLYPPT.M.sl5"])+
                                     40*(out[1,"CLYPPT.M.sl5"]+out[1,"CLYPPT.M.sl6"]))
              }else {
                sandagg = (1/400)*(5*(out[1,"SNDPPT.M.sl1"]+out[1,"SNDPPT.M.sl2"])+
                                     10*(out[1,"SNDPPT.M.sl2"]+out[1,"SNDPPT.M.sl3"])+
                                     15*(out[1,"SNDPPT.M.sl3"]+out[1,"SNDPPT.M.sl4"])+
                                     30*(out[1,"SNDPPT.M.sl4"]+out[1,"SNDPPT.M.sl5"])+
                                     40*(out[1,"SNDPPT.M.sl5"]+out[1,"SNDPPT.M.sl6"])+
                                     100*(out[1,"SNDPPT.M.sl6"]+out[1,"SNDPPT.M.sl7"]))
                
                clayagg = (1/400)*(5*(out[1,"CLYPPT.M.sl1"]+out[1,"CLYPPT.M.sl2"])+
                                     10*(out[1,"CLYPPT.M.sl2"]+out[1,"CLYPPT.M.sl3"])+
                                     15*(out[1,"CLYPPT.M.sl3"]+out[1,"CLYPPT.M.sl4"])+
                                     30*(out[1,"CLYPPT.M.sl4"]+out[1,"CLYPPT.M.sl5"])+
                                     40*(out[1,"CLYPPT.M.sl5"]+out[1,"CLYPPT.M.sl6"])+
                                     100*(out[1,"CLYPPT.M.sl6"]+out[1,"CLYPPT.M.sl7"]))
                
              }
            }
            
            
            if ((sandagg >= 52) | (clayagg > 27)) {
              texturesuit = append(texturesuit, 25)
            } else {
              texturesuit = append(texturesuit, 100)
            }
          }
          
          
        }
        #}
        
        txtursuit <- as.numeric(texturesuit)
        txtursuits <- append(txtursuits, txtursuit)
        #names(texturesuit) <- c(sprintf("texturesuit",seq(1:k)))
        
        #totalsoilsuit= append (totalsoilsuit,(as.numeric(phsuit) * as.numeric(depthsuit) * as.numeric(txtursuits))/10000)
        averagesoilsuit=append(averagesoilsuit, sum(0.6*as.numeric(phsuit) , 0.2*as.numeric(depthsuit) , 0.2*as.numeric(txtursuit)))
        
        
      #}
      
      ########################################################
      # calculate total  suitability 
      ########################################################
  
      clim_suit_norain <- cbind(CROP_ID = ecology[k,2],TCnorains[k,], 
                                AvgMonths = mean(as.numeric(TCnorains[k,])))
      
      avgTotSoilSuit=cbind.data.frame( cropid = ecology$id[k] , averagesoilsuit=averagesoilsuit[k], name = ecology$name[k])
      
      # to enable the print of cropid as numeric
      options(scipen = 50)
      climSoil <- as.data.frame(cbind(clim_suit_norain,avgTotSoilSuit))
      climSoil$avgclimsoil <- mean(c(climSoil$AvgMonths, climSoil$averagesoilsuit))
  
      # remove cropid and keep CROP_ID
      climSoil_ID <- subset(climSoil, select = -c(cropid) )
      # now aggragating the resuls for all the crops
      climSoil_ID$lon = coords[1,1]
      climSoil_ID$lat = coords[1,2]
      climSoil_ID_crops = rbind(climSoil_ID_crops, climSoil_ID)
      }
  
  }
  
    # now aggregating results for all points
    
    climsoil_ID_crops_gridpoints[[i]] <- climSoil_ID_crops
    
    #print(paste0("i = ", i, ", ncol(climsoil_ID_crops_gridpoints_topcrop) = ",ncol(climsoil_ID_crops_gridpoints_topcrop)))
    climsoil_ID_crops_gridpoints_topcrop = rbind(climsoil_ID_crops_gridpoints_topcrop,
                    climSoil_ID_crops[which(climSoil_ID_crops$avgclimsoil == max(climSoil_ID_crops$avgclimsoil,na.rm = T))[1],])
    
  #  print(paste("i =", i, ",k =", k, "lat = ", climsoil_ID_crops_gridpoints_topcrop$lat[i],
  #              "lon = ", climsoil_ID_crops_gridpoints_topcrop$lon[i]))
  }
  
  saveRDS(climsoil_ID_crops_gridpoints, "climsoil_ID_crops_gridpoints")
  write.csv(climsoil_ID_crops_gridpoints_topcrop, "climsoil_ID_crops_gridpoints_topcrop.csv")
  
    

  # topdata = climsoil_ID_crops_gridpoints_topcrop[complete.cases(climsoil_ID_crops_gridpoints_topcrop),]
  # topdata$name = as.factor(topdata$name)
  # library(sp)
  # print(topdata)
  # coordinates(topdata) <- ~ lon + lat
  # datasf = st_as_sf(topdata, coords = c("lon", "lat"), crs = 4326)
  # 
  # library(ggplot2)
  # ggplot() + geom_sf(data = datasf, aes(col = name, size = avgclimsoil))
  # ggsave("preliminarymap.png")

}

f(ngrid = 3390, ncrop = 1842)
#f(ngrid = 15, ncrop = 10)
