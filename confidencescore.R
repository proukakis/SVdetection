## Confidence Score Script ##

  # import table with ploidy value per cell 
  pl<-read.table("/path/to/ploidies.txt",header = F)
  # import segmented data with copy number ratios from Ginkgo
  data<-read.table("path/to/Segmented_Data.txt",header=T)

# each column in the data represents the values of one cell

    for(i in 4:ncol(data)){
  
  # take values of CN ratios for each cell

      name<-colnames(data)[i]
      seg<-data[,i]
  
  # find ploidy value for that cell  from ploidies txt
  
        for(j in 1:nrow(pl)){
    
    # check if the cell has a ploidy value 
    
          if(pl[j,1]==name){
            ploidy<-pl[j,2]
    
  
    # calculate CN state by multiplying ratios with ploidy for each cell
    
            CNseg<-data[,i]*ploidy
    
            # calculate Confidence Score for the copy number states of that cell
            CS <-1-2*(median(abs(CNseg-round(CNseg)),na.rm = T))
            # save cell name and confidence score
            tableCS<-c(name,CS)
            
            # write output to file
            setwd("path/to/save/output")
            write.table(tableCS,paste(name,"_CS.txt",sep = ""),row.names = F,quote = F)
          }
          else{
              print("sample not found or not in this group")
          }
        }
      }
