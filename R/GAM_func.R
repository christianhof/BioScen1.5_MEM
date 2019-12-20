GAM_eco <- function(data.model, outDir, species, PA, clim.var, family="binomial", fx, k, bs, blocks){
  
  #Get predictor names
  predNames <- sapply(clim.var,function(x,...) colnames(data.model)[which(colnames(data.model)==x)]) 
  formula <- as.formula(paste("presence ~ ",paste(sapply(predNames,function(x,...) paste("s(",x,", fx=",fx,", k=",k,", bs= '",bs,"')",sep="")),collapse="+",sep=" "))) #This is the model formula                    
  
  ## Read in the blocks and set one as predictions block and the others as model fit blocks each round
  final.mods <- lapply(blocks,function(blockNo){
    cat('\n', "Fitting final GAM model to block",blockNo,'\n',sep=" ")
    fit.blocks <- subset(data.model, block != blockNo) #Fit block: Subset by the block number - build on nine blocks and predict and test using the leftout tenth block
    test.block <- subset(data.model, block == blockNo) #Test block
    
    possibleError <- tryCatch(model1 <- gam(formula,family=family, data=fit.blocks, gamma=1.4), error=function(e) e) #The GAM model
    if(!inherits(possibleError, "error")){
      
      ## Predict the fitted model to the predictions block
      PRED <- as.data.frame(predict(model1,newdata=test.block,type="response",se.fit=FALSE)) #Predict to the left out block
      colnames(PRED) <- paste("pred",blockNo,sep=".")
      eval.data.auc <- cbind(test.block[,c("cell.id","presence")],PRED)
      
      ## Threshold independent - Set AUC
      AUC <- round(auc(eval.data.auc,st.dev=FALSE),4)
      return(list(species=species,Block=blockNo,AUC=AUC,mod=model1,clim.var=clim.var))
    }
  })
  # Save final model output
  return(final.mods)
  #save(final.mods, file=paste(outDir, "/", 
  #                            PA,"_", paste(clim.var,collapse="_"),
  #                            "_model_output_GAM.RData",sep=""), compress="xz") 
}

GAM_split <- function(data.model, outDir, species, PA, clim.var, family="binomial", 
                      fx, k, bs,blocks){
  
  # Get predictor names
  predNames <- sapply(clim.var,function(x,...) colnames(data.model)[which(colnames(data.model)==x)])
  formula <- as.formula(paste("presence ~ ",paste(sapply(predNames,function(x,...) paste("s(",x,", fx=",fx,", k=",k,", bs= '",bs,"')",sep="")),collapse="+",sep=" ")))                    
  print(formula)
  
  ## Repeat modelling ten times on a 30/70 split
  r <- 1:10
  
  final.mods <- lapply(r,function(x){
    
    smp_size <- floor(0.33 * nrow(data.model))
    train_ind <- sample(seq_len(nrow(data.model)), size = smp_size)
    
    fit.blocks <- data.model[-train_ind, ]
    test.block <- data.model[train_ind, ]
    
    possibleError <- tryCatch(model1 <- gam(formula, family=family, 
                                            data=fit.blocks, gamma=1.4), 
                              error=function(e) e)
    if(!inherits(possibleError, "error")){
      
      blockNo <- x
      
      ## Predict the fitted model to the predictions block
      PRED <- as.data.frame(predict(model1,newdata=test.block,type="response",se.fit=FALSE))
      colnames(PRED) <- paste("pred",blockNo,sep=".")
      eval.data.auc <- cbind(test.block[,c("cell.id","presence")],PRED)
      
      ## Threshold independent - Set AUC
      AUC <- round(auc(eval.data.auc,st.dev=FALSE),4)
      return(list(species=species,Block=blockNo,AUC=AUC,mod=model1,clim.var=clim.var))
    }
  })
  
  # This saves final model output for each of the 10 absence selection samples (Each R script then contains ten versions with a different block left out each time)
  climVarName <- paste(clim.var,collapse="_")
  return(final.mods)
  #save(final.mods, file=paste(outDir, "/",PA,"_",climVarName,
  #                            "_model_output_GAM_30_70.RData",sep=""),compress="xz")  ### modify to say where you want to save
}