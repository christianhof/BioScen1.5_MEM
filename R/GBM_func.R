GBM_eco <- function(data.model, outDir, plotPath, species, eval=TRUE,
                    tree.comp = c(1:3), learn.rate = c(0.01,0.001),
                    PA, blocks, clim.var, distribution="bernoulli"){    
  
  predNames <- sapply(clim.var,function(x,...) colnames(data.model)[which(colnames(data.model)==x)])## Get predictor names
  formula <- as.formula(paste("presence ~ ",paste(predNames, collapse=" + "),sep=''))  
  
  mods <- lapply(tree.comp, function(tc){    ## Loop through tree complexities
    mods <- lapply(learn.rate, function(lr, tc){ ## Loop through learning rates
      print(paste('tree complexity=', tc, 'learning rate =', lr))
      
      mods <- lapply(blocks, function(blockNo){
        
        data.model <- na.omit(data.model)
        dat.blockNo <- data.model[data.model$block!=blockNo,]
        dat.blockNo <- rbind(dat.blockNo, data.model[data.model$block==blockNo,]) ## Put the data from the test block at the end
        
        tfrac <- sum(dat.blockNo$block!=blockNo)/nrow(dat.blockNo)  ## Work out what proportion of the data that are used for training (train.fraction)
        dat.blockNo <- dat.blockNo[order(dat.blockNo$block==blockNo),] ## Put the test block at the end so that it is used as the test data set by the gbm function.
        
        model1 <- gbm(formula,distribution=distribution,
                      data=dat.blockNo,
                      n.trees=10500,
                      n.minobsinnode = 10,
                      interaction.depth=tc,
                      shrinkage=lr,
                      bag.fraction=0.5,
                      verbose=F,
                      keep.data=F,
                      train.fraction=tfrac)
        
        cv.error <-model1$valid.err
        return(list(model=blockNo, cv.error=cv.error))})
      
      ## Calculate the cross-validation error across all the 4 models
      cv.error <- Reduce('+', lapply(mods, function(x) x$cv.error))/nrow(data.model)
      return(list(mods=mods, cv.error=cv.error))}, tc=tc)
    
    names(mods) <- learn.rate
    return(mods)
  })
  
  ## Now work out which model performs best and choose those settings for the 'best model'
  perf.table <- expand.grid(lr=learn.rate, tc=tree.comp)
  perf.table$nt <- do.call('c', (lapply(mods, function(model)
    unlist(lapply(model, function(model) which(model$cv.error==min(model$cv.error)))))))
  perf.table$cv.err <- do.call('c', (lapply(mods, function(model)
    unlist(lapply(model, function(model) (min(model$cv.error)))))))
  write.table(perf.table,file=paste0(plotPath, "/", PA,"_GBM_performance.txt",sep=""))
  perf.table <- perf.table[perf.table$nt > 1000 & perf.table$nt <= 10000,]  ## Remove any models that are based on fewer than 1000 trees as they aren't supposed to be good
  
  if(nrow(perf.table)==0) return(NULL)
  
  else{
    
    perf.table <- perf.table[order(perf.table$cv.err, perf.table$nt),]
    cat('\n', 'finalising BYBLOCK model for species ', species, '\n')
    
    ## Starting final models
    best.mod <- lapply(blocks, function(blockNo, lr, tc, nt, species){
      
      data.model <- na.omit(data.model)
      tfrac <- sum(data.model$block!=blockNo)/nrow(data.model)  ## Work out what proportion of the data is in the training data
      dat.blockNo <- data.model[order(data.model$block==blockNo),]
      mods.blockNo <- gbm(formula, distribution=distribution, 
                          data=dat.blockNo, n.trees=nt,bag.fraction=0.5, 
                          verbose=F, shrinkage=lr, interaction.depth=tc, 
                          keep.data=F, train.fraction=tfrac)
      cv.error <- mods.blockNo$valid.err[nt]
      
      test.dat <- subset(data.model, block==blockNo)
      
      if(eval==TRUE){
        if(sum(test.dat$pres)<1) AUC <- NA
        
        else {
          pred <- stats::predict(mods.blockNo, newdata=test.dat, n.trees=nt, type='response')
          
          ## Define test dataframe for evaluation
          test_df <- cbind(test.dat[3], pred) ## Full dataframe
          
          pres <- test_df[test_df[,1]==1, 2] ## Presence dataframe
          abs <- test_df[test_df[,1]==0, 2] ## Absence dataframe
          
          ## Evaluate model prediction  
          mod.eval <- evaluate(p=pres, a=abs)
          
          AUC <- mod.eval@auc ## Extract AUC value
          TSS <- max(mod.eval@TPR + mod.eval@TNR) - 1 ## Define TSSmax value
          
          ## Save response curve plots
          jpeg(paste0(plotPath, "/", PA,"_",blockNo,"_partial_dependence", ".jpg", sep=""), height=2000, width=2000, res=300)
          list <- c(1:4)
          par(mfrow=c(2,2), tcl=-0.5, family="serif")
          lapply(list,function(x){plot.gbm(mods.blockNo,i.var=x,n.trees=mods.blockNo$n.trees,xlab = mods.blockNo$var.names[x])})
          dev.off()
          
          ## Save relative variable importance
          jpeg(paste0(plotPath, "/", PA,"_",blockNo,"_relative_influence", ".jpg", sep=""), height=2000, width=2000, res=300)
          par(mfrow=c(1,1), tcl=-0.5, family="serif")
          summary(mods.blockNo)
          dev.off()
          write.table(summary(mods.blockNo),file=paste0(plotPath, "/", PA,"_",blockNo,"_relative_influence", ".txt"))
          
        }
        return(list(species=species, block=blockNo, AUC=AUC, TSS=TSS, mod=mods.blockNo, cv.error=cv.error,learn.rate=lr,ntrees=nt,tree.complexity=tc))
      } else{
        return(list(species=species, block=blockNo, mod=mods.blockNo, cv.error=cv.error, learn.rate=lr, ntrees=nt, tree.complexity=tc))
      }
    }, lr=perf.table$lr[1], tc=perf.table$tc[1], nt=perf.table$nt[1], species=species)
    
    ## Save models
    return(best.mod)
    #save(best.mod, file=paste(outDir, "/", PA,"_", paste(clim.var,collapse="_"), 
    #                          "_model_output_GBM_Eco_block.RData",sep=""), compress="xz")  
    
    #cat('\n'); print(paste('finished  processing', species, sep=" "))
  }
}

GBM_split <- function(data.model, outDir, plotPath, species, PA, 
                      learn.rate = c(0.01,0.001), tree.comp = c(1:3), 
                      distribution="bernoulli", clim.var, eval=TRUE){    
  
  predNames <- sapply(clim.var,function(x,...) colnames(data.model)[which(colnames(data.model)==x)])## Get predictor names
  formula <- as.formula(paste("presence ~ ",paste(predNames, collapse=" + "),sep=''))  
  

  
  mods <- lapply(tree.comp, function(tc){    ## Loop through tree complexities
    mods <- lapply(learn.rate, function(lr, tc){ ## Loop through learning rates
      print(paste('tree complexity=', tc, 'learning rate =', lr))
      mods <- lapply(1:10, function(blockNo){
        smp_size <- floor(0.33 * nrow(data.model)) ## 30/70 split
        train_ind <- sample(seq_len(nrow(data.model)), size = smp_size)
        
        fit.blocks <- data.model[-train_ind, ]
        test.block <- data.model[train_ind, ]
        
        fit.blocks$block <- 1
        test.block$block <- 2
        
        dat.blockNo <- rbind(fit.blocks,test.block)
        
        model1 <- gbm(formula,distribution=distribution,data=dat.blockNo,n.trees=10500, 
                      n.minobsinnode = 10, interaction.depth=tc,shrinkage=lr,
                      bag.fraction=0.5, verbose=F, keep.data=F, train.fraction=0.7)
        cv.error <-model1$valid.err
        return(list(model=blockNo, cv.error=cv.error))})
      
      ## Calculate the cross-validation error across all the models
      cv.error <- Reduce('+', lapply(mods, function(x) x$cv.error))/nrow(data.model)
      return(list(mods=mods, cv.error=cv.error))}, tc=tc)
    
    names(mods) <- learn.rate
    return(mods)
  })
  
  ## Now work out which model performs best and choose those settings for the 'best model'
  perf.table <- expand.grid(lr=learn.rate, tc=tree.comp)
  perf.table$nt <- do.call('c', (lapply(mods, function(model)
    unlist(lapply(model, function(model) which(model$cv.error==min(model$cv.error)))))))
  perf.table$cv.err <- do.call('c', (lapply(mods, function(model)
    unlist(lapply(model, function(model) (min(model$cv.error)))))))
  write.table(perf.table,file=paste0(plotPath, "/", PA, "_GBM_performance_30_70.txt",sep=""))
  perf.table <- perf.table[perf.table$nt>1000 & perf.table$nt <= 10000,]  ## Remove any models that are based on fewer than 1000 trees as they aren't supposed to be good
  
  if(nrow(perf.table)==0){
    
    return(NULL)
    
  }else{
    
    perf.table <- perf.table[order(perf.table$cv.err, perf.table$nt),]
    cat('\n', 'finalising BYBLOCK model for species ', species, '\n')
    
    ## Starting final models
    best.mod <- lapply(1:10, function(blockNo, lr, tc, nt, species){
      
      smp_size <- floor(0.33 * nrow(data.model))
      train_ind <- sample(seq_len(nrow(data.model)), size = smp_size)
      
      fit.blocks <- data.model[-train_ind, ]
      test.block <- data.model[train_ind, ]
      
      fit.blocks$block <- 1
      test.block$block <- 2
      
      dat.blockNo <- rbind(fit.blocks,test.block)
      
      mods.blockNo <- gbm(formula, distribution=distribution, data=dat.blockNo,
                          n.trees=nt,bag.fraction=0.5, verbose=F, shrinkage=lr,
                          interaction.depth=tc, keep.data=F, train.fraction=0.7)
      cv.error <- mods.blockNo$valid.err[nt]
      
      if(eval==TRUE){
        if(sum(test.block$pres)<1){AUC <- NA; TSS <- NA} else {
          
          pred <- stats::predict(mods.blockNo, newdata=test.block, n.trees=nt, type='response')
          
          ## Define test dataframe for evaluation
          test_df <- cbind(test.block[3], pred) 
          
          pres <- test_df[test_df[,1]==1, 2] ## Presence dataframe
          abs <- test_df[test_df[,1]==0, 2] ## Absence dataframe
          
          ## Evaluate model prediction  
          mod.eval <- evaluate(p=pres, a=abs)
          
          AUC <- mod.eval@auc ## Extract AUC value
          TSS <- max(mod.eval@TPR + mod.eval@TNR) - 1 ## Define TSSmax value
          
          ## Save response curve plots
          jpeg(paste0(plotPath, "/", PA,"_",blockNo,"_partial_dependence_30_70", ".jpg", sep=""), height=2000, width=2000, res=300)
          list <- c(1:4)
          par(mfrow=c(2,2), tcl=-0.5, family="serif")
          lapply(list,function(x){plot.gbm(mods.blockNo,i.var=x,n.trees=mods.blockNo$n.trees,xlab = mods.blockNo$var.names[x])})
          dev.off()
          
          ## Save relative variable importance
          jpeg(paste0(plotPath, "/", PA,"_",blockNo,"_relative_influence_30_70", ".jpg", sep=""), height=2000, width=2000, res=300)
          par(mfrow=c(1,1), tcl=-0.5, family="serif")
          summary(mods.blockNo)
          dev.off()
          write.table(summary(mods.blockNo),file=paste0(plotPath,"/", PA,"_",blockNo,"_relative_influence_30_70", ".txt"))
        }
        return(list(species=species, block=blockNo, AUC=AUC, TSS=TSS, mod=mods.blockNo, cv.error=cv.error,learn.rate=lr,ntrees=nt,tree.complexity=tc))
      } else{
        return(list(species=species, block=blockNo, mod=mods.blockNo, cv.error=cv.error,learn.rate=lr,ntrees=nt,tree.complexity=tc))
      }
    }, lr=perf.table$lr[1], tc=perf.table$tc[1], nt=perf.table$nt[1], species=species)
    
    ## Save models
    climVarName <- paste(clim.var,collapse="_")
    return(best.mod)
    #save(best.mod, file=paste(outDir, "/", PA,"_",climVarName,"_model_output_GBM_30_70.RData",sep=""), compress="xz")  
    
    #cat('\n'); print(paste('finished  processing', species, sep=" "))
  }
}