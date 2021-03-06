descrip <-
function(x, y, method, Q1, Q3) {
   if (inherits(y,"Surv"))
     y<-factor(y[,2],labels=c('Alive','Dead'))
   n <- tapply(x, y, length)
   n[is.na(n)]<-0
   n.all <- length(x)
   if (method=="param") {
     mm <- tapply(x, y, mean)
     ss <- tapply(x, y, sd)
     mm.all <- mean(x)
     ss.all <- sd(x)
     ans <- cbind(n, mm, ss)
     ans <- rbind(c(n.all, mm.all, ss.all),ans) 
     colnames(ans) <- c("n", "mean", "sd")
     rownames(ans) <- c("[ALL]",levels(y))
   } else {
     med <- tapply(x, y, median)
     q1 <- tapply(x, y, quantile, prob=Q1)
     q3 <- tapply(x, y, quantile, prob=Q3)
     med.all <- median(x)
     q1.all <- quantile(x,prob=Q1)
     q3.all <- quantile(x,prob=Q3)
     ans<-cbind(n, med, q1, q3)
     ans <- rbind(c(n.all,med.all,q1.all,q3.all), ans)
     q1.lab<-paste("P",Q1*100,sep="") 
     q3.lab<-paste("P",Q3*100,sep="") 
     if (Q1==0) 
      q1.lab<-"Min."
     if (Q1==1) 
      q1.lab<-"Max."
     if (Q3==0) 
      q3.lab<-"Min."
     if (Q3==1) 
      q3.lab<-"Max."
     if (Q1==0.25) 
      q1.lab<-"Q1"
     if (Q1==0.75) 
      q1.lab<-"Q3"
     if (Q3==0.25) 
      q3.lab<-"Q1"
     if (Q3==0.75) 
      q3.lab<-"Q3"      
     colnames(ans) <- c("n","med",q1.lab,q3.lab)
     rownames(ans) <- c("[ALL]",levels(y))
   }
   ans <- ifelse(is.na(ans),NaN,ans)
   ans
}

