CreateGraph <- function(dataset,k,s,r_cluster,R, pr_max, pr_min, merge, r_merge, pr_max_merge, noise,db3){
  # expl:
  # dataset = 1 (toyproblem), 2 (ps sensors), 3 (camera)
  # k: S_stream : history data size, usually 20
  # s: S_cluster : size of the data cluster, usually 60
  # r_cluster : numer of repetitions etest, usually 6
  # R: number of repetitons for test, usually 30   
  # pr_max: probability to exceed for adding to that cluster 
  # pr_min: centainty to create new cluster with history data size
  # merge: boolean for merge y/n
  # r_merge: number of reperitions for etest
  # pr_max_merge
  # noise
  
  # give low mid high and run all data
  # how to make graphs?? > read("mid") value and low and high for other ones
  
  print("start")
  #library(ggplot2)
  #library(reshape2)
  
  database = array(NA,dim=c(10,4,4))
  
  file_number = 1
  
  #table = read.table("~/Documents/Projecten/AutomaticSR/Rprogramming/Rscripts/results/datatoy/ASR_etest(1,20,60,10,60,0.4,0.03,0,0,0,0).txt", sep="\t", header=TRUE)
  #database[,,file_number] = as.matrix(table)
  #file_number = file_number+1
  
  for (K in k){
    for(S in s){
      for(r in r_cluster){
        for(RR in R){
          for(pr in pr_max){
            for(prm in pr_min){
              for(m in merge){
                for(rm in r_merge){
                  for(n in noise){
                    for(prmm in pr_max_merge){
                   
                      
                      #file = paste("ASR_etest(",dataset,",",K,",",S,",",r,",",RR,",", pr,",", prm,",",m,",", rm, ",",prmm,",", n,")", sep="")
                      file = paste("ASR_etest(",dataset,",",K,",",S,",",r,",",RR,",", pr,",", prm,",",m,",", rm, ",",prmm,",", n,",",db3,")", sep="")
                      
                      file = paste("~/Documents/Projecten/AutomaticSR/Rprogramming/Rscripts/results/datatoy/",file,".txt",sep="")
                      
                      print(file)
                      print("ok")
                      table = read.table(file, sep="\t", header=TRUE)
                      database[,,file_number] = as.matrix(table)
                      file_number = file_number+1
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  
  
  print(database)
  
png(file="~/Documents/Projecten/AutomaticSR/Rprogramming/Rscripts/results/datatoy/db3noise5.png",width=600,height=1000,res=100)
par(mfrow=c(2,1))
par(mar = c(5,5,1,4))
# automise x labels and number of x labels
boxplot(database[,2,1],database[,2,2],database[,2,3],database[,2,4], col=(c("white")), ylab="# clusters", ylim=c(0, 35),lwd=1,cex=0.3,col.axis="darkgrey",col.main="darkgrey", col.sub="darkgrey", col.lab="darkgrey", frame=F, xaxt='n', boxwex=0.3,cex.lab=2.5, cex.axis=2, cex.main=2, cex.sub=2)
#boxplot(database[,3,1],database[,3,2],database[,3,3],database[,3,4], col=(c("cadetblue3")), ylab="# merges", ylim=c(0, 5),lwd=1,cex=0.3,col.axis="darkgrey",col.main="darkgrey", col.sub="darkgrey", col.lab="darkgrey", frame=F, xaxt='n',boxwex=0.3,cex.lab=2.5, cex.axis=2, cex.main=2, cex.sub=2)
boxplot(database[,4,1],database[,4,2],database[,4,3],database[,4,4], col=(c("darkgrey")), xlab="noise", ylab="runtime (s)", ylim=c(0, 2000),lwd=1,cex=0.3,col.axis="darkgrey",col.main="darkgrey", col.sub="darkgrey", col.lab="darkgrey", frame=F,boxwex=0.3, cex.lab=2.5, cex.axis=2, cex.main=2, cex.sub=2,names=c("0.4","0.6","0.8","1"))
dev.off()
}
