ASR_etest <- function(dataset,k,s,r_cluster,R, pr_max, pr_min, merge, r_merge, pr_max_merge, noise, db3){
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

repetitions = 10

results = matrix(NA,repetitions,4)
  
for (reps in 1:repetitions){  
  
archiveclusterno = matrix(NA,k,2) # keep track of active cluster per datapoint
number_merge = 0

# get the data from the database

if(dataset ==1){ #toyproblem
  database = read.table(paste(paste("~/Documents/Projecten/AutomaticSR/Rprogramming/Rscripts/results/datatoy/toyproblem_database_",reps,sep="") ,".txt", sep=""))
  maxgens = 2000
  nbr_sens = 2
  database[,"V1"] = as.double(database[,"V1"]) #+ rnorm(length(database[,"V1"]),0,0.1)
  database[,"V2"] = as.double(database[,"V2"]) #+ rnorm(length(database[,"V2"]),0,0.1)
  database = cbind(database[,"V1"],database[,"V2"])
}

if (dataset ==2){
  database = read.csv("~/Documents/Projecten/AutomaticSR/Rprogramming/Rscripts/PS_data_clean.csv", encoding="UTF-16", sep=";", stringsAsFactors=FALSE)
  maxgens = 7000
  nbr_sens = 4
  
  #print("plot")
  #help = database[1:2000,2]
  #plot(database[1:2000,1], database[1:2000,2], type = "l",lwd=1, col="lightblue", xlab="time step", ylab="value",col.axis="darkgrey",col.main="darkgrey", col.sub="darkgrey", col.lab="black", xlim=c(1, 2000), ylim=c(-2.5, 2), frame=F)
  #lines(database[1:2000,1], database[1:2000,3], lwd=1, pch = 16, cex = .5,col="blue")
  #lines(database[1:2000,1], database[1:2000,5], lwd=1, pch = 16, cex = .5,col="darkblue")
  #lines(database[1:2000,1], database[1:2000,6], lwd=1,  pch = 16, cex = .5,col="grey")
  #plot(1:maxgens,database[1:maxgens,"ps1"],pch = 16, cex = .9)
  
  database[,"ps1"] = as.double(database[,"ps1"]) + rnorm(length(database[,"ps1"]),0,noise)
  database[,"ps2"] = as.double(database[,"ps2"]) + rnorm(length(database[,"ps2"]),0,noise)
  database[,"ps4"] = as.double(database[,"ps4"]) + rnorm(length(database[,"ps4"]),0,noise)
  database[,"ps5"] = as.double(database[,"ps4"]) + rnorm(length(database[,"ps5"]),0,noise)
  
  #plot(database[1:2000,1], database[1:2000,2], type = "l",lwd=1, col="lightblue", xlab="time step", ylab="value",col.axis="darkgrey",col.main="darkgrey", col.sub="darkgrey", col.lab="black", xlim=c(1, 2000), ylim=c(-2.5, 2), frame=F)
  #lines(database[1:2000,1], help, pch = 16, cex = .3,col="black", xlab="time step", ylab="value",col.axis="darkgrey",col.main="darkgrey", col.sub="darkgrey", col.lab="black", frame=F)
  #legend(1400,2, c("w/o noise","noise"), lty=c(1,1),lwd=c(2.5,2.5),col=c("black","lightblue"), bty = "n")
  
  #why add noise? > test can't handle static data but can detect a shift of 1
  # look at database 3070-3109, 3110-3149, 3150-3189. 1 and two are different and 2 and 3 the same
  # if data different (static > 0.25, with noise >0.25)
  # if data same (static > 0.03, with noise also high values)
  
  database = cbind(database[,"ps1"],database[,"ps2"],database[,"ps4"], database[,"ps5"])
}

if (dataset == 3){
  if(db3==5){
    database = read.table("~/Documents/Projecten/AutomaticSR/Rprogramming/database/Thymio/camera/logs/log_all_data_5perc.txt", header = TRUE, encoding="UTF-16", stringsAsFactors=FALSE)
  } else {
    database = read.table("~/Documents/Projecten/AutomaticSR/Rprogramming/database/Thymio/camera/logs/log_all_data.txt", header = TRUE, encoding="UTF-16", stringsAsFactors=FALSE)
  }
  database = database[1:31,]
  #database = rbind(database[1:31,],database[97:117,])
  dup_factor = 30
  maxgens = dim(database)[1]*dup_factor
  nbr_sens = dim(database)[2] -1
  
  database = matrix( rep( t( database ) , dup_factor ) , ncol =  dim(database)[2] , byrow = TRUE )
  database = database[order(database[,1]),]
  #print(database[1:5,])
  #print("max dataset")
  #print(max(database))
  
  database[,2:dim(database)[2]] = (database[,2:dim(database)[2]])/max(database[,2:dim(database)[2]])
 
  #print("max dataset")
  #print(max(database[,2:dim(database)[2]]))
  
  #0.15 noise is limit to see difference with eyes. at 0.2 good identical images are clustered
  database = database + matrix(rnorm(dim(database)[1]*dim(database)[2],0,noise),dim(database)[1])
  database = (database[,2:dim(database)[2]])
  database = matrix( rep( t( database ) , 2 ) , ncol =  dim(database)[2] , byrow = TRUE )
  #print("max dataset")
  #print(nrow(database))
  #print(max(database[,2:dim(database)[2]]))
  
  #print(database[1:2,])
  

  #print(eqdist.etest(database[271:330,], c(30,30), distance = FALSE,"original", R)$p.value)
  
}


clusters = 1 # minimal of 1 clsuter
cluster_archive = array(NA,dim=c(s,nbr_sens,1)) # dataset per cluster with maximum size of s

# check used database
#print(database[1:1,1:1])

counts = matrix(0,1,nbr_sens+1) # keep track of number points total in cluster and means per sensor dimension

# add first k points to the first cluster
for(i in 1:k) {
  counts[1,1] = counts[1,1] + 1
  cluster_archive[i,,1] = database[i,]
  archiveclusterno[i,] = c(i, clusters)
  
  if(i>1){
    counts[1,2:(nbr_sens+1)] = colMeans(cluster_archive[1:i,,1])
  }
}

# start the clustering
TIME_BEGIN = proc.time()
for(g in 1:(maxgens-k)){
  if(g==1 || (g>2 && is.na(history[1,1]))){
    # create new history vector if neccesary (first time and when history is deleted)
    history = matrix(c(database[g+k,]),1,nbr_sens)
  } else {
    # add point to history vector
    history = rbind(history[,], c(database[g+k,]))
  }

  # only test when history is full again, size k
  if(nrow(history) >= k){
    
    pcbestv = 0 # probability best fitting cluster
    pcbesti = 0 # best fitting cluster
    
    for(c in 1:clusters){
      # calculate probability that history follows same distribution as current clusters (low value > unlikely)
      # use eqdist.etest $p.value 
      
      
      pc = 0
      for(trial in 1:r_cluster){ # do trials and take maximum because of stochasticity in test to reduce the probability of making new clusters
        
        cnt = min(counts[c,1],s)

        #test distribution by adding history by cluster and check whether they are from same distribution
 
        #print(cluster_archive[1:cnt,,])
        pc_trial = eqdist.etest(rbind(history[,], cluster_archive[1:cnt,,c]), c(k,cnt), distance = FALSE,"original", R)$p.value
        
        #Explain this test in 1 dimension
        #follwing values 0.025 - 0.15, increasing R to 50 > not above 0.01
        #eqdist.etest(rbind(matrix(rnorm(20,1,1)),matrix(rnorm(20,0,1))) ,c(20,20),distance = FALSE,"original", R = 39)
        # changing std dev id way more volatile. below up till 0.8
        #eqdist.etest(rbind(matrix(rnorm(20,0,2)),matrix(rnorm(20,0,1))) ,c(20,20),distance = FALSE,"original", R = 50)
        # is better when dataset increased and R too
        # same distribution gives values > 0.2
        #eqdist.etest(rbind(matrix(rnorm(40,0,1)),matrix(rnorm(40,0,1))) ,c(40,40),distance = FALSE,"original", R = 60)
        
        #Explain this test in 2 dimensions
        # same distribution gives volatile values
        #eqdist.etest(rbind(cbind(matrix(rnorm(40,0,1)),matrix(rnorm(40,0,1))),cbind(matrix(rnorm(40,0,1)),matrix(rnorm(40,0,1)))) ,c(40,40),distance = FALSE,"original", R = 40)
        #not when changing the stdev or mean. 
        #eqdist.etest(rbind(cbind(matrix(rnorm(40,0,2)),matrix(rnorm(40,0,2))),cbind(matrix(rnorm(40,0,1)),matrix(rnorm(40,0,1)))) ,c(40,40),distance = FALSE,"original", R = 40)
        #eqdist.etest(rbind(cbind(matrix(rnorm(40,1,1)),matrix(rnorm(40,1,1))),cbind(matrix(rnorm(40,0,1)),matrix(rnorm(40,0,1)))) ,c(40,40),distance = FALSE,"original", R = 40)
        
        if(pc_trial>pc){
          pc = pc_trial
        } 
      }
      
      if(pc > pcbestv){
        pcbestv = pc
        pcbesti = c
      }
      
    }
    
    if(pcbestv > pr_max){ # pr is border, certainty that it belongs to this cluster
    # add first point of history to the cluster
    counts[pcbesti,1] = counts[pcbesti,1] + 1
    if(counts[pcbesti,1] <= s){
      cluster_archive[counts[pcbesti,1],,pcbesti] =  c(history[1,])
    } else{
      cluster_archive[,,pcbesti] = rbind(cluster_archive[2:s,,pcbesti], c(history[1,]))
    }
    
    counts[pcbesti,2:(nbr_sens+1)] = colMeans(cluster_archive[1:min(s,counts[pcbesti,1]),,pcbesti])
    
    history = history[2:nrow(history),]
    archiveclusterno = rbind(archiveclusterno[,],c(g+1,pcbesti))
    
    } else {
    
    if(pcbestv < pr_min) {
      #print("new cluster at generation")
      #print(g)
      
      plot(archiveclusterno, pch = 16, cex = .5,col="black", xlab="time step", ylab="cluster",col.axis="darkgrey",col.main="darkgrey", col.sub="darkgrey", col.lab="black",xlim=c(1, maxgens), frame=F, axes=FALSE)
      axis(1, col="darkgrey")
      axis(2, at = seq(1, clusters, by = 1), las = 1, col = "darkgrey")
      
      
      clusters = clusters + 1;
      tmp = rbind(history[,], matrix(NA, s-k,nbr_sens))

      cluster_archive = abind(cluster_archive, tmp)
      counts = rbind(counts, cbind(matrix(nrow(history),1,1), matrix(0,1,nbr_sens)))
  
      history = matrix(NA,1,nbr_sens)
      rows = nrow(archiveclusterno)
      archiveclusterno = rbind(archiveclusterno[,], matrix(cbind((rows+1):(rows+k),clusters),k,2))
      
      counts[clusters,2:(nbr_sens+1)] = colMeans(cluster_archive[1:min(s,counts[clusters,1]),,clusters])

      #print("new clusters")
      #print(counts[,1:min(5,nbr_sens)])
      #plot(archiveclusterno, pch = 16, cex = .9)
      
      
      
    } else { # do nothing with point, stay in same cluster and add point to cluster archive
      
      
      rows = nrow(archiveclusterno)
      counts[archiveclusterno[rows,2],1] = counts[archiveclusterno[rows,2],1] + 1
      
      if(counts[archiveclusterno[rows,2],1] <= s){
        cluster_archive[counts[archiveclusterno[rows,2],1],,archiveclusterno[rows,2]] =  c(history[1,])
      } else{
        cluster_archive[,,archiveclusterno[rows,2]] = rbind(cluster_archive[2:s,,archiveclusterno[rows,2]], c(history[1,]))
      }
      
      history = history[2:nrow(history),]
      archiveclusterno = rbind(archiveclusterno[,],c(g+1, archiveclusterno[rows,2] ))
      counts[archiveclusterno[rows,2],2:(nbr_sens+1)] = colMeans(cluster_archive[1:min(s,counts[archiveclusterno[rows,2],1]),,archiveclusterno[rows,2]])
    }
    
    }
    
  }
  
  #merge clusters
  
  if (merge == 1 & clusters >1 & maxgens%%10 == 0){

  clusters_old = clusters
  for(c in clusters_old:2){
    
    pcbestv = 0 # probability best fitting cluster
    pcbesti = 0 # best fitting cluster
    
    if(counts[c,1] <= s){
      cnt_c = counts[c,1]
    } else {
      cnt_c = s
    }
    
    for(i in 1:max(c-1,1) ){
      
      if(counts[i,1] <= s){
        cnt_i = counts[i,1]
      } else {
        cnt_i = s
      } 
      
      pc = 0
      for(trial in 1:r_merge){ # do trials and take maximum because of stochasticity in test to reduce the probability of making new clusters
        #test distribution by adding history by cluster and check whether they are from same distribution
       
        pc_trial = eqdist.etest(rbind(cluster_archive[1:cnt_c,,c], cluster_archive[1:cnt_i,,i]), c(cnt_c,cnt_i), distance = FALSE,"original", R)$p.value

        if(pc_trial>pc){
          pc = pc_trial
        } 
      }
      
      if(pc > pcbestv){
        pcbestv = pc
        pcbesti = i
        }
      }

    if(pcbestv > pr_max_merge){ # merge cluster c with pcbesti
      

      
      #print("MERGE")
      #print(c)
      #print("with")
      #print(pcbesti)
      number_merge = number_merge +1
      #print(counts[,1:min(5,nbr_sens)])
      # add newest half of every cluster together
      
      tmp = rbind(cluster_archive[1:min(s,counts[pcbesti,1]),,pcbesti],cluster_archive[1:min(s,counts[c,1]),,c])
      cnt_tmp = nrow(tmp)
      
      if(cnt_tmp <= s){
      tmp = rbind(tmp, matrix(NA, s-cnt_tmp,nbr_sens))
      } else{
        tmp = tmp[sample(nrow(tmp),s),]
      }
      #replace cluster by merged cluster
      cluster_archive[,,pcbesti] = tmp
      counts[pcbesti,1] = counts[pcbesti,1] + counts[c,1]
      counts[pcbesti,2:(nbr_sens+1)] = colMeans(cluster_archive[1:min(s,cnt_tmp), ,pcbesti])
      
      # for better analysis of the graph afterwards
      archiveclusterno[,2]= ifelse(archiveclusterno[,2] == c, pcbesti, archiveclusterno[,2])
    
      # remove everything from highest cluster
      # replace all clusters after to previous one
      
      for(j in c:clusters){
        # replace cluster
        if(j==clusters){
          #remove the last cluster
          if(clusters>2){
            cluster_archive = cluster_archive[,,1:(clusters-1)]
          } else {
            cluster_archive = array(cluster_archive[1:min(s,cnt_tmp),,1],dim = c(min(s,cnt_tmp),nbr_sens,1))
          }
          counts = head(counts,-1)
          clusters = clusters-1
        }else{
          cluster_archive[,,j] = cluster_archive[,,j+1]
          counts[j,1] = counts[j+1,1]
          counts[j,2:(nbr_sens+1)] = colMeans(cluster_archive[1:min(s,counts[j,1]),,j])
          archiveclusterno[,2]= ifelse(archiveclusterno[,2] == (j+1), j, archiveclusterno[,2])
        }
        
      }
      #print(counts[,1:min(5,nbr_sens)]) 
    }
      
  }
  
  } # end merge
  
    
}

 TIME = proc.time() - TIME_BEGIN
 print(TIME)
 print(number_merge)

 experiment_name = paste("ASR_etest(",dataset,",",k,",",s,",",r_cluster,",",R,",", pr_max,",", pr_min,",", merge,",", r_merge, ",",pr_max_merge,",", noise,",",db3,")", sep="")
 png(file=paste("~/Documents/Projecten/AutomaticSR/Rprogramming/Rscripts/results/datatoy/",experiment_name,"_clusters_rep",reps,".png",sep=""),width=800,height=700,res=200)
 plot(archiveclusterno, pch = 16, cex = .5,col="black", xlab="time step", ylab="cluster",col.axis="darkgrey",col.main="darkgrey", col.sub="darkgrey", col.lab="black",xlim=c(1, maxgens), frame=F, axes=FALSE)
 axis(1, col="darkgrey")
 axis(2, at = seq(1, clusters, by = 1), las = 1, col = "darkgrey")
 dev.off()
 
 results[reps,] = cbind(reps,clusters,number_merge,TIME)[3,]
 #print(results)
} 

file =paste("~/Documents/Projecten/AutomaticSR/Rprogramming/Rscripts/results/datatoy/",experiment_name,".txt", sep="")
colnames(results) <- c("rep","nbr_clusters", "nbr_merge", "time")
write.table(results, file, sep="\t", row.names = FALSE)

#database = read.table(file, sep="\t", header=TRUE)
  
}











