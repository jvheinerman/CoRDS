RunALL <- function(){
  
    #function(dataset,k,s,r_cluster,R, pr_max, pr_min, merge, r_merge, pr_max_merge, noise){
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
    dataset = 1
    k = c(20,40)
    s = c(60,80)
    r_cluster = c(4,8)
    R = c(40,60)
    pr_max = c(0.4,0.5)
    pr_min = c(0.03,0.05)
    merge = c(0,1)
    r_merge = c(4,8)
    pr_max_merge = c(0.4,0.5)
    noise = 0
    
    print("start")
    
    for (K in k){
      for(S in s){
        for(r in r_cluster){
          for(RR in R){
            for(pr in pr_max){
              for(prm in pr_min){
                for(m in merge){
                  if(m ==1){
                    
                    for(rm in r_merge){
                      for(prmm in pr_max_merge){
                        print("new")
                        print(paste("settings",dataset,",",K,",",S,",",r,",",RR,",",pr,",",prm,",",m,",",rm,",",prmm,",",noise, sep=""))
                        ASR_etest(dataset,K,S,r,RR,pr,prm,m,rm,prmm,noise)
                      }
                    }
                    
                  } else {
                    print("new")
                    print(paste("settings",dataset,",",K,",",S,",",r,",",RR,",",pr,",",prm,",",m,",nvt,nvt,",noise, sep=""))
                    ASR_etest(dataset,K,S,r,RR,pr,prm,m,0,0,noise)
                  }
                  
                }
              }
            }
          }
        }
      }
    }
    
  
  
}
  