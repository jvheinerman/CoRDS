RunSpecific <- function(){
  
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
  
  print("camera data")
  
  #ASR_etest(3,20,60,10,60,0.4,0.03,1,5,0.4,1,10)
  #ASR_etest(3,20,60,10,60,0.4,0.03,1,5,0.4,0.4,5)
  
  print("0.4")
  #ASR_etest(3,20,60,10,60,0.4,0.03,1,5,0.4,0.4,10)
  print("0.6")
  #ASR_etest(3,20,60,10,60,0.4,0.03,1,5,0.4,0.6,5)
  #ASR_etest(3,20,60,10,60,0.4,0.03,1,5,0.4,0.6,10)
  print("0.8")
  #ASR_etest(3,20,60,10,60,0.4,0.03,1,5,0.4,0.8,5)
  #ASR_etest(3,20,60,10,60,0.4,0.03,1,5,0.4,0.8,10)
  print("1")
  #ASR_etest(3,20,60,10,60,0.4,0.03,1,5,0.4,0.2,5)
  
  ASR_etest(3,20,60,10,60,0.4,0.03,1,5,0.4,0.4,5)
  ASR_etest(3,20,60,10,60,0.4,0.03,1,5,0.4,0.6,5)
  ASR_etest(3,20,60,10,60,0.4,0.03,1,5,0.4,0.8,5)
  ASR_etest(3,20,60,10,60,0.4,0.03,1,5,0.4,1,5)
  ASR_etest(3,20,60,10,60,0.4,0.03,1,5,0.4,0.2,10)
  
  
  print("start toy")
  
  
  print("improvement")
  #ASR_etest(1,30,40,15,40,0.4,0.03,1,5,0.4,0) #nope
  #ASR_etest(1,30,40,15,40,0.4,0.04,1,5,0.4,0) #nope
  #ASR_etest(1,30,60,15,40,0.4,0.04,1,5,0.4,0) #nope
  #ASR_etest(1,30,60,15,40,0.4,0.03,1,5,0.4,0) #yes
  #ASR_etest(1,20,60,15,40,0.3,0.03,1,5,0.4,0) #yes
  
  print("start toy")
  #ASR_etest(1,20,60,10,60,0.4,0.03,0,0,0,0)
  
  print("reps R")
  #ASR_etest(1,20,60,10,20,0.4,0.03,1,5,0.4,0)
  #ASR_etest(1,20,60,10,40,0.4,0.03,1,5,0.4,0)
  #ASR_etest(1,20,60,10,80,0.4,0.03,1,5,0.4,0)
  
  print("stream size")
  #ASR_etest(1,10,60,10,60,0.4,0.03,1,5,0.4,0)
  #ASR_etest(1,30,60,10,60,0.4,0.03,1,5,0.4,0)
  #ASR_etest(1,40,60,10,60,0.4,0.03,1,5,0.4,0)
  
  print("cluster size")
  #ASR_etest(1,20,40,10,60,0.4,0.03,1,5,0.4,0)
  #ASR_etest(1,20,80,10,60,0.4,0.03,1,5,0.4,0)
  #ASR_etest(1,20,100,10,60,0.4,0.03,1,5,0.4,0)
  
  print("r cluster")
  #ASR_etest(1,20,60,2,60,0.4,0.03,1,5,0.4,0)
  #ASR_etest(1,20,60,5,60,0.4,0.03,1,5,0.4,0)
  #ASR_etest(1,20,60,15,60,0.4,0.03,1,5,0.4,0)
  
  print("p min")
  #ASR_etest(1,20,60,10,60,0.4,0.02,1,5,0.4,0)
  #ASR_etest(1,20,60,10,60,0.4,0.04,1,5,0.4,0)
  #ASR_etest(1,20,60,10,60,0.4,0.05,1,5,0.4,0)
  
  print("p max")
  #ASR_etest(1,20,60,10,60,0.3,0.03,1,5,0.4,0)
  #ASR_etest(1,20,60,10,60,0.2,0.03,1,5,0.4,0)
  #ASR_etest(1,20,60,10,60,0.5,0.03,1,5,0.4,0)
  
  print("start PS")
  print("start from toy best")
  #ASR_etest(2,20,60,10,60,0.4,0.03,0,0,0,0.5)
  
  print("test same hypotheses")
  print("test rcluster")
  #ASR_etest(2,20,60,2,60,0.4,0.03,1,5,0.4,0.5)
  #ASR_etest(2,20,60,5,60,0.4,0.03,1,5,0.4,0.5)
  #ASR_etest(2,20,60,15,60,0.4,0.03,1,5,0.4,0.5)
  
  print("test noise")
  #ASR_etest(2,20,60,10,60,0.4,0.03,1,5,0.4,0.2)
  #ASR_etest(2,20,60,10,60,0.4,0.03,1,5,0.4,0.7)
  #ASR_etest(2,20,60,10,60,0.4,0.03,1,5,0.4,0.9)
  
  print("test pmincluster")
  #ASR_etest(2,20,60,10,60,0.4,0.02,1,5,0.4,0.5)
  #ASR_etest(2,20,60,10,60,0.4,0.04,1,5,0.4,0.5)
  #ASR_etest(2,20,60,10,60,0.4,0.05,1,5,0.4,0.5)
  
  
 
  
}
