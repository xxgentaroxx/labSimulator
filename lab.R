# input
# studentnum: 学生の数
# labnum: 研究室の数
# initseat: 研究室ごとの募集人数
# loopnum: ループ回数

# output
# choicetable: 希望する研究室への配属可能性
# labtable: 各ループにおいて配属された研究室
# seattable: 各研究室が何巡目で埋まるか

studentnum <- 160
labnum <- 20
initseat <- rep(8,20)
loopnum <- 100

random <- rnorm(studentnum, mean=2.5, sd=0.56)
random[random>4] <- 4
random[random<0] <- 0
random <- random[rev(order(random))]
# plot(random)
hist(random)
lab <- seq(15,1,length=labnum)
lablow <- seq(2,1,length=labnum)

stdtable <- data.frame(gpa=random,choice1=NA,choice2=NA,choice3=NA,lab=NA)
labtable <- matrix(0,studentnum,labnum)
labtable <- cbind(stdtable[1],labtable)
choicetable <- data.frame(gpa=stdtable$gpa,first=0,second=0,third=0,none=0)
seattable <- data.frame(lab=1:labnum,first=0,second=0,third=0,none=0)

for(loop in 1:loopnum){
  for(i in 1:studentnum){
    choice <- sample(1:labnum,3,prob=(lab*(studentnum-i)+lablow*i)/studentnum)
    ifelse(rnorm(1)>0, choice <- sort(choice), 
           choice <- c(choice[1],sort(choice[2:3])))
    stdtable[i,-c(1,5)] <- choice
    stdtable$lab <- NA
  }
  
  seat <- initseat
  
  for(i in 1:studentnum){
    if(seat[stdtable$choice1[i]]>0){
      stdtable$lab[i] <- stdtable$choice1[i]
      if(seat[stdtable$choice1[i]]==1){
        seattable$first[stdtable$choice1[i]] <- seattable$first[stdtable$choice1[i]] + 1
      }
      seat[stdtable$choice1[i]] <- seat[stdtable$choice1[i]] - 1
    }
  }
  for(i in 1:studentnum){
    if(is.na(stdtable$lab[i])){
      if(seat[stdtable$choice2[i]]>0){
        stdtable$lab[i] <- stdtable$choice2[i]
        if(seat[stdtable$choice2[i]]==1){
          seattable$second[stdtable$choice2[i]] <- seattable$second[stdtable$choice2[i]] + 1
        }
        seat[stdtable$choice2[i]] <- seat[stdtable$choice2[i]] - 1
      }    
    }
  }
  for(i in 1:studentnum){
    if(is.na(stdtable$lab[i])){
      if(seat[stdtable$choice3[i]]>0){
        stdtable$lab[i] <- stdtable$choice3[i]
        if(seat[stdtable$choice3[i]]==1){
          seattable$third[stdtable$choice3[i]] <- seattable$third[stdtable$choice3[i]] + 1
        }
        seat[stdtable$choice3[i]] <- seat[stdtable$choice3[i]] - 1
      }    
    }
  }
  
  for(i in 1:studentnum){
    if(!is.na(stdtable$lab[i])){
      if(stdtable$lab[i]==stdtable$choice1[i]){
        choicetable$first[i] <- choicetable$first[i] + 1
      }else if(stdtable$lab[i]==stdtable$choice2[i]){
        choicetable$second[i] <- choicetable$second[i] + 1
      }else if(stdtable$lab[i]==stdtable$choice3[i]){
        choicetable$third[i] <- choicetable$third[i] + 1
      }
      labtable[i,stdtable$lab[i]+1] <- labtable[i,stdtable$lab[i]+1] + 1
    }else{
      choicetable$none[i] <- choicetable$none[i] + 1
    }
  }
}
seattable$none <- loopnum-(seattable$first+seattable$second+seattable$third)
