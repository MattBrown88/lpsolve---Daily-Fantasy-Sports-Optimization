####Generate optimal daily fantasy baseball line up using lpsolve function####


library(lpSolve)

#Read in dataset
dataset<-read.csv("Rotogrinders MLB Projections 6 17 2016.csv", stringsAsFactors = FALSE)

#Change variables to appropriate types
dataset$position <- as.factor(dataset$position)
dataset$salary <-as.numeric(dataset$salary)


#### Prepare constraint matrix #####
A <- matrix(0, nrow = 8, ncol = nrow(dataset))

#1B
j<-1
i<-1

#Designate the positions that are equivalent to each other when generating the optimal lineup
for (i in 1:nrow(dataset)){
  if (dataset$position[i]=="1B"    || 
      dataset$position[i]=="1B/2B" || 
      dataset$position[i]=="1B/3B" ||
      dataset$position[i]=="1B/OF")
    A[j,i]<-1
}
#2B
j<-2
i<-1
for (i in 1:nrow(dataset)){
  if (dataset$position[i]=="2B"    || 
      dataset$position[i]=="1B/2B" || 
      dataset$position[i]== "2B/OF"||
      dataset$position[i]=="2B/SS" ||
      dataset$position[i]=="2B/3B")
    A[j,i]<-1
}
#3B
j<-3
i<-1
for (i in 1:nrow(dataset)){
  if (dataset$position[i]=="3B"    || 
      dataset$position[i]=="1B/3B" || 
      dataset$position[i]== "2B/3B"||
      dataset$position[i]=="3B/SS" ||
      dataset$position[i]=="3B/OF")
    A[j,i]<-1
}
#SS
j<-4
i<-1
for (i in 1:nrow(dataset)){
  if (dataset$position[i]=="SS"    || 
      dataset$position[i]=="1B/SS" || 
      dataset$position[i]== "2B/SS"||
      dataset$position[i]=="3B/SS" ||
      dataset$position[i]=="OF/SS")
    A[j,i]<-1
}
#C
j<-5
i<-1
for (i in 1:nrow(dataset)){
  if (dataset$position[i]=="C"    || 
      dataset$position[i]=="1B/C" || 
      dataset$position[i]== "2B/C"||
      dataset$position[i]=="3B/C" ||
      dataset$position[i]=="C/OF")
    A[j,i]<-1
}
#SP
j<-6
i<-1
for (i in 1:nrow(dataset)){
  if (dataset$position[i]=="SP") 
    A[j,i]<-1
}

#OF
j<-7
i<-1
for (i in 1:nrow(dataset)){
  if (dataset$position[i]=="OF"    || 
      dataset$position[i]=="1B/OF" || 
      dataset$position[i]== "2B/OF"||
      dataset$position[i]=="3B/OF" ||
      dataset$position[i]=="C/OF")
    A[j,i]<-1
}
i<-1

A[8, ] <- dataset$salary                # salary <= 50000

# Prepare input for LP solver
objective.in <- dataset$fpts
const.mat <- A
const.dir <- c("==", "==", "==", "==","==","==","==", "<=")
const.rhs <- c(1, 1, 1, 1,1,2,3, 50000)

# Generate optimal lineup with lp solve
require(lpSolve)
sol <- lp(direction = "max", objective.in, # maximize objective function
          const.mat, const.dir, const.rhs,   # constraints
          all.bin = TRUE)                    # use binary variables only

# View the solution

inds <- which(sol$solution == 1)
sum(dataset$salary[inds])


solution1<-dataset[inds, ]

#Print players in optimal lineup
solution1

#Write csv file of the optimal lineup
write.table(solution1, "mydata.txt", sep="\t")

