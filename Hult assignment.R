#1.	Chapter 3.4: Exercises: 7 
#Creating a user defined function named studentfunction that could help
#transposes a numeric matrix and subsets and subset the same matrix in a way
#that I only have 5 rows in my new dataset

                              #My studentfunction
studentfunction<- function(x) {
  head(t(x))
}
                  #first vector and establishing rows and col
cel<-c(10, 11, 9, 15, 19,52,19,7, 10,22,28, 40, 6,99,33,35, 26, 5, 87, 91,0, 12, 16, 81, 200)
ro_names<-c("RowA","RowB","RowC","RowD","RowE")
col_names<-c("Col1","Col2","Col3","Col4","Col5")
                          # transformation to matrix
my_matrix<- matrix(cel, nrow=5, ncol=5, byrow=TRUE, dimnames = list(ro_names,col_names))
print(my_matrix)
                      #second vector with rows and col
cel_2<-c(1,4,7,2,5,8,3,6,9)
ro_names<-c("RowA","RowB","RowC")
col_names<-c("Col1","Col2","Col3")
                      # transformation to matrix
my_matrix_2<- matrix(cel_2, nrow = 3, ncol = 3, byrow = TRUE,dimnames = list(ro_names,col_names))
print(my_matrix_2)
#A)
studentfunction(my_matrix)
#B)
studentfunction(my_matrix_2)


#2.	Chapter 3.4: Exercises:8
#Creating a user defined function  transformmatrix that
#takes the diagonal of a matrix and calculates a vector with two
#elements.the mean of the diagonal and the median.
#My transformmatrix function
transformmatrix <- function(x){
  diag_nal <- c()
  for(i in 1:nrow(x)){
    diag_nal[i] <- x[i,i]
  }
  a<-as.vector(c(median(diag_nal),(mean(diag_nal))))
  print(a)
}
#A)
transformmatrix(my_matrix)
#B)
transformmatrix(my_matrix_2)


#3 Chapter 4.4: Exercise: 9
          #calling up the iris dataset and assigning it as a mydf_i
mydf_i<- iris
head(mydf_i)

    #A
#changing  the Species column from a character type to numeric.
#while Assigning 1 for setosa, 2 for virginica, and 3 for versicolor
#the easiest way we could have done this "according to only me" was by doing gsub, however, because the professor asked
#for a loop and not the use of any predefined f(x) we had to look for a hands-on way of accomplishing 
#We were able to find a way through turning mydf_i as character so that once we run the for loop
#it will interate as desired , easy to put into action , hard to find!
#running mydf_i as character then running the for loop

mydf_i$Species<- as.character(mydf_i$Species)

for (i in 1:nrow(mydf_i))
{
  if (mydf_i$Species[i] == "setosa") {
    
    mydf_i$Species[i] <- 1
    
  }else if(mydf_i$Species[i] == "virginica") {
    
    mydf_i$Species[i] <- 2
  }else if(mydf_i$Species[i] == "versicolor") {
    
    mydf_i$Species[i] <- 3
    
  }
  
}
mydf_i
      # B
#New column that groups the Petal.Length into 3 groups
#group#1 for Petal.Length from 0 to 2 or >=0 <=2 
#group #2 from 2.01 to 4.5 or >=2.01 <=4.5
#group #3 from 4.51 to 7 or >=4.51 <=7
#we are running a for loop function

for (i in 1:nrow(mydf_i)){
  if(mydf_i$Petal.Length[i] >= 0 & mydf_i$Petal.Length[i] <= 2){mydf_i$group_lgth[i]<-'G1'}
  else if (mydf_i$Petal.Length[i] >= 2.01 & mydf_i$Petal.Length[i] <= 4.5){mydf_i$group_lgth[i]<-'G2'}
  else if (mydf_i$Petal.Length[i] >= 4.51 & mydf_i$Petal.Length[i] <= 7){mydf_i$group_lgth[i]<-'G3'}
}

mydf_i

#4 Chapter 6.7: Exercise: 3
#Using the iris dataset,combining the Setosa and Versicolor into group "0" and labeling the
#Virginica to "1". and recalling the new variable iris$Grp with the 0 or 1 labels so that we can
#identify a better corroletion
  # A
mydf_i2 <- iris
for(b in 1:nrow(mydf_i2)){
  if(mydf_i2$Species[b] == "setosa"){mydf_i2$Grp[b] <- as.numeric(0)}
  else if(mydf_i2$Species[b]    == "versicolor"){mydf_i2$Grp[b] <- as.numeric(0)}
  else if(mydf_i2$Species[b]    == "virginica"){mydf_i2$Grp[b]<- as.numeric(1)}
}
mydf_i2
  # B logistic regression to run our predictive analysis 
virginica_obs_pred<- glm(Grp~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=mydf_i2, family = "binomial")
summary(virginica_obs_pred)


  # C Probability new plant virginica
summary(mydf_i2)
new_plant_virginica<- data.frame(Sepal.Width=5, Petal.Length=10,Petal.Width=7, Sepal.Length=9)
prob=plogis(predict(virginica_obs_pred,new_plant_virginica))

prob