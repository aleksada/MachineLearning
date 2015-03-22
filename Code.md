
```{r setup, cache = F, echo = F, message = F, warning = F, tidy = F}
# make this an external chunk that can be included in any file
options(width = 100)
opts_chunk$set(message = F, error = F, warning = F, comment = NA, fig.align = 'center', dpi = 100, tidy = F, cache.path = '.cache/', fig.path = 'fig/')

library(caret)
pmltrain<-read.csv('pml-training.csv',header=T)
pmltest<-read.csv('pml-testing.csv',header=T)
newWin<-subset(dataset,new_window=='yes')
##CART model
mod1=train(classe~roll_belt++pitch_belt+yaw_belt+roll_arm+pitch_arm+
        yaw_arm	+roll_dumbbell+pitch_dumbbell+yaw_dumbbell,
      data=pmltrain,method='gbm')
#Random Forest
mod2=train(classe~roll_belt+pitch_belt+yaw_belt+
             roll_arm+pitch_arm+yaw_arm+
             roll_dumbbell+pitch_dumbbell+yaw_dumbbell+
             roll_forearm+pitch_forearm+yaw_forearm,
           data=pmltrain,method='rf',trControl = trainControl(method = "cv"))
mod3=train(classe~roll_belt+pitch_belt+yaw_belt+
             roll_arm+pitch_arm+yaw_arm+
             roll_dumbbell+pitch_dumbbell+yaw_dumbbell+
             roll_forearm+pitch_forearm+yaw_forearm,
           data=pmltrain,method='lda',trControl = trainControl(method = "cv"))
mod4=train(classe~roll_belt+pitch_belt+yaw_belt+
             roll_arm+pitch_arm+yaw_arm+
             roll_dumbbell+pitch_dumbbell+yaw_dumbbell+
             roll_forearm+pitch_forearm+yaw_forearm,
           data=pmltrain,method='nb',trControl = trainControl(method = "cv"))

answer1=predict(mod1,pmltest)
answer2=predict(mod2,pmltest)
answer3=predict(mod3,pmltest)
answer4=predict(mod4,pmltest)
true=answer2
table(answer4==true)


pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
```


## About this code

* I reduce the predictors of the data into 12 predictors,  

* Four methods are employed

* Boosting, Random Forest, Naive Bayes, and LDA


