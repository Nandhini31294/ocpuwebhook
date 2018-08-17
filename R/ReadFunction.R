Read_Train_Files<-function(){
  print("Upload Train Data")
  train_from_user<-read.csv("C:/Users/nandhini.sureshkumar/Downloads/train_data.csv",header=T)
  return(train_from_user)
  
}

Read_Test_Files<-function(){
  
  print("Upload Test Data")
  test_from_user<-read.csv("C:/Users/nandhini.sureshkumar/Downloads/test_data.csv",header=T)
  return(test_from_user)
}

train_data<-Read_Train_Files()
test_data<-Read_Test_Files()