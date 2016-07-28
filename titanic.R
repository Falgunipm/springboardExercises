titanic_df<-titanic3
embarked<-titanic_df$embarked
#nchar(embarked)
str(embarked)
embarked<-sub("^$","S",embarked)
titanic_df$embarked<-embarked
str(titanic_df$embarked)
mean(titanic_df$age,na.rm = TRUE)
median(titanic_df$age,na.rm = TRUE)


titanic_df$age[is.na(titanic_df$age)]<-mean(titanic_df$age,na.rm = TRUE)
titanic_df$age[is.na(titanic_df$age)]
titanic_df$age
boat<-titanic_df$boat
boat<-sub("^$",NA,boat)
titanic_df$boat<- boat
cabin <- titanic_df$cabin
cabin<-sub("^$",NA,cabin)
titanic_df$cabin<-cabin
titanic_df$hascabin <- ifelse(is.na(titanic_df$cabin), 0, 1)
titanic_df$hascabin 
