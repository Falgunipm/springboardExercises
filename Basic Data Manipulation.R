refine
refine[,1]
refine_df<-refine
refine_df
refine_df$company
colnames(refine_df)
colnames(refine_df)[2]<- "productCode_number"
refine_df$company<-tolower(refine_df$company)
refine_df$company
refine_df$company[grep("ph",refine_df$company)]<- "philips"
refine_df$company[grep("f",refine_df$company)]<- "philips"
refine_df$company[grep("^a",refine_df$company)]<- "akzo"
refine_df$company[grep("^v",refine_df$company)]<- "van houten"
refine_df$company[grep("^u",refine_df$company)]<- "unilever"
refine_df$productNumber<-sub(".-*","",refine_df$productCode_number)
refine_df$productcode<-sub("*-.*","",refine_df$productCode_number)
refine_df$productCode_number<-NULL
#colnames(refine_df)<-"product_category
refine_df$product_category[refine_df$productcode=="p"]<-"Smartphone"
refine_df$product_category[refine_df$productcode=="v"]<-"TV"
refine_df$product_category[refine_df$productcode=="x"]<-"Laptop"
refine_df$product_category[refine_df$productcode=="q"]<-"Tablet"
refine_df<-transform(refine_df,full_address=paste(address,city,country,sep=","))
refine_df$company_philips <- ifelse(refine_df$company=="philips", 1, 0)
refine_df$company_akzo <- ifelse(refine_df$company=="akzo", 1, 0)
refine_df$company_van_houten <- ifelse(refine_df$company=="van houten", 1, 0)
refine_df$company_unilever <- ifelse(refine_df$company=="unilever", 1, 0)
refine_df$product_smartphone <- ifelse(refine_df$product_category=="Smartphone", 1, 0)
refine_df$product_tv <- ifelse(refine_df$product_category=="TV", 1, 0)
refine_df$product_laptop <- ifelse(refine_df$product_category=="Laptop", 1, 0)
refine_df$product_tablet <- ifelse(refine_df$product_category=="Tablet", 1, 0)
