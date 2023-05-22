library(mRMRe)


## the data input should be label as 1st column, then radiomics features; and default treatment of na = 0;

set.seed(7)
dir <- readline(prompt="Enter the file location: ")
print(dir)
raw_data=read.csv(dir)
raw_data_wo_names=raw_data
f_data=raw_data_wo_names

f_data[is.na(f_data)]=0
f_data_ori=f_data
colnames(f_data)[1]="label"

ratio <- readline(prompt="Enter the percentage of your expected error rate (e.g 0.2 to represent 20%): ")
print(ratio)
ratio=as.numeric(ratio)

 for (outerloop in 1:20){

    train=f_data;
    dim_train=dim(train)[1]
    train_label=train[,1]
    
    set.seed(42+outerloop)
    res_seed=findseed(dim_train,ratio,train_label)
    new_label=changeOutput(res_seed,train_label)
    train[,1]=new_label
    
    normTrain= normalisePreprocess(train)

    #filter 5 (mRMRe)
    selectedFeatures=featureSelection_MRMR(train)
    assign(paste0("selected_data_",outerloop),selectedFeatures)
  }


## Rank The features
      new_list=list()
      
      for (i in 1:20){
        a=list(get(paste0("selected_data_",i)))
        new_list=append(new_list,a)
      }
      
      new_unlist=unlist(new_list)
      feature_freq=table(new_unlist)
      sorted_features= sort(feature_freq, decreasing = TRUE)
      print("Top features are (with rank) ")
      print(rownames(sorted_features))
      
#Functions

findseed = function(dim_train,ratio,train_label){
  shuffle_no=ceiling(ratio*dim_train)-1
  shuffle_0=ceiling(shuffle_no/2)
  shuffle_1=floor(shuffle_no/2)
  
  label_1=which(train_label==1)
  label_0=which(train_label==0)
  
  
  if(length(label_1)<length(label_0)){lbl=length(label_1)}else{lbl=length(label_0)}
  
  if(length(label_0)<shuffle_0){
    shuffle_1=shuffle_1+(shuffle_0-length(label_0))
    shuffle_0=length(label_0)
  }
  
  if(length(label_1)<shuffle_1){
    shuffle_0=shuffle_0+(shuffle_1-length(label_1))
    shuffle_1=length(label_1)
  }
  random_sed=sample(1:(lbl-shuffle_0),1)
  return(c(random_sed, shuffle_0, shuffle_1))
}

changeOutput=function(res_seed,train_label){
  label_1=which(train_label==1)
  label_0=which(train_label==0)
  
  for(tln in 1:res_seed[2]){
    x_tln=label_0[res_seed[1]+tln-1]
    train_label[x_tln]=1
  }
  
  for(tln in 1:res_seed[3]){
    x_tln=label_1[res_seed[1]+tln-1]
    #print(x_tln)
    train_label[x_tln]=0
  }
  
  return(train_label)
  
}

normalisePreprocess=function(train){
  process=preProcess(as.data.frame(train),method=c("center"))
  train_norm <- predict(process, as.data.frame(train))
  train_norm[,1]=train[,1]
  train= train_norm
  colnames(train)[1]='label'
  return(train)
}

featureSelection_MRMR=function(data,nFeature=20){
  set.thread.count(2)
  features_select=list()
  
  data_for_mRMRe=data.frame(lapply(data,as.numeric))
  dd= mRMR.data(data=data_for_mRMRe)
  dd_mRMR=mRMR.ensemble(data=dd, target_indices=c(1), feature_count=nFeature,solution_count = 10)
  selected.features.mrmre <- mRMRe::solutions(dd_mRMR)
  
  top_features=table(selected.features.mrmre)
  top_features_ordered=top_features[order(top_features,decreasing=TRUE)]
  selected_data_5=dd_mRMR@feature_names[as.numeric(rownames(top_features_ordered))]
  rownames(top_features_ordered)=selected_data_5
  return(selected_data_5)
}
    