library(caret)
library(dplyr)
library(ModelMetrics)

# load data
data = read.csv("C:\\nf.csv")

# na with mean and remove columns
data = data %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) 
data = data %>% select(-c("Show", "release_date"))

# convert to factors
data$renewed = as.factor(data$renewed) 
levels(data$renewed)

# split model building and prediction
final = data[42, ]
data = data[1:41,]

# function for simulation
build_models = function(i){
    # set random seed values
    set.seed(sample(1:1000, 1))
    
  
    # create data partition
    split = runif(1, 0.6, 0.9)
    samp = createDataPartition(y = data$renewed, p = split)
    x = data[samp$Resample1, ]
    y = data[-samp$Resample1, ]
    
    #10 folds repeat 3 times
    control = trainControl(method='repeatedcv', 
                            number=10, 
                            repeats=5)
    
    #Metric compare model is Accuracy
    metric = "Accuracy"
    set.seed(sample(1:1000, 1))
    
    #Number randomely variable selected is mtry
    svmgrid = expand.grid(cost=seq(0.05,1,by = 0.05))
    
    # train the model
    model = train(renewed~., 
                        data=x, 
                        method='svmLinear2', 
                        metric='Accuracy', 
                        tuneGrid=svmgrid, 
                        trControl=control,
                        importance = TRUE, 
                        preProcess= c("center", "scale")
                       )
    # print the results of the model
    print(model)

    # get the metrics for test set
    test = caret::confusionMatrix(factor(y$renewed), predict(model, y))
  
    # return the simulation results
    return(data.frame(train_accuracy = max(model$results$Accuracy),
                      test_accuracy = as.numeric(test$overall[1]),
                      AUC = auc(factor(y$renewed), predict(model, y)),
                      train_split = split,
                      final = predict(model, final)))
}

# run simulation
sim_Results = do.call(rbind, lapply(1:30, build_models))

sim_Results

# sumarize sim results
sim_Results %>% summarize(avg_train_accuracy = mean(train_accuracy),
                          avg_test_accuracy = mean(test_accuracy),
                          avg_AUC = mean(AUC)
                          )

# variable importance for the model
var = varImp(model)
