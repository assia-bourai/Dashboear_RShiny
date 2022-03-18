library(shiny)
library(dplyr) # select_if for select categoric or numeric variables
library(factoextra) # visualization of factorial analysis method
library(FactoMineR) # factorial analysis
library(skmeans) # skmeans
library(nnet) # logistic regression 
library(e1071) # naive bayes
library(rpart) # tree decision
library(caret) # confusionMatrix
library(ROCR) # prediction, performance
library(Hmisc) # describe funtion for describe categorical variables
library(ggplot2)# ggplot function
library(cowplot)# plot_grid function

source("factorial_analysis.R")
source("clustering.R")
source("supervised_classification.R")
options(shiny.maxRequestSize=30*1024^2) # Pour pouvoir traiter des csv de tailles > 20MO
server = function(input, output, session){
  
  
  # Read the csv file 
  data = reactive({
    if (is.null(input$file) == FALSE){
      return(read.csv(input$file$datapath, header = TRUE))
      updateSelectInput(session = session, inputId="remove",label = 'Remove some variables', choices = names(data()))
    }
    else
      return(NULL)
  })
  
  # Apply preprocessing
  data_preprocess = eventReactive(input$apply,{
    if(is.null(data())){
      return(NULL)
    }
    else{
        data_tmp = data()
        numeric_col = select_if(data_tmp, is.numeric)
        categoric_col = select_if(data_tmp, is.character)
        #names(categoric_col) = names(which(!unlist(lapply(data(), is.numeric))))
        
        # Type of variable to preprocess
        if(input$type == "numeric"){
          data_tmp = numeric_col
        }
        else
          if(input$type == "categorical"){
            data_tmp = categoric_col 
            
          }
        
        # Number of raws to select from the dataset
        data_tmp = data_tmp[1:as.integer(nrow(data_tmp)*input$rows/100),]
        
        
        # preprocessing of numerical values
        numeric_col = select_if(data_tmp, is.numeric)
        categoric_col = select_if(data_tmp, is.character)
        #names(categoric_col) = names(which(!unlist(lapply(data(), is.numeric))))
        
        if(input$numeric_nan == "delete"){
          data_tmp = data.frame(na.omit(data_tmp))
          numeric_col = select_if(data_tmp, is.numeric)
          categoric_col = select_if(data_tmp, is.character)
        }
        if(input$numeric_nan == "most frequent"){
          numeric_col <-data.frame(apply(numeric_col, 2, function(x) ifelse(is.na(x), getmode(x), x)))
          data_tmp[, which(names(data_tmp) %in% names(numeric_col))] = numeric_col
        }
        if(input$numeric_nan == "mean"){
          numeric_col <-data.frame(apply(numeric_col, 2, function(x) ifelse(is.na(x), mean(x,na.rm = TRUE), x)))
          data_tmp[, which(names(data_tmp) %in% names(numeric_col))] = numeric_col
        }
        if(input$numeric_nan == "std"){
          numeric_col <-data.frame(apply(numeric_col, 2, function(x) ifelse(is.na(x), sd(x,na.rm = TRUE), x)))
          data_tmp[, which(names(data_tmp) %in% names(numeric_col))] = numeric_col
        }
        if(input$numeric_nan == "constant"){
          numeric_col <-data.frame(apply(numeric_col, 2, function(x) ifelse(is.na(x), input$nan_numeric_value, x)))
          data_tmp[, which(names(data_tmp) %in% names(numeric_col))] = numeric_col
        }
        # preprocessing of categoric values
        if(input$categoric_nan == "delete"){
          data_tmp = data.frame(na.omit(data_tmp))
          numeric_col = select_if(data_tmp, is.numeric)
          categoric_col = select_if(data_tmp, is.character)
        }
        if(input$categoric_nan == "most frequent"){
          categoric_col = data.frame(apply(categoric_col, 2, function(x) ifelse(is.na(x), getmode(x), x)))
          data_tmp[, which(names(data_tmp) %in% names(categoric_col))] = categoric_col
        
        }
        if(input$categoric_nan == "constant"){
          categoric_col = data.frame(apply(categoric_col, 2, function(x) ifelse(is.na(x),input$nan_categoric_value, x)))
          data_tmp[, which(names(data_tmp) %in% names(categoric_col))] = categoric_col
        }
    
        # normalization
        if(length(input$norm)!=0){
          if(length(input$norm) == 2) # center and scale
            data_tmp[,unlist(lapply(data_tmp, is.numeric))] = data.frame(scale(data_tmp[,unlist(lapply(data_tmp, is.numeric))]))
          else{
            if("center" %in% input$norm) # center only
              data_tmp[,unlist(lapply(data_tmp, is.numeric))] = data.frame(scale(data_tmp[,unlist(lapply(data_tmp, is.numeric))], scale=FALSE))
            else 
              if("scale" %in% input$norm)# scale only
                data_tmp[,unlist(lapply(data_tmp, is.numeric))] = data.frame(scale(data_tmp[,unlist(lapply(data_tmp, is.numeric))], center = FALSE))
          }
        }
        
        # remove some variables
        if(length(input$remove)>0)
          data_tmp = data_tmp[, -which(names(data_tmp) %in% input$remove)]
        return(data_tmp)
    }
  })
  
  # Load the dataset
  output$dataset_content = renderDataTable({
      return(data_preprocess())
  })
  
  # Give the value of the constant for preprocessing
  observe(
    {
      x = input$numeric_nan
      y = input$categoric_nan
      
      # Take the name of columns 
      if(length(data_preprocess())>0){
        updateSelectInput(session = session, inputId="remove",label = 'Remove some variables', choices = names(data_preprocess()))
        updateSelectInput(session = session, inputId = "label_name", label = "Label", choices = c("",names(data_preprocess())))
        updateSelectInput(session = session, inputId = "univariate_var", label = "Variable", choices = c("None", names(data_preprocess())))
        updateSelectInput(session = session, inputId = "univariate_label", label = "Label", choices = c("None", names(select_if(data_preprocess(), is.character)) ))
        updateSelectInput(session = session, inputId = "bivariate_var1", label = "Variable 1", choices = c("None",names(data_preprocess())))
        updateSelectInput(session = session, inputId = "bivariate_var2", label = "Variable 2", choices = c("None",names(data_preprocess())))
        updateSelectInput(session = session, inputId = "bivariate_label", label = "Label", choices = c("None", names(select_if(data_preprocess(), is.character)) ))
        
      }
      if(input$label_name!=""){
        updateSelectInput(session = session, inputId = "positive", label = 'Positive', choices = c("",as.vector(unique(data_preprocess()[,which(names(data_preprocess()) %in% input$label_name)]))))
      }
      if (x == 'constant'){
        output$constant_numeric_nan = renderUI(
          {
            numericInput('nan_numeric_value', 'Enter the value', 2)
          }
        )
      }
      
      if(y == 'constant'){ 
        output$constant_categoric_nan = renderUI(
          {
            textInput("nan_categoric_value", "Enter the value")
          }
        )
      }
      
    }
  )
  
  # Exploration
  output$exploration = renderPrint({
    if(!is.null(data_preprocess())){
      if(input$variable_type == "All variables")
        return(summary(data_preprocess()))
      if(input$variable_type=="Categorical variables")
        return(describe(select_if(data_preprocess(), is.character)))
      return(summary(select_if(data_preprocess(), is.numeric)))
    }
  })
  
  # Dimension Reduction
  dimension_reduction = reactive({
    if(length(data_preprocess())>0){
      ncp = input$ncp
      data = select_if(data_preprocess(), is.numeric)
      
      if(input$dimension_reduction == "PCA"){
        return(PCA(data, scale.unit = FALSE, ncp=ncp, graph=FALSE)) 
      }
      else{
        if(input$dimension_reduction == "CA"){
          return(CA(data, ncp=ncp, graph = FALSE))
        }
        else{
          if(input$dimension_reduction=="MCA"){
            return(MCA(data, ncp=ncp, graph=FALSE))
          }
          else{
            if(input$dimension_reduction=="FAMD"){
              return(FAMD(data_preprocess(), ncp=ncp, graph=FALSE))
            }
            else{
              if(input$dimension_reduction=="MFA"){
                # Do after
                # MFA(data, group=c(), graph = FALSE)
              }
            }
          }
        }
      }
    }
  })
  
  output$summary_dimension_reduction = renderPrint({
    if(length(dimension_reduction())>0)
      return(summary(dimension_reduction()))
  })
  
  output$infos_output = renderPrint({
    if(length(dimension_reduction())>0){
      if(input$infos_input == "EigenValues"){
        return(dimension_reduction()$eig)
      }
    else{
      if(input$infos_input=="Variables"){
        return(dimension_reduction()$var)
      }
      else{
        if(input$infos_input=="Index"){
          return(dimension_reduction()$ind)
        }
      }
    }
  }
  })
  
  output$plot1 = renderPlot({
    if(length(dimension_reduction())>0){
      
      if(input$graph1 == "eigenvalues/variances"){
        return(plot_fa(dimension_reduction(), input$dimension_reduction, 1))
      }
      
      if(input$graph1 == "variables - contrib"){
        return(plot_fa(dimension_reduction(), input$dimension_reduction, 2))
      }
      
      if(input$graph1 == "variables - cos2"){
        return(plot_fa(dimension_reduction(), input$dimension_reduction, 3))
      }
      
      if(input$graph1 == "individuals - contrib"){
        return(plot_fa(dimension_reduction(), input$dimension_reduction, 4))
      }
      
      if(input$graph1 == "individuals - cos2"){
        return(plot_fa(dimension_reduction(), input$dimension_reduction, 5))
      }
      
      if(input$graph1 == "variables and individuals"){
        return(plot_fa(dimension_reduction(), input$dimension_reduction, 6))
      }
    }
  })
  
  # Clustering
  kmeans_algo = reactive({
    if( length(data_preprocess()) >0){
      if(input$kmeans == "kmeans"){
        return(kmeans(select_if(data_preprocess(), is.numeric), input$kmeans_cluster, input$kmeans_iter))
      }
      if(input$kmeans == "spherical kmeans"){
        return(skmeans(as.matrix(select_if(data_preprocess(), is.numeric)), input$kmeans_cluster))
      }
    }
  })
  
  output$kmeans_summary = renderPrint({
    if(length(kmeans_algo())>0){
      if(input$kmeans == "kmeans")
        return(print(kmeans_algo()))
      else
        if(input$kmeans=="spherical kmeans")
          return(print(kmeans_algo()$cluster))
    }
  })
  output$kmeans_plot = renderPlot({
    if(length(kmeans_algo())>0){
      if(input$kmeans == "kmeans")
        return(fviz_cluster(kmeans_algo(), select_if(data_preprocess(), is.numeric), ellipse.type = "norm"))
      if(input$kmeans == "spherical kmeans")
        return(plot(select_if(data_preprocess(), is.numeric)[,1], select_if(data_preprocess(), is.numeric)[,2],
                    col = kmeans_algo()$cluster, xlab=names(select_if(data_preprocess(), is.numeric))[1],
                    ylab = names(select_if(data_preprocess(), is.numeric))[2]))
    }
  })
  
  # supervised classification
  tr = reactive({
    if(length(data_preprocess())>0 && input$algo_sc!=""){
      return(sample(nrow(data_preprocess()), input$train_size*nrow(data_preprocess())))
    }
  })
  Xtrain = reactive({
    if(length(tr())>0){
      return(data_preprocess()[tr(),])
    }
  })
  Xtest = reactive({
    if(length(tr())>0){
      return(data_preprocess()[-tr(), ])
    }
  })
  model = reactive({
    if(length(Xtest())>0 && input$label_name!="" && input$positive!="" && input$algo_sc!=""){
      if(input$algo_sc == "Logistic Regression"){
        return(multinom(as.factor(Xtrain()[,which(names(Xtrain()) %in% input$label_name)])~., Xtrain()))
      }
      if(input$algo_sc == "rpart")
        return(rpart(Xtrain()[,which(names(Xtrain()) %in% input$label_name)]~.,Xtrain()))
    }
    if(input$algo_sc == "Naive Bayes"){
      return(naiveBayes(Xtrain()[,-which(names(Xtrain()) %in% input$label_name)], Xtrain()[,which(names(Xtrain()) %in% input$label_name)]))
    }
  })
  output$model_summary = renderPrint({
    if(length(model())>0 && input$label_name!=""){
      return(summary(model()))
    }
    return(NULL)
  })
  output$confusion_matrix = renderPrint({
    if(length(model())>0 && input$label_name!="" && input$positive!=""){
      p = predict(model(), Xtest(), type="class")
      t = table(as.factor(p), as.factor(Xtest()[,which(names(Xtest()) %in% input$label_name)]))
      return(confusionMatrix(t, positive = input$positive))
    }
    return(NULL)
  })
  output$roc_curve = renderPlot({
    
    if(length(model())>0){
      
      if(input$algo_sc %in% c("Logistic Regression", "rpart"))
        pred = predict(model(), Xtest(), type="prob")
      else
        pred = predict(model(), Xtest(), type="raw")
      if(input$algo_sc == "Logistic Regression")
        pred = prediction(pred, Xtest()[,which(names(Xtest()) %in% input$label_name)])
      else
        pred = prediction(pred[,2], Xtest()[,which(names(Xtest()) %in% input$label_name)])
      roc = performance(pred, "tpr", "fpr")
      plot(roc, colorize=TRUE, main="ROC curve", xlab = "1-specificity", ylab="sensitivity")
      abline(a=0, b=1)
      auc = performance(pred, "auc")
      auc = unlist(slot(auc, "y.values"))
      auc = round(auc, 4)
      legend(.6, .6, auc, title = "AUC", cex=1)
    }
    return(NULL)
    
  })
  
  # univariate analysis
  output$univariate_summary = renderPrint({
    if(input$univariate_var!="None"){
      if(input$univariate_var %in% colnames(select_if(data_preprocess(), is.character)))
        return(describe(data_preprocess()[input$univariate_var], descript = input$univariate_var))
      return(summary(data_preprocess()[input$univariate_var], descript=input$univariate_var))
    }
    
  })
  output$univariate_plot1 = renderPlot({
    if(input$univariate_var!="None"){
      if(input$univariate_label!="None"){
        if(input$univariate_var %in% colnames(select_if(data_preprocess(), is.character))){
          bar =  ggplot(data=data_preprocess(), aes_string(x= input$univariate_var, fill=input$univariate_label)) + geom_bar()
          return(plot_grid(bar,  labels=paste("Barplot of ", input$univariate_var)))
        }
        bar =  ggplot(data=data_preprocess(), aes_string(x= input$univariate_var, fill=input$univariate_label)) + geom_histogram(bins=30)
        return(plot_grid(bar,  labels=paste("Histogram of ", input$univariate_var)))
      }
      else{
        if(input$univariate_var %in% colnames(select_if(data_preprocess(), is.character))){
          bar =  ggplot(data=data_preprocess(), aes_string(x= input$univariate_var)) + geom_bar()
          return(plot_grid(bar,  labels=paste("Barplot of ", input$univariate_var)))
        }
        bar =  ggplot(data=data_preprocess(), aes_string(x= input$univariate_var)) + geom_histogram(bins=30)
        return(plot_grid(bar,  labels=paste("Histogram of ", input$univariate_var)))
      }
    }
    
  })
  output$univariate_plot2 = renderPlot({
    if(input$univariate_var!="None"){
      if(input$univariate_var %in% colnames(select_if(data_preprocess(), is.numeric))){
        if(input$univariate_label!="None"){
          box = ggplot(data=data_preprocess(), aes_string(x= input$univariate_label, y=input$univariate_var, color=input$univariate_label)) + geom_boxplot()
          return(plot_grid(box,  labels=paste("Boxplot of : ", input$univariate_var)))
        }
        box = ggplot(data=data_preprocess(), aes_string(y=input$univariate_var)) + geom_boxplot()
        return(plot_grid(box,  labels=paste("Boxplot of : ", input$univariate_var)))
      }
    }
  })
  
  # Bivariate analysis
  output$bivariate_summary = renderPrint({
    if(input$bivariate_var1!="None" && input$bivariate_var2!="None"){
      if(input$bivariate_var1 %in% colnames(select_if(data_preprocess(), is.character)) &&input$bivariate_var2 %in% colnames(select_if(data_preprocess(), is.character))){
        print(paste("Table de contingence de ", input$bivariate_var1, " and ", input$bivariate_var2, sep=""))
        print(table(unlist(data()[input$bivariate_var1]), unlist(data()[input$bivariate_var2])))  
      }else{
        if(input$bivariate_var1 %in% colnames(select_if(data_preprocess(), is.numeric)) &&input$bivariate_var2 %in% colnames(select_if(data_preprocess(), is.character))){
          df = data.frame()
          for(i in unique(unlist(data_preprocess()[input$bivariate_var2]))){
            a = data_preprocess()[data_preprocess()[input$bivariate_var2]==i, input$bivariate_var1]
            value = c(mean(a), sd(a), length(a))
            df = rbind.data.frame(df, value)
          }
          colnames(df)=c('Moyenne', 'Ecart-type', 'Effectif')
          rownames(df) = unique(unlist(data_preprocess()[input$bivariate_var2]))
          print(paste("Relation entre la variable catégorielle ", input$bivariate_var2, " et la variable quantitative ", input$bivariate_var1, sep = ""))
          print("\n")
          print(df)
        }
        else{
        if(input$bivariate_var1 %in% colnames(select_if(data_preprocess(), is.character)) &&input$bivariate_var2 %in% colnames(select_if(data_preprocess(), is.numeric))){
          df = data.frame()
          for(i in unique(unlist(data_preprocess()[input$bivariate_var1]))){
            a = data_preprocess()[data_preprocess()[input$bivariate_var1]==i, input$bivariate_var2]
            value = c(mean(a), sd(a), length(a))
            df = rbind.data.frame(df, value)
          }
          colnames(df)=c('Moyenne', 'Ecart-type', 'Effectif')
          rownames(df) = unique(unlist(data_preprocess()[input$bivariate_var1]))
          print(paste("Relation entre la variable catégorielle ", input$bivariate_var1, " et la variable quantitative ", input$bivariate_var2, sep = ""))
          print("\n")
          print(df)
        }
        else{
          print(paste("Relation entre la variable quantitative ", input$bivariate_var1, " et la variable quantitative ", input$bivariate_var2, sep=""))
          print("Coefficient de corrélation des deux variables:  ")
          print("#########")
          a = cor(data_preprocess()[input$bivariate_var1], data_preprocess()[input$bivariate_var2])
          print(a)
        }
        }
      }
    }
  })
  
  output$bivariate_plot = renderPlot({
    if(input$bivariate_var1!="None" && input$bivariate_var2!="None"){
      choice1 = input$bivariate_var1
      choice2 = input$bivariate_var2
      categoric = select_if(data_preprocess(), is.character)
      numeric = select_if(data_preprocess(), is.numeric)
      if(choice1 %in% colnames(categoric) && choice2 %in% colnames(categoric)){
        bar =  ggplot(data=data(), aes_string(x= choice1, fill=choice2)) + geom_bar()
        plot_grid(bar,  labels=paste("Barplot of ", choice1, " on ", choice2))
      }
      else{
        quali = choice1
        quanti = choice2
        if(choice1 %in% colnames(categoric) && choice2 %in% colnames(numeric)){
          quali = choice1
          quanti = choice2
        }else{
          if(choice2 %in% colnames(categoric) && choice1 %in% colnames(numeric)){
            quali = choice2
            quanti = choice1
          }
        }
        if( (quali %in% colnames(categoric) && quanti %in% colnames(numeric))){
          box = ggplot(data=data_preprocess(), aes_string(x= quali, y=quanti, color=quali)) + geom_boxplot()
          plot_grid(box,  labels=paste("Boxplot of : ", quanti, " on ", quali))
        }else{
          if(input$bivariate_label!="None"){
            box = ggplot(data=data_preprocess(), aes_string(x= choice1, y=choice2, color=input$bivariate_label)) + geom_point()
            plot_grid(box,  labels=paste("Scatter of : ", choice2, " on ", choice1))
          }else{
            box = ggplot(data=data_preprocess(), aes_string(x= choice1, y=choice2)) + geom_point()
            plot_grid(box,  labels=paste("Scatter of : ", choice2, " on ", choice1))
          }
        }
      }
      
    }
  })
  output$test = renderPrint({
    return("Bilal FAYE, Sara JARRAD, Assia BOURAI- M2 Apprentissage Machine pour la Science de Données, 2021-2022, Unviersité Paris Descartes")
  })
}