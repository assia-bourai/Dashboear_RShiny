# mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# dimension reduction plots
plot_fa = function(fa, fa_type, graph_type){
  
  if(graph_type == 1){# eigenvalues/variances
    return(fviz_screeplot(fa, addlabels = TRUE, ylim = c(0, 50)))
  }
  
  if(graph_type == 2){# variables contribution
    if(fa_type == "PCA"){
      return(fviz_pca_var(fa, col.var="contrib",
                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          repel = TRUE # Avoid text overlapping
      ))
    }
    if(fa_type == "MCA"){
      return(fviz_mca_var(fa, col.var = "contrib"))
    }
    if(fa_type == "FAMD"){
      return(fviz_famd_var(fa, col.var = "contrib"))
    }
    return(fviz_ca_col(fa, col.col = "contrib")) # if the user select CA
  }
  
  if(graph_type == 3){# variables - cos2
    if(fa_type == "PCA"){
      return(fviz_pca_var(fa, col.var="cos2",
                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          repel = TRUE # Avoid text overlapping
      ))
    }
    if(fa_type == "MCA"){
      return(fviz_mca_var(fa, col.var = "cos2"))
    }
    if(fa_type == "FAMD"){
      return(fviz_famd_var(fa, col.var = "cos2"))
    }
    return(fviz_ca_col(fa, col.col = "cos2")) # if the user select CA
  }
  
  if(graph_type == 4){# Individual - contribution
    if(fa_type == "PCA"){
      return(fviz_pca_ind(fa, col.ind="contrib",
                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          repel = TRUE # Avoid text overlapping
      ))
    }
    if(fa_type == "CA"){
      return(fviz_ca_row(fa, col.row = "contrib"))
    }
    if(fa_type == "MCA"){
      return(fviz_mca_ind(fa, col.ind = "contrib"))
    }
    if(fa_type == "FAMD"){
      return(fviz_famd_ind(fa, col.ind = "contrib")) 
    }
  }
  
  if(graph_type == 5){ # Individual - cos2
    if(fa_type == "PCA"){
      return(fviz_pca_ind(fa, col.ind="cos2",
                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          repel = TRUE # Avoid text overlapping
      ))
    }
    if(fa_type == "CA"){
      return(fviz_ca_row(fa, col.row = "cos2"))
    }
    if(fa_type == "MCA"){
      return(fviz_mca_ind(fa, col.ind = "cos2"))
    }
    if(fa_type == "FAMD"){
      return(fviz_famd_ind(fa, col.ind = "cos2"))
    }
  }
  
  if(graph_type == 6){# biplot
    if(fa_type == "PCA"){
      return(fviz_pca_biplot(fa, repel = TRUE))
    }
    if(fa_type == "CA"){
      return(fviz_ca_biplot(fa, repel = TRUE))
    }
    if(fa_type == "MCA"){
      return(fviz_mca_biplot(fa, repel = TRUE))
    }
    if(fa_type == "FAMD"){
      return(fviz_famd(fa))
    }
  }
}