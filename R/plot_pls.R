#' Visualizing PLS regression outputs
#' 
#' 


plot_pls <- function(plsrResults){
  
  
  # Define the VIP function to avoid referring to chillR::VIP
  
  VIP <- function(object) {
    
    if (object$method != "oscorespls")
      stop(
        "Only implemented for orthogonal scores algorithm.  Refit with 'method = \"oscorespls\"'"
      )
    if (nrow(object$Yloadings) > 1)
      stop("Only implemented for single-response models")
    
    SS <- c(object$Yloadings) ^ 2 * colSums(object$scores ^ 2)
    Wnorm2 <- colSums(object$loading.weights ^ 2)
    SSW <- sweep(object$loading.weights ^ 2, 2, SS / Wnorm2, "*")
    sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS))
  }
  
  # Extract the VIP scores with VIP() from chillR. Not sure now what is "Comp 1" (component we guess)
  
  vipResult <- VIP(plsrResults)["Comp 1",]
  
  # The same for the pls coefficients. The use of ,, is new to me
  
  coef <- plsrResults$coefficients[, , 1]
  
  # Create a df for further plotting
  
  pls_outputs <- data.frame(Variable = names(vipResult),
                            VIP = vipResult,
                            Coef = coef)
  
  # Remove the ugly rownames
  
  rownames(pls_outputs) <- NULL
  
  
  # PLS plot
  
  ggplot(filter(pls_outputs, VIP > 1),
         aes(VIP, reorder(Variable, VIP), fill = Coef > 0)) +
    geom_col() +
    geom_vline(aes(xintercept = 1)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
    scale_y_discrete(expand = expansion(add = 0.5)) +
    labs(x = "Variable Importance in Projection",
         y = NULL) +
    theme_bw() +
    theme(legend.position = "none")
  
  
  }
  
