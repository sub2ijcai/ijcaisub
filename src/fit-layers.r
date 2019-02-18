fit_cc_layers <-
  function(T1, T2,
           form_T1, form_T2,
           test, classifier) {

    T1$T1_lbl <- as.factor(T1$T1_lbl)
    T2$T2_lbl <- as.factor(T2$T2_lbl)

    probs_t1 <-
      Classification(
        form = form_T1,
        train = T1,
        test = test,
        predictive_algorithm = classifier
      )
    
    probs_t2 <-
      Classification(
        form = form_T2,
        train = T2,
        test = test,
        predictive_algorithm = classifier
      )

    nc <- ncol(T1)
    if (nc > 50) {
      T1_X <- subset(T1, select = 1:111)
      T2_X <- subset(T2, select = 1:111)
      test_X <- subset(test, select = 1:111)
    } else {
      T1_X <- subset(T1, select = 1:21)
      T2_X <- subset(T2, select = 1:21)
      test_X <- subset(test, select = 1:21)
    }
   

    bpRules <- bprules(rbind.data.frame(T1_X,T2_X,test_X))
    bpRules <- tail(bpRules, nrow(test_X))

    bpRules[] <- lapply(bpRules, function(u) 1 - tsensembler::normalize(u))
    avgR <- unname(apply(bpRules,1,median, na.rm = TRUE))
    
    list(f1_hat = probs_t1, 
         f2_hat = probs_t2,
         prob_row = avgR)
  }
