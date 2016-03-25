cloud <- function (stmobj, topic = NULL, type = c("model", "documents"), 
    documents, thresh = 0.9, max.words = 100, ...)  {
    if (!requireNamespace("wordcloud", quietly = TRUE)) {
        stop("wordcloud package required to use this function.")
    }
    else {
        if (class(stmobj) != "STM") 
            stop("cloud function only works for STM models.  See wordcloud package for general tools.")
        if (length(topic) > 1) 
            stop("Please only select 1 topic.")
        mod <- stmobj
        type <- match.arg(type)
        vocab <- mod$vocab
        if (is.null(topic)) 
            type <- "documents"
        if (type == "model") {
            if (length(mod$beta$logbeta) == 1) {
                vec <- exp(mod$beta$logbeta[[1]])[topic, ] * 
                  sum(mod$settings$dim$wcounts$x)
            }
            else {
                levels <- table(mod$settings$covariates$betaindex)
                weights <- levels/sum(levels)
                vec <- weights[1] * exp(mod$beta$logbeta[[1]])[topic, 
                  ]
                for (i in 2:length(mod$beta$logbeta)) {
                  vec <- vec + weights[i] * exp(mod$beta$logbeta[[i]])[topic, 
                    ]
                }
                vec <- vec * sum(mod$settings$dim$wcounts$x)
            }
        }
        else {
            if (is.null(topic)) {
                vec <- mod$settings$dim$wcounts$x
            }
            else {
                if (is.null(documents)) 
                  stop("documents needed to give topic specific document values.")
                docnums <- which(mod$theta[, topic] > thresh)
                if (length(docnums) == 0) 
                  stop(sprintf("No documents have a topic loading higher than %s", 
                    thresh))
                subdoc <- documents[docnums]
                indices <- unlist(lapply(subdoc, "[", 1, ))
                counts <- unlist(lapply(subdoc, "[", 2, ))
                out <- aggregate(counts, by = list(indices), 
                  FUN = sum)
                vec <- rep(0, length(vocab))
                vec[out$Group.1] <- out$x
            }
        }
    }
    return(data.frame("words"=vocab, "freq"=vec))           
}
