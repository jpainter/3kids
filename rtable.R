# functions for response tables
# Tom Minka 11/8/01

# returns the elements of x not indexed by i
not <- function(x,i) {
  if(is.numeric(i)) return(x[-i])
  if(is.character(i)) return(x[setdiff(names(x),i)])
  if(is.logical(i)) return(x[!i])
}

# returns the name of the response variable
# works for formulas, model frames, and fitted models
response.var <- function(object) {
  if(inherits(object, "terms")) {
    a <- attributes(object)
    if(!a$response) return(character(0))
    return(as.character(a$variables[2]))
  }
  response.var(terms(object))
}

# returns only the predictors which appear as bare terms
predictor.vars <- function(object) {
  if(inherits(object, "terms")) {
    a <- attributes(object)
    return(a$term.labels[a$order == 1])
  }
  predictor.vars(terms(object))
}

# should already be in R
terms.data.frame <- function(df) {
  fmla <- attr(df,"terms")
  if(is.null(fmla)) {
    nam <- names(df)
    resp <- nam[length(nam)]
    pred <- nam[1:(length(nam)-1)]
    fmla <- terms(formula(paste(resp,"~",paste(pred,collapse="+"))))
  }
  fmla
}

rep.mat <- function(x,n,m) {
  array(rep(x,n*m),c(length(x)*n,m))
}

rep.col <- function(x,n) {
  rep.mat(x,1,n)
}

rep.row <- function(x,n) {
  t(rep.col(x,n))
}

#############################################################################

# given an aov fit, plot the effects for each predictor
# as well as a boxplot of the residuals
# you can do an F-test visually by comparing the spread of the effects
# with the spread of the residuals
effects.plot <- function(object,se=F) {
  mt <- model.tables(object,se=se)
  vars <- names(mt$tables)
  nvar <- length(vars)
  
  opar <- par(mar=c(2.5,4,0,0.1))
  on.exit(par(opar))
  plot.new()
  ylim <- c(min(sapply(mt$tables,min)), max(sapply(mt$tables,max)))
  plot.window(xlim=c(0.5,nvar+1+0.5),ylim=ylim)
  axis(1,1:(nvar+1), labels=c(vars,"residuals"))
  axis(2)
  title(ylab=response.var(object))
  
  if(se) {
    if(!is.numeric(se)) se <- 1.96
    p <- (1-2*pnorm(-se))*100
    cat("Displaying ",format(p,digits=2),"% confidence intervals\n",sep="")
  }
  for(k in 1:nvar) {
    eff <- mt$tables[[k]]
    text(k, eff, names(eff))
    if(se) {
      jitter <- runif(length(eff))*0.1
      arrows(k+jitter, eff-se*mt$se[[k]], k+jitter, eff+se*mt$se[[k]], 
             code = 3, col = "green", angle = 75, length = .1)
    }
  }
  
  res <- residuals(object)
  x <- model.frame(object)
  res <- tapply(res, x[vars], mean)
  boxplot(res, at=nvar+1, add=T)
}

#############################################################################

print.rtable <- function(rt,...) {
  cat(response.var(rt),"\n")
  # strip attributes and print as a matrix
  attributes(rt) <- attributes(rt)[c("dim","dimnames")]
  print(rt)
}

terms.rtable <- function(rt) {
  attr(rt,"terms")
}

is.ordered.rtable <- function(rt) {
  # "ordered" attribute is not guaranteed to be ordered correctly,
  # because of t() and aperm()
  attr(rt,"ordered")[names(dimnames(rt))]
}

as.data.frame.rtable <- function(rt) {
  df <- as.data.frame.table(rt)
  len <- length(names(df))
  names(df)[len] <- response.var(rt)
  df
}

aov.rtable <- function(rt,med=F) {
  frame <- as.data.frame.rtable(rt)
  if(med) {
    p <- medpolish(rt,trace.iter=F)
    return(structure(terms=terms(rt), model=frame, class="aov"))
  }
  aov(terms(rt), frame)
}

# returns a table of responses, stratified according to the terms in fmla
# and aggregated according to fun.
rtable.terms <- function(fmla, x, fun = mean) {
  resp <- response.var(fmla)
  pred <- predictor.vars(fmla)
  rt <- tapply(x[[resp]], x[pred], fun)
  class(rt) <- "rtable"
  attr(rt,"terms") <- fmla
  attr(rt,"ordered") <- sapply(x[pred],is.ordered)
  rt
}
# model.frame is a data.frame with a "terms" attribute describing a model
rtable.data.frame <- function(object, ...) {
  rtable(terms(object), object, ...)
}
rtable.aov <- function(object, ...) {
  x <- model.frame(object)
  resp <- response.var(x)
  pred <- predictor.vars(x)
  y <- expand.grid(lapply(x[pred],levels))
  y[[resp]] <- predict(object,y)
  y <- model.frame(terms(x),y)
  rtable(y, ...)
}
# legal input:
# rtable(y~f1+f2,x)
# rtable(y~.,x)
# rtable(x$y ~ x$f1 + x$f2)
rtable.formula <- function(formula, data, ...) {
  expr <- match.call(expand = F)
  expr$... <- NULL
  expr[[1]] <- as.name("model.frame.default")
  model <- eval(expr, parent.frame())
  return(rtable(model, ...))
}
rtable <- function(object, ...) UseMethod("rtable")

# plots rows of x as traces
profile.plot <- function(y,standard=F,med=F,ylab,...) {
  reorder.cols <- function(y,i) {
    a <- attributes(y); y <- y[,i]
    a$dimnames[[2]] <- a$dimnames[[2]][i]; attributes(y) <- a
    y
  }
  
  #if(missing(ylab)) ylab <- deparse(substitute(y))
  row.var <- names(dimnames(y))[1]
  col.var <- names(dimnames(y))[2]
  
  if(med) {
    library(eda)
    fit <- medpolish(y,trace.iter=F)
    col.centers <- fit$col + fit$overall
    resid <- fit$residuals
  } else {
    col.centers <- apply(y,2,function(z) mean(z,na.rm=T))
    row.effects <- apply(y,1,function(z) mean(z,na.rm=T)) - mean(y)
    resid <- y - outer(row.effects, col.centers, "+")
  }
  if(standard) {
    # subtract column centers
    y <- y - rep.row(col.centers,length(rownames(y)))
    # choose x to make the profiles linear
    # fails if there are missing values
    if(F) {
      s <- La.svd(resid)
      v <- s$vt[1,]
    } else {
      s <- svd(resid)
      v <- s$v[,1]
    }
    #v <- v + rnorm(length(v))*1e-1
    i <- order(v)
    if(!is.ordered.rtable(y)[2] || all(i == 1:length(i))) {
      y <- reorder.cols(y,i)
      x <- v[i]
    } else {
      x <- seq(along=colnames(y))
    }
    if(missing(ylab)) ylab <- paste(row.var, "effect on", response.var(y))
  } else {
    # sort columns by center
    i <- order(col.centers)
    if(!is.ordered.rtable(y)[2] || all(i == 1:length(col.centers))) {
      y <- reorder.cols(y,i)
      # space x axis by center
      x <- col.centers[i]
    } else {
      x <- seq(along=colnames(y))
    }
    if(missing(ylab)) ylab <- response.var(y)
  }
  
  opar <- par(mar=c(4.5,4,0,0.1))
  on.exit(par(opar))
  plot.new()
  xlim <- range(x)
  w <- max(strwidth(rownames(y),units="inches"))/par("pin")[1]
  w <- w+0.05
  xlim[2] <- xlim[2] + diff(xlim)*w/(1-w)
  xspc <- 0.05*diff(xlim)
  plot.window(xlim=xlim,ylim=range(as.vector(y),finite=T))
  axis(1,x, labels=colnames(y))
  axis(2)
  box()
  for(i in 1:nrow(y)) {
    color <- ((i-1) %% 6) + 1
    lines(x, y[i,],col=color, type="o", ...)
    j <- rev(which(!is.na(y[i,])))[1]
    text(x[j]+xspc, y[i,j], rownames(y)[i], col=color, adj=0, ...)
  }
  title(xlab=col.var,ylab=ylab)
}