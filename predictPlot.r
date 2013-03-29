# extra routines for linear regression
# Tom Minka 11/29/01

source("rtable.r")

# sort a factor according to group medians
# f is a factor
# x is a vector
sort.levels <- function(f,x,fun=median) {
  if(length(x) == length(f)) x <- tapply(x,f,fun)
  if(length(x) != length(levels(f))) stop("wrong number of values to sort by")
  factor(f,levels=levels(f)[order(x)])
}

# convert from interval to range notation
interval2range <- function(x) {
  x <- unlist(strsplit(x,","))
  x[1] <- substr(x[1],2,nchar(x[1]))
  x[1] <- format(as.numeric(x[1]))
  if(x[1] == "-Inf") x[1] <- "."
  x[2] <- substr(x[2],1,nchar(x[2])-1)
  x[2] <- format(as.numeric(x[2]))
  if(x[2] == "Inf") x[2] <- "."
  paste(x[1],x[2],sep="-")
}

# used by predict.plot given
rcut <- function(x,b) {
  f <- cut(x,b,include.lowest=T)
  levels(f) <- sapply(levels(f),interval2range)
  f
}

##############################################################################

# default color for partial residuals
formals(termplot)$col.res <- 1
# default lowess span
formals(panel.smooth)$span <- 4/5

# returns a model frame where the responses have been replaced by their
# residuals under the fit
residual.frame <- function(object,data) {
  if(missing(data)) {
    x <- model.frame(object)
    resp <- response.var(x)
    if(inherits(object,"glm")) {
      p <- object$fitted.value
      x[[resp]] <- (object$y - p)/p/(1-p)
    }
    else x[[resp]] <- residuals(object)
  }
  else {
    x <- model.frame(terms(object),data)
    resp <- response.var(x)
    # only for lm, not glm
    if(inherits(object,"glm")) stop("can't compute residuals for glm")
    x[[resp]] <- x[[resp]] - predict(object,x)
  }
  x
}

# an na.action which accepts NA
# should be built into R
na.ok <- function(object,...) {
  object
}

##############################################################################

predict.plot.data.frame <- function(x,given,given.lab,layout,partial,
                                    type=c("prob","logit","probit"),
                                    identify.pred=F,scol="red",...) {
  type <- match.arg(type)
  resp <- response.var(x)
  pred <- predictor.vars(x)
  if(!missing(given) && !is.factor(given)) {
    # make given a factor
    b <- as.numeric(quantile(given,seq(0,1,length=5)))
    given <- rcut(given,b)
  }
  if(missing(layout)) {
    len <- length(pred) + !missing(given)
    layout <- c(1,len)
    if(len > 3) {
      layout[2] <- ceiling(sqrt(len))
      layout[1] <- ceiling(len/layout[2])
    }
  }
  opar <- par(mfrow=layout, mar=c(4.5,4,0,0.1))
  on.exit(par(opar))
  if(!missing(partial)) {
    pres <- data.frame(residuals(partial,type="partial"))
  }
  for(i in pred) {
    if(!missing(partial)) {
      x[[resp]] <- pres[[make.names(i)]]
      if(is.null(x[[resp]])) stop(paste("partial of",i,"not found"))
    }
    k <- !is.na(x[,i])
    if(is.factor(x[[i]]) && !is.ordered(x[[i]])) {
      x[[i]] <- sort.levels(x[[i]],as.numeric(x[[resp]]))
    }
    if(!missing(given) && is.factor(x[[i]])) {
      plot.new()
      xlim <- length(levels(x[[i]]))
      plot.window(xlim=c(0.5,xlim+0.5),ylim=range(x[[resp]]))
      axis(1,1:xlim,labels=levels(x[[i]]))
      axis(2)
      box()
      title(xlab=i, ylab=resp)
      cat(paste("jittering",i,"\n"))
    } else {
      if(is.factor(x[[resp]])) {
        if(type=="prob") {
          if(is.factor(x[[i]])) {
            mosaicplot(table(x[[i]], x[[resp]]), xlab=i, ylab=resp)
          } else {
            plot(x[[i]], x[[resp]], xlab=i, ylab=resp, ...)
          }
        }
      } else {
        plot(x[[i]], x[[resp]], xlab=i, ylab=resp, ...)
      }
    }
    if(missing(given) && !is.na(scol)) {
      if(is.factor(x[[resp]])) {
        if(length(levels(x[[resp]]))==2 && !is.factor(x[[i]])) {
          if(type=="prob") {
            lines(loprob(x[k,i], x[k,resp]), col=scol)
          } else {
            xy <- loprob(x[k,i], x[k,resp])
            p <- xy$y-1
            p <- switch(type,logit=log(p/(1-p)),probit=qnorm(p))
            xy$y <- p+1.5
            plot(xy,col=scol,type="l",xlab=i,ylab=type)
            points(x[[i]],2*as.numeric(x[[resp]])-3)
          }
        }
      } else {
        lines(lowess(x[k,i], x[k,resp]),col=scol)
      }
    }
    if((identify.pred == T) || (i %in% identify.pred)) {
      identify(x[k,i],x[k,resp],labels=rownames(x)[k])
    }
    if(!missing(given)) {
      lev <- levels(given)
      for(g in 1:length(lev)) {
        color <- ((g-1) %% 6) + 1
        val <- lev[g]
        k <- (given == val)
        if(is.factor(x[[i]])) {
          jitter <- (runif(length(x[k,i]))-0.5)/5
          points(as.numeric(x[k,i])+jitter, x[k,resp], col=color, ...)
        } else {
          points(x[k,i], x[k,resp], col=color, ...)
        }
        if(is.factor(x[[resp]])) {
          lines(loprob(x[k,i], x[k,resp]),col=color)
        } else {
          lines(lowess(x[k,i], x[k,resp]),col=color)
          #abline(lm(x[k,resp]~x[k,i]),col=color)
        }
      }
    }
  }
  if(!missing(given)) {
    # legend
    plot.new()
    if(!missing(given.lab)) title(xlab=given.lab)
    y <- cumsum(strheight(lev)+0.02)
    for(i in 1:length(lev)) {
      color <- ((i-1) %% 6) + 1
      val <- lev[i]
      text(0.5,0.75-y[i],val,col=color,adj=0.5) 
    }
  }
}
predict.plot.lm <- function(object,data,partial=F,...) {
  if(!partial) {
    if(missing(data)) {
      res <- residual.frame(object)
    } else {
      res <- residual.frame(object,data)
    }
    if(F) {
      expr <- match.call(expand = F)
      expr$... <- NULL
      expr[[1]] <- as.name("residual.frame")
      res <- eval(expr, parent.frame())
    }
    cat("plotting residuals\n")
    predict.plot.data.frame(res,...)
  } else {
    if(missing(data)) data <- model.frame(object)
    cat("plotting partial residuals\n")
    predict.plot.data.frame(data,partial=object,...)
  }
}
predict.plot.formula <- function(formula,data=parent.frame(),...) {
  # formula has givens?
  rhs <- formula[[3]]
  if(is.call(rhs) && (deparse(rhs[[1]]) == "|")) {
    # remove givens from formula
    given <- deparse(rhs[[3]])
    formula[[3]] <- rhs[[2]]
    if(is.environment(data)) g <- get(given,env=data)
    else g <- data[[given]]
    if(is.null(g)) 
      stop(paste("variable \"",given,"\" not found",sep=""))
    return(predict.plot.formula(formula,data,
                                given=g,given.lab=given,...))
  }
  if(F) {
    expr <- match.call(expand = F)
    expr$... <- NULL
    expr$na.action <- na.ok
    expr[[1]] <- as.name("model.frame.default")
    x <- eval(expr, parent.frame())
  } else {
    # formula has its own environment 
    x <- model.frame.default(formula,data,na.action=na.ok)
  }
  predict.plot.data.frame(x,...)
}
predict.plot <- function(object, ...) UseMethod("predict.plot")

step.up <- function(object) {
  resp <- response.var(object)
  pred <- predictor.vars(object)
  scope <- terms(formula(paste(resp,"~",paste(pred,collapse="*"))))
  step(object,scope)
}

##############################################################################
# glm stuff

loprob <- function(x,y) {
  lev <- levels(y)
  if(length(lev) != 2) stop("y must have two levels")
  if(!is.factor(x)) {
    from <- min(x)
    to <- max(x)
  }
  if(T) {
    # density estimate - too bumpy
    densf <- function(x) {
      if(is.factor(x)) {
        list(x=levels(x),y=tabulate(x)/length(x))
      } else {
        density(x,from=from,to=to,adjust=1.5)
      }
    }
    dens <- lapply(split(x,y),densf)
    p <- sum(y==levels(y)[2])/length(y)
    p <- p*dens[[2]]$y/((1-p)*dens[[1]]$y + p*dens[[2]]$y)
    list(x=dens[[1]]$x,y=p+1)
  } else if(length(y) > 100) {
    # loess
    library(modreg)
    y <- as.numeric(y)-1
    fit <- loess(y~x,degree=0)
    r <- seq(from,to,length=50)
    p <- predict(fit,r)
    list(x=r,y=p+1)
  } else if(F) {
    # hill density estimate - too noisy
    xr <- seq(from,to,length=50)
    densf <- function(x) hill.density(x,xr)
    dens <- lapply(split(x,y),densf)
    p <- dens[[2]]/(dens[[1]] + dens[[2]])
    list(x=xr,y=p+1)
  } else {
    # gam smooth - too slow
    library(mgcv)
    y <- as.numeric(y)-1
    fit <- gam(y~s(x),family=binomial)
    r <- seq(from,to,length=50)
    p <- predict(fit,data.frame(x=r),type="response")
    list(x=r,y=p+1)
  }
}

model.plot <- function(object,data,se=F,col="green") {
  if(missing(data)) data <- model.frame(object)
  if(inherits(object,"multinom")) {
    se <- F
    p <- predict(object,data,type="prob")
  } else {
    p <- predict(object,data,type="response",se=se)
  }
  pred <- predictor.vars(object)
  if(length(pred) == 1) {
    x <- data[[pred]]
  } else {
    x <- predict(object,data)
  }
  i <- order(x)
  x <- sort(x)
  if(!se) {
    lines(x,p[i]+1,col=col)
  } else {
    lines(x,p$fit[i]+1,col=col)
    lines(x,p$fit[i]+1+p$se[i],col=col,lty=2)
    lines(x,p$fit[i]+1-p$se[i],col=col,lty=2)
  }
}

cplot.glm <- function(object,jitter=T,add=F,col=3) {
  if(!add) cplot.data.frame(model.frame(object))
  # use the par options set by cplot.data.frame
  
  w <- coef(object)
  z <- log(0.75/0.25)
  if(length(w) == 3) {
    abline(-w[1]/w[3],-w[2]/w[3],col=col)
    abline((-z-w[1])/w[3],-w[2]/w[3],col=col,lty=2)
    abline((z-w[1])/w[3],-w[2]/w[3],col=col,lty=2)
  } else {
    xlim <- par("usr")[1:2]
    x <- seq(xlim[1],xlim[2],length=50)
    y <- (0-w[1]-w[2]*x)/(w[3]+w[4]*x)
    i <- (w[3]+w[4]*x < 0)
    lines(x[i],y[i],col=col)
    lines(x[!i],y[!i],col=col)
    for(q in c(-z,z)) {
      y <- (q-w[1]-w[2]*x)/(w[3]+w[4]*x)
      lines(x[i],y[i],col=col,lty=2)
      lines(x[!i],y[!i],col=col,lty=2)
    }
  }
}
cplot.multinom <- cplot.glm
cplot <- function(object, ...) UseMethod("cplot")

misclass.glm <- function(fit,data) {
  if(missing(data)) data <- model.frame(fit)
  resp <- response.var(fit)
  p <- factor.logical(predict(fit,data,type="response")>0.5)
  return(sum(as.numeric(p) != as.numeric(data[[resp]])))
}
misclass <- function(object, ...) UseMethod("misclass")

deviance.glm <- function(object, newdata) {
  if(missing(newdata)) return(object$deviance)
  resp <- response.var(object)
  if(object$family$family == "binomial") {
    truth <- (as.numeric(newdata[[resp]]) == 2)
    p <- predict(object,newdata,type="response")
    p[p == 0] <- 1e-3
    p[!truth] <- 1-p[!truth]
    -2*sum(log(p))
  } else {
    stop("family not handled")
  }
}

confusion.tree <- function(tr,data) {
  resp <- response.var(tr)
  p <- predict(tr,data,type="class")
  table(truth=data[[resp]],predicted=p)
}

factor.logical <- function(x,labels=c("No","Yes")) {
  f <- factor(x)
  levels(f) <- labels
  f
}

confusion.glm <- function(fit,data) {
  resp <- response.var(fit)
  p <- factor.logical(predict(fit,data,type="response")>0.5)
  table(truth=data[[resp]],predicted=p)
}
confusion <- function(object, ...) UseMethod("confusion")

cplot.data.frame <- function(x,...) {
  resp <- response.var(x)
  y <- x[[resp]]
  if(is.numeric(y)) {
    b <- as.numeric(quantile(y, seq(0,1,by=1/4)))
    yf <- rcut(y,b)
    print(levels(yf))
    q <- as.numeric(yf)
  } else {
    print(levels(y))
    q <- as.numeric(y)
  }
  # skip color 0
  color <- ((q-1) %% 6) + 1
  
  pred <- predictor.vars(x)
  # should allow derived features
  #if(length(pred) > 2) stop("must have at most two predictors")
  x1 <- x[[pred[1]]]
  if(length(pred) >= 2) {
    x2 <- x[[pred[2]]]
  }
  if(length(levels(factor(x1))) < length(x1)) {
    jitter1 <- (runif(length(x1))-0.5)*diff(range(x1))/100
    x1 <- x1+jitter1
  }
  if(length(pred) >= 2) {
    if(length(levels(factor(x2))) < length(x2)) {
      jitter2 <- (runif(length(x2))-0.5)*diff(range(x2))/100
      x2 <- x2+jitter2
    }
  }
  # change mar permanently so we can add things on top
  par(mar=c(4.5,4,0,0.1))
  if(length(pred) >= 2) {
    plot(x1, x2, xlab=pred[1], ylab=pred[2], col=color,...)
  } else {
    plot(x1, y, col=color,xlab=pred[1],ylab=resp,...)
  }
}

# expands a model formula to include all squares and cross-products of the 
# predictors
expand.quadratic <- function(fmla,cross=T) {
  resp <- response.var(fmla)
  pred <- predictor.vars(fmla)
  pred2 <- sapply(pred,function(s) paste("I(",s,"^2)",sep=""))
  s <- paste(resp,"~",paste(pred,collapse="+"),
             "+",paste(pred2,collapse="+"))
  len <- length(pred)
  if(cross && len > 1) {
    pred3 <- c()
    for(i in 1:(len-1)) {
      for(j in (i+1):len) {
        pred3 <- c(pred3, paste(pred[i],":",pred[j],sep=""))
      }
    }
    s <- paste(s,"+",paste(pred3,collapse="+"))
  }
  formula(s)
}

##############################################################################
# knn

knn.model <- function(formula,data,k=1) {
  # works for Splus too
  library(class)
  data <- model.frame(formula,data)
  object <- list(terms=terms(data),data=data,k=k)
  class(object) <- "knn"
  pred <- predictor.vars(data)
  object$scale <- sd(data[pred])
  resp <- response.var(data)
  p <- table(data[[resp]])
  object$tie.breaker <- levels(data[[resp]])[which.max(p)]
  object
}

print.knn <- function(object) {
  cat("nearest neighbor classifier, k =",format(object$k),"\n")
  pred <- predictor.vars(object)
  cat(nrow(object$data), "examples in", length(pred),"dimensions\n")
}

model.frame.knn <- function(object) {
  object$data
}

predict.knn <- function(object,test,k,type=c("class","vector")) {
  train <- object$data
  resp <- response.var(object)
  pred <- predictor.vars(object)
  s <- object$scale
  x <- scale(train[pred],center=F,scale=s)
  xt <- scale(test[pred],center=F,scale=s)
  if(missing(k)) k <- object$k
  type <- match.arg(type)
  r <- knn(x,xt,train[[resp]],k=k,l=floor(k/2+1),prob=(type=="vector"))
  r[is.na(r)] <- object$tie.breaker
  if(type == "vector") {
    p <- attr(r,"prob")
    i <- (r == levels(r)[1])
    p[i] <- 1-p[i]
    r <- cbind(1-p,p)
    dimnames(r) <- list(rownames(test),levels(train[[resp]]))
  }
  r
}

misclass.knn <- function(object,data) {
  if(missing(data)) data <- model.frame(object)
  resp <- response.var(object)
  sum(predict(object,data) != data[[resp]])
}

# same as confusion.tree
confusion.knn <- function(tr,data) {
  resp <- response.var(tr)
  p <- predict(tr,data,type="class")
  table(truth=data[[resp]],predicted=p)
}

cplot.knn <- function(object,x,levels=c(0.5),add=F,col=3) {
  if(missing(x)) x <- model.frame(object)
  else x <- model.frame(terms(object),x)
  if(!add) cplot.data.frame(x)
  # use the par options set by cplot.data.frame
  
  # x is only used to get plotting range
  resp <- response.var(object)
  pred <- predictor.vars(object)
  r1 <- range(x[[pred[1]]])
  x1 <- seq(r1[1],r1[2],length=50)
  # allow > 2 for derived features
  if(length(pred) >= 2) {
    r2 <- range(x[[pred[2]]])
    x2 <- seq(r2[1],r2[2],length=50)
    xt <- expand.grid(x1,x2)
  } else { 
    xt <- data.frame(x1)
  }
  names(xt) <- pred[1:2]
  if(inherits(object,"glm") || inherits(object,"gam")) {
    z <- predict(object,xt,type="response")
  } else {
    z <- predict(object,xt,type="vector")[,2]
  }
  if(length(pred) >= 2) {
    dim(z) <- c(length(x1),length(x2))
    contour(x1,x2,z,add=T, levels=levels,col=col)
  } else {
    lines(x1,z+1,col=col)
  }
}

# should use deviance, not misclass
best.k.knn <- function(object,ks=1:20) {
  # uses leave-one-out only
  train <- object$data
  resp <- response.var(object)
  pred <- predictor.vars(object)
  s <- object$scale
  x <- scale(train[pred],center=F,scale=s)
  r <- c()
  for(i in 1:length(ks)) {
    y <- knn.cv(x,train[[resp]],k=ks[i],l=floor(ks[i]/2+1))
    y[is.na(y)] <- object$tie.breaker
    r[i] <- sum(y != train[[resp]])
  }
  plot(ks,r,xlab="k",ylab="misclass",type="o")
  i <- which.min(r)
  cat("best k is",ks[i],"\n")
  object$k <- ks[i]
  object
}

reduce.knn <- function(object) {
  train <- object$data
  resp <- response.var(object)
  pred <- predictor.vars(object)
  s <- object$scale
  x <- scale(train[pred],center=F,scale=s)
  y <- train[[resp]]
  if(object$k == 1) {
    if(F) keep <- condense(x,y,trace=F)
    else keep <- 1:nrow(x)
    keep <- reduce.nn(x,keep,y)
  } else {
    keep <- multiedit(x,y,object$k)
  }
  object$data <- object$data[keep,]
  if(object$k > nrow(object$data)) {
    object$k <- nrow(object$data)-1
    cat("changing k to",object$k,"\n")
  }
  object
}

##############################################################################
# bug fixes

model.frame.multinom <- function(object) {
  oc <- object$call
  oc[[1]] <- NULL
  do.call("model.frame",as.list(oc))
}

plot.default <- function(x, y=NULL, type="p", xlim=NULL, ylim=NULL,
                         log="", main=NULL, sub=NULL, xlab=NULL, ylab=NULL,
                         ann=par("ann"), axes=TRUE, frame.plot=axes,
                         panel.first=NULL, panel.last=NULL,
                         col=par("col"), bg=NA, pch=par("pch"),
                         cex = 1, lty=par("lty"), lab=par("lab"),
                         lwd=par("lwd"), asp=NA, ...)
{
  xlabel <- if (!missing(x)) deparse(substitute(x))
  ylabel <- if (!missing(y)) deparse(substitute(y))
  xy <- xy.coords(x, y, xlabel, ylabel, log)
  xlab <- if (is.null(xlab)) xy$xlab else xlab
  ylab <- if (is.null(ylab)) xy$ylab else ylab
  xlim <- if (is.null(xlim)) range(xy$x[is.finite(xy$x)]) else xlim
  ylim <- if (is.null(ylim)) range(xy$y[is.finite(xy$y)]) else ylim
  plot.new()
  plot.window(xlim, ylim, log, asp, ...)
  panel.first
  plot.xy(xy, type, col=col, pch=pch, cex=cex, bg=bg, lty=lty, lwd=lwd, ...)
  panel.last
  if (axes) {
    axis(1, ...)
    # minka
    if(is.factor(y)) {
      lev <- levels(y)
      axis(2, at=1:length(lev), labels=lev, ...)
    }
    else axis(2, ...)
  }
  if (frame.plot)
    box(...)
  if (ann)
    title(main=main, sub=sub, xlab=xlab, ylab=ylab, ...)
  invisible()
}
