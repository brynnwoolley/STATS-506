## waldCI.R

setClass("waldCI",
         slots = c(level = "numeric",
                   mean  = "numeric",
                   sterr = "numeric"))

setValidity("waldCI", function(object) {
  if (object@level <= 0 | object@level >= 1) {
    stop("level must be in (0, 1)")
  }
  if (object@sterr <= 0) {
    stop("stderr must be positive")
  }
  if (!is.finite(object@sterr)) {
    stop("Infinite CI not supported")
  }
  TRUE
})

##' Create `waldCI` object. Either `lb` and `ub`, or `mean` and `sterr` must be
##' provided.
makeCI <- function(level,
                   lb = NULL,
                   ub = NULL,
                   mean = NULL,
                   sterr = NULL) {
  if (!is.null(mean) && !is.null(sterr) &&
      is.null(lb) && is.null(ub)) {
    return(new("waldCI", level = level, mean = mean, sterr = sterr))
  } else if (is.null(mean) && is.null(sterr) &&
             !is.null(lb) && !is.null(ub)) {
    if (lb > ub) {
      stop("lb must be less than ub")
    }
    z <- qnorm((1 + level) / 2)
    mean  <- (lb + ub) / 2
    sterr <- (ub - lb) / (2 * z)
    return(new("waldCI", level = level, mean = mean, sterr = sterr))
  } else {
    stop("Input must be either lb/ub or mean/sterr")
  }
}

## internal: get bounds
.getBounds <- function(ci) {
  z  <- qnorm((1 + ci@level)/2)
  lb <- ci@mean - z * ci@sterr
  ub <- ci@mean + z * ci@sterr
  c(lb, ub)
}

## show method
setMethod("show", "waldCI", function(object) {
  bound <- .getBounds(object)
  cat(round(object@level * 100), "% CI: (",
      bound[1], ", ", bound[2], ")\n", sep = "")
  invisible(object)
})

## internal: general slot getter
.getSlot <- function(object, slotname) {
  if (slotname %in% c("level", "mean", "sterr")) {
    return(slot(object, slotname))
  }
  bounds <- .getBounds(object)
  if (slotname == "lb") {
    return(bounds[1])
  } else if (slotname == "ub") {
    return(bounds[2])
  } else {
    stop("Invalid slotname")
  }
}

## Getters / setters

setGeneric("level", function(object) standardGeneric("level"))
setMethod("level", "waldCI", function(object) .getSlot(object, "level"))

## mean is S3 generic: define method
mean.waldCI <- function(x, ...) .getSlot(x, "mean")

setGeneric("sterr", function(object) standardGeneric("sterr"))
setMethod("sterr", "waldCI", function(object) .getSlot(object, "sterr"))

setGeneric("lb", function(object) standardGeneric("lb"))
setMethod("lb", "waldCI", function(object) .getSlot(object, "lb"))

setGeneric("ub", function(object) standardGeneric("ub"))
setMethod("ub", "waldCI", function(object) .getSlot(object, "ub"))

## Setters

setGeneric("level<-", function(object, value) standardGeneric("level<-"))
setMethod("level<-", "waldCI", function(object, value) {
  makeCI(level = value,
         mean  = object@mean,
         sterr = object@sterr)
})

setGeneric("mean<-", function(object, value) standardGeneric("mean<-"))
setMethod("mean<-", "waldCI", function(object, value) {
  makeCI(level = object@level,
         mean  = value,
         sterr = object@sterr)
})

setGeneric("sterr<-", function(object, value) standardGeneric("sterr<-"))
setMethod("sterr<-", "waldCI", function(object, value) {
  makeCI(level = object@level,
         mean  = object@mean,
         sterr = value)
})

setGeneric("lb<-", function(object, value) standardGeneric("lb<-"))
setMethod("lb<-", "waldCI", function(object, value) {
  makeCI(level = object@level,
         lb    = value,
         ub    = .getBounds(object)[2])
})

setGeneric("ub<-", function(object, value) standardGeneric("ub<-"))
setMethod("ub<-", "waldCI", function(object, value) {
  makeCI(level = object@level,
         lb    = .getBounds(object)[1],
         ub    = value)
})

## contains / overlap

contains <- function(ci, value) {
  stopifnot(is.numeric(value), length(value) == 1)
  bounds <- .getBounds(ci)
  value > bounds[1] & value < bounds[2]
}

overlap <- function(ci1, ci2) {
  b1 <- .getBounds(ci1)
  b2 <- .getBounds(ci2)
  max(b1[1], b2[1]) <= min(b1[2], b2[2])
}

## as.numeric

setGeneric("as.numeric")
setMethod("as.numeric", "waldCI", function(x) .getBounds(x))

## transformCI

transformCI <- function(ci, fn) {
  stopifnot(is.function(fn))
  stopifnot(is(ci, "waldCI"))
  makeCI(level = ci@level,
         mean  = fn(ci@mean),
         sterr = fn(ci@sterr))
}
