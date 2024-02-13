library(shiny)

options(shiny.sanitize.errors = FALSE)

shinyServer(function(input, output){

        d.dist = function(x) {
          if (input$dist=="norm") dnorm(x, input$p1, input$p2)
          else if (input$dist=="t") dt(x, input$p1)
          else if (input$dist=="chisq") dchisq(x, input$p1)
          else if (input$dist=="f") df(x, input$p1, input$p2)
          else if (input$dist=="unif") dunif(x, input$p1, input$p2)
          else if (input$dist=="beta") dbeta(x, input$p1, input$p2)
          else if (input$dist=="exp") dexp(x, input$p1)
          else if (input$dist=="gamma") dgamma(x, input$p1, input$p2)
          else if (input$dist=="weibull") dweibull(x, input$p1, input$p2)
          else if (input$dist=="sn") sn::dsn(x, input$p1, input$p2,input$p3)
          else if (input$dist=="binom") dbinom(x, input$p1, input$p2)
          else if (input$dist=="pois") dpois(x, input$p1)
          else if (input$dist=="geom") dgeom(x, input$p1)
          else if (input$dist=="nbinom") dnbinom(x, input$p1, input$p2)
          else if (input$dist=="hyper") dhyper(x, input$p1, input$p2, input$p3)
        }
        
        p.dist = function(x) {
          if (input$dist=="norm") pnorm(x, input$p1, input$p2)
          else if (input$dist=="t") pt(x, input$p1)
          else if (input$dist=="chisq") pchisq(x, input$p1)
          else if (input$dist=="f") pf(x, input$p1, input$p2)
          else if (input$dist=="unif") punif(x, input$p1, input$p2)
          else if (input$dist=="beta") pbeta(x, input$p1, input$p2)
          else if (input$dist=="exp") pexp(x, input$p1)
          else if (input$dist=="gamma") pgamma(x, input$p1, input$p2)
          else if (input$dist=="weibull") pweibull(x, input$p1, input$p2)
          else if (input$dist=="sn") sn::psn(x, input$p1, input$p2,input$p3)
          else if (input$dist=="binom") pbinom(x, input$p1, input$p2)
          else if (input$dist=="pois") ppois(x, input$p1)
          else if (input$dist=="geom") pgeom(x, input$p1)
          else if (input$dist=="nbinom") pnbinom(x, input$p1, input$p2)
          else if (input$dist=="hyper") phyper(x, input$p1, input$p2, input$p3)
        }
        
        q.dist = function(q) {
          if (input$dist=="norm") qnorm(q, input$p1, input$p2)
          else if (input$dist=="t") qt(q, input$p1)
          else if (input$dist=="chisq") qchisq(q, input$p1)
          else if (input$dist=="f") qf(q, input$p1, input$p2)
          else if (input$dist=="unif") qunif(q, input$p1, input$p2)
          else if (input$dist=="beta") qbeta(q, input$p1, input$p2)
          else if (input$dist=="exp") qexp(q, input$p1)
          else if (input$dist=="gamma") qgamma(q, input$p1, input$p2)
          else if (input$dist=="weibull") qweibull(q, input$p1, input$p2)
          else if (input$dist=="sn") sn::qsn(q, input$p1, input$p2,input$p3)
          else if (input$dist=="binom") qbinom(q, input$p1, input$p2)
          else if (input$dist=="pois") qpois(q, input$p1)
          else if (input$dist=="geom") qgeom(q, input$p1)
          else if (input$dist=="nbinom") qnbinom(q, input$p1, input$p2)
          else if (input$dist=="hyper") qhyper(q, input$p1, input$p2, input$p3)
        }
        
        r.dist = function(n) {
          if (input$dist=="norm") rnorm(n, input$p1, input$p2)
          else if (input$dist=="t") rt(n, input$p1)
          else if (input$dist=="chisq") rchisq(n, input$p1)
          else if (input$dist=="f") rf(n, input$p1, input$p2)
          else if (input$dist=="unif") runif(n, input$p1, input$p2)
          else if (input$dist=="beta") rbeta(n, input$p1, input$p2)
          else if (input$dist=="exp") rexp(n, input$p1)
          else if (input$dist=="gamma") rgamma(n, input$p1, input$p2)
          else if (input$dist=="weibull") rweibull(n, input$p1, input$p2)
          else if (input$dist=="sn") sn::rsn(n, input$p1, input$p2,input$p3)
          else if (input$dist=="binom") rbinom(n, input$p1, input$p2)
          else if (input$dist=="pois") rpois(n, input$p1)
          else if (input$dist=="geom") rgeom(n, input$p1)
          else if (input$dist=="nbinom") rnbinom(n, input$p1, input$p2)
          else if (input$dist=="hyper") rhyper(n, input$p1, input$p2, input$p3)
        }
  
        h.dist = function(x) { d.dist(x)/(1-p.dist(x)) }
        
        output$dist <- renderUI({
            if (is.null(input$type)) return()
            switch(input$type,
               "cont" = selectInput("dist", "Distribution: ", choices = c(`Normal` = "norm", `t` = "t", `Chi-Squared` = "chisq", `F` = "f", `Uniform` = "unif", `Beta` = "beta", `Exponential` = "exp", `Gamma` = "gamma", `Weibull` = "weibull", `Skew Normal` = "sn"), selected = "norm"),
               "disc" = selectInput("dist", "Distribution: ", choices = c(`Binomial` = 'binom', `Poisson` = 'pois', `Geometric` = 'geom', `Negative Binomial` = 'nbinom', `Hypergeometric` = 'hyper'), selected = "binom"))
        })
        
        output$p1 = renderUI(
                {
                  if (input$dist == "norm") {numericInput("p1","Mean: ", value = 0)}
                  else if (input$dist == "chisq" | input$dist == "t") {numericInput("p1", "Degrees of Freedom: ", min = 1, value=5)}
                  else if (input$dist == "f") {numericInput("p1", "Degrees of Freedom 1: ", min = 1, value=10)}
                  else if (input$dist == "unif") {numericInput("p1", "min: ", max=input$p2-0.1, value=0, step=0.1)}
                  else if (input$dist == "beta") {numericInput("p1", "alpha: ", min = 0.1, value=0.75, step=0.1)}
                  else if (input$dist == "exp") {numericInput("p1", "Rate: ", min = 0.1, value=1, step=0.1)}
                  else if (input$dist == "gamma") {numericInput("p1", "Shape: ", min = 0.1, value=1, step=0.1)}
                  else if (input$dist == "weibull") {numericInput("p1", "Shape: ", min = 0.1, value=1, step=0.1)}
                  else if (input$dist == "sn") {numericInput("p1", "Location: ", value=0)}
                  else if (input$dist == "binom") {numericInput("p1", "n: ", min = 1, value=10)}
                  else if (input$dist == "pois") {numericInput("p1", "Mean: ", min = 0.1, value=1, step=0.1)}
                  else if (input$dist == "geom") {numericInput("p1", "p: ", min = 0.1, max = 1, value=0.5, step=0.1)}
                  else if (input$dist == "nbinom") {numericInput("p1", "size: ", min = 1, value=10)}
                  else if (input$dist == "hyper") {numericInput("p1", "m (the number of white balls in the urn): ", min=1, value=10)}
                })
        
        output$p2 = renderUI(
                {
                  if (input$dist == "norm") {numericInput("p2", "Standard Deviation: ", min = 0.1, value = 1, step=0.1)}
                  else if (input$dist == "f") {numericInput("p2", "Degrees of Freedom 2: ", min=1, value=10)}
                  else if (input$dist == "unif") {numericInput("p2", "max: ", min=input$p1+0.1, value=1, step=0.1)}
                  else if (input$dist == "beta") {numericInput("p2", "beta: ", min = 0.1, value=0.75, step = 0.1)}
                  else if (input$dist == "gamma") {numericInput("p2", "Rate: ", min = 0.1, value=1, step=0.1)}
                  else if (input$dist == "weibull") {numericInput("p2", "Scale: ", min = 0.1, value=1, step=0.1)}
                  else if (input$dist == "sn") {numericInput("p2", "Scale: ", min = 0.1, value=1, step=0.1)}
                  else if (input$dist == "binom") {numericInput("p2", "p: ", min = 0.1, max = 1, value=.5, step=0.1)}
                  else if (input$dist == "nbinom") {numericInput("p2", "p: ", min = 0.1, max = 1, value=.5, step=0.1)}
                  else if (input$dist == "hyper") {numericInput("p2", "n (the number of black balls in the urn): ", min = 1, value=10)}
                })
                
        output$p3 = renderUI(
                {
                  if (input$dist == "sn") {numericInput("p3", "Skewnewss: ", min = -5000, max = 5000, value=1)}
                  else if (input$dist == "hyper") {numericInput("p3", "k (the number of balls drawn from the urn): ", min = 0, max = input$p1+input$p2, value = 10)}
                })

        output$prob <- renderUI({
          if(input$fun == "prob"){
            if (is.null(input$type)) return()
            switch(input$type,
                 "cont" = selectInput("prob", "Probability: ", choices = c(`< (less)` = "less", `> (greater)` = "greater"), selected = "less"),
                 "disc" = selectInput("prob", "Probability: ", choices = c(`= (equal)` = "equal", `<= (at most)` = "less", `> (greater)` = "greater"), selected = "equal")
          )}
        })
        
        output$a = renderUI({
          if(input$fun == "prob"){
            numericInput("a","a:", value = round(q.dist(0.5),3), min = q.dist(0), max = q.dist(1), step = ifelse(input$type=="cont",0.01,1))
          }
        })
        
        output$q = renderUI({
          if(input$fun == "quantile"){
            sliderInput("q", "Quantile", min = 0, max = 1, value = 0.5, step = 0.005)
          }
        })
        
        output$g.choice = renderUI({
          if(input$type == "cont"){
            radioButtons('g.choice', 'Show:', c("PDF" = "pdf", "CDF" = "cdf", "Hazard function" = "hazard"), selected= "pdf", inline = TRUE)
          } else radioButtons('g.choice', 'Show:', c("PDF" = "pdf", "CDF" = "cdf"), selected= "pdf", inline = TRUE)
        })

        output$area = renderText({
          if (input$fun=="prob" && input$prob=="less") {text = paste("P( X","<=",round(input$a,3),")","=",round(p.dist(input$a),4))}
          else if (input$fun=="prob" && input$prob=="greater") {text = paste("P( X",">",round(input$a,3),")","=",1-round(p.dist(input$a),4))}
          else if (input$fun=="prob" && input$prob=="equal") {text = paste("P( X","=",round(input$a,3),")","=",round(d.dist(input$a),4))}
          else if(input$fun == "quantile") {text = paste("prob( X","<",round(q.dist(input$q),3),")","=",round(p.dist(q.dist(input$q)),4))}
          text
        })

        CplotGraph <- function(bound,lower,upper,header) {
          if (bound<lower) {bound <- lower} else if (bound>upper) {bound <- upper}; ngrid=400;
          if (input$g.choice=="pdf") {func <- d.dist} else if (input$g.choice=="cdf") {func <- p.dist} else {func <- h.dist}
          if (input$prob=="less" || input$fun == "quantile") {
            Xs <- seq(lower,bound,length.out=ngrid*abs(bound-lower)/(upper-lower)); cord.x <- c(lower,Xs,bound);
          } else {
            Xs <- seq(bound,upper,length.out=ngrid*abs(upper-bound)/(upper-lower)); cord.x <- c(bound,Xs,upper) 
          }; cord.y <- c(0,func(Xs),0); X <- seq(lower,upper,length.out=ngrid);
          plot(X,func(X),ylab=input$g.choice,axes=FALSE,main=header,type="l",ylim=range(c(0,func(X))))
          polygon(cord.x,cord.y,col="lightblue", cex = 2)
          if (input$g.choice=="cdf") {segments(q.dist(0.0005),p.dist(bound),bound,col=2,lty=2,lwd=2)}
          places <- c(lower,bound,q.dist(0.5),upper)
          axis(1,at=places,labels=c(as.character(round(places[1:4],3)))); axis(2)
        }

        DplotGraph <- function(x,lower,upper,header) {
          if (input$g.choice=="pdf") {func <- d.dist} else {func <- p.dist}
          X <- lower:upper; Xs <- x; plot(X,func(X),ylab=input$g.choice,axes=FALSE,col=0,ylim=range(c(0,func(X))))
          if (input$prob=="less" || input$fun == "quantile") {Xs <- lower:trunc(x)} else if (input$prob=="greater") Xs <- (trunc(x)+1):upper;
          polygon(rep(X,each=4)+c(-1,-1,1,1)*.2,as.vector(t(cbind(0,matrix(rep(func(X),2),nc=2),0))), cex = 2)
          if ((x>=lower || input$prob=="greater") && (x<=upper || input$prob=="less")) 
            polygon(rep(Xs,each=4)+c(-1,-1,1,1)*.2,as.vector(t(cbind(0,matrix(rep(func(Xs),2),nc=2),0))),col="lightblue", cex = 2)
          if (input$g.choice=="cdf") {segments(q.dist(0),p.dist(x),x,col=2,lty=2,lwd=2)}
          places <- c(lower,x,q.dist(0.5),upper)
          axis(1,at=places,labels=c(as.character(round(places[1:4],3)))); axis(2)
        }
        
        output$plot = renderPlot({ 
          if (input$fun == "prob") {x <- input$a} else {x <- q.dist(input$q)}
          if (input$type == "cont") {CplotGraph(x, lower = q.dist(0.00125), upper = q.dist(0.99875), header = input$dist)}
          if (input$type == "disc") {DplotGraph(x, lower = q.dist(0.00125), upper = q.dist(0.99875), header = input$dist)}
        })

})
