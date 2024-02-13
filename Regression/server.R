library(ggfortify); library(ggplot2); library(shiny); library(car); library(MASS); library(alr3) # library(plotly)

options(shiny.sanitize.errors = FALSE)

shinyServer(function(input, output, clientData, session) {
  
  load("data.Rda"); data <- NULL; 
  
  panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
  {
    usr <- par("usr"); on.exit(par(usr)) 
    par(usr = c(0, 1, 0, 1)) 
    r <- (cor(x, y)) 
    txt <- format(c(r, 0.123456789), digits=digits)[1] 
    txt <- paste(prefix, txt, sep="") 
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
    
    test <- cor.test(x,y) 
    # borrowed from printCoefmat
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                     symbols = c("***", "**", "*", ".", " ")) 
    
    text(0.5, 0.5, txt, cex = cex * abs(r)) 
    text(.8, .8, Signif, cex=cex, col=2) 
  }
  
  observeEvent(input$Panel, {
    updateSelectInput(session,"resp", choices = names(data));  
    updateSelectizeInput(session,"pred", choices = names(data)[-which(names(data)==input$resp)])
  }); 
  
  simulate <- function() {if (memory.size()>700) gc();
    set.seed(input$n*input$sigma); #set.seed(10)
    if (input$model %in% c("lin","orth")) X <- cbind(1,matrix(rnorm(input$n*(input$p-1)),nrow=input$n)*input$sig.x)
    if (input$model == "orth") X <- svd(X)$u*input$n*input$sig.x
    if (input$model == "poly") {X <- cbind(1,rnorm(input$n,1,1)*input$sig.x); if (input$p>2) for (j in 2:(input$p-1)) X <- cbind(X,X[,2]^(3*j))}
    beta <- c(input$b0,input$b1); if (input$p>2) beta[3] <- input$b2; if (input$p>3) beta[4] <- input$b3; if (input$p>4) beta[5] <- input$b4
    eps <- rnorm(input$n,0,input$sigma); y <- X%*%beta+eps; if (input$model == "orth") data <- data.frame(y,X) else data <- data.frame(y,X[,-1])
    return(data)
  }
  
  output$b2 <- renderUI({if (input$f.choice!="sim" || input$p<3) return();
    sliderInput("b2", HTML("b2"), min = -10, max = 10, value = 1, step = .1, width="210px")
  })
  
  output$b3 <- renderUI({if (input$f.choice!="sim" || input$p<4) return();
    sliderInput("b3", HTML("b3"), min = -10, max = 10, value = 1, step = .1, width="210px")
  })
  
  output$b4 <- renderUI({if (input$f.choice!="sim" || input$p<5) return();
    sliderInput("b4", HTML("b4"), min = -10, max = 10, value = 1, step = .1, width="210px")
  })
  
  output$simul <- renderUI({if (input$f.choice!="sim") return();
    actionButton('simul', paste('Simulate Data'))
  })
  
  run_simul <- eventReactive(input$run.fpca, {if (memory.size()>700) gc();
    return(simulate())
  })
  
  output$s.choice <- renderUI({
    if (input$f.choice!="server") return(); 
    s.choices <- 1:length(data.serv); names(s.choices) <- names(data.serv); 
    selectInput("s.choice","Select a file from server: ", choices = s.choices, width="250px"); 
  })
  
  output$file <- renderUI({
    if (input$f.choice!="upload") return();
    fileInput('file', 'Choose CSV File', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
  })
  output$sep <- renderUI({if (input$f.choice!="upload") return(); radioButtons('sep', 'Separator', c(","=',', ":"=':', ";"=';', Tab='\t'), ',', inline = TRUE)})
  output$header <- renderUI({if (input$f.choice!="upload") return(); checkboxInput('header', 'Header', TRUE)})
  
  output$ts.selected = renderText({
    if (input$f.choice=="upload" && is.null(input$file)) return("<b>Select a 'csv' file that contain the variables in its columns</b>")
    if (input$f.choice=="upload") {data <- as.matrix(read.table(input$file$datapath, header=input$header, sep=input$sep))
    } else if (input$f.choice=="server") {i <- as.numeric(input$s.choice); data <- data.serv[[i]]
    } else if (input$f.choice=="sim") {data <- matrix(0,nrow=input$n,ncol=(input$p-1))}
    text <- paste("<b>",ncol(data),"Variables of length",nrow(data),"</b>"); return(text)
  })
  
  output$data <- renderTable({if (memory.size()>700) gc();
    if ((input$f.choice=="upload" && is.null(input$file)) || (input$f.choice=="sim" && !length(input$model))) return();
    if (input$f.choice=="upload") {data <<- as.data.frame(read.table(input$file$datapath, header=input$header, sep=input$sep));
    } else if (input$f.choice=="server") {i <- as.numeric(input$s.choice); data <<- data.serv[[i]];
    } else {simul <- simulate(); data <<- simul}; if (is.null(colnames(data))) {colnames(data) <<- c("y",paste("fn",1:(ncol(data)-1)))};
    return(head(as.matrix(data[,1:min(9,ncol(data))]),15))
  })
  
  output$data.plot = renderPlot({if (memory.size()>700) gc();
    if ((input$f.choice=="upload" && is.null(input$file)) || (input$f.choice=="sim" && !length(input$model))) return();
    if (input$f.choice=="server") {i <- as.numeric(input$s.choice); fname <- names(data.serv)[i]; data <- data.serv[[i]]} 
    else if (input$f.choice=="upload") {fname <- input$file$name; data <- as.data.frame(read.table(input$file$datapath, header=input$header, sep=input$sep))} 
    else {fname <- "Simulation"; simul <- simulate(); data <- simul};
    if (ncol(data)>1) pairs(data, upper.panel=panel.smooth, lower.panel=panel.cor) else {
      plot(data[2:1], asp=input$asp); abline(h=0,v=0,lty=2); 
      if (input$f.choice=="sim" && input$model == "orth") abline(a=0,b=lm(data[,1]~data[,2]-1)$coeff,col=3) else abline(lm(data),col=3); 
      if (input$f.choice=="sim") if (input$model != "orth") abline(a=input$b0,b=input$b1,col=2) else abline(a=0,b=input$b0,col=2);
    }
  })
  
  output$resp <- renderUI({
    selectizeInput('resp', 'Response variable', choices = names(data))
  })
  
  output$pred <- renderUI({
    selectizeInput('pred', 'Predictors', choices = names(data)[-which(names(data)==input$resp)], multiple = TRUE, options = list(maxItems = 5))
  })
  
  output$x0 <- renderUI({
    if (length(input$pred)<2) x0 <- rep(0,length(input$pred)) else x0 <- paste0("c(",paste(rep(0,length(input$pred)),collapse=","),")")
    textInput("x0","x0: ", value=x0, width="210px")
  })
  
  output$regr.desc = renderPlot({if (memory.size()>700) gc(); 
    form <- as.formula(paste(input$resp, paste(input$pred, collapse=ifelse("in.int" %in% input$glm.int, " * ", " + ")), sep=" ~ ")); ind <- c(input$resp,input$pred)
    if (input$scaling=="unL") data <- as.data.frame(t(t(scale(data[,ind],scale = F))/sqrt(apply(scale(data[,ind],scale = F)^2,2,sum))))
    else if (input$scaling=="unN") data <- as.data.frame(scale(data[,ind]))
    else data <- data[,ind]; if (sum(c("logm","linm") %in% input$glm.int) && is.factor(data[,input$resp])) data[,input$resp] <- as.numeric(data[,input$resp]) - 1
    if ((input$model == "orth" && input$f.choice=="sim") || (input$scaling!="none")) {form <- as.formula(paste(paste(input$resp, paste(input$pred, collapse=ifelse("in.int" %in% input$glm.int, " * ", " + ")), sep=" ~ "),"-1 "))}
    X <- model.matrix(form, data = data);
    fit = lm(form, data = data); if ("logm" %in% input$glm.int) fit2 = glm(form, data = data, family=binomial)
    regRes <- list(); tmp.pred <- list(); tmp.x0 <- matrix(eval(parse(text=input$x0)),nr=length(input$pred))
    for (i in 1:length(input$pred)) {tmp.pred[[input$pred[i]]] <- tmp.x0[i,]}
    if ("conf" %in% input$sh.int) regRes[["CI.E(y|x0)"]] <- format(predict(fit,data.frame(tmp.pred),interval = "confidence", level=input$level)[,1:3], digits=4)
    if ("pred" %in% input$sh.int) regRes[["PI.y|x0"]] <- format(predict(fit,data.frame(tmp.pred),interval = "prediction", level=input$level)[,1:3], digits=4)
    if (nrow(tmp.x0)<ncol(X)) tmp.x0 <- rbind(1,tmp.x0); tmp.x0 <- c(tmp.x0,rep(0,ncol(X)-length(tmp.x0))); regRes[["h00"]] <- try(format(diag(t(tmp.x0)%*%solve(t(X)%*%X)%*%(tmp.x0)), scientific=TRUE, digits=3))
    output$regRes <- renderPrint({ regRes }); 
    if (input$reg.text=="summary") {
      summar <- list(); if ("linm" %in% input$glm.int) summar$lm <- summary(fit); if ("logm" %in% input$glm.int) summar$glm <- summary(fit2);
      output$RegRes <- renderPrint({summar})
    } else if (input$reg.text=="anova") {
      anov <- list(); if ("linm" %in% input$glm.int) anov$lm <- anova(fit); if ("logm" %in% input$glm.int) anov$glm <- anova(fit2);
      output$RegRes <- renderPrint({anov})
    } else if (input$reg.text=="infM") output$RegRes <- renderPrint({list(infM=influence.measures(fit),PRESS=sum(rstandard(fit, type="pred")^2))})
    else if (input$reg.text=="res") {
      res <- residuals(fit); stan_res<- res/sigma(fit); stud_res <- rstandard(fit); PRESS_res <- rstandard(fit, type="pred"); Rstu_res <- studres(fit)
      output$RegRes <- renderPrint({format(data.frame(res,stan_res,stud_res,PRESS_res,Rstu_res),digits=3)})
    } else if (input$reg.text=="xtxi") {
      out <- list("X'X"=format(data.frame(t(X)%*%X), scientific=TRUE, digits=3), "inv(X'X)"=format(data.frame(solve(t(X)%*%X)), scientific=TRUE, digits=3))
      out[["Condition # of X'X"]] = format(kappa(t(X)%*%X), scientific=TRUE, digits=3);
      if (ncol(X)>2) out[["VIF"]] = round(vif(fit),2)
      output$RegRes <- renderPrint({out})
    } else output$RegRes <- NULL
    
    if (input$reg.plot=="reg.lin") {
      d = data.frame(data[complete.cases(data),], predict(fit, interval=c("confidence"), level=input$level), 
                     predict(fit, interval=c("prediction"), level=input$level)[,2:3]); 
      pl <- ggplot(d,aes_string(x=input$pred[1],y=input$resp)) + geom_point() 
      if ("linm" %in% input$glm.int) pl <- pl + geom_line(aes(y = fit), color='blue', size=2) 
      if ("logm" %in% input$glm.int) pl <- pl + geom_line(aes(y = predict(fit2,type = "response")), color='red', size=2) 
      if (length(input$pred) && length(input$sh.int) && "linm" %in% input$glm.int) { 
        if ("conf" %in% input$sh.int) pl <- pl + geom_ribbon(aes(ymin=lwr,ymax=upr,fill='confidence'),alpha=0.3)
        if ("pred" %in% input$sh.int) pl <- pl + geom_ribbon(aes(ymin=lwr.1,ymax=upr.1,fill='prediction'),alpha=0.3)
        if (input$sh.int %in% c("conf","pred")) pl <- pl + scale_fill_manual('Interval', values = c('green', 'yellow'))
      }
      # geom_smooth(method="lm", aes(fill='confidence'), alpha=0.3, level=input$level) +
      # geom_smooth(method="lm", se=FALSE, color='blue') +
      # geom_smooth(alpha=0.3, level=input$level) 
      if (input$asp) pl <- pl + coord_equal();      pl
    } else if (input$reg.plot=="par.reg") { avPlots(fit)
    } else if (input$reg.plot=="res.fit") { plot(fit$fitted,fit$resid, pch=20, cex=.3)
    } else if (input$reg.plot=="boxcox") { 
      if (min(data[,input$resp])<=0) data[,input$resp] <- data[,input$resp]-min(data[,input$resp])+.1
      boxcox(form, data = data)
    } else { autoplot(fit, which = 1:6, ncol = 3, label.size = 3) }
  })
})
