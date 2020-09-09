# Author: Yubo Xiao
# Date: 09-Sep-2020 
# options(warn = -1)

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(PKPDsim)
library(mrgsolve)
library(deSolve)

##### env out of shiny
code_pk <- '
        $PARAM KA = 1, FR = 0.8, CL = 3, V2 = 20, V3 = 200, Q = 30

        $MAIN
        double KAi = KA*exp(ETA_KA);
        double CLi = CL*exp(ETA_CL);
        double V2i = V2*exp(ETA_V2);
        double ke = CLi/V2i;

        $INIT GUT = 0, CENT = 0, PERH = 0
        
        $ODE 
        double PKCONC = CENT/V2i*1000;
        dxdt_GUT = -KAi*GUT;
        dxdt_CENT = FR*KAi*GUT - CLi/V2i*CENT - Q/V2i*CENT + Q/V3*PERH;
        dxdt_PERH = Q/V2i*CENT - Q/V3*PERH;

        $OMEGA @annotated @block 
        ETA_CL: 0.04 : ETA on clearance
        ETA_V2: 0 0.09 : ETA on volume2
        ETA_KA: 0 0 0.09 : ETA on absorption rate constant
        
        $SIGMA 0
        
        $CAPTURE @annotated
        PKCONC: Concentration (ng/mL)
    '
mod_pk <- mcode("pk_model", code_pk)


##### shiny
shinyServer(function(input, output, session) {
  
  pkconc <- reactive({
    
    amt <- input$amt
    tau <- input$tau
    n <- input$n
    
    cl <-input$cl
    v2 <- input$v2
    v3 <- input$v3
    q <- input$q
    ka <- input$ka
    fr <- input$fr
    
    omega_cl <- (input$bsv_cl)^2
    omega_v2 <- (input$bsv_v2)^2
    omega_v3 <- (input$bsv_v3)^2
    omega_ka <- (input$bsv_ka)^2
    
    idv <- input$idv
    
    dos <- expand.ev(cmt = 1,
                      time = 0,
                      amt = amt,
                      ii = tau,
                      addl = n - 1,
                      CL = cl,
                      V2 = v2, 
                      V3 = v3, 
                      Q = q, 
                     KA = ka,
                     FR = fr,
                     ID = seq(1, idv, 1)) %>% 
      mutate(dose = amt) %>% 
      arrange(ID, time, addl, ii)
    
    out <- mod_pk %>%
      data_set(dos) %>%
      mrgsim(set.seed(909),
             end = tau*n,
             delta = 0.1)
    
    # browser()
    
    pkconc <- out@data %>%
      select(ID, time, PKCONC) %>% 
      mutate(subject = ID, time = time, conc = PKCONC) %>%
      distinct() %>%
      as.data.frame()
    # browser()
    
    pkconc
  })
  
  
  sumpkconc <- reactive({
    tmp <- pkconc() %>% group_by(time) %>% mutate(
      p_upper = quantile(conc, probs = c(input$p[2]), na.rm = TRUE),
      p_lower = quantile(conc, probs = c(input$p[1]), na.rm = TRUE),
      p_50 = quantile(conc, probs = c(0.5), na.rm = TRUE)) %>% 
      select(., subject, time, conc, p_upper, p_lower, p_50) %>% 
      ungroup() %>% 
      filter(subject == 1)
  })
  
  ymax <- reactive({
    round(max(pkconc()$conc, input$mec[2]))+0.5
  })
  
  
  output$idvpkplot <- renderPlot({
    
    pkconc <- pkconc()
    sumpkconc <- sumpkconc()
    
    ggplot(pkconc, aes(x = time, y = conc, group=subject)) + 
      # geom_line(alpha=0.2) +
      scale_x_continuous(limits = c(input$obs[1], input$obs[2]), breaks = seq(0,100*24,24)) +
      scale_y_continuous(limits = c(0,ymax())) +
      # geom_ribbon(data=sumpkconc, aes(x=time, ymin=p_lower, ymax=p_upper), linetype=0, alpha=0.2, fill="#002288") +
      geom_line(data=sumpkconc, aes(x=time, y=p_50), linetype="solid", size = 1, colour="black") +
      geom_hline(yintercept = input$mec[2], linetype="dashed", size = 1.5, colour = "darkred") +
      geom_hline(yintercept = input$mec[1], linetype="dashed", size = 1.5, colour = "darkblue") +
      theme_bw(base_rect_size = 1) +
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size = 21),
            axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
            axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) +
      xlab("Time (h)") + ylab("Concentration (ng/mL)")
  })
  
  output$poppkplot <- renderPlot({
    
    pkconc <- pkconc()
    sumpkconc <- sumpkconc()
    # browser()
    
    ggplot(pkconc, aes(x = time, y = conc, group=subject)) + 
      geom_line(alpha=0.2) +
      scale_x_continuous(limits = c(input$obs[1], input$obs[2]), breaks = seq(0,100*24,24)) +
      scale_y_continuous(limits = c(0,ymax())) +
      geom_ribbon(data=sumpkconc, aes(x=time, ymin=p_lower, ymax=p_upper), linetype=0, alpha=0.3, fill="#002288") +
      geom_line(data=sumpkconc, aes(x=time, y=p_50), linetype="solid", size = 1, colour="black") +
      geom_hline(yintercept = input$mec[2], linetype="dashed", size = 1.5, colour = "darkred") +
      geom_hline(yintercept = input$mec[1], linetype="dashed", size = 1.5, colour = "darkblue") +
      theme_bw(base_rect_size = 1) +
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size = 21),
            axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
            axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) +
      xlab("Time (h)") + ylab("Concentration (ng/mL)")
  })
  
})



