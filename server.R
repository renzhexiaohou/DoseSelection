# Author: Yubo Xiao
# Date: 11-Jul-2019 
# options(warn = -1)
# suppressWarnings(library(shiny))
# suppressWarnings(library(shinythemes))
# suppressWarnings(library(ggplot2))
# suppressWarnings(library(dplyr))
# suppressWarnings(library(PKPDsim))

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(PKPDsim)

##### env out of shiny
mod <- new_ode_model(code = "
                      CLi = CL * exp(eta1)
                      Vi = V * exp(eta2)
                      F1i =  exp(F1 + eta3) / (1 + exp(F1 + eta3))
                      dAdt[1] = -KA * A[1]
                      dAdt[2] = KA * A[1] - (CLi/Vi) * A[2]
                      ", declare_variables = c("CLi", "Vi", "F1i"),
                     obs = list(cmt = 2, scale = "V * exp(eta2)"),
                     dose = list(cmt = 1, bioav = "F1i"))  


##### shiny
shinyServer(function(input, output, session) {
  
  pkconc <- reactive({
    
    n <- input$n
    amt <- input$amt
    interval <- input$interval
    mec <- input$mec[1]
    
    cl <- input$cl
    v <- input$v
    ka <- input$ka
    f <- input$f
    
    eta1 <- 0
    eta2 <- 0
    eta3 <- 0
    
    omega1 <- input$omega1
    omega2 <- input$omega2
    omega3 <- input$omega3
    
    idv <- input$idv
    
    obs_lower <- input$obs[1]
    obs_upper <- input$obs[2]
    
    reg <- new_regimen(amt = amt, n = n, interval = interval, type="oral")
    
    dat <- sim_ode (
      seed = 6,
      ode = mod,
      regimen  = reg,
      parameters = list(eta1 = eta1, eta2 = eta2, eta3 = eta3,
                        CL = cl, V = v, KA = ka, F1 = f),
      t_obs = seq(obs_lower,obs_upper, 1),
      omega = c(omega1^2,
                0.05, omega2^2,
                0, 0, omega3^2),
      n = idv,
      omega_type = "normal",
      output_include = list("parameters" = TRUE, variables = TRUE), only_obs = TRUE
    )
    dat
  })
  
  
  sumpkconc <- reactive({
    tmp <- pkconc() %>% group_by(t) %>% mutate(
      p_upper = quantile(y, probs = c(input$p[2]), na.rm = TRUE),
      p_lower = quantile(y, probs = c(input$p[1]), na.rm = TRUE),
      p_50 = quantile(y, probs = c(0.5), na.rm = TRUE)) %>% 
      select(., id, t, y, p_upper, p_lower, p_50) %>% 
      ungroup() %>% 
      filter(id == 1)
  })
  
  ymax <- reactive({
    round(max(pkconc()$y, input$mec[2]))+0.5
  })
  
  output$proportion1 <- renderText({
    paste0(input$amt," mg benefits for average population ?")
  })
  output$proportion2 <- renderText({
    paste0(input$amt," mg benefits for the majority ?")
  })
  
  output$idvpkplot <- renderPlot({
    
    pkconc <- pkconc()
    sumpkconc <- sumpkconc()
    
    ggplot(pkconc, aes(x = t, y = y, group=id)) + 
      # geom_line(alpha=0.2) +
      scale_x_continuous(limits = c(input$obs[1], input$obs[2]), breaks = seq(0,100*24,24)) +
      scale_y_continuous(limits = c(0,ymax())) +
      # geom_ribbon(data=sumpkconc, aes(x=t, ymin=p_lower, ymax=p_upper), linetype=0, alpha=0.2, fill="#002288") +
      geom_line(data=sumpkconc, aes(x=t, y=p_50), linetype="solid", size = 1, colour="black") +
      geom_hline(yintercept = input$mec[2], linetype="dashed", size = 1.5, colour = "darkred") +
      geom_hline(yintercept = input$mec[1], linetype="dashed", size = 1.5, colour = "darkblue") +
      theme_bw(base_rect_size = 1) +
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size = 21),
            axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
            axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) +
      xlab("Time (h)") + ylab("Concentration (mg/mL)")
  })
  
  output$poppkplot <- renderPlot({
    
    pkconc <- pkconc()
    sumpkconc <- sumpkconc()
    
    ggplot(pkconc, aes(x = t, y = y, group=id)) + 
      geom_line(alpha=0.2) +
      scale_x_continuous(limits = c(input$obs[1], input$obs[2]), breaks = seq(0,100*24,24)) +
      scale_y_continuous(limits = c(0,ymax())) +
      geom_ribbon(data=sumpkconc, aes(x=t, ymin=p_lower, ymax=p_upper), linetype=0, alpha=0.3, fill="#002288") +
      geom_line(data=sumpkconc, aes(x=t, y=p_50), linetype="solid", size = 1, colour="black") +
      geom_hline(yintercept = input$mec[2], linetype="dashed", size = 1.5, colour = "darkred") +
      geom_hline(yintercept = input$mec[1], linetype="dashed", size = 1.5, colour = "darkblue") +
      theme_bw(base_rect_size = 1) +
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size = 21),
            axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
            axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) +
      xlab("Time (h)") + ylab("Concentration (mg/mL)")
  })
  
})



