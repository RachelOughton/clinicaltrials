library(shiny)

### UI

ui <- fluidPage(
  sliderInput(inputId="thresh", label = "Threshold", min=0, max=1, value=0.5, step=0.01),
  div(style="display:flex;",
      plotOutput("Distplot",width = "49vw"),
      plotOutput("ROCplot",width = "49vw")
  )
)

server <- function(input, output){
  ROC_dist_plot = function(
    alpha0,   # mean and SD for X=0
    beta0,
    alpha1,  # mean and SD for X=1
    beta1,
    thresh  # threshold for this plot
  ){
    thresh_vec = seq(0,1, by=0.01)
    nt = length(thresh_vec)
    
    T = thresh_vec
    Spec = sapply(1:nt, function(i){pbeta(thresh_vec[i], shape1 = alpha0, shape2=beta0)})
    Sens = sapply(1:nt, function(i){1-pbeta(thresh_vec[i], shape1 = alpha1, shape2=beta1)})
    dis_0 = sapply(1:nt, function(i){dbeta(thresh_vec[i], shape1 = alpha0, shape2=beta0)})
    dis_1 = sapply(1:nt, function(i){dbeta(thresh_vec[i], shape1 = alpha1, shape2=beta1)})
    
    sum_df = data.frame(T=T, Spec=Spec, Sens=Sens, dis_0=dis_0, dis_1 = dis_1)
    sum_df$oneMspec = 1-sum_df$Spec
    
    subDF_0 = sum_df[sum_df$T <= thresh,]
    subDF_1 = sum_df[sum_df$T >= thresh,]
    
    
    dist_plot = ggplot() + xlim(0,1) +
      geom_function(fun = dbeta, args=list(shape1 = alpha0, shape2 = beta0), 
                    linewidth=0.75, col="turquoise4")  +
      geom_function(fun = dbeta, args=list(shape1 = alpha1, shape2 = beta1), 
                    linewidth=0.75, col="orangered2") +
      annotate("text", x=c(0.2,0.8), y=c(0.25, 0.15), label = c("X=0", "X=1"), 
               col = c("turquoise4", "orangered2"), size=4) +
      geom_vline(aes(xintercept = thresh), linewidth=0.75) +
      geom_ribbon(data = subDF_0, 
                  aes(x=T, ymin=0, ymax=dis_0), fill="turquoise3", alpha=0.3)+
      geom_ribbon(data = subDF_1, 
                  aes(x=T, ymin=0, ymax=dis_1), fill="orangered", alpha=0.3)+
      xlab("Fitted value p-hat") +
      ggtitle(sprintf("Threshold = %g, Sensitivity = %g, Specificity = %g",
                      thresh, 
                      round(sum_df$Sens[sum_df$T == thresh],3), 
                      round(sum_df$Spec[sum_df$T == thresh],3))) +
      theme_bw()
    dist_plot
  }
  
  ROC_allT_plot = function(
    alpha0,   # mean and SD for X=0
    beta0,
    alpha1,  # mean and SD for X=1
    beta1,
    highlight = NULL,
    cutoff_tail = FALSE
  ){
    thresh_vec = seq(0,1, by=0.01)
    nt = length(thresh_vec)
    
    T = thresh_vec
    Spec = sapply(1:nt, function(i){pbeta(thresh_vec[i], shape1 = alpha0, shape2=beta0)})
    Sens = sapply(1:nt, function(i){1-pbeta(thresh_vec[i], shape1 = alpha1, shape2=beta1)})
    dis_0 = sapply(1:nt, function(i){dbeta(thresh_vec[i], shape1 = alpha0, shape2=beta0)})
    dis_1 = sapply(1:nt, function(i){dbeta(thresh_vec[i], shape1 = alpha1, shape2=beta1)})
    
    sum_df = data.frame(T=T, Spec=Spec, Sens=Sens, dis_0=dis_0, dis_1 = dis_1)
    sum_df$oneMspec = 1-sum_df$Spec
    
    ## Calculate AUC using x and y coords:
    AUC = 0
    for (i in 1:(nt-1)){
      # seems wrong way around but T is going from right to left
      x_diff_i = sum_df$oneMspec[i] - sum_df$oneMspec[i+1] 
      height_i = (sum_df$Sens[i] + sum_df$Sens[i+1])/2
      #    height_i = sum_df$Sens[i]
      area_i = height_i * x_diff_i
      AUC = AUC + area_i
    }
    
    sum_df_plot = sum_df
    if (cutoff_tail == TRUE){
      sum_df_plot = sum_df[(sum_df$Sens<=0.999)&(sum_df$Sens>0.001),]
    }
    
    plot = ggplot(data=sum_df_plot, aes(x=oneMspec, y=Sens, col=T)) + geom_path() + 
      xlab("1 - Specificity") + ylab("Sensitivity") + xlim(0,1) + ylim(0,1) +
      scale_color_gradient2(
        low = "orange", 
        mid = "magenta",
        high = "blue",
        midpoint = mean(sum_df_plot$T)
      ) + geom_abline(slope=1, lty=2) +
      ggtitle(sprintf("AUC = %g (3 s.f.)", round(AUC, digits=3)))
    if(is.numeric(highlight)){
      plot = plot + geom_point(data= sum_df_plot[sum_df_plot$T == highlight,], aes(x=oneMspec, y=Sens), size=2, col="black") + theme_bw() + theme(legend.position="none")
    } 
    plot
    
  }
  
  
  output$Distplot = renderPlot(
    ROC_dist_plot(alpha0=1.3, beta0=5, alpha1=5, beta1=1.3, thresh=input$thresh)
  )
  output$ROCplot = renderPlot(
    ROC_allT_plot(alpha0=1.3, beta0=5, alpha1=5, beta1=1.3, highlight = input$thresh)
  )
  
}


shinyApp(ui = ui, server = server)