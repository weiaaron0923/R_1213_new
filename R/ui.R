library(shiny)
# Define UI for application that plots random distributions
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Hello Shiny!"),
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("obs"
                ,
                "Number of observations:"
                , min = 0, max = 1000, value = 500)
  ),
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot")
  )
))

library(shiny)
# Define UI for application that plots random distributions
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Hello Shiny!"),
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("obs"
                ,
                "Number of observations:"
                , min = 0, max = 1000, value = 500)
  ),
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot")
  )
))

obs = 500
M = 170
S = 10
Coler = "blue"
dist = rnorm(obs,mean=M,sd=S)
hist(dist,col=Coler)

library(shiny)
# Define UI for application that plots random distributions
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Hello Shiny!"),
  # Sidebar with inputs for number of observations, mean, SD, and coler.
  sidebarPanel(
    sliderInput("obs"
                ,
                "Number of observations:"
                , min = 0, max = 1000, value = 500),
    numericInput("M"
                 ,
                 "Mean of this normal distribution:"
                 , min = -200, max = 200, value = 100),
    numericInput("S"
                 ,
                 "SD of this normal distribution:"
                 , min = 0, max = 50, value = 10),
    radioButtons("Color"
                 ,
                 "Select the color of histogram:"
                 , choices = c("Red" = "red"
                               ,
                               "Blue" = "blue"
                               ,
                               "Green" = "green"))
  ),
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot")
  )
))

Sig = 0.05 #two-sided significance level
p = 0.5 #The expected rate
X = 0.03 #Acceptable error range
Z = qnorm(1-Sig/2) #The Z value based on the two-sided significance level
n = p*(1-p)*Z^2/X^2 #Calculate the needed number of sample
n #Print the outcome
Samplesize = function(Sig,p,X) {
  Z = qnorm(1-Sig/2)
  n = p*(1-p)*Z^2/X^2
  n
}
Samplesize(Sig = 0.05,p = 0.5, X = 0.03) #Test 1
Samplesize(Sig = 0.05,p = 0.2, X = 0.03) #Test 2
Samplesize(Sig = 0.05,p = 0.2, X = 0.05) #Test 3

library(shiny)
# Define UI for application that calculate the needed sample size
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Sample size calculator"),
  # Sidebar with numeric inputs for significance level, expected rate, acceptable error range.
  sidebarPanel(
    numericInput("Sig"
                 ,
                 "Two-sided significance level:"
                 , min = 0, max = 1.000, value = 0.05),
    numericInput("p"
                 ,
                 "The expected rate:"
                 , min = 0, max = 1, value = 0.5),
    numericInput("X"
                 ,
                 "Acceptable error range:"
                 , min = 0, max = 1, value = 0.03)
  ),
  # Show the outcome
  mainPanel(
    h3(textOutput("Size"))
  )
))

Samplesize_OR = function(alpha,power,OR,r,p0,corr=FALSE) {
  Za=qnorm(1-alpha/2)
  Zb=qnorm(power)
  q0=1-p0
  p1=OR*p0/(OR*p0+q0)
  q1=1-p1
  pbar=(p0+r*p1)/(r+1)
  qbar=1-pbar
  n0=(Za*sqrt((r+1)*pbar*qbar)+Zb*sqrt(r*p0*q0+p1*q1))^2/(r*(p0-p1)^2)
  n1=r*n0
  n0corr=n0/4*(1+sqrt(1+(2*(1+r)/(n1*r*sqrt((p0-p1)^2)))))^2
  n1corr=r*n0corr
  if (corr==FALSE) {return(paste("Control:"
                                 ,round(n0)+1,
                                 "; Case:"
                                 ,round(n1)+1,
                                 "; Total:"
                                 ,round(n0)+round(n1)+2,sep=""))}
  if (corr==TRUE) {return(paste("Control:"
                                ,round(n0corr)+1,
                                "; Case:"
                                ,round(n1corr)+1,
                                "; Total:"
                                ,round(n0corr)+round(n1corr)+2,sep=""))}
}
Samplesize_OR(alpha=0.05,power=0.8,OR=1.5,r=1,p0=0.2,corr=FALSE)
Samplesize_OR(alpha=0.05,power=0.8,OR=1.5,r=1,p0=0.2,corr=TRUE)

library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Sample size for Case Control study"),
  sidebarPanel(
    numericInput("alpha"
                 ,
                 "Two-sided significance level:"
                 , min = 0.001, max = 0.999, value = 0.05),
    numericInput("power"
                 ,
                 "Power:"
                 , min = 0.001, max = 0.999, value = 0.8),
    numericInput("OR"
                 ,
                 "Smallest difference of clinical/biological importance:"
                 , min = 0.01, max = 100, value = 1.5),
    numericInput("r"
                 ,
                 "The ratio of Case/Control:"
                 , min = 0.01, max = 100, value = 1),
    numericInput("p0"
                 ,
                 "Proportion of controls with exposures:"
                 , min = 0.001, max = 0.999, value = 0.2)
  ),
  mainPanel(
    h3(textOutput("text1")),
    h4(textOutput("text2")),
    h3(textOutput("text3")),
    h4(textOutput("text4"))
  )
))

library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Generating distribution"),
  sidebarPanel(
    selectInput("method"
                ,
                "Choose distribution:"
                , choices=c("Normal"="norm"
                            ,
                            "Student t"="st")),
    helpText("Setting parameter(s) for distribution model"),
    conditionalPanel(condition="input.method=='norm'"
                     ,
                     numericInput(inputId="mu"
                                  , label="mean"
                                  , value=0),
                     numericInput(inputId="sd"
                                  , label="standard deviation"
                                  , value=1, min=0)
    ),
    conditionalPanel(condition="input.method=='st'"
                     ,
                     numericInput(inputId="df"
                                  , label="Df"
                                  , value=10, min=1)
    ),
    sliderInput(inputId="obs"
                ,
                label="Number of observations:"
                ,
                min = 1, max = 1000, value = 500)
  ),
  mainPanel(
    plotOutput("distPlot")
  )
))

library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Sample size calculator"),
  sidebarPanel(
    selectInput("method"
                ,
                "Choose a method:"
                , choices=c("Rate"="rate"
                            ,
                            "Case Control Study"="cc")),
    conditionalPanel(condition="input.method=='rate'"
                     ,
                     numericInput("Sig"
                                  ,
                                  "Two-sided significance level:"
                                  , min = 0, max = 1.000, value = 0.05),
                     numericInput("p"
                                  ,
                                  "The expected rate:"
                                  , min = 0, max = 1, value = 0.5),
                     numericInput("X"
                                  ,
                                  "Acceptable error range:"
                                  , min = 0, max = 1, value = 0.03)
    ),
    conditionalPanel(condition="input.method=='cc'"
                     ,
                     numericInput("alpha"
                                  ,
                                  "Two-sided significance level:"
                                  , min = 0.001, max = 0.999, value = 0.05),
                     numericInput("power"
                                  ,
                                  "Power:"
                                  , min = 0.001, max = 0.999, value = 0.8),
                     numericInput("OR"
                                  ,
                                  "Smallest difference of clinical/biological importance:"
                                  , min = 0.01, max = 100, value = 1.5),
                     numericInput("r"
                                  ,
                                  "The ratio of Case/Control:"
                                  , min = 0.01, max = 100, value = 1),
                     numericInput("p0"
                                  ,
                                  "Proportion of controls with exposures:"
                                  , min = 0.001, max = 0.999, value = 0.2)
    )
  ),
  mainPanel(
    h3(textOutput("text1")),
    h4(textOutput("text2")),
    h3(textOutput("text3")),
    h4(textOutput("text4"))
  )
))


