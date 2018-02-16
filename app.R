# This code implements an automated GUI risk score calculator , using the Shiny package in R.
# install the shiny app before using

library(DT)
library(shiny)

specify.decimal <- function(t, nsmall=3, zero=T) {
  if (round(t, nsmall) == 0 & !zero) {
    return(signif(t, 1))
  }
  return(format(round(t, nsmall), nsmall=nsmall))
}


ui = navbarPage("ASCVD",
                
                tabPanel("Risk Calculator",
                         
                         fluidPage(
                           
                           titlePanel("Risk Equations for Atherosclerotic CVD 10-year incidence"),
                           
                           fluidRow(
                             column(width=6,
                                    numericInput("age", label = "Age (years)", value = 60),
                                    selectInput("sex", label = "Sex", choices = list(Male=0, Female=1)),
                                    radioButtons("black", label = "Black?", choices = list(No=0, Yes=1), inline = TRUE),
                                    radioButtons("dm", label = "Diabetes mellitus?", choices = list(No=0, Yes=1), inline = TRUE),
                                    radioButtons("cursmoke", label = "Currently smoking tobacco?", choices = list(No=0, Yes=1), inline = TRUE)
                             ),
                             column(width=6,
                                    numericInput("totchol", label = "Total cholesterol (mg/dL)", value = 190),
                                    numericInput("hdlc", label = "HDL cholesterol (mg/dL)", value = 50),
                                    numericInput("sysbp", label = "Systolic blood pressure (mm Hg)", value = 140),
                                    radioButtons("rxbp", label = "On blood pressure treatment?", choices = list(No=0, Yes=1), inline = TRUE)
                             )
                           ),
                           
                           hr(),
                           
                           fluidRow(
                             column(width=6,
                                    h4("Estimated 10-yr risk of ASCVD:", align = "center")
                             ),
                             column(width=6,
                                    h4(textOutput("ascvd_risk"), align = "center")
                             )
                           ),
                           
                           
                           hr(),
                           "Note: This calculator is intended for informational purposes only, and has not been prospectively 
                           evaluated for impact on clinical practice or patient outcomes. Calculations must be re-checked and 
                           should not be used alone to guide patient care, nor should they substitute for clinical judgment.
                           Contact: Sanjay Basu, basus@stanford.edu
                           
                           "
                           
                         )
                         
                ),
                
                tabPanel("Summary Statistics",
                         
                         h4("Risk model was derived from ARIC, JHS, MESA, CHS, CARDIA, and the Framingham Offspring Study. Summary statistics for the pooled development and validation cohorts are presented here:"),
                         
                         br(),
                         
                         fluidRow(
                           column(width=12,
                                  align="center",
                                  tableOutput('summary')
                           )
                         )
                         
                ),
                
                
                tabPanel("Disclaimers",
                         
                         h5("This website contains clinical tools and data intended for use by healthcare professionals. These tools do not give professional advice; physicians and other healthcare professionals who use these tools or data should exercise their own clinical judgment as to the information they provide. Consumers who use the tools or data do so at their own risk. Individuals with any type of medical condition are specifically cautioned to seek professional medical advice before beginning any sort of health treatment. For medical concerns, including decisions about medications and other treatments, users should always consult their physician or other qualified healthcare professional.
                            
                            Our content developers have carefully tried to create its content to conform to the standards of professional practice that prevailed at the time of development. However, standards and practices in medicine change as new data become available and the individual medical professional should consult a variety of sources.
                            
                            The contents of the Site, such as text, graphics and images are for informational purposes only. We do not recommend or endorse any specific tests, physicians, products, procedures, opinions, or other information that may be mentioned on the Site.
                            
                            While information on this site has been obtained from sources believed to be reliable, neither we nor our content providers warrant the accuracy of the information contained on this site.
                            
                            We do not give medical advice, nor do we provide medical or diagnostic services. Medical information changes rapidly. Neither we nor our content providers guarantee that the content covers all possible uses, directions, precautions, drug interactions, or adverse effects that may be associated with any therapeutic treatments.
                            
                            Your reliance upon information and content obtained by you at or through this site is solely at your own risk. Neither we nor our content providers assume any liability or responsibility for damage or injury (including death) to you, other persons or property arising from any use of any product, information, idea or instruction contained in the content or services provided to you.
                            
                            We cannot and will not be held legally, financially, or medically responsible for decisions made using these calculators, equations, and algorithms, and this Site is for the use of medical professionals only."),
                         
                         br(),
                         
                         h5("This calculator was prepared using ARIC, CHS, CARDIA, and Framingham Study research materials obtained from the NHLBI Biologic Specimen and Data Repository Information Coordinating Center and does not necessary reflect the opinions or views of the CHS, CARDIA, Framingham Study, or the NHLBI."),
                         
                         br(),
                         h5("Calculations reported here were supported by the National Institute On Minority Health And Health Disparities of the National Institutes of Health under Award Numbers DP2MD010478 and U54MD010724, and by the National Heart, Lung and Blood Institute of the National Institutes of Health under Award Number K08HL121056. The content is solely the responsibility of the authors and does not necessarily represent the official views of the National Institutes of Health. Steve Yadlowsky is supported by the Stanford University Graduate Fellowship."),
                         
                         br(),
                         h5("The Jackson Heart Study is supported by contracts HHSN268201300046C, HHSN268201300047C, HHSN268201300048C, HHSN268201300049C, HHSN268201300050C from the National Heart, Lung, and Blood Institute and the National Institute on Minority Health and Health Disparities, with additional support from the National Institute on Biomedical Imaging and Bioengineering. "),
                         
                         br(),
                         h5("The MESA Study is supported by contracts HHSN268201500003I, N01-HC-95159, N01-HC-95160, N01-HC-95161, N01-HC-95162, N01-HC-95163, N01-HC-95164, N01-HC-95165, N01-HC-95166, N01-HC-95167, N01-HC-95168 and N01-HC-95169 from the National Heart, Lung, and Blood Institute, and by grants UL1-TR-000040, UL1-TR-001079, and UL1-TR-001420 from NCATS.  The authors thank the other investigators, the staff, and the participants of the MESA study for their valuable contributions.  A full list of participating MESA investigators and institutions can be found at http://www.mesa-nhlbi.org.")
                         
                         
                         
                         )
                
                
                
                )

server = function(input, output) {
  # Access input values with input$*
  # Save output objects to output$*
  # Build objects with render*({ code })
  ascvd_estimator = reactive({
    female.risk <- 1.0 / (1.0 + exp( - (
      -12.823110 +
        0.106501 * as.numeric(input$age) +
        0.432440 * as.numeric(input$black) +
        0.000056 * (as.numeric(input$sysbp) ^ 2) +
        0.017666 * as.numeric(input$sysbp) +
        0.731678 * as.numeric(input$rxbp) +
        0.943970 * as.numeric(input$dm) +
        1.009790 * as.numeric(input$cursmoke) +
        0.151318 * (as.numeric(input$totchol) / as.numeric(input$hdlc)) +
        -0.008580 * as.numeric(input$age) * as.numeric(input$black) +
        -0.003647 * as.numeric(input$sysbp) * as.numeric(input$rxbp) +
        0.006208 * as.numeric(input$sysbp) * as.numeric(input$black) +
        0.152968 * as.numeric(input$black) * as.numeric(input$rxbp) +
        -0.000153 * as.numeric(input$age) * as.numeric(input$sysbp) +
        0.115232 * as.numeric(input$black) * as.numeric(input$dm) +
        -0.092231 * as.numeric(input$black) * as.numeric(input$cursmoke) +
        0.070498 * as.numeric(input$black) * (as.numeric(input$totchol) / as.numeric(input$hdlc)) +
        -0.000173 * as.numeric(input$black)  * as.numeric(input$sysbp) * as.numeric(input$rxbp) +
        -0.000094 * as.numeric(input$age) * as.numeric(input$sysbp) * as.numeric(input$black)
    )))
    male.risk <- 1.0 / (1.0 + exp( - (
      -11.679980 +
        0.064200 * as.numeric(input$age) +
        0.482835 * as.numeric(input$black) +
        -0.000061 * (as.numeric(input$sysbp) ^ 2) +
        0.038950 * as.numeric(input$sysbp) +
        2.055533 * as.numeric(input$rxbp) +
        0.842209 * as.numeric(input$dm) +
        0.895589 * as.numeric(input$cursmoke) +
        0.193307 * (as.numeric(input$totchol) / as.numeric(input$hdlc)) +
        -0.014207 * as.numeric(input$sysbp) * as.numeric(input$rxbp) +
        0.011609 * as.numeric(input$sysbp) * as.numeric(input$black) +
        -0.119460 * as.numeric(input$rxbp) * as.numeric(input$black) +
        0.000025 * as.numeric(input$age) * as.numeric(input$sysbp) +
        -0.077214 * as.numeric(input$black) * as.numeric(input$dm) +
        -0.226771 * as.numeric(input$black) * as.numeric(input$cursmoke) +
        -0.117749 * (as.numeric(input$totchol) / as.numeric(input$hdlc)) * as.numeric(input$black) +
        0.004190 * as.numeric(input$black) * as.numeric(input$rxbp) * as.numeric(input$sysbp) +
        -0.000199 * as.numeric(input$black) * as.numeric(input$age) * as.numeric(input$sysbp)
    )))
    paste(
      specify.decimal(100*(ifelse(as.numeric(input$sex) == 1, female.risk, male.risk)), nsmall=1, zero=F),
      "%", sep="")
  })
  
  output$ascvd_risk = renderText({ ascvd_estimator() })
  
  output$summary = renderTable({
    df <- as.data.frame(rbind(
      c("", "Black", "", "", "", "White", "", "", ""),
      c("", "Development", "", "Validation", "", "Development", "", "Validation", ""),
      c("", "Mean", "Std. dev.", "Mean", "Std. dev.", "Mean", "Std. dev.", "Mean", "Std. dev."),
      c("Women", "n = 3765", "", "n = 944", "", "n = 7354", "", "n = 1816", ""),
      c("Age Range", "(40, 79)", "", "(40, 79)", "", "(40, 79)", "", "(40, 79)", ""),
      c("Age (yrs)", "55.5", "9.4", "55.7", "9.4", "57.6", "9.8", "57.3", "9.7"),
      c("Total Cholesterol (mg/dl)", "206.1", "42.2", "206.4", "41.1", "216.0", "40.6", "213.9", "39.5"),
      c("HDL Cholesterol (mg/dL)", "56.9", "16.1", "57.1", "15.5", "58.5", "16.6", "58.5", "16.4"),
      c("Untreated SBP (mmHg)", "124.1", "19.0", "122.6", "18.1", "118.3", "18.6", "118.2", "18.1"),
      c("Treated SBP (mmHg)", "134.6", "21.5", "134.4", "21.3", "133.9", "19.9", "133.5", "19.7"),
      c("BP Meds (%)", "45.26", "", "47.03", "", "20.44", "", "20.81", ""),
      c("Current Smoker (%)", "18.22", "", "16.10", "", "22.57", "", "21.81", ""),
      c("Diabetes (%)", "16.89", "", "19.28", "", "6.23", "", "6.11", ""),
      c("10yr ASCVD incidence per 1,000 person-yrs", "6.59", "", "6.78", "", "6.51", "", "6.44", ""),
      c("", "Development", "", "Validation", "", "Development", "", "Validation", ""),
      c("", "Mean", "Std. dev.", "Mean", "Std. dev.", "Mean", "Std. dev.", "Mean", "Std. dev."),
      c("Men", "n = 2503", "", "n = 623", "", "n = 6261", "", "n = 1582", ""),
      c("Age Range", "(40, 79)", "", "(40, 79)", "", "(40, 79)", "", "(40, 79)", ""),
      c("Age (yrs)", "56.1", "9.4", "55.9", "9.9", "57.0", "9.4", "57.2", "9.5"),
      c("Total Cholesterol (mg/dl)", "199.3", "41.8", "198.8", "42.4", "205.5", "37.9", "207.6", "38.7"),
      c("HDL Cholesterol (mg/dL)", "48.4", "14.9", "48.0", "14.4", "44.3", "12.6", "44.2", "12.2"),
      c("Untreated SBP (mmHg)", "127.1", "19.2", "127.2", "18.8", "121.7", "17.0", "121.9", "16.8"),
      c("Treated SBP (mmHg)", "134.1", "19.0", "134.8", "18.9", "132.4", "19.9", "133.3", "21.2"),
      c("BP Meds (%)", "36.56", "", "33.87", "", "19.20", "", "19.41", ""),
      c("Current Smoker (%)", "27.53", "", "27.45", "", "22.89", "", "21.37", ""),
      c("Diabetes (%)", "16.30", "", "16.21", "", "8.48", "", "8.85", ""),
      c("10yr ASCVD incidence per 1,000 person-yrs", "9.64", "", "9.33", "", "11.31", "", "11.85", "")
    ))
  }, colnames=F, rownames=F)
  
  
}



shinyApp(ui = ui, server = server)

