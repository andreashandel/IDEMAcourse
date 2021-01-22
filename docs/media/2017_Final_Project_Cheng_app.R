### EPID8150 project supplement: Shiny app for Structured Decision-Making
### written by: Ellen Cheng
### Install the R packages 'rhandsontable' and 'shiny' if you don't have them already
### Then to run this app, source the code and then type 'SDM()' in the command line
### Some tables are wide, so it may be best to select 'open in browser' in the Shiny window, or resize the Shiny window to tables are not cut off

library(rhandsontable)
library(shiny)

### Create default tables----
row.impl.cost <- c(15, 170, 1128,	1109, 1298, 1279) # implementation cost, in thousands
row.econ.prod <- c(59256,	60390, 49522, 35929, 50824, 36283) # economic productivity loss, in thousands
row.infected <- c(274, 251,	229, 166, 211, 150) # number infected, in thousands
row.support <- c(0, 0, 38, 54, 38, 54) # proportion of complaints addressed
row.future <- c(0, 1, 0, 0, 1, 1)

(DF.default <- data.frame(ImplementCost=as.numeric(row.impl.cost), EconomicProdLoss=row.econ.prod, Infected=row.infected, PublicSupport=row.support, FutureBenefits=row.future))
rownames(DF.default) <- c("Base", "Isolate", "Subsidize", "School", "Iso_Subsidize", "Iso_School")
# saveRDS(DF.default, file="table.RDS")

(cw.default <- data.frame(ImplementCost=1, EconomicProdLoss=0.25))
rownames(cw.default) <- "Relative weights for cost objective"
# saveRDS(cw.default, "costwt.RDS")

(rank.default <- data.frame(benchmark=5, alt.cost=2, alt.infect=1, alt.public=3, alt.future=4))
rownames(rank.default) <- "Swing ranks (1 = best)"

(score.default <- data.frame(benchmark=0, alt.cost=95, alt.infect=100, alt.public=20, alt.future=10))
rownames(score.default) <- "Swing scores (100 = best)"

SDM <- function(DF=DF.default, cw=cw.default, rank=rank.default, score=score.default, outdir=getwd()){
  ui <- shinyUI(fluidPage(
    tabsetPanel(
      ### TAB ONE: INPUT THE RAW DATA----
      tabPanel("(1) Input data",
               sidebarLayout(
                 sidebarPanel(width = 3,
                              helpText("INSTRUCTIONS: Modify these data by clicking in cells you want to change."),
                              helpText("Click the 'Accept and proceed' button BEFORE moving on to the Swing weighting tab."), # helptext
                              
                              wellPanel(
                                h4("Accept current data"), 
                                actionButton("save", "Accept and proceed")
                                ) # wellpanel
                              ), # sidebarpanel
                 
                 mainPanel(titlePanel("STEP 1: Input the initial data"),
                           wellPanel(
                             uiOutput("message", inline=TRUE)
                             ),
                           
                           helpText("The data in these tables will be used to fill out the Swing weighting table on the next tab.",
                                    "The upper table gives the relative weights of two cost objectives--implementation cost and economic productivity loss--for computing the weighted cost objective for each influenza mitigation strategy. For example, the default weights are 1 and 0.25, meaning the weighted cost will be calculated as (1 * implementation cost) + (0.25 * economic productivity loss) for each strategy."), br(),
                           helpText("The lower table is the initial consequence table. Table values are the predicted consequences of each influenza mitigation strategy (rows) on each fundamental objective (columns). Information for this table should come from expert consultation, simulation modeling, social surveys, peer-reviewed literature, or other reliable sources."), # helptext
                           br(),
                           helpText("NOTE: If you only see a partial table, click in the table area--the entire table should then appear."),
                           
                           rHandsontableOutput("hotcostwt"),
                           br(), br(),
                           
                           rHandsontableOutput("hot"),
                           br(),
                           
                           helpText("ROW DESCRIPTIONS (ALTERNATIVE STRATEGIES):"),
                           helpText("Base = status quo; Isolate = education campaign to encourage self-quarantine when sick with influenza; Subsidize = free vaccination for persons living below the federal poverty level; School = vaccination clinics set up at all elementary, middle, and high schools and free vaccinination for students living below the federal poverty level; Iso_Subsidize = combination of Isolate + Subsidize; Iso_School = combination of Isolate + School"), br(),
                           
                           helpText("COLUMN DESCRIPTIONS (FUNDAMENTAL OBJECTIVES):"), 
                           helpText("(1) ImplementCost = cost ($, in thousands) of implementing the strategy."),
                           helpText("(2) EconomicProdLoss = value ($, in thousands) of person-workdays missed due to influenza."),
                           helpText("(3) Infected = total number of people (in thousands) infected by influenza during the season."),
                           helpText("(4) PublicSupport = percent (%) of influenza-management-related complaints from the past five years, addressed by the strategy."), 
                           helpText("(5) FutureBenefits = Will the strategy yield benefits that carry over to future years, such as future cost savings? 1 = Yes, 0 = No.") # helptext
                           ) # mainpanel
                 ) # sidebarlayout
               ), # END OF TAB 1
      
      ### TAB TWO: SWING WEIGHTING----
      tabPanel("(2) Swing weighting",
               sidebarLayout(
                 sidebarPanel(width = 3,
                              helpText("INSTRUCTIONS: Modify the ranks and scores for swing alternatives (bottom two tables) by clicking in cells you want to change."),
                              helpText("Click the 'Accept and proceed' button BEFORE moving on to the Multi-attribute trade-off tab."), # helptext
                              
                              wellPanel(
                                h4("Accept swing ranks and scores"), 
                                actionButton("save2", "Accept and proceed")
                              ), # wellpanel
                              
                              helpText("After you modify and accept the scores in the bottom table, they will be normalized to a scale from 0 to 1. The normalized weight represents the relative importance of each objective, for choosing among the considered influenza management strategies. It is calculated as the score for that objective divided by the sum of scores across all objectives. The most important objective for choosing among strategies will have the highest weight."),
                              
                              helpText("THE NORMALIZED OBJECTIVE WEIGHTS ARE:"),
                              
                              tableOutput("finalwt")
                 ), # sidebarpanel
                 
                 mainPanel(titlePanel("STEP 2: Weight the fundamental objectives"),
                           
                           helpText("The data in these tables were populated based on the information you provided in the 'input data' tab...."),
                           br(),

                           helpText("CONSEQUENCE TABLE"),
                           helpText("Same as on prior tab, but transposed for easier conversion to a swing table. Now, the influenza management strategies are in columns and fundamental objectives are in rows. The two cost objectives ('implementation cost' and 'economic productivity loss') have been replaced by a single 'weighted cost' objective."),
                           
                           tableOutput("transtab"), br(),
                           
                           helpText("RANGE OF PREDICTED VALUES FOR EACH OBJECTIVE"),
                           helpText("A summary of the range (min - max) of predicted consequences for each fundamental objective (rows). 'UNITS' is the metric response for each objective and 'GOAL' is whether we are trying to maximize or minimize that objective response. For example, the weighted cost objective is measured in thousands of dollars and we are trying to find a strategy that minimizes the cost. Among the strategies considered, the 'worst' (highest) and 'best' (lowest) costs are presented."),
                           
                           tableOutput("minmax"), br(),
                           
                           helpText("SWING TABLE FOR OBJECTIVE WEIGHTING"),
                           helpText("The columns ('benchmark' ... 'alt.future') present hypothetical scenarios that you will rank, to help clarify which fundamental objectives matter more to you for choosing among the considered influenza management strategies. In this table, 'benchmark' represents a hypothetical scenario with the worst predicted value for each objective. Each of the other hypothetical scenarios has the worst value for all but one objective (and the best value for that one objective). For example, 'alt.cost' is a scenario in which an influenza mitigation strategy has low cost, but high numbers of infections, no public support, and no future benefits. The best and worst values in this table are not made-up--they come from the min-max table above."),
                           
                           tableOutput("swingtable"), br(),
                           
                           helpText("ENTER RANKS FOR THE SWING TABLE SCENARIOS"),
                           helpText("Rank each of the hypothetical scenarios (columns) in the swing table, to indicate your preference for each, with 1 = best and 5 = worst (5 should be the 'benchmark', since it has the worst value for every objective."),
                           
                           rHandsontableOutput("hotrank"), br(),
                           
                           helpText("ENTER SCORES FOR THE SWING TABLE SCENARIOS"),
                           helpText("Score each of the hypothetical scenarios with a number from 0 (worst) to 100 (best). Use your swing ranks as a guide. The scenario ranked 1 should have a score of 100, the alternative ranked 5 ('benchmark') should have a score of 0, and other alternatives should fall somewhere in between."),

                           rHandsontableOutput("hotscore")
                 ) # mainpanel
               ) # sidebarlayout
      ), # END OF TAB 2

      ### TAB THREE: MULTI-ATTRIBUTE TRADE-OFF----
      tabPanel("(3) Multi-attribute trade-off",
               titlePanel("Results from multi-attribute trade-off"),
               helpText("CONSEQUENCE TABLE (repeated here for reference)"),
               tableOutput("transtab2"), br(),
               
               fluidRow(
                 column(3, 
                        helpText("NORMALIZED OBJECTIVE WEIGHTS (repeated here for reference)"),
                        tableOutput("finalwt2")
                 ),
                 column(9,
                        helpText("NORMALIZED CONSEQUENCE TABLE"),
                        helpText("The consequence table values are normalized to a scale from 0 to 1, by row (fundamental objective). That is, for each objective, the value for each strategy is recalculated as (value - worst)/(best - worst), where 'best' and 'worst' refer to the best and worst predicted values for that objective across all considered strategies (min-max table on the prior tab). For each objective, the strategy with a normalized value of '1' ranks best; '0' ranks worst."),

                        tableOutput("normscore")
                 )),
               
               wellPanel(
                   helpText("FINAL RANKING OF ALTERNATIVE STRATEGIES (HIGHEST IS BEST)"),
                   helpText("Weighted average scores for influenza management strategies. The preferred alternative has the highest score. For each alternative, this final score is calculated by weighting the normalized consequence values by the normalized objective weights."),

                   tableOutput("mato")
                   ) # wellpanel
               ) # END OF TAB 3
      ) # tabsetpanel
    ) # fluidpage
    ) # shinyUI
  
  ### SERVER----
  server <- shinyServer(function(input, output) {
    
    values <- reactiveValues()
    
    ## Handsontable
    observe({
      if (!is.null(input$hotcostwt)) {
        values[["previous"]] <- isolate(values[["cw"]])
        cw = hot_to_r(input$hotcostwt)
      } else {
        if (is.null(values[["cw"]]))
          cw <- cw
        else
          cw <- values[["cw"]]
      }
      values[["cw"]] <- cw
    }) # observe hotcostwt
    
    output$hotcostwt <- renderRHandsontable({
      cw <- values[["cw"]]
      if (!is.null(cw))
        rhandsontable(cw, useTypes = FALSE, rowHeaderWidth=250, td.style.textAlign = 'center', stretchH = "all")
    }) # output hotcostwt
    
    observe({
      if (!is.null(input$hot)) {
        values[["previous"]] <- isolate(values[["DF"]])
        DF = hot_to_r(input$hot)
      } else {
        if (is.null(values[["DF"]]))
          DF <- DF
        else
          DF <- values[["DF"]]
      }
      values[["DF"]] <- DF
    }) # observe hot
    
    output$hot <- renderRHandsontable({
      DF <- values[["DF"]]
      if (!is.null(DF))
        rhandsontable(DF, useTypes = FALSE, rowHeaderWidth=160, td.style.textAlign = 'center', stretchH = "all")
    }) # output hot
    
    observe({
      if (!is.null(input$hotrank)) {
        values[["previous"]] <- isolate(values[["rank"]])
        rank = hot_to_r(input$hotrank)
      } else {
        if (is.null(values[["rank"]]))
          rank <- rank
        else
          rank <- values[["rank"]]
      }
      values[["rank"]] <- rank
    }) # observe hotrank

    output$hotrank <- renderRHandsontable({
      rank <- values[["rank"]]
      if (!is.null(rank))
        rhandsontable(rank, useTypes = FALSE, rowHeaderWidth=180, td.style.textAlign = 'center', stretchH = "all")
    }) # output hotrank
    
    observe({
      if (!is.null(input$hotscore)) {
        values[["previous"]] <- isolate(values[["score"]])
        score = hot_to_r(input$hotscore)
      } else {
        if (is.null(values[["score"]]))
          score <- score
        else
          score <- values[["score"]]
      }
      values[["score"]] <- score
    }) # observe hotscore
    
    output$hotscore <- renderRHandsontable({
      score <- values[["score"]]
      if (!is.null(score))
        rhandsontable(score, useTypes = FALSE, rowHeaderWidth=180, td.style.textAlign = 'center', stretchH = "all")
    }) # output hotscore
    
    ## Save 
    observeEvent(input$save, {
      finalDF <- isolate(values[["DF"]])
      saveRDS(finalDF, file=file.path(outdir, "table.RDS"))
      
      finalcw <- isolate(values[["cw"]])
      saveRDS(finalcw, file=file.path(outdir, "costwt.RDS"))
      
      output$transtab <- renderTable(
        align='c', rownames=TRUE, digits=0, # create the swing table
        {
          t.tab <- finalDF
          t.tab$WeightedCost <- round(finalcw$ImplementCost*t.tab$ImplementCost + finalcw$EconomicProdLoss*t.tab$EconomicProdLoss)
          t.tab <- t(t.tab[, c("WeightedCost", "Infected", "PublicSupport", "FutureBenefits")])
          return(t.tab)
        }) # transtab 
      
      output$transtab2 <- renderTable(
        align='c', rownames=TRUE, digits=0, # create the swing table
        {
          t.tab <- finalDF
          t.tab$WeightedCost <- round(finalcw$ImplementCost*t.tab$ImplementCost + finalcw$EconomicProdLoss*t.tab$EconomicProdLoss)
          t.tab <- t(t.tab[, c("WeightedCost", "Infected", "PublicSupport", "FutureBenefits")])
          return(t.tab)
        }) # transtab2
      
      output$normscore <- renderTable(
        align='c', rownames=TRUE, # create the normalized swing score table
        {
          t.tab <- finalDF
          t.tab$WeightedCost <- round(finalcw$ImplementCost*t.tab$ImplementCost + finalcw$EconomicProdLoss*t.tab$EconomicProdLoss)
          t.tab <- t(t.tab[, c("WeightedCost", "Infected", "PublicSupport", "FutureBenefits")])
          units <- c("$ (1000's)", "# people (1000's)", "% complaints addressed", "yes=1, no=0")
          goal <- c("MIN", "MIN", "MAX", "MAX") 
          norm.tab <- data.frame(UNITS=units, GOAL=goal, t.tab)
          for (r in 1:nrow(norm.tab)) {
            if(norm.tab[r, "GOAL"]=="MIN") {
              best.val <- min(norm.tab[r, 3:ncol(norm.tab)])
              worst.val <- max(norm.tab[r, 3:ncol(norm.tab)])
            } else {
              best.val <- max(norm.tab[r, 3:ncol(norm.tab)])
              worst.val <- min(norm.tab[r, 3:ncol(norm.tab)])
            }
            for (alt in 3:ncol(norm.tab)) {
              norm.tab[r, alt] <- as.numeric((norm.tab[r, alt]-worst.val)/(best.val - worst.val))
              } # alt
            } # r
          norm.tab[, 3:ncol(norm.tab)] <- round(norm.tab[, 3:ncol(norm.tab)], 2)
          return(norm.tab)
          }) # normscore
      
      output$minmax <- renderTable(
        align='c', rownames=TRUE, digits=0, # create the swing table
        {
          t.tab <- finalDF
          t.tab$WeightedCost <- round(finalcw$ImplementCost*t.tab$ImplementCost + finalcw$EconomicProdLoss*t.tab$EconomicProdLoss)
          t.tab <- t(t.tab[, c("WeightedCost", "Infected", "PublicSupport", "FutureBenefits")])
          units <- c("$ (1000's)", "# people (1000's)", "% complaints addressed", "yes=1, no=0")
          goal <- c("MIN", "MIN", "MAX", "MAX") 
          worst <- c(max(t.tab["WeightedCost",]), max(t.tab["Infected",]), min(t.tab["PublicSupport",]), min(t.tab["FutureBenefits",]))
          best <- c(min(t.tab["WeightedCost",]), min(t.tab["Infected",]), max(t.tab["PublicSupport",]), max(t.tab["FutureBenefits",]))
          mm.tab <- data.frame(cbind(UNITS=units, GOAL=goal, worst, best))
          rownames(mm.tab) <- rownames(t.tab)
          mm.tab$worst <- as.numeric(as.character(mm.tab$worst)) # convert to numeric
          mm.tab$best <- as.numeric(as.character(mm.tab$best)) # convert to numeric
          return(mm.tab)
        }
      ) # minmax
      
      output$swingtable <- renderTable(
        align='c', rownames=TRUE, digits=0, # create the swing table
        {
          t.tab <- finalDF
          t.tab$WeightedCost <- round(finalcw$ImplementCost*t.tab$ImplementCost + finalcw$EconomicProdLoss*t.tab$EconomicProdLoss)
          t.tab <- t(t.tab[, c("WeightedCost", "Infected", "PublicSupport", "FutureBenefits")])
          units <- c("$ (1000's)", "# people (1000's)", "% complaints addressed", "yes=1, no=0")
          goal <- c("MIN", "MIN", "MAX", "MAX") 
          worst <- c(max(t.tab["WeightedCost",]), max(t.tab["Infected",]), min(t.tab["PublicSupport",]), min(t.tab["FutureBenefits",]))
          best <- c(min(t.tab["WeightedCost",]), min(t.tab["Infected",]), max(t.tab["PublicSupport",]), max(t.tab["FutureBenefits",]))
          mm.tab <- data.frame(cbind(UNITS=units, GOAL=goal, worst, best))
          rownames(mm.tab) <- rownames(t.tab)
          mm.tab$worst <- as.numeric(as.character(mm.tab$worst)) # convert to numeric
          mm.tab$best <- as.numeric(as.character(mm.tab$best)) # convert to numeric
          s.tab <- cbind(mm.tab[, c("UNITS", "GOAL")], data.frame(benchmark=mm.tab$worst, alt.cost=mm.tab$worst, alt.infect=mm.tab$worst, alt.public=mm.tab$worst, alt.future=mm.tab$worst))
          rownames(s.tab) <- rownames(mm.tab)
          s.tab["WeightedCost", "alt.cost"] <- min(t.tab["WeightedCost",])
          s.tab["Infected", "alt.infect"] <- min(t.tab["Infected",])
          s.tab["PublicSupport", "alt.public"] <- max(t.tab["PublicSupport",])
          s.tab["FutureBenefits", "alt.future"] <- max(t.tab["FutureBenefits",])
          return(s.tab)
        }
      ) # swingtable
    }) # observeevent
    
    ## Save2
    observeEvent(input$save2, {
      finalscore <- isolate(values[["score"]])
      
      output$finalwt <- renderTable(
        align='c', rownames=FALSE, digits=2, # normalized objective weights
        {
          final.temp <- data.frame(c("X", "WeightedCost", "Infected", "PublicSupport", "FutureBenefits"), t(finalscore))
          colnames(final.temp) <- c("OBJECTIVES", "weights (normalized)")
          rownames(final.temp) <- NULL
          final.temp <- final.temp[-1,]
          final.temp[,2] <- round(final.temp[,2]/sum(final.temp[,2]), 2)
          return(final.temp)
        })
      
      output$finalwt2 <- renderTable(
        align='c', rownames=FALSE, digits=2, # normalized objective weights
        {
          final.temp <- data.frame(c("X", "WeightedCost", "Infected", "PublicSupport", "FutureBenefits"), t(finalscore))
          colnames(final.temp) <- c("OBJECTIVES", "weights (normalized)")
          rownames(final.temp) <- NULL
          final.temp <- final.temp[-1,]
          final.temp[,2] <- round(final.temp[,2]/sum(final.temp[,2]), 2)
          return(final.temp)
        })
      
      output$mato <- renderTable(
        align='c', rownames=FALSE, digits=2, # normalized objective weights
        {
          t.tab <- isolate(values[["DF"]])
          finalcw <- isolate(values[["cw"]])
          t.tab$WeightedCost <- round(finalcw$ImplementCost*t.tab$ImplementCost + finalcw$EconomicProdLoss*t.tab$EconomicProdLoss)
          t.tab <- t(t.tab[, c("WeightedCost", "Infected", "PublicSupport", "FutureBenefits")])
          units <- c("$ (1000's)", "# people (1000's)", "% complaints addressed", "yes=1, no=0")
          goal <- c("MIN", "MIN", "MAX", "MAX") 
          norm.tab <- data.frame(UNITS=units, GOAL=goal, t.tab)
          for (r in 1:nrow(norm.tab)) {
            if(norm.tab[r, "GOAL"]=="MIN") {
              best.val <- min(norm.tab[r, 3:ncol(norm.tab)])
              worst.val <- max(norm.tab[r, 3:ncol(norm.tab)])
            } else {
              best.val <- max(norm.tab[r, 3:ncol(norm.tab)])
              worst.val <- min(norm.tab[r, 3:ncol(norm.tab)])
            }
            for (alt in 3:ncol(norm.tab)) {
              norm.tab[r, alt] <- as.numeric((norm.tab[r, alt]-worst.val)/(best.val - worst.val))
            } # alt
          } # r
          norm.tab[, 3:ncol(norm.tab)] <- round(norm.tab[, 3:ncol(norm.tab)], 2)
          final.temp <- data.frame(c("X", "WeightedCost", "Infected", "PublicSupport", "FutureBenefits"), t(finalscore))
          colnames(final.temp) <- c("OBJECTIVES", "weights (normalized)")
          rownames(final.temp) <- NULL
          final.temp <- final.temp[-1,]
          final.temp[,2] <- round(final.temp[,2]/sum(final.temp[,2]), 2)
          mato.res <- as.numeric(final.temp[,2]) %*% as.matrix(norm.tab[,3:8])
          return(mato.res)
        })
      }) # observeevent
    
    ## Message
    output$message <- renderUI({
      if(input$save==0){
        helpText(sprintf("These data will be saved in folder \"%s\" once you press the 'Accept and proceed' button.", outdir))
      }else{
        list(helpText(sprintf("File saved: \"%s\".", file.path(outdir, "costwt.RDS"))),
             helpText(sprintf("File saved: \"%s\".", file.path(outdir, "table.RDS"))))
        }
      })
    
  }) # shinyserver
  
  ## run app 
  runApp(list(ui=ui, server=server))
  return(invisible())
}
