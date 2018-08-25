library(shiny)
library(magrittr)
library(rhandsontable)

ui <- fluidPage(
   titlePanel("研究室配属シミュレーター"),
   conditionalPanel("!output.numflag",
       numericInput("studentnum", "学生の人数", 100, 1, 999),
       numericInput("labnum", "研究室の数", 10, 1, 99),
       actionButton("numbutton", "決定")
   ),
   conditionalPanel("output.capflag",
                    h4("研究室ごとの募集人数を入力してください。"),
                    h5("(左の数字は研究室の人気順を表しています。)"),
                    rHandsontableOutput("captable"),
                    actionButton("capbutton", "決定")
   ),
   conditionalPanel("output.labflag",
                    uiOutput("gpaui"),
                    uiOutput("firstui"),
                    uiOutput("secondui"),
                    uiOutput("thirdui"),
                    h5("GPAは乱数により疑似的に生成されたものです。また、希望研究室は人気順に並べた時の数字を入力してください。"),
                    actionButton("labbutton", "決定"),
                    h5("(計算に少々時間がかかります)")
   ),
   conditionalPanel("output.outflag",
                    h4("希望する研究室への配属可能性"),
                    tableOutput("choice")
   )

)

server <- function(input, output, session) {
  studentnum <- 100
  labnum <- 10
  seat <- rep(10, 10)
  random <- rep(2.5, 100)
  
  output$numflag <- reactive(as.logical(input$numbutton))
  outputOptions(output, "numflag", suspendWhenHidden = FALSE)
  output$capflag <- reactive(as.logical(input$numbutton))
  outputOptions(output, "capflag", suspendWhenHidden = FALSE)
  output$capflag <- reactive(if(as.logical(input$numbutton))!as.logical(input$capbutton))
  output$labflag <- reactive(as.logical(input$capbutton))
  outputOptions(output, "labflag", suspendWhenHidden = FALSE)
  output$labflag <- reactive(if(as.logical(input$capbutton))!as.logical(input$labbutton))
  output$outflag <- reactive(as.logical(input$labbutton))
  outputOptions(output, "outflag", suspendWhenHidden = FALSE)
  
  observeEvent(input$numbutton, {
    studentnum <<- input$studentnum
    labnum <<- input$labnum
    output$captable <- renderRHandsontable({
      avecapacity <- studentnum %/% labnum
      addcapacity <- studentnum %% labnum
      df <- data.frame(lab = 1:labnum, capacity = avecapacity)
      addlab <- sample(1:labnum, addcapacity)
      df$capacity[addlab] %<>% `+`(1)
      df$capacity %<>% as.integer()
      colnames(df) <- c("研究室", "募集人数")
      rhandsontable(df[2]) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })
  })
  
  observeEvent(input$capbutton, {
    seat <<- hot_to_r(input$captable)[,1]
    random <<- rnorm(studentnum, mean=2.5, sd=0.56)
    random[random>4] <<- 4
    random[random<0] <<- 0
    random <<- random %>% .[rev(order(.))]
    output$gpaui <- renderUI({selectInput("gpa", "あなたに近いGPA", random)})
    output$firstui <- renderUI({numericInput("firstlab", "第一志望", 1, 1, labnum)})
    output$secondui <- renderUI({numericInput("secondlab", "第ニ志望", 1, 1, labnum)})
    output$thirdui <- renderUI({numericInput("thirdlab", "第三志望", 1, 1, labnum)})
  })
  
  observeEvent(input$labbutton, {
    stdtable <- data.frame(gpa=random,choice1=NA,choice2=NA,choice3=NA,lab=NA)
    choicetable <- data.frame(gpa=input$gpa,first=0,second=0,third=0,none=0)
    mygpa <- which(random == input$gpa)
    lab <- seq(15,1,length=labnum)
    lablow <- seq(2,1,length=labnum)
    initseat <- seat
    
    loopnum <- 100
    
    for(loop in 1:loopnum){
      seat <<- initseat
      for(i in 1:studentnum){
        choice <- sample(1:labnum,3,prob=(lab*(studentnum-i)+lablow*i)/studentnum)
        ifelse(rnorm(1)>0, choice <- sort(choice), 
               choice <- c(choice[1],sort(choice[2:3])))
        stdtable[i,-c(1,5)] <- choice
      }
      stdtable$lab <- NA
      stdtable[mygpa,2:4] <- c(input$firstlab, input$secondlab, input$thirdlab)
      
      for(i in 1:studentnum){
        if(seat[stdtable$choice1[i]]>0){
          stdtable$lab[i] <- stdtable$choice1[i]
          seat[stdtable$choice1[i]] <<- seat[stdtable$choice1[i]] - 1
        }
      }
      for(i in 1:studentnum){
        if(is.na(stdtable$lab[i])){
          if(seat[stdtable$choice2[i]]>0){
            stdtable$lab[i] <- stdtable$choice2[i]
            seat[stdtable$choice2[i]] <<- seat[stdtable$choice2[i]] - 1
          }    
        }
      }
      for(i in 1:studentnum){
        if(is.na(stdtable$lab[i])){
          if(seat[stdtable$choice3[i]]>0){
            stdtable$lab[i] <- stdtable$choice3[i]
            seat[stdtable$choice3[i]] <<- seat[stdtable$choice3[i]] - 1
          }    
        }
      }
      
      if(!is.na(stdtable$lab[mygpa])){
        if(stdtable$lab[mygpa]==stdtable$choice1[mygpa]){
          choicetable$first[1] <- choicetable$first[1] + 1
        }else if(stdtable$lab[mygpa]==stdtable$choice2[mygpa]){
          choicetable$second[1] <- choicetable$second[1] + 1
        }else if(stdtable$lab[mygpa]==stdtable$choice3[mygpa]){
          choicetable$third[1] <- choicetable$third[1] + 1
        }
      }else{
        choicetable$none[1] <- choicetable$none[1] + 1
      }
    }
    colnames(choicetable) <- c("あなたのGPA","第一希望(%)","第二希望(%)","第三希望(%)","配属されない(%)")
    output$choice <- renderTable(choicetable)
  })
}

shinyApp(ui = ui, server = server)
