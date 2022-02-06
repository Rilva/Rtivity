########################## GRAPHICS AESTETHICS ##############################

############################### VARIABLES ###################################

#Graph colors
colorPallete <- c("black", "red","blue","darkgreen","orange","yellow", "purple","grey","darksalmon","steelblue","springgreen","coral",
                  "black", "red","blue","darkgreen","orange","yellow", "purple","grey","darksalmon","steelblue","springgreen","coral",
                  "black", "red","blue","darkgreen","orange","yellow", "purple","grey","darksalmon","steelblue","springgreen","coral",
                  "black", "red","blue","darkgreen","orange","yellow", "purple","grey","darksalmon","steelblue","springgreen","coral")


#Graphs aestethics variables
graphsAestethics <- reactiveValues(df = data.frame(Label = character(), lineType = character(), lineColor = character()))
changeAestethics <- reactiveValues(selectedCondition = "None", selectedLine  = character(), selectedColor = character())


############################### FUNCTIONS ###################################

#Remove background from plots
whiteBackground <- function(fig, titleLetter = 16, axisLetter = 14,
                            axisNumberLetter = 12, legendLetter = 13){
  
  fig <- fig +
    theme(axis.line = element_line(colour = "black", size = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          legend.title = element_blank(),
          legend.key = element_rect(fill = NA),
          axis.title.x = element_text(margin = margin(10,10,10,10)),
          axis.title.y = element_text(margin = margin(10,10,10,10)),
          legend.text = element_text(size = legendLetter),
          axis.text = element_text(size = axisNumberLetter, face  ="bold", color = "black"),
          axis.title = element_text(size = axisLetter, face  ="bold"),
          plot.title = element_text (size = titleLetter, face  ="bold", margin = margin(b = 20, t = 10), hjust = 0.5 ),
          strip.text = element_text(size = axisLetter, face  ="bold")
          )
  
  
  
  return(fig)
}

#Standard error
se <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}


#Get user input aestethics
observeEvent(input$startanalysis,{
  
  req(damData$dt)
  if(nrow(damData$dt[,,meta=T])>0){
    
    labels <- unique(damData$dt[,labels,meta=T])

    updateSelectInput(session,"addedConditions","Choose condition", choices = c("None",labels), selected = "None")
    updateSelectInput(session,"addedConditions2","Choose condition", choices = c("None",labels), selected = "None")
    updateSelectInput(session,"addedConditions3","Choose condition", choices = c("None",labels), selected = "None")
    updateSelectInput(session,"addedConditions4","Choose condition", choices = c("None",labels), selected = "None")
    updateSelectInput(session,"addedConditions5","Choose condition", choices = c("None",labels), selected = "None")
    
    if (nrow(graphsAestethics$df)<length(labels)){
      for (i in seq((length(labels)-nrow(graphsAestethics$df)),1, -1)){
        add <- data.frame (Label = labels[length(labels)], lineType = "solid", lineColor = colorPallete[length(labels)-i+1])
        graphsAestethics$df<- rbind(graphsAestethics$df,add)
      }
    }
    else{
      if(nrow(graphsAestethics$df)>length(labels)){
        for (i in seq(nrow(graphsAestethics$df),1,-1)){
          if (is.na(match(graphsAestethics$df[i,'Label'],labels))){
            graphsAestethics$df <- graphsAestethics$df[-i,]
          }
        }
      }
    }
    graphsAestethics$df[,'Label'] <- labels
  }
  else{
    updateSelectInput(session,"addedConditions","Choose condition", choices = c("None"), selected = "None")
    updateSelectInput(session,"addedConditions2","Choose condition", choices = c("None",labels), selected = "Nope")
    updateSelectInput(session,"addedConditions3","Choose condition", choices = c("None",labels), selected = "Nope")
    updateSelectInput(session,"addedConditions4","Choose condition", choices = c("None",labels), selected = "Nope")
    updateSelectInput(session,"addedConditions5","Choose condition", choices = c("None",labels), selected = "Nope")
    
    graphsAestethics$df <- data.frame(Label = character(), lineType = character(), lineColor = character())
  }
})


#Update linetype and color fields after condition selection
observeEvent(changeAestethics$selectedCondition,{
  
  labels <- unique(damData$dt[,labels,meta=T])
  
  if (changeAestethics$selectedCondition != "None"){
    n = match(changeAestethics$selectedCondition,labels)
    
    if (input$pages == "Survival analysis"){
      updateColourInput(session,"color", value = graphsAestethics$df[n,'lineColor'], allowTransparent = TRUE)
      
    }  
    if (input$pages == "Visual inspection"){
      updateSelectInput(session,"linetype", selected = graphsAestethics$df[n,'lineType'])
      updateColourInput(session,"color2", value = graphsAestethics$df[n,'lineColor'], allowTransparent = TRUE)
    }
    if (input$pages == "Activity analysis"){
      updateColourInput(session,"color3", value = graphsAestethics$df[n,'lineColor'], allowTransparent = TRUE)
    }
    if (input$pages == "Sleep analysis"){
      updateColourInput(session,"color4", value = graphsAestethics$df[n,'lineColor'], allowTransparent = TRUE)
    }
    if (input$pages == "Rhythm and fractal analysis"){
      updateSelectInput(session,"linetype2", selected = graphsAestethics$df[n,'lineType'])
      updateColourInput(session,"color5", value = graphsAestethics$df[n,'lineColor'], allowTransparent = TRUE)
    }
  }
})

#Change linetype and color
observeEvent(changeAestethics$selectedLine,{
  
  labels <- unique(damData$dt[,labels,meta=T])
  
  if (changeAestethics$selectedCondition != "None"){
    n = match(changeAestethics$selectedCondition,labels)
    
    graphsAestethics$df[n,'lineType'] <- changeAestethics$selectedLine}
})
observeEvent(changeAestethics$selectedColor,{
  
  labels <- unique(damData$dt[,labels,meta=T])
  
  if (changeAestethics$selectedCondition != "None"){
    n = match(changeAestethics$selectedCondition,labels)
    
    graphsAestethics$df[n,'lineColor'] <- changeAestethics$selectedColor}
})

#Update line and colors
observe({

  req(damData$dt)

  if (input$pages == "Survival analysis"){
    changeAestethics$selectedCondition <- input$addedConditions
    changeAestethics$selectedColor <- input$color
  }
  if (input$pages == "Visual inspection"){
    changeAestethics$selectedCondition <- input$addedConditions2
    changeAestethics$selectedLine <- input$linetype
    changeAestethics$selectedColor <- input$color2
  }
  if (input$pages == "Activity analysis"){
    changeAestethics$selectedCondition <- input$addedConditions3
    changeAestethics$selectedColor <- input$color3
  }
  if (input$pages == "Sleep analysis"){
    changeAestethics$selectedCondition <- input$addedConditions4
    changeAestethics$selectedColor <- input$color4
  }
  if (input$pages == "Rhythm and fractal analysis"){
    changeAestethics$selectedCondition <- input$addedConditions5
    changeAestethics$selectedLine <- input$linetype2
    changeAestethics$selectedColor <- input$color5
  }

})


#Update color palettes
observeEvent(input$pallete,{
  
  updateColourInput(session,'color', palette=input$pallete)
  updateRadioButtons(session,'pallete',selected = input$pallete)
})
observeEvent(input$pallete2,{
  
  updateColourInput(session,'color2', palette=input$pallete2)
  updateRadioButtons(session,'pallete2',selected = input$pallete2)
})
observeEvent(input$pallete3,{
  
  updateColourInput(session,'color3', palette=input$pallete3)
  updateRadioButtons(session,'pallete3',selected = input$pallete3)
})
observeEvent(input$pallete4,{
  
  updateColourInput(session,'color4', palette=input$pallete4)
  updateRadioButtons(session,'pallete4',selected = input$pallete4)
})
observeEvent(input$pallete5,{
  
  updateColourInput(session,'color5', palette=input$pallete5)
  updateRadioButtons(session,'pallete5',selected = input$pallete5)
})

observeEvent(input$pages,{
  updateSelectInput(session,"addedConditions","Choose condition", selected = "None")
  updateSelectInput(session,"addedConditions2","Choose condition",  selected = "None")
  updateSelectInput(session,"addedConditions3","Choose condition", selected = "None")
  updateSelectInput(session,"addedConditions4","Choose condition", selected = "None")
  updateSelectInput(session,"addedConditions5","Choose condition", selected = "None")
  
})
