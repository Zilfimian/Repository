CatTabWg <- function(x, y, format = "long", excl = T, colrow = 1){
  # colrow = 2  is for graph 5 worsen
  # excl = F is for graph ?
  
  tab.weighted <- questionr::wtd.table(x = covid[,x], 
    y = covid[,y], 
    weights = covid$Weight)
  
  if(excl == T & (tail(colnames(tab.weighted),1) == "Not applicable"|
      #   tail(colnames(tab.weighted),1) == "Hard to say"|
      tail(colnames(tab.weighted),1) == "I do not know"
  ) 
  ){ 
    
    tab.weighted <- tab.weighted[,1:(ncol(tab.weighted)-1)]
    
  }else if(tail(colnames(tab.weighted),2) == "I do not use"|
      tail(colnames(tab.weighted),1) == "I am not aware" ){
    tab.weighted <- tab.weighted[,1:(ncol(tab.weighted)-2)]
  }
  
  # Note: by row
  Proportion <- cbind(as.data.frame(prop.table(tab.weighted, colrow)), ABS = as.data.frame(tab.weighted)[,3])
  
  Proportion$Freq <- round(Proportion$Freq*100, 1)
  
  Proportion$Chisq = getElement(chisq.test(tab.weighted), "statistic")
  
  Proportion$Pval = getElement(chisq.test(tab.weighted), "p.value")
  
  
  if(format == "long"){ # for visualization
    
    
    return(Proportion)
    
  }else if(format == "wide"){
    
    Proportion$ABS <- round(Proportion$ABS)
    Proportion <-  reshape(Proportion[,-5], idvar = "Var1", timevar = "Var2", direction = "wide")
    
    return(Proportion)
  }
  
}

# for example
# CatTabWg(x = "Q2.Gender", y = "Q1.Marz", format = "wide")
# CatTabWg(x = "Q2.Gender", y = "Q17.Ill")
# CatTabWg(x = "Q2.Gender", y = "Q26.CSComMed")
# 
# CatTabWg(x = "Q2.Gender", y = "Q23.Education", format = "wide")


# Selecting variables for wrap and only category "yes"
covid.sub.tot <- data.frame()

SubData <- function(x, y){
  
  for (i in 1:length(y)) {
    
    covid.sub <- CatTabWg(x, y[i]) %>%
      filter(Var2 %in% c("Yes")) %>% 
      mutate(
        Variable = gsub(get_label(covid)[grep(pattern = paste0(y[i],"$"), colnames(covid))],
          pattern = ".*: |.*- ", replacement = ""),
        SampleSize = sum(CatTabWg(x, y[i])[,4]) # for footnote (deleted)
      ) 
    
    covid.sub.tot <- rbind(covid.sub.tot, covid.sub)
    
  }
  covid.sub.tot$Variable <- factor(covid.sub.tot$Variable, levels = unique( covid.sub.tot$Variable))
  
  covid.sub.tot
  
}

# for example
# SubData(x = "Q2.Gender", y = c("Q17.Ill", "Q17.IllFam", "Q17.Mental"))


GGcatYes <- function(x, y, my_angl = 0, vaj = 0.8, Size = 2.5, num = 2){
  
  ggplot(data = SubData(x,y),  aes(y = Freq, x = Var1))+
    geom_bar(position = "dodge", stat = "identity", fill = eu_colors[1]) +
    ylab("%") + xlab("") + ggtitle("") +
    geom_text(aes(y=Freq+num, label = paste0(Freq, "%", " (", round(ABS),")")), 
#      vjust = vaj, # for 0 cases 
      color = "black",
      position = position_dodge(0.9), size = Size, fontface = "bold") +
    
    # scale_fill_manual(values = tail(colorRampPalette(
    #       RColorBrewer::brewer.pal(9, "Blues"))(23)[(length(levels(covid[,x]))+9):9],
    #   length(levels(covid[,x]))))   + # for middle blue values
    
    # 
    # scale_fill_distiller(palette = "PuBu",  trans = "reverse", breaks = pretty_breaks(n = 1))+
    
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    theme_bw() + 
    theme(#text = element_text(size = 2),
      axis.text.y = element_text(size = 7),
      axis.text.x = element_text(angle = my_angl, #hjust = 0.9,
        size = 9),
      legend.position = "None",
      plot.caption = element_text(hjust = 0, vjust = 10, face = "italic", size = 6.5) # footnote
    ) +
    facet_wrap(~Variable, ncol = 1, scales = "free_y")+
    
    labs(caption = paste( 
      "* Chi-Squared P-value", 
      paste(round(getElement(SubData(x,y) %>% filter(Var1 == levels(covid[,x])[1]), "Pval"), 4), collapse = ", "),
      "\n** Only showing 'yes' category.",
      "\n*** Note: N =", 
      paste(round(getElement(SubData(x,y) %>% filter(Var1 == levels(covid[,x])[1]), "SampleSize")), collapse = ", "),
      
      "\n**** Note: Plots do not include categories 'Not applicable' and/or 'I do not know'"
      
    ))
  
}
# GGcatYes(x = "Q2.Gender", y = c("Q17.Ill", "Q17.IllFam", "Q17.Mental"))

covid.sub.tot.all <- data.frame()

SubDataAll <- function(x, y){
  
  for (i in 1:length(y)) {
    
    covid.sub <- CatTabWg(x, y[i]) %>%
      mutate(
        Variable = gsub(get_label(covid)[grep(pattern = paste0(y[i],"$"), colnames(covid))],
          pattern = "\\?$|(.*: )|(.*- )|(.*experience )", replacement = ""),
        SampleSize = sum(CatTabWg(x, y[i])[,4]) # for footnote (deleted)
      ) 
    
    covid.sub.tot.all <- rbind(covid.sub.tot.all, covid.sub)
    
  }
  covid.sub.tot.all$Variable <- factor(covid.sub.tot.all$Variable, levels = unique(covid.sub.tot.all$Variable))
  
  covid.sub.tot.all
  
}


GGcatAll <- function(x, y, my_angl = 0, Coltext = "black", Size = 2.5){
  
  ggplot(data = SubDataAll(x,y),  aes(y = Freq, x = Var1,  fill = Var2))+
    geom_bar(position = "stack", stat = "identity", alpha = 0.8) +
    ylab("%") + xlab("") + ggtitle("") + labs(fill = "") +
    geom_text(aes( label = ifelse(Freq > 0, 
      paste0(Freq, "%"), "")
    ), 
      color = Coltext,
      position = position_stack(vjust = 0.5),
      size = Size, 
      fontface = "bold"
    ) +
    scale_fill_manual(values = eu_colors)+
    # scale_fill_brewer(palette = "Dark2")+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    theme_bw() + 
    theme( axis.text.y = element_text(size = 7),
      axis.text.x = element_text(angle = my_angl, 
        size = 9),
      legend.position = "top",
      legend.text = element_text(size = 7.5),
      legend.key.size = unit(0.5, 'lines'),
      plot.caption = element_text(hjust = 0, vjust = 10, face = "italic", size = 6.5) # footnote
    ) +
    facet_wrap(~Variable, ncol = 1, scales = "free_y")+
    
    labs(caption = paste( 
      "* Chi-Squared P-value", 
      paste(round(getElement(distinct(SubDataAll(x, y), Variable, .keep_all = T), "Pval") , 4), collapse = ", "),
      "\n** Note: N =", 
      paste(round(getElement(distinct(SubDataAll(x, y), Variable, .keep_all = T), "SampleSize")), collapse = ", "),
      "\n**** Note: Plots do not include categories 'Not applicable' and/or 'I do not know'"
    ))
  
}





data.by <- c()

RowTable1 <- function(by = "Q1.Marz", variable){
  
  for(i in as.character(levels(covid[,by]))){
    
    
    d.b.m.current <- questionr::wtd.table(x = covid[covid[,by]==i,"Q2.Gender"], y = covid[covid[,by]== i,variable], weights = covid[covid[,by]== i,]$Weight)
    
    
    # for y
    if((tail(colnames(d.b.m.current),1) == "Not applicable"|
        tail(colnames(d.b.m.current),1) == "I do not know")){
      
      d.b.m.current <- d.b.m.current[,-length(colnames(d.b.m.current))]
      
    }else if(tail(colnames(d.b.m.current),2) == "I do not use"|
        tail(colnames(d.b.m.current),1) == "I am not aware" ){
      d.b.m.current <- d.b.m.current[,1:(ncol(d.b.m.current)-2)]
    }
    
    d.b.m.current <- cbind(rep(i,2), as.data.frame(d.b.m.current),
      
      d.b.m.current %>% prop.table(margin = 1), 
      getElement(chisq.test(d.b.m.current), "p.value"))
    d.b.m.current <- d.b.m.current[,-c(5,6)]
    colnames(d.b.m.current) <- c("Var", "Var1", "Var2", "Freq", "Prop", "Pval") 
    d.b.m.current$Prop <- round(d.b.m.current$Prop, 3)*100
    data.by <- rbind(data.by, d.b.m.current)
    
  }
  
  return(data.by)
}


data.by.Mult.tot <- c()

RowTable1.Mult <-  function(by = "Q1.Marz", y, excl.Yes=T){
  
  for (i in 1:length(y)) {
    
    if(excl.Yes == T){
      data.by.Mult <- RowTable1(by, y[i]) %>%
        filter(Var2 %in% c("Yes")) %>% 
        mutate(Variable = gsub(get_label(covid)[grep(pattern = paste0(y[i],"$"), colnames(covid))],pattern = ".*: |.*- ", replacement = "")) 
      
    }else{
      data.by.Mult <- RowTable1(by, y[i]) %>%
        mutate(Variable = gsub(get_label(covid)[grep(pattern = paste0(y[i],"$"), colnames(covid))],pattern = ".*: |.*- ", replacement = "")) 
      
    }
    
    data.by.Mult.tot <- rbind(data.by.Mult.tot, data.by.Mult)
    
  }
  data.by.Mult.tot$Variable <- factor(data.by.Mult.tot$Variable, levels = unique(data.by.Mult.tot$Variable))
  
  return(data.by.Mult.tot)
  
}

RowTable1.Mult(y = c("Q17.Ill", "Q17.IllFam", "Q17.Mental"))





