library(shiny)
library(tidyverse)
library(friends)
library(ggthemes)
library(ggrepel)
library(reshape2)
library(wordcloud)
library(wesanderson)
library(showtext)
library(remotes)
library(tidytext)
library(textdata)


# set the font               

font_add("Josef Xuereb's Friends",
         regular = "Josef Xuereb s Friends - Regular.ttf",
         bold = "Josef Xuereb s Friends - Bold.ttf")

showtext_auto()

nrc_df <- read_csv("nrc.csv") # nrc lexicon


font_family1 <- "Josef Xuereb's Friends"


text_df <- tibble(friends)



main_chars <- c("Rachel Green", "Monica Geller", "Ross Geller", 
                "Joey Tribbiani", "Chandler Bing", "Phoebe Buffay")

# customized stop words
undesirable_words <- c("hey", "yeah", "uh", "whoa", "la", "ha", 
                       "ow", "ooh","ho","gonna", "huh", "hmm", "woo", 
                       "hoo","blah", "ew","woah","mmmhm","wow", "ohh", "ah", 
                       "umm", "wanna", "hey", "y'know", "alright", "gotta", "uh", "ya", stop_words$word)

text_bigram <- text_df %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  filter(!word1 %in% undesirable_words) %>%
  filter(!word2 %in% undesirable_words)


# Define UI for application 
ui <- navbarPage(
  
  "All About Friends", 
  
 tabsetPanel(id="friends", 
  tabPanel("Character's Emotions",
  
  fluidPage(
            theme = shinythemes::shinytheme("slate"),
    
            
    fluidRow(
        column(5,
               selectInput("mainChars", "Choose one character", main_chars)
        )
    ),
  
 fluidRow(
        column(4, imageOutput("picture")),
        column(8, dataTableOutput("scripts"))
        
    ),
 
 fluidRow(
     column(12, plotOutput("sentimentbytime"))
     
 ),
 
 tags$footer("The lexicon used for the sentiment analysis comes from",tags$u(tags$a(href="http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm", "NRC Word-Emotion Association.", target="_blank",style="color:#707070")),style = "
              
              color: #707070;
              padding: 15px;
          "),
)
 
),
tabPanel("Commonly Used Words",
         sidebarLayout(
           sidebarPanel(
             radioButtons("characters", "Choose one character", main_chars)
           ),
           mainPanel(
             plotOutput("wordsfreq"),
             tags$div(class="jumbotron", tags$h4(class="display-3", "Connect with the Transcripts"),
                      tags$p(class="lead", "Want to dig more about the reasons behind the emotional change? Click
                             below button to explore the transcripts using the commonly used bigrams from the above chart.", style = "font-size: 1em"),
                      actionButton(inputId = "button", label = "Tell me a story")),
             )
           )
         ))

)

server <- function(input, output, session){
   
  selected <- reactive(text_df %>% filter(speaker == input$mainChars) %>% 
                         unnest_tokens(word, text) %>% 
                         filter(!word %in% undesirable_words))
   
    selected2 <- reactive(text_df %>% 
                              filter(speaker==input$mainChars) %>% 
                              select(-c("scene", "utterance")))
    
    selected3 <- reactive(text_bigram %>% 
                            filter(speaker==input$characters) %>% 
                            unite(bigram, word1, word2, sep = " ") %>% 
                            filter(!bigram=="NA NA") %>% 
                            group_by(season,bigram) %>%
                            summarise(n=n()) %>% 
                            slice_max(order_by = n, n=5, with_ties=FALSE))
                            
    
    output$picture <- renderImage({
       filename <- normalizePath(file.path("WWW/", paste(input$mainChars, ".png", sep="")))
       
       list(src = filename,
            alt = paste("picture of ", input$mainChars))
                                 
                                 }, deleteFile = FALSE)
       
    output$scripts <- renderDataTable(
        
        selected2(),
        
        options = list(pageLength = 5) 
        
    )
    
    output$sentimentbytime <- renderPlot(
      selected() %>% 
        group_by(season) %>%
        mutate(total_words = n()) %>%
        ungroup() %>%
        inner_join(nrc_df)%>%
        filter(sentiment %in% c("anger", "disgust", "fear", "joy","sadness") ) %>%
        count(season, sentiment, total_words) %>%
        ungroup() %>%
        mutate(percent = (n / total_words)) %>% 
        
        ggplot(aes(season, percent, color = sentiment)) +
        geom_line(size = 1.5) +
        geom_smooth(method = "lm", se = FALSE, lty = 2) +
        expand_limits(y = 0)+
        labs(title=paste(input$mainChars, "'s Emotions by Season"), color = "Emotions") + 
        scale_color_manual(values=wes_palette("Darjeeling1"))+ 
        scale_x_continuous(name="", breaks = seq(1,10, 2), labels = c("Season 1", "Season 3", "Season 5", "Season 7", "Season 9"))+
        theme_minimal()+
        theme(axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.text.x=element_text(angle=36, vjust = 0.6, colour = "white", size = 12),
              axis.title = element_blank(),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 12.5),
              plot.background = element_rect(fill = "#272B30", colour = "#272B30"),
              panel.border = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid = element_line(colour = "#495464"),
              text = element_text(colour = "white"),
              plot.title = element_text(family = font_family1, size = 28)
              
        ), res=96
    )
    
 output$wordsfreq <- renderPlot(
   selected3() %>% 
     ggplot(aes(fct_reorder(bigram, n),n)) +
     geom_col(fill = "#ffdd93") +
     labs(x="", y="", title = paste(input$characters,"'s Words Bigram Frequency by Season")) +
     coord_flip() + facet_wrap(~season, scales = "free", ncol=3)+ theme_clean() +
     theme(plot.background = element_rect(fill = "#272B30", colour = "#272B30"),
           text = element_text(colour = "white"),
           plot.title = element_text(family = font_family1, size = 28),
           panel.border = element_blank(),
           strip.background = element_rect(colour = "#272B30", fill = "#272B30"),
           strip.text = element_text(colour = "white", size = 13),
           axis.text = element_text(colour = "white")
           ), res=96
 ) 
 
 observeEvent(input$button, {
   updateTabsetPanel(session, "friends", "Character's Emotions")
 })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
