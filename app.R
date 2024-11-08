library(shiny)
library(tidyverse)
library(plotly)
library(scales)

ds <- read_csv("https://raw.githubusercontent.com/dkunz2024/ds502final/master/pokemon_ds.csv")
ds <- ds %>% 
  mutate(Type_1 = as.factor(Type_1), 
         Type_2 = as.factor(Type_2), 
         Color = as.factor(Color), 
         Egg_Group_1 = as.factor(Egg_Group_1), 
         Egg_Group_2 = as.factor(Egg_Group_2), 
         Body_Style = as.factor(Body_Style))

types <- read_csv("https://raw.githubusercontent.com/dkunz2024/ds502final/master/pokemon_type_ds.csv")
types <- types %>% 
  rename("Type 1"=...1)
types <- types %>% 
  pivot_longer(
    cols=2:19,
    names_to = "Type 2",
    values_to = "Effectiveness"
  )

print(str(ds))
print(str(types))

# Define UI 
ui <- fluidPage(
  
  # Application title
  titlePanel("Gapminder: Simple Shiny Demo"),
  
  # Sidebar with a slider input the year 
  sidebarLayout(
    sidebarPanel(
      textInput("p1", 
                  h4("First Pokemon"),
                  value = "Pikachu"),
      textInput("p2", 
                  h4("Second Pokemon"), 
                  value = "Charmander")
    ),
    
    # Show the plots
    mainPanel(
      plotOutput("radarPlot"), 
    )
  )
)

# Define server 
server <- function(input, output) {
  
  output$radarPlot <- renderPlot({
    
    p1 <- input$p1
    p2 <- input$p2
    
    #Create dataset of the two pokemon
    dsc <- ds %>%
      filter(Name==p1 | Name==p2) %>%
      mutate(Number=(Name==p2)+1) %>%
      select(Number, HP, Attack, Defense, Sp_Atk, Sp_Def, Speed) %>%
      pivot_longer(
        cols=2:7,
        names_to = "Stat_Type",
        values_to = "Stat_Value"
      ) %>%
      mutate(Stat_Type=factor(Stat_Type, levels=c("HP", "Attack", "Defense", "Speed", "Sp_Def", "Sp_Atk"))) %>%
      arrange(Number)

    #Create the plot
    dsc %>%
      ggplot(aes(x=Stat_Type, y=Stat_Value))+
      geom_col(fill="lightblue")+
      facet_wrap(.~Number, labeller=as_labeller(c('1'=p1, '2'=p2)))+
      coord_polar(start=-0.5, theta="x")+
      scale_y_continuous(limits=function(limits){
        c(0, limits[2])
      })+
      labs(x="", y="", title="Stat Differences", fill=paste(p1, ">", p2),
           caption="Dataset from Kaggle: https://www.kaggle.com/datasets/alopez247/pokemon")+
      theme_minimal()+
      theme(
        strip.text.x = element_text(size=10, face="bold")
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)