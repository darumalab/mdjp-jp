# Libray -----------------------------------------------------------------

# library(googlesheets)
library(tidyr)
library(dplyr)
library(shiny)
library(DT)
library(shinydashboard)
library(ggplot2)
library(zoo)
library(dygraphs)
library(scales)
library(plotly)

# color-blind colors
black = "#000000"
gray = grey = "#999999"
bluish_green = "#009E73"
blue = "#0072B2"
sky_blue = "#56B4E9"
yellow = "#F0E442"
orange = "#E69F00"
reddish_purple = "#CC79A7"
vermilion = "#D55E00"
pal = c(vermilion, reddish_purple, orange, yellow, sky_blue, blue, bluish_green,
        gray, black)

# Data --------------------------------------------------------------------

data_path = "./data"

## import the annual/monthly data for dygraphs
annual_marriage = file.path(data_path, "annual_marriage.csv")
annual_marriage = read.csv(annual_marriage, stringsAsFactors=F)

annual_divorce = file.path(data_path, "annual_divorce.csv")
annual_divorce = read.csv(annual_divorce, stringsAsFactors=F)

monthly_marriage = file.path(data_path, "monthly_marriage.csv")
monthly_marriage = read.csv(monthly_marriage)

monthly_divorce = file.path(data_path, "monthly_divorce.csv")
monthly_divorce = read.csv(monthly_divorce)

## import nationality data for ggplot
long_bicultural = file.path(data_path, "long_bicultural.csv")
foreign_rate = read.csv(long_bicultural, stringsAsFactors=F)

## import marriage/divorce rate for tables
df_marriage = file.path(data_path, "df_marriage.csv")
df_marriage = read.csv(df_marriage, check.names = FALSE)

df_divorce = file.path(data_path, "df_divorce.csv")
df_divorce = read.csv(df_divorce, check.names = FALSE)

df_monthly_marriage = file.path(data_path, "df_monthly_marriage.csv")
df_mo_marriage = read.csv(df_monthly_marriage)

df_monthly_divorce = file.path(data_path, "df_monthly_divorce.csv")
df_mo_divorce = read.csv(df_monthly_divorce)

df_foreign_wife = file.path(data_path, "df_foreign_wife.csv")
df_frgn_wife = read.csv(df_foreign_wife)

df_foreign_husband = file.path(data_path, "df_foreign_husband.csv")
df_frgn_husband = read.csv(df_foreign_husband)

age_difference = file.path(data_path, "age_difference.csv")
age_diff = read.csv(age_difference)


prefecture <- unique(annual_marriage$都道府県)
year <- unique(monthly_divorce$年)

# Simple header -----------------------------------------------------------

header <- dashboardHeader(title = "統計で分かる日本の婚姻率と離婚率のデータ (1935-2013)",
                          titleWidth = 675)

# Sidebar --------------------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("グラフで見る", icon = icon("bar-chart-o"),
             menuSubItem("年ごとの値", tabName = "annual_rate", icon = icon("institution")),
             menuSubItem("月ごとの値", tabName = "monthly_rate", icon = icon("line-chart")),
             menuSubItem("国際結婚のデータ", tabName = "foreign", icon = icon("heart"))
    ),
    menuItem("数字で見る", icon = icon("archive"),
             menuSubItem("年ごとの値", tabName = "num_rate"),
             menuSubItem("月ごとの値", tabName = "num_monthly_rate"),
             menuSubItem("国際結婚のデータ", tabName = "num_foreign"),
             menuSubItem("平均結婚年齢", tabName = "num_age")
    ),
    menuItem("制作 Cabaceo LLC", href = "https://jp.cabaceo.com"),
    # insert google ads
    tags$script(async="async", src="//pagead2.googlesyndication.com/pagead/js/adsbygoogle.js"),
    includeHTML("www/google-ads-size-auto.html"),
    tags$script("(adsbygoogle = window.adsbygoogle || []).push({});")
  )
)

# Compose dashboard body --------------------------------------------------

body <- dashboardBody(
  # CSS
  tags$head(tags$style(HTML('
                            .main-header .logo {
                            font-family: "freight-sans-pro", Helvetica Neue, "Helvetica", Arial, "sans-serif";
                            font-size: 24px;
                            }
                            '))),

  # Items
  tabItems(
    tabItem("annual_rate",
            fluidRow(
              box(width = 12, title = "年ごとの婚姻率と離婚率",
                  dygraphOutput("plot_annual"))
            ),
            # Select prefecture
            fluidRow(
              box(selectInput("都道府県", "都道府県:", choices = prefecture)),
              box(p(tags$b("データの説明")),
                  p("チャートには１９３５年から２０１３年にかけての婚姻率、離婚率の推移が表示されています。左のドロップダウン・メニューを開けて、都道府県別のデータを選択できます。"))
            )
     ),

    tabItem("monthly_rate",
            fluidRow(
              box(width = 12, title ="月ごとの婚姻率と離婚率",
                                dygraphOutput("plot_monthly"))
            ),
            # Select year
            fluidRow(
              box(selectInput("年", "年:", choices = year)),
              box(p(tags$b("データの説明")),
                  p("チャートには月ごとの婚姻率、離婚率の推移が表示されています。左のドロップダウン・メニューを開けて、１９４７年から２０１３年にかけてのデータを選択できます。"))
            )
    ),

    tabItem("foreign",
            fluidRow(
              box(width = 12, title ="国籍別にみた国際結婚",
                  plotlyOutput("plot_frgn"))
            ),
            fluidRow(
              column(width = 6,
                     # Select prefecture
                     box(width = NULL,
                         selectInput("foreign_prefecture", "都道府県:", choices = prefecture)),
                     # Select type
                     box(width = NULL,
                         radioButtons("type", "夫婦別:", choices = unique(foreign_rate$夫婦別)))
              ),
              column(width = 6,
                     box(width = NULL, p(tags$b("データの説明")),
                         p("グラフには１９９５年から２０１３年にかけての夫妻の国籍別にみた婚姻データが表示されています。左のメニューをから、男女、都道府県別のデータを選択できます。"))
              )
            )
    ),

    tabItem("num_rate",
            fluidRow(
              valueBoxOutput("vbox_marriage"),
              valueBoxOutput("vbox_max_divorce"),
              valueBoxOutput("vbox_min_divorce")
            ),
            tabsetPanel(
              tabPanel("年ごとの婚姻率",
                       fluidRow(
                         box(width = 12, solidHeader = TRUE,
                             p(tags$b("年ごとの婚姻率")),
                             dataTableOutput('dt_marriage'))
                       )
              ),
              tabPanel("年ごとの離婚率",
                       fluidRow(
                         box(width = 12, solidHeader = TRUE,
                             p(tags$b("年ごとの離婚率")),
                             dataTableOutput('dt_divorce'))
                       )
              )
            )
    ),

    tabItem("num_monthly_rate",
            fluidRow(
              valueBoxOutput("vbox_pop_marriage"),
              valueBoxOutput("vbox_unpop_marriage"),
              valueBoxOutput("vbox_pop_divorce")
            ),
            tabsetPanel(
              tabPanel("月ごとの婚姻率",
                       fluidRow(
                         box(width = 12, solidHeader = TRUE,
                             p(tags$b("月ごとの婚姻率")),
                             dataTableOutput('dt_mo_marriage'))
                       )
              ),
              tabPanel("月ごとの離婚率",
                       fluidRow(
                         box(width = 12, solidHeader = TRUE,
                             p(tags$b("月ごとの離婚率")),
                             dataTableOutput('dt_mo_divorce'))
                       )
              )
            )
    ),

    tabItem("num_foreign",
            fluidRow(
              valueBoxOutput("vbox_frgn_wife"),
              valueBoxOutput("vbox_frgn_husband")
            ),
            tabsetPanel(
              tabPanel("妻が外国人",
                       fluidRow(
                         box(width = 12, solidHeader = TRUE,
                             p(tags$b("国籍別にみた国際結婚の割合")),
                             dataTableOutput('dt_frgn_wife'))
                       )
              ),
              tabPanel("夫が外国人",
                       fluidRow(
                         box(width = 12, solidHeader = TRUE,
                             p(tags$b("国籍別にみた国際結婚割合")),
                             dataTableOutput('dt_frgn_husband'))
                       )
              )
            )

    ),

    tabItem("num_age",
            fluidRow(
              valueBoxOutput("vbox_age_gap"),
              valueBoxOutput("vbox_age_women"),
              valueBoxOutput("vbox_age_men")
            ),
            fluidRow(
              box(width = 12, solidHeader = TRUE,
                  p(tags$b("年齢別")),
                  dataTableOutput('dt_age'))
            )
    )
   )
)

# Setup Shiny app UI components -------------------------------------------

ui <- dashboardPage(header, sidebar, body, skin = "black")

# Setup Shiny app back-end components -------------------------------------

server <- function(input, output) {

  # dygraphs for annual marriage/divorce rates
  an_marriage <- reactive({annual_marriage[annual_marriage$都道府県 == input$都道府県, ]})
  an_divorce<- reactive({annual_divorce[annual_divorce$都道府県 == input$都道府県, ]})
  output$plot_annual <- renderDygraph({
    an_td = as.Date(an_marriage()$time, format="%m/%d/%Y")
    婚姻率 = zoo(x = an_marriage()$割合, order.by = an_td)
    an_td2 = as.Date(an_divorce()$time, format = "%m/%d/%Y")
    離婚率 = zoo(x = an_divorce()$割合, order.by = an_td2)
    annual <- cbind(婚姻率, 離婚率)
    dygraph(annual) %>%
      dyAxis("y", label = "婚姻率 (人口1000人比)",
             valueRange = c(0, 15), independentTicks = TRUE) %>%
      dyAxis("y2", label = "離婚率 (人口1000人比)",
             valueRange = c(0, 3), independentTicks = TRUE) %>%
      dySeries("離婚率", axis = 'y2')
  })

  # dygraphs for monthly marriage/divorce rates
  mo_marriage <- reactive({monthly_marriage[monthly_marriage$年 == input$年, ]})
  mo_divorce<- reactive({monthly_divorce[monthly_divorce$年 == input$年, ]})
  output$plot_monthly <- renderDygraph({
    mo_td = as.Date(mo_marriage()$time, format = "%m/%d/%Y")
    婚姻率 = zoo(x = mo_marriage()$割合, order.by = mo_td)
    mo_td2 = as.Date(mo_divorce()$time, format = "%m/%d/%Y")
    離婚率 = zoo(x = mo_divorce()$割合, order.by = mo_td2)
    monthly <- cbind(婚姻率, 離婚率)
    dygraph(monthly) %>%
      dyAxis("y", label = "比率 (人口1000人比)",
             valueRange = c(3, 13.2))
  })

  # ggplot for natinality rates
  df_frgn <- reactive({foreign_rate[foreign_rate$都道府県 == input$foreign_prefecture &
                                      foreign_rate$夫婦別 == input$type, ]})
  output$plot_frgn <- renderPlotly({ # renderPlot
          dat = df_frgn() %>% mutate(年 = factor(年)) %>%
                  spread(key = 国籍, value = pct)
          plot_ly(dat, x = ~年, y = ~その他の国々, type='bar', color = I(black),
                  name="その他の国々") %>%
                  add_trace(y = ~イギリス, color = I(gray),
                            name = "イギリス") %>%
                  add_trace(y = ~ペルー, color = I(bluish_green),
                            name = "ペルー") %>%
                  add_trace(y = ~アメリカ, color = I(blue),
                            name = "アメリカ") %>%
                  add_trace(y = ~ブラジル, color = I(sky_blue),
                            name = "ブラジル") %>%
                  add_trace(y = ~タイ, color = I(yellow),
                            name = "タイ") %>%
                  add_trace(y = ~韓国, color = I(orange),
                            name = "韓国") %>%
                  add_trace(y = ~フィリピン, color = I(reddish_purple),
                            name = "フィリピン") %>%
                  add_trace(y = ~中国, color = I(vermilion),
                            name = "中国") %>%
                  layout(barmode = 'stack',
                         yaxis = list(title = '% 国籍別',
                                      ticksuffix = '%',
                                      tickfont = list(size=10)),
                         xaxis = list(title = '', tickfont = list(size=10)),
                         legend = list(x=0.4, y=1.12, orientation="h"))
  })

  # table - marriage rate
  output$dt_marriage = DT::renderDataTable({
    df_marriage
  }, options = list(scrollX = TRUE)
  )

  # table - divorce rate
  output$dt_divorce = DT::renderDataTable({
    df_divorce
  }, options = list(scrollX = TRUE)
  )

  # table - monthly marriage rate
  output$dt_mo_marriage = DT::renderDataTable({
    df_mo_marriage
  }, options = list(scrollX = TRUE)
  )

  # table - monthly divorce rate
  output$dt_mo_divorce = DT::renderDataTable({
    df_mo_divorce
  }, options = list(scrollX = TRUE)
  )

  # table - foreign wife
  output$dt_frgn_wife = DT::renderDataTable({
    df_frgn_wife
  }, options = list(scrollX = TRUE)
  )

  # table - foreign husband
  output$dt_frgn_husband = DT::renderDataTable({
    df_frgn_husband
  }, options = list(scrollX = TRUE)
  )

  # table - average marriyng age
  output$dt_age = DT::renderDataTable({
    age_diff
  }, options = list(scrollX = TRUE)
  )

  # infoBox - prefecture with highest marriage rate in 2013
  output$vbox_marriage = renderValueBox({
    valueBox(
      df_marriage[which.max(df_marriage$'2013'), "都道府県"],
      "2013年、最も婚姻率の高かった都道府県",
      icon = icon("heart-o"),
      color = "aqua"
    )
  })

  # infoBox - prefecture with highest divorce rate in 2013
  output$vbox_max_divorce = renderValueBox({
    valueBox(
      df_divorce[which.max(df_divorce$'2013'), "都道府県"],
      "2013年、最も離婚率の高かった都道府県",
      icon = icon("edit"),
      color = "aqua"
    )
  })

  # infoBox - prefecture with lowest divorce rate in 2013
  output$vbox_min_divorce = renderValueBox({
    valueBox(
      df_divorce[which.min(df_divorce$'2013'), "都道府県"],
      "2013年、最も離婚率の低かった都道府県",
      icon = icon("edit"),
      color = "aqua"
    )
  })

  # infoBox - most popular month for marriage in 2013
  output$vbox_pop_marriage = renderValueBox({
    valueBox(
      names(which.max(
        df_mo_marriage[which(df_mo_marriage[, '年'] == "2013"),
                       2:ncol(df_mo_marriage)])),
      "2013年、最も結婚の多かった月",
      icon = icon("heart-o"),
      color = "aqua"
    )
  })

  # infoBox - most unpopular month for marriage in 2013
  output$vbox_unpop_marriage = renderValueBox({
    valueBox(
      names(which.min(
        df_mo_marriage[which(df_mo_marriage[, '年'] == "2013"),
                       2:ncol(df_mo_marriage)])),
      "2013年、最も結婚の少なかった年",
      icon = icon("edit"),
      color = "aqua"
    )
  })

  # infoBox - most popular month for divorce in 2013
  output$vbox_pop_divorce = renderValueBox({
    valueBox(
      names(which.max(
        df_mo_divorce[which(df_mo_divorce[, '年'] == "2013"),
                      2:ncol(df_mo_divorce)])),
      "2013年、最も離婚の多かった月",
      icon = icon("edit"),
      color = "aqua"
    )
  })

  # infoBox - The year with the highest number of American husbands
  output$vbox_frgn_wife = renderValueBox({
    valueBox(
      df_frgn_wife[which.min(df_frgn_wife$アメリカ), "年"],
      "アメリカ人男性との結婚が最も多かった年",
      icon = icon("female"),
      color = "aqua"
    )
  })

  # infoBox - The year with the highest number of American wives
  output$vbox_frgn_husband = renderValueBox({
    valueBox(
      df_frgn_husband[which.min(df_frgn_husband$アメリカ), "年"],
      "アメリカ人女性との結婚が最も多かった年",
      icon = icon("male"),
      color = "aqua"
    )
  })

  # infoBox - age gap
  output$vbox_age_gap = renderValueBox({
    valueBox(
      age_diff$年齢差の平均[nrow(age_diff)],
      "2013年、夫婦の年齢差の平均",
      icon = icon("heart-o"),
      color = "aqua"
    )
  })

  # infoBox - mean age of men
  output$vbox_age_men = renderValueBox({
    valueBox(
      age_diff$男性の平均結婚年[nrow(age_diff)],
      "2013年、男性の平均結婚年齢",
      icon = icon("male"),
      color = "aqua"
    )
  })

  # infoBox - mean age of women
  output$vbox_age_women = renderValueBox({
    valueBox(
      age_diff$女性の平均結婚年齢[nrow(age_diff)],
      "2013年、女性の平均結婚年齢",
      icon = icon("female"),
      color = "aqua"
    )
  })

}

# Render Shiny app --------------------------------------------------------

shinyApp(ui, server)
