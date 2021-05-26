library(shiny)
library(shinydashboard)
library(dashboardthemes)



### creating custom theme object
theme_blue_gradient <- shinyDashboardThemeDIY(
    
    ### general
    appFontFamily = "Montserrat"
    ,appFontColor = "#6d6e70"
    ,bodyBackColor = "#f2f2f2"
    
    ### header
    ,logoBackColor = "rgb(241,90,41)"
    
    ,headerButtonBackColor = "rgb(241,90,41)"
    ,headerButtonIconColor = "rgb(75,75,75)"
    ,headerButtonBackColorHover = "rgb(210,210,210)"
    ,headerButtonIconColorHover = "rgb(0,0,0)"
    
    ,headerBackColor = "rgb(241,90,41)"
    ,headerBoxShadowColor = "#aaaaaa"
    ,headerBoxShadowSize = "2px 2px 2px"
    
    ### sidebar
    ,sidebarBackColor = cssGradientThreeColors(
        direction = "down"
        ,colorStart = "#f7f8f8"
        ,colorMiddle = "#f7f8f8"
        ,colorEnd = "#f7f8f8"
        ,colorStartPos = 0
        ,colorMiddlePos = 50
        ,colorEndPos = 100
    )
    ,sidebarPadding = 10
    
    ,sidebarMenuBackColor = "rgb(190,30,45)"
    ,sidebarMenuPadding = 5
    ,sidebarMenuBorderRadius = 10
    
    ,sidebarShadowRadius = "3px 5px 5px"
    ,sidebarShadowColor = "#aaaaaa"
    
    ,sidebarUserTextColor = "rgb(190,30,45)"
    
    ,sidebarSearchBackColor = "rgb(190,30,45)"
    ,sidebarSearchIconColor = "rgb(255,255,255)"
    ,sidebarSearchBorderColor = "rgb(190,30,45)"
    
    ,sidebarTabTextColor = "rgb(253,214,138)"
    ,sidebarTabTextSize = 13
    ,sidebarTabBorderStyle = "none none solid none"
    ,sidebarTabBorderColor = "rgb(35,106,135)"
    ,sidebarTabBorderWidth = 1
    
    ,sidebarTabBackColorSelected = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgba(44,222,235,1)"
        ,colorMiddle = "rgba(44,222,235,1)"
        ,colorEnd = "rgba(0,255,213,1)"
        ,colorStartPos = 0
        ,colorMiddlePos = 30
        ,colorEndPos = 100
    )
    ,sidebarTabTextColorSelected = "rgb(253,214,138)"
    ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
    
    ,sidebarTabBackColorHover = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgba(44,222,235,1)"
        ,colorMiddle = "rgba(44,222,235,1)"
        ,colorEnd = "rgba(0,255,213,1)"
        ,colorStartPos = 0
        ,colorMiddlePos = 30
        ,colorEndPos = 100
    )
    ,sidebarTabTextColorHover = "rgb(50,50,50)"
    ,sidebarTabBorderStyleHover = "none none solid none"
    ,sidebarTabBorderColorHover = "rgb(75,126,151)"
    ,sidebarTabBorderWidthHover = 1
    ,sidebarTabRadiusHover = "0px 20px 20px 0px"
    
    ### boxes
    ,boxBackColor = "rgb(255,255,255)"
    ,boxBorderRadius = 5
    ,boxShadowSize = "0px 0px 0px"
    ,boxShadowColor = "rgba(0,0,0,.1)"
    ,boxTitleSize = 16
    ,boxDefaultColor = "rgb(253,214,138)"
    ,boxPrimaryColor = "rgb(190,30,45)"
    ,boxSuccessColor = "rgb(190,30,45)"
    ,boxWarningColor = "rgb(190,30,45)"
    ,boxDangerColor = "rgb(255,255,255)"
    
    ,tabBoxTabColor = "rgb(190,30,45)"
    ,tabBoxTabTextSize = 14
    ,tabBoxTabTextColor = "#fcaf17"
    ,tabBoxTabTextColorSelected = "rgb(255,255,255)"
    ,tabBoxBackColor = "rgb(255,255,255)"
    ,tabBoxHighlightColor = "rgb(255,255,255)"
    ,tabBoxBorderRadius = 5
    
    ### inputs
    ,buttonBackColor = "rgb(245,245,245)"
    ,buttonTextColor = "rgb(0,0,0)"
    ,buttonBorderColor = "rgb(200,200,200)"
    ,buttonBorderRadius = 20
    
    ,buttonBackColorHover = "rgb(235,235,235)"
    ,buttonTextColorHover = "rgb(100,100,100)"
    ,buttonBorderColorHover = "rgb(200,200,200)"
    
    ,textboxBackColor = "rgb(255,255,255)"
    ,textboxBorderColor = "rgb(200,200,200)"
    ,textboxBorderRadius = 5
    ,textboxBackColorSelect = "rgb(245,245,245)"
    ,textboxBorderColorSelect = "rgb(200,200,200)"
    
    ### tables
    ,tableBackColor = "rgb(255,255,255)"
    ,tableBorderColor = "rgb(240,240,240)"
    ,tableBorderTopSize = 1
    ,tableBorderRowSize = 1
    
)

header <- dashboardHeader(titleWidth = 0, disable = TRUE)
sidebar <- dashboardSidebar(uiOutput("sidebar"), width = 510)

body <- dashboardBody(uiOutput("body"),
                      
                      tags$head(
                          tags$style(HTML('.box.box-solid.box-primary>.box-header {
                          color:#fff;
                          background:#666666 }')
                          )))

ui <- dashboardPage(header, sidebar, body,   
                    theme_blue_gradient
                    
)
