# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(zoo)
library(tidyr)
library(stringr)

ui <- fluidPage(

    titlePanel("Contec: On Hand Inventory Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            ## get csv file
            fileInput("file1", "Choose File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            

           
            ## select equipment to analyze
            selectInput('equipment', 'Choose a Equipment',
                        choices=c('Original Uploaded File'
                                  ,'NonWireless-Modem'
                                  ,'Wireless-Modem'
                                  ,'EMTA-Wireless'
                                  ,'EeroBase'
                                  ,'EeroBeacon'
                                  ,'WOWTV+')),
            
            
            ## ask the user of the time of the report
            ##dateInput('date','Give me the date of the report'
               ##       ,format='yyyy-mm-dd'),
            
            ## using text input to replace date
            ##textInput('date2','Input the date of the On Hand Inventory Report')
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h3('Please only upload On Hand Inventory CSV file'), 
           ##textOutput('data1'),
           tableOutput('data'),
           
        )
    )
)



server <- function(input, output) {

    output$data <- renderTable({
        req(input$file1)
        
        ## check to make sure the data is accurate from upload
        df<-data.frame(read.csv(input$file1$datapath,
                                skip = 4,
                                header = TRUE))
        
        df1<-data.frame(read.csv(input$file1$datapath,
                     skip = 4,
                     header = TRUE))
        
        ## get market level data
        df1$Market<-trimws(substr(df1$Item.Name,1,5))
        
        df1$Market[df1$Market!='ICOMS' & df1$Market!='USHA' & df1$Market!='GLDS' & df1$Market!='COMPO']<-'OEM'
        
        
        ## get unit status of the devices. D means that the device is Tested Bad Used and Not Tested Bad
        ## L means that it is tested good used or also tested good new (NL)
        
        substrRight<-function(x,n){
            substr(x, nchar(x),nchar(x))
        }
    
        df1$Unit.Status<-substrRight(df1$Contec.Item.ID,nchar(df1$Contec.Item.ID))
        
        ## from unit status, make another column of for not tested used, bad tested used not(tested used bad and tested bad used),
        ## good tested used, tested used, new
        
        df1$Tested<-ifelse(str_detect(df1$Item.Name,'NOT TESTED USED'),'NOT TESTED USED',
                           ifelse(str_detect(df1$Item.Name,'TESTED GOOD USED'),'GOOD TESTED USED',
                                  ifelse(str_detect(df1$Item.Name,'TESTED USED BAD'),'BAD TESTED USED', 
                                         ifelse(str_detect(df1$Item.Name,'TESTED BAD USED'),'BAD TESTED USED',
                                                ifelse(str_detect(df1$Item.Name,'TESTED USED'),'TESTED USED',
                                                       ifelse(str_detect(df1$Item.Name,'NEW'),'NEW',
                                                              ifelse(str_detect(df1$Item.Name,'USED'),'USED','NOT SURE'))
                                                )
                                         )
                                  )
                           )
                           
        )

        
        
        ## get Equipment Type
        df1$Equipment<-'Others'
        
        ## Wireless Modem:	DG2470A, DG3270A, CGA4234DGW, DG3450A, 832-0020, 832-0021, 832-0017,832-0015, DG3270, DG3450, DG3450G, CGA4234
        df1$Equipment[grep('DG2470A|DG3270A|DG3450A|CGA4234DGW|832-0020|832-0021|832-0017|832-0015|DG3270|DG3450|DG3450G|CGA4234',
                           df1$Vendor.Part.Number,ignore.case = TRUE)]<-'Wireless Modem'
        
        ## Eero Base: B010001, 832-0038, 832-0040    
        df1$Equipment[grep('B010001|832-0038|832-0040',
                           df1$Vendor.Part.Number, ignore.case=TRUE)]<-'Eero Base'
        
        ## Eero Beacon: D010001 
        df1$Equipment[grep('D010001|832-0039',
                           df1$Vendor.Part.Number, ignore.case=TRUE)]<-'Eero Beacon'
        
        ## Wireless EMTA:	CGA4234VGW	TG3452A	TG2472G	TG862G CG4234V TG3452G 822-0021 822-018 822-0020 TG3452 TG862G TG2472
        df1$Equipment[grep('CGA4234VGW|TG2472G|TG3452A|TG862G|CG4234V|TG3452G|822-0021|822-018|822-0020|TG3452|TG862G|TG2472',
                           df1$Vendor.Part.Number,ignore.case = TRUE)]<-'Wireless EMTA'
        
        ## IP Video / WOW TV+: UIW4020	UIW4020WOW
        df1$Equipment[grep('UIW4020WOW|819-0005',
                           df1$Vendor.Part.Number,ignore.case = TRUE)]<-'IP Video / WOW TV+'
        
        ## Non-Wireless Modem	CM8200	TC4400-AM
        df1$Equipment[grep('CM8200|TC4400-AM|TC4400|831-0008|831-0007',
                           df1$Vendor.Part.Number, ignore.case = TRUE)]<-'Non-Wireless Modem'
        
        
        
        ## filter by Equipment and Tested. On Tested filter out 'Not Sure'. This will not help for the analysis
        if(input$equipment=='Wireless-Modem'){
            data<-df1%>%
                filter(Equipment=='Wireless Modem' & Tested!='NOT SURE')%>%
                select(Market,Tested,Total)%>%
                group_by(Market,Tested)%>%
                summarise(total=sum(Total))%>%
                pivot_wider(names_from = Market, values_from = total)%>%
                mutate(ICOMS=as.integer(replace_na(ICOMS,0)),
                       USHA=as.integer(replace_na(USHA,0)),
                       GLDS=as.integer(replace_na(GLDS,0)),
                       COMPO=as.integer(replace_na(COMPO,0)),
                       OEM=as.integer(replace_na(OEM,0))  
                       )
            
            return(data)
        }
        else if (input$equipment=='NonWireless-Modem'){
            
            data<-df1%>%
                filter(Equipment=='Non-Wireless Modem' & Tested!='NOT SURE')%>%
                select(Market,Tested,Total)%>%
                group_by(Market,Tested)%>%
                summarise(total=sum(Total))%>%
                pivot_wider(names_from = Market, values_from = total)%>%
                mutate(ICOMS=as.integer(replace_na(ICOMS,0)),
                       USHA=as.integer(replace_na(USHA,0)),
                       GLDS=as.integer(replace_na(GLDS,0)),
                       COMPO=as.integer(replace_na(COMPO,0)),
                       OEM=as.integer(replace_na(OEM,0))  
                )
            
            return(data)
        }
        else if (input$equipment=='EMTA-Wireless'){
            
            data<-df1%>%
                filter(Equipment=='Wireless EMTA' & Tested!='NOT SURE')%>%
                select(Market,Tested,Total)%>%
                group_by(Market,Tested)%>%
                summarise(total=sum(Total))%>%
                pivot_wider(names_from = Market, values_from = total)%>%
                mutate(ICOMS=as.integer(replace_na(ICOMS,0)),
                       USHA=as.integer(replace_na(USHA,0)),
                       GLDS=as.integer(replace_na(GLDS,0)),
                       COMPO=as.integer(replace_na(COMPO,0)),
                       OEM=as.integer(replace_na(OEM,0))  
                )
            

            return(data)
        }
        else if (input$equipment=='EMTA-Wireless'){
            
            data<-df1%>%
                filter(Equipment=='Wireless EMTA' & Tested!='NOT SURE')%>%
                select(Market,Tested,Total)%>%
                group_by(Market,Tested)%>%
                summarise(total=sum(Total))%>%
                pivot_wider(names_from = Market, values_from = total)%>%
                mutate(ICOMS=as.integer(replace_na(ICOMS,0)),
                       USHA=as.integer(replace_na(USHA,0)),
                       GLDS=as.integer(replace_na(GLDS,0)),
                       COMPO=as.integer(replace_na(COMPO,0)),
                       OEM=as.integer(replace_na(OEM,0))  
                )

            return(data)
        }
        else if (input$equipment=='EeroBase'){
            
            data<-df1%>%
                filter(Equipment=='Eero Base' & Tested!='NOT SURE')%>%
                select(Market,Tested,Total)%>%
                group_by(Market,Tested)%>%
                summarise(total=sum(Total))%>%
                pivot_wider(names_from = Market, values_from = total)%>%
                mutate(ICOMS=as.integer(replace_na(ICOMS,0)),
                       USHA=as.integer(replace_na(USHA,0)),
                       GLDS=as.integer(replace_na(GLDS,0)),
                       COMPO=as.integer(replace_na(COMPO,0)),
                       OEM=as.integer(replace_na(OEM,0))  
                )
            
            return(data)
            
        }
        else if (input$equipment=='WOWTV+'){
            
            data<-df1%>%
                filter(Equipment=='IP Video / WOW TV+' & Tested!='NOT SURE')%>%
                select(Market,Tested,Total)%>%
                group_by(Market,Tested)%>%
                summarise(total=sum(Total))%>%
                pivot_wider(names_from = Market, values_from = total)%>%
                mutate(ICOMS=as.integer(replace_na(ICOMS,0)),
                       USHA=as.integer(replace_na(USHA,0)),
                       ##GLDS=as.integer(replace_na(GLDS,0)),
                       ##COMPO=as.integer(replace_na(COMPO,0)),
                       OEM=as.integer(replace_na(OEM,0))  
                )
            
            return(data)
        }
        else if (input$equipment=='EeroBeacon'){
            
            data<-df1%>%
                filter(Equipment=='Eero Beacon'& Tested!='NOT SURE')%>%
                select(Market,Tested,Total)%>%
                group_by(Market,Tested)%>%
                summarise(total=sum(Total))%>%
                pivot_wider(names_from = Market, values_from = total)%>%
                mutate(ICOMS=as.integer(replace_na(ICOMS,0)),
                       USHA=as.integer(replace_na(USHA,0)),
                       GLDS=as.integer(replace_na(GLDS,0)),
                       COMPO=as.integer(replace_na(COMPO,0)),
                       OEM=as.integer(replace_na(OEM,0))  
                )
            
            return(data)
        }
        
        
        ##df1$Date<-as.Date(input$date, origin = "1970-01-01",'%m/%d/%Y')
        ##df1$Date<-input$date2
        
    
        return(df)    
    })
    output$data1<-renderPrint({
        as.Date(input$date, origin = "1970-01-01",'%m/%d/%Y')
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
