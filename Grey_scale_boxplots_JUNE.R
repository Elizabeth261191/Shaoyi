

library(ggplot2)
library(ggpubr)
library(readr)

(p1 <- ggplot(H3, aes(x=Treatment, y=AG_DW, fill=Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none") + labs(x = "Soil Moisture Treatment", y = "Aboveground dry weight (grams)") +  facet_grid(.~Species) 
     +   theme(strip.background = element_rect(colour="black", fill="white",))+ scale_fill_grey())

(p2 <- ggplot(H3, aes(x=Treatment, y=BG_DW, fill=Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none") + labs(x = "Soil Moisture Treatment", y = "Belowground dry weight (grams)") +  facet_grid(.~Species) 
 +   theme(strip.background = element_rect(colour="black", fill="white",))+ scale_fill_grey())

(p3 <- ggplot(H3, aes(x=Treatment, y=RS, fill=Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none") + labs(x = "Soil Moisture Treatment", y = "Root: shoot ratio") +  facet_grid(.~Species) 
       +   theme(strip.background = element_rect(colour="black", fill="white",))+ scale_fill_grey())

ggarrange(p1, p2, p3, = c("a", "b", "c"))

  
stable_iso_area <- read_csv("GitHub/Masters_dream/stable_iso_area.csv")
                                                                            
View(stable_iso_area)
stable_iso_area$Treatment <- factor(stable_iso_area$Treatment, levels=c('4%','8%','16%'))
stable_iso_area$Species <- factor(stable_iso_area$Species, levels=c('VS','VE'))
iso_VE<-subset(stable_iso_area, Species=="VE")
iso_VS<-subset(stable_iso_area, Species=="VS")
stable_iso_area<-na.omit(stable_iso_area)

Height <- read_csv("GitHub/Masters_dream/Height.csv")
 View(Height)
Height$IndividualID <- as.factor(paste(as.character(Height$Tag),as.character(Height$Pot),sep="_")) 
 Height$Treatment <- factor(Height$Treatment, levels=c('4%','8%','16%'))
 Height$Species <- factor(Height$Species, levels=c('VS','VE'))
H3<-subset(Height, Harvest == "3")
VS<-subset(H3, Species=="VS")
 VE<-subset(H3, Species=="VE")

 (p18 <- ggplot(stable_iso_area, aes(Treatment, N,fill= Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Treatment", y = "Leaf N content (per 250 um)") 
       +   facet_wrap(.~Species) +   theme(strip.background = element_rect(colour="black", fill="white",)) + scale_x_discrete(limits=c("4%","8%","16%")))

 (p19 <- ggplot(stable_iso_area, aes(Treatment, delta15N,fill= Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Treatment", y = "Leaf 15N content (per 200 um)") 
 +   facet_wrap(.~Species) +   theme(strip.background = element_rect(colour="black", fill="white",)) + scale_x_discrete(limits=c("4%","8%","16%")))

 (p20 <- ggplot(stable_iso_area, aes(Treatment, C,fill= Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Treatment", y = "Leaf C content (per 200 um)") 
       +   facet_wrap(.~Species) +   theme(strip.background = element_rect(colour="black", fill="white",)) + scale_x_discrete(limits=c("4%","8%","16%")))
                                                                             
 (p21 <- ggplot(stable_iso_area, aes(Treatment, delta13C, fill= Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Treatment", y = "Leaf 13C content (per 200 um)") 
     +   facet_wrap(.~Species)+theme(strip.background = element_rect(colour="black", fill="white",)) + scale_x_discrete(limits=c("4%","8%","16%")))
                                                                           
 
  ggarrange(p18, p19, p20, p21,labels = c("a", "b", "c", "d"))

 (p21 <- ggplot(stable_iso_area, aes(Treatment, delta13C, fill= Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Treatment", y = "Leaf 13C") 
    +   facet_wrap(.~Species)+theme(strip.background = element_rect(colour="black", fill="white",)) + scale_x_discrete(limits=c("4%","8%","16%"))+ scale_fill_grey())

  
stable_iso_area <- read_csv("GitHub/Masters_dream/stable_iso_area.csv")
                                                                       
stable_iso_area$Treatment <- factor(stable_iso_area$Treatment, levels=c('4%','8%','16%'))
 stable_iso_area$Species <- factor(stable_iso_area$Species, levels=c('VE','VS'))
 iso_VE<-subset(stable_iso_area, Species=="VE")
iso_VS<-subset(stable_iso_area, Species=="VS")
(p4 <- ggplot(stable_iso_area, aes(Treatment, delta13C, fill= Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Treatment", y = "Leaf 13C") 
  +   facet_wrap(.~Species)+theme(strip.background = element_rect(colour="black", fill="white",)) + scale_x_discrete(limits=c("4%","8%","16%"))+ scale_fill_grey())
                                                                       > Height <- read_csv("GitHub/Masters_dream/Height.csv")
      
                                                                       > Height$IndividualID <- as.factor(paste(as.character(Height$Tag),as.character(Height$Pot),sep="_")) 
                                                                       > Height$Treatment <- factor(Height$Treatment, levels=c('4%','8%','16%'))
                                                                       > Height$Species <- factor(Height$Species, levels=c('VE','VS'))
                                                                       > Height3<-subset(Height, Harvest == "3")
                                                                       > p<-ggplot(Height3,aes(Week, Height, group=Treatment, col=Treatment, shape= Treatment))  +geom_point() + geom_smooth(method=lm,  aes(fill=Treatment))+theme.clean()+ labs(x = "Week", y = "Height (mm)")+  facet_grid(.~Species) 
                                                                       > p+ scale_x_continuous(breaks = seq(1,15 , by = 1))
                                                                       `geom_smooth()` using formula 'y ~ x'
                                                                       > theme.clean <- function(){
                                                                         +   theme_bw()+
                                                                           +     theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 1, hjust = 1),
                                                                                       +           axis.text.y = element_text(size = 12),
                                                                                       +           axis.title.x = element_text(size = 12, face = "plain"),             
                                                                                       +           axis.title.y = element_text(size = 12, face = "plain"),             
                                                                                       +           panel.grid.major.x = element_blank(),                                          
                                                                                       +           panel.grid.minor.x = element_blank(),
                                                                                       +           panel.grid.minor.y = element_blank(),
                                                                                       +           panel.grid.major.y = element_blank(),  
                                                                                       +           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
                                                                                       +           plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
                                                                                       +           legend.text = element_text(size = 12, face = "italic"),          
                                                                                       +           legend.title = element_blank(),                              
                                                                                       +           legend.position = c(0.9, 0.9))
                                                                         + }
                                                                       > p<-ggplot(Height3,aes(Week, Height, group=Treatment, col=Treatment, shape= Treatment))  +geom_point() + geom_smooth(method=lm,  aes(fill=Treatment))+theme.clean()+ labs(x = "Week", y = "Height (mm)")+  facet_grid(.~Species) 
                                                                       > p+ scale_x_continuous(breaks = seq(1,15 , by = 1))
                                                                       `geom_smooth()` using formula 'y ~ x'
                                                                       > p<-ggplot(Height3,aes(Week, Height, group=Treatment, col=Treatment, shape= Treatment))  +geom_point() + geom_smooth(method=lm,  aes(fill=Treatment))+theme.clean()+ labs(x = "Week", y = "Height (mm)")+  facet_grid(.~Species) 
                                                                       > theme.clean <- function(){
                                                                         +   theme_bw()+
                                                                           +     theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 1, hjust = 1),
                                                                                       +           axis.text.y = element_text(size = 12),
                                                                                       +           axis.title.x = element_text(size = 12, face = "plain"),             
                                                                                       +           axis.title.y = element_text(size = 12, face = "plain"),             
                                                                                       +           panel.grid.major.x = element_blank(),                                          
                                                                                       +           panel.grid.minor.x = element_blank(),
                                                                                       +           panel.grid.minor.y = element_blank(),
                                                                                       +           panel.grid.major.y = element_blank(),  
                                                                                       +           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
                                                                                       +           plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
                                                                                       +           legend.text = element_text(size = 12, face = "italic"),          
                                                                                       +           legend.title = element_blank(),                              
                                                                                       +           legend.position = c(0.9, 0.9)),
                                                                         Error: unexpected ',' in:
                                                                           "          legend.title = element_blank(),                              
          legend.position = c(0.9, 0.9)),"
                                                                         >   theme(strip.background =element_rect(fill="white")
                                                                                   + }
                                                                       Error: unexpected '}' in:
                                                                         "  theme(strip.background =element_rect(fill="white")
}"
                                                                       > theme.clean <- function(){
                                                                         +   theme_bw()+
                                                                           +     theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 1, hjust = 1),
                                                                                       +           axis.text.y = element_text(size = 12),
                                                                                       +           axis.title.x = element_text(size = 12, face = "plain"),             
                                                                                       +           axis.title.y = element_text(size = 12, face = "plain"),             
                                                                                       +           panel.grid.major.x = element_blank(),                                          
                                                                                       +           panel.grid.minor.x = element_blank(),
                                                                                       +           panel.grid.minor.y = element_blank(),
                                                                                       +           panel.grid.major.y = element_blank(),  
                                                                                       +           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
                                                                                       +           plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
                                                                                       +           legend.text = element_text(size = 12, face = "italic"),          
                                                                                       +           legend.title = element_blank(),                              
                                                                                       +           legend.position = c(0.9, 0.9)),
                                                                         Error: unexpected ',' in:
                                                                           "          legend.title = element_blank(),                              
          legend.position = c(0.9, 0.9)),"
                                                                         >   theme(strip.background =element_rect(fill="white"))
                                                                         List of 1
                                                                         $ strip.background:List of 5
                                                                         ..$ fill         : chr "white"
                                                                         ..$ colour       : NULL
                                                                         ..$ size         : NULL
                                                                         ..$ linetype     : NULL
                                                                         ..$ inherit.blank: logi FALSE
                                                                         ..- attr(*, "class")= chr [1:2] "element_rect" "element"
                                                                         - attr(*, "class")= chr [1:2] "theme" "gg"
                                                                         - attr(*, "complete")= logi FALSE
                                                                         - attr(*, "validate")= logi TRUE
                                                                         > }
                                                                       Error: unexpected '}' in "}"
                                                                       > theme.clean <- function(){
                                                                         +   theme_bw()+
                                                                           +     theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 1, hjust = 1),
                                                                                       +           axis.text.y = element_text(size = 12),
                                                                                       +           axis.title.x = element_text(size = 12, face = "plain"),             
                                                                                       +           axis.title.y = element_text(size = 12, face = "plain"),             
                                                                                       +           panel.grid.major.x = element_blank(),                                          
                                                                                       +           panel.grid.minor.x = element_blank(),
                                                                                       +           panel.grid.minor.y = element_blank(),
                                                                                       +           panel.grid.major.y = element_blank(),  
                                                                                       +           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
                                                                                       +           plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
                                                                                       +           legend.text = element_text(size = 12, face = "italic"),          
                                                                                       +           legend.title = element_blank(),                              
                                                                                       +           legend.position = c(0.9, 0.9))
                                                                         + }
                                                                       > p+ scale_x_continuous(breaks = seq(1,15 , by = 1))+theme(strip.background =element_rect(fill="red")
                                                                                                                                  + )
                                                                       `geom_smooth()` using formula 'y ~ x'
                                                                       > p+ scale_x_continuous(breaks = seq(1,15 , by = 1))+theme(strip.background =element_rect(fill="white")) + scale_fill_grey()
                                                                       `geom_smooth()` using formula 'y ~ x'
                                                                       > p+ scale_x_continuous(breaks = seq(1,15 , by = 1))+theme(strip.background =element_rect(fill="white")) + scale_fill_grey() + scale_color_grey()
                                                                       `geom_smooth()` using formula 'y ~ x'
                                                                       > theme.clean <- function(){
                                                                         +   theme_bw()+
                                                                           +     theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 1, hjust = 1),
                                                                                       +           axis.text.y = element_text(size = 12),
                                                                                       +           axis.title.x = element_text(size = 12, face = "plain"),             
                                                                                       +           axis.title.y = element_text(size = 12, face = "plain"),             
                                                                                       +           panel.grid.major.x = element_blank(),                                          
                                                                                       +           panel.grid.minor.x = element_blank(),
                                                                                       +           panel.grid.minor.y = element_blank(),
                                                                                       +           panel.grid.major.y = element_blank(),  
                                                                                       +           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
                                                                                       +           plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
                                                                                       +           legend.text = element_text(size = 12, face = "italic"),          
                                                                                       +           legend.title = element_blank(),                              
                                                                                       +           legend.position = c(0.7, 0.7))
                                                                         + }
                                                                       > p<-ggplot(Height3,aes(Week, Height, group=Treatment, col=Treatment, shape= Treatment))  +geom_point() + geom_smooth(method=lm,  aes(fill=Treatment))+theme.clean()+ labs(x = "Week", y = "Height (mm)")+  facet_grid(.~Species) + scale_fill_grey()
                                                                       > p+ scale_x_continuous(breaks = seq(1,15 , by = 1))+theme(strip.background =element_rect(fill="white")) + scale_fill_grey() + scale_color_grey()
                                                                       Scale for 'fill' is already present. Adding another scale for 'fill', which will replace the existing
                                                                       scale.
                                                                       `geom_smooth()` using formula 'y ~ x'
                                                                       > theme.clean <- function(){
                                                                         +   theme_bw()+
                                                                           +     theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 1, hjust = 1),
                                                                                       +           axis.text.y = element_text(size = 12),
                                                                                       +           axis.title.x = element_text(size = 12, face = "plain"),             
                                                                                       +           axis.title.y = element_text(size = 12, face = "plain"),             
                                                                                       +           panel.grid.major.x = element_blank(),                                          
                                                                                       +           panel.grid.minor.x = element_blank(),
                                                                                       +           panel.grid.minor.y = element_blank(),
                                                                                       +           panel.grid.major.y = element_blank(),  
                                                                                       +           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
                                                                                       +           plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
                                                                                       +           legend.text = element_text(size = 12, face = "italic"),          
                                                                                       +           legend.title = element_blank(),                              
                                                                                       +           legend.position = c(0.9, 0.7))
                                                                         + }
                                                                       > p<-ggplot(Height3,aes(Week, Height, group=Treatment, col=Treatment, shape= Treatment))  +geom_point() + geom_smooth(method=lm,  aes(fill=Treatment))+theme.clean()+ labs(x = "Week", y = "Height (mm)")+  facet_grid(.~Species) + scale_fill_grey()
                                                                       > p+ scale_x_continuous(breaks = seq(1,15 , by = 1))+theme(strip.background =element_rect(fill="white")) + scale_fill_grey() + scale_color_grey()
                                                                       Scale for 'fill' is already present. Adding another scale for 'fill', which will replace the existing
                                                                       scale.
                                                                       `geom_smooth()` using formula 'y ~ x'
                                                                       > theme.clean <- function(){
                                                                         +   theme_bw()+
                                                                           +     theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 1, hjust = 1),
                                                                                       +           axis.text.y = element_text(size = 12),
                                                                                       +           axis.title.x = element_text(size = 12, face = "plain"),             
                                                                                       +           axis.title.y = element_text(size = 12, face = "plain"),             
                                                                                       +           panel.grid.major.x = element_blank(),                                          
                                                                                       +           panel.grid.minor.x = element_blank(),
                                                                                       +           panel.grid.minor.y = element_blank(),
                                                                                       +           panel.grid.major.y = element_blank(),  
                                                                                       +           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
                                                                                       +           plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
                                                                                       +           legend.text = element_text(size = 12, face = "italic"),          
                                                                                       +           legend.title = element_blank(),                              
                                                                                       +           legend.position = c(0.5, 0.9))
                                                                         + }
                                                                       > p<-ggplot(Height3,aes(Week, Height, group=Treatment, col=Treatment, shape= Treatment))  +geom_point() + geom_smooth(method=lm,  aes(fill=Treatment))+theme.clean()+ labs(x = "Week", y = "Height (mm)")+  facet_grid(.~Species) + scale_fill_grey()
                                                                       > p+ scale_x_continuous(breaks = seq(1,15 , by = 1))+theme(strip.background =element_rect(fill="white")) + scale_fill_grey() + scale_color_grey()
                                                                       Scale for 'fill' is already present. Adding another scale for 'fill', which will replace the existing
                                                                       scale.
                                                                       `geom_smooth()` using formula 'y ~ x'
                                                                       > theme.clean <- function(){
                                                                         +   theme_bw()+
                                                                           +     theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 1, hjust = 1),
                                                                                       +           axis.text.y = element_text(size = 12),
                                                                                       +           axis.title.x = element_text(size = 12, face = "plain"),             
                                                                                       +           axis.title.y = element_text(size = 12, face = "plain"),             
                                                                                       +           panel.grid.major.x = element_blank(),                                          
                                                                                       +           panel.grid.minor.x = element_blank(),
                                                                                       +           panel.grid.minor.y = element_blank(),
                                                                                       +           panel.grid.major.y = element_blank(),  
                                                                                       +           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
                                                                                       +           plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
                                                                                       +           legend.text = element_text(size = 12, face = "italic"),          
                                                                                       +           legend.title = element_blank(),                              
                                                                                       +           legend.position = c(0.4, 0.87))
                                                                         + }
                                                                       > p<-ggplot(Height3,aes(Week, Height, group=Treatment, col=Treatment, shape= Treatment))  +geom_point() + geom_smooth(method=lm,  aes(fill=Treatment))+theme.clean()+ labs(x = "Week", y = "Height (mm)")+  facet_grid(.~Species) + scale_fill_grey()
                                                                       > p+ scale_x_continuous(breaks = seq(1,15 , by = 1))+theme(strip.background =element_rect(fill="white")) + scale_fill_grey() + scale_color_grey()
                                                                       Scale for 'fill' is already present. Adding another scale for 'fill', which will replace the existing
                                                                       scale.
                                                                       `geom_smooth()` using formula 'y ~ x'
                                                                       > theme.clean <- function(){
                                                                         +   theme_bw()+
                                                                           +     theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 1, hjust = 1),
                                                                                       +           axis.text.y = element_text(size = 12),
                                                                                       +           axis.title.x = element_text(size = 12, face = "plain"),             
                                                                                       +           axis.title.y = element_text(size = 12, face = "plain"),             
                                                                                       +           panel.grid.major.x = element_blank(),                                          
                                                                                       +           panel.grid.minor.x = element_blank(),
                                                                                       +           panel.grid.minor.y = element_blank(),
                                                                                       +           panel.grid.major.y = element_blank(),  
                                                                                       +           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
                                                                                       +           plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
                                                                                       +           legend.text = element_text(size = 12, face = "italic"),          
                                                                                       +           legend.title = element_blank(),                              
                                                                                       +           legend.position = c(0.6, 0.87))
                                                                         + }
                                                                       > p<-ggplot(Height3,aes(Week, Height, group=Treatment, col=Treatment, shape= Treatment))  +geom_point() + geom_smooth(method=lm,  aes(fill=Treatment))+theme.clean()+ labs(x = "Week", y = "Height (mm)")+  facet_grid(.~Species) + scale_fill_grey()
                                                                       > p+ scale_x_continuous(breaks = seq(1,15 , by = 1))+theme(strip.background =element_rect(fill="white")) + scale_fill_grey() + scale_color_grey()
                                                                       Scale for 'fill' is already present. Adding another scale for 'fill', which will replace the existing
                                                                       scale.
                                                                       `geom_smooth()` using formula 'y ~ x'
                                                                       > theme.clean <- function(){
                                                                         +   theme_bw()+
                                                                           +     theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 1, hjust = 1),
                                                                                       +           axis.text.y = element_text(size = 12),
                                                                                       +           axis.title.x = element_text(size = 12, face = "plain"),             
                                                                                       +           axis.title.y = element_text(size = 12, face = "plain"),             
                                                                                       +           panel.grid.major.x = element_blank(),                                          
                                                                                       +           panel.grid.minor.x = element_blank(),
                                                                                       +           panel.grid.minor.y = element_blank(),
                                                                                       +           panel.grid.major.y = element_blank(),  
                                                                                       +           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
                                                                                       +           plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
                                                                                       +           legend.text = element_text(size = 12, face = "italic"),          
                                                                                       +           legend.title = element_blank(),                              
                                                                                       +           legend.position = c(0.7, 0.87))
                                                                         + }
                                                                       > p<-ggplot(Height3,aes(Week, Height, group=Treatment, col=Treatment, shape= Treatment))  +geom_point() + geom_smooth(method=lm,  aes(fill=Treatment))+theme.clean()+ labs(x = "Week", y = "Height (mm)")+  facet_grid(.~Species) + scale_fill_grey()
                                                                       > p+ scale_x_continuous(breaks = seq(1,15 , by = 1))+theme(strip.background =element_rect(fill="white")) + scale_fill_grey() + scale_color_grey()
                                                                       Scale for 'fill' is already present. Adding another scale for 'fill', which will replace the existing
                                                                       scale.
                                                                       `geom_smooth()` using formula 'y ~ x'
                                                                       > theme.clean <- function(){
                                                                         +   theme_bw()+
                                                                           +     theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 1, hjust = 1),
                                                                                       +           axis.text.y = element_text(size = 12),
                                                                                       +           axis.title.x = element_text(size = 12, face = "plain"),             
                                                                                       +           axis.title.y = element_text(size = 12, face = "plain"),             
                                                                                       +           panel.grid.major.x = element_blank(),                                          
                                                                                       +           panel.grid.minor.x = element_blank(),
                                                                                       +           panel.grid.minor.y = element_blank(),
                                                                                       +           panel.grid.major.y = element_blank(),  
                                                                                       +           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
                                                                                       +           plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
                                                                                       +           legend.text = element_text(size = 12, face = "italic"),          
                                                                                       +           legend.title = element_blank(),                              
                                                                                       +           legend.position = c(0.85, 0.87))
                                                                         + }
                                                                       > p<-ggplot(Height3,aes(Week, Height, group=Treatment, col=Treatment, shape= Treatment))  +geom_point() + geom_smooth(method=lm,  aes(fill=Treatment))+theme.clean()+ labs(x = "Week", y = "Height (mm)")+  facet_grid(.~Species) + scale_fill_grey()
                                                                       > p+ scale_x_continuous(breaks = seq(1,15 , by = 1))+theme(strip.background =element_rect(fill="white")) + scale_fill_grey() + scale_color_grey()
                                                                       Scale for 'fill' is already present. Adding another scale for 'fill', which will replace the existing
                                                                       scale.
                                                                       `geom_smooth()` using formula 'y ~ x'
                                                                       > theme.clean <- function(){
                                                                         +   theme_bw()+
                                                                           +     theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 1, hjust = 1),
                                                                                       +           axis.text.y = element_text(size = 12),
                                                                                       +           axis.title.x = element_text(size = 12, face = "plain"),             
                                                                                       +           axis.title.y = element_text(size = 12, face = "plain"),             
                                                                                       +           panel.grid.major.x = element_blank(),                                          
                                                                                       +           panel.grid.minor.x = element_blank(),
                                                                                       +           panel.grid.minor.y = element_blank(),
                                                                                       +           panel.grid.major.y = element_blank(),  
                                                                                       +           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
                                                                                       +           plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
                                                                                       +           legend.text = element_text(size = 12, face = "italic"),          
                                                                                       +           legend.title = element_blank(),                              
                                                                                       +           legend.position = c(0.89, 0.89))
                                                                         + }
                                                                       > p<-ggplot(Height3,aes(Week, Height, group=Treatment, col=Treatment, shape= Treatment))  +geom_point() + geom_smooth(method=lm,  aes(fill=Treatment))+theme.clean()+ labs(x = "Week", y = "Height (mm)")+  facet_grid(.~Species) + scale_fill_grey()
                                                                       > p+ scale_x_continuous(breaks = seq(1,15 , by = 1))+theme(strip.background =element_rect(fill="white")) + scale_fill_grey() + scale_color_grey()
                                                                       Scale for 'fill' is already present. Adding another scale for 'fill', which will replace the existing
                                                                       scale.
                                                                       `geom_smooth()` using formula 'y ~ x'
                                                                       > (p5 <- ggplot(LabHarvest_VS, aes(Harvest, Nodules_W, fill = Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none") + labs(x = "Harvest", y = "Nodule Weight (grams)")+   facet_grid(. ~ Treatment)+theme(strip.background = element_rect(colour="black", fill="white",)))
                                                                       > (p5 <- ggplot(LabHarvest_VS, aes (x = Nodules_w, y = BG_DW)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+labs(y = "Belowground biomass (grams)", x = "Nodule biomass (grams)")+geom_smooth(method=lm))
                                                                       Error in FUN(X[[i]], ...) : object 'Nodules_w' not found
                                                                       > (p5 <- ggplot(LabHarvest_VS, aes (x = Nodules_W, y = BG_DW)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+labs(y = "Belowground biomass (grams)", x = "Nodule biomass (grams)")+geom_smooth(method=lm))
                                                                       `geom_smooth()` using formula 'y ~ x'
                                                                       > (p6<-ggplot(LabHarvest_VS, aes (x = Nodules_W, y = BG_DW, colour = Treatment,shape=Treatment))  +geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+theme(legend.position = "none")+labs(x = "Nodule biomass (grams)", y = "Belowground biomass (grams)")+geom_smooth(method=lm, aes(fill=Treatment)))
                                                                       `geom_smooth()` using formula 'y ~ x'
                                                                       > (p18 <- ggplot(iso_VS, aes(Treatment, Nf, fill=Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Treatment", y = "Nitrogen Fixation") +   theme(strip.background = element_rect(colour="black", fill="white",)) + scale_x_discrete(limits=c("4%","8%","16%")))
                                                                       > (p5 <- ggplot(LabHarvest_VS, aes(Harvest, Nodules_W, fill = Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none") + labs(x = "Harvest", y = "Nodule biomass (grams)")+   facet_grid(. ~ Treatment)+theme(strip.background = element_rect(colour="black", fill="white",))+ scale_fill_grey())
                                                                       > (p6<-ggplot(LabHarvest_VS, aes (x = Nodules_W, y = BG_DW, colour = Treatment,shape=Treatment))  +geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+theme(legend.position = "none")+labs(x = "Nodule biomass (grams)", y = "Belowground biomass (grams)")+geom_smooth(method=lm, aes(fill=Treatment))+ scale_fill_grey())
                                                                       `geom_smooth()` using formula 'y ~ x'
                                                                       > (p7 <- ggplot(iso_VS, aes(Treatment, Nf, fill=Treatment)) + geom_boxplot( alpha = 0.8, colourp24<- ggplot(iso_VS, aes (x =Nf, y=SLA, colour = Treatment,shape=Treatment)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+labs(x = "Nitrogen fixation", y = "Leaf Mass per Area")+geom_smooth(method=lm,aes(fill=Treatment))+ scale_fill_grey())
                                                                          + ggarrange(p5,p6,p7, labels = c ("a", "b", "c"))
                                                                          Error: unexpected symbol in:
                                                                            "ent,shape=Treatment)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+labs(x = "Nitrogen fixation", y = "Leaf Mass per Area")+geom_smoo
ggarrange"
                                                                          > (p7 <- ggplot(iso_VS, aes(Treatment, Nf, fill=Treatment)) + geom_boxplot( alpha = 0.8, colourp24<- ggplot(iso_VS, aes (x =Nf, y=SLA, colour = Treatment,shape=Treatment)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+labs(x = "Nitrogen fixation", y = "Leaf Mass per Area")+geom_smooth(method=lm,aes(fill=Treatment))+ scale_fill_grey())
                                                                             + )
                                                                          Error: `mapping` must be created by `aes()`
                                                                          Did you use %>% instead of +?
                                                                            Run `rlang::last_error()` to see where the error occurred.
                                                                          > (p7 <- ggplot(iso_VS, aes(Treatment, Nf, fill=Treatment)) + geom_boxplot( alpha = 0.8, colourp24<- ggplot(iso_VS, aes (x =Nf, y=SLA, colour = Treatment,shape=Treatment)) + geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+labs(x = "Nitrogen fixation", y = "Leaf Mass per Area")+geom_smooth(method=lm,aes(fill=Treatment))+ scale_fill_grey()
                                                                       
                                                                          > (p18 <- ggplot(iso_VS, aes(Treatment, Nf, fill=Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Treatment", y = "Nitrogen Fixation") +   theme(strip.background = element_rect(colour="black", fill="white",)) + scale_x_discrete(limits=c("4%","8%","16%")))
                                                                          > (p5 <- ggplot(LabHarvest_VS, aes(Harvest, Nodules_W, fill = Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none") + labs(x = "Harvest", y = "Nodule biomass (grams)")+   facet_grid(. ~ Treatment)+theme(strip.background = element_rect(colour="black", fill="white",))+ scale_fill_grey())
                                                                          > (p6 <-ggplot(LabHarvest_VS, aes (x = Nodules_W, y = BG_DW, colour = Treatment,shape=Treatment))  +geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+theme(legend.position = "none")+labs(x = "Nodule biomass (grams)", y = "Belowground biomass (grams)")+geom_smooth(method=lm, aes(fill=Treatment))+ scale_fill_grey() + scale_color_grey())
                                                                          `geom_smooth()` using formula 'y ~ x'
                                                                          > (p7 <- ggplot(iso_VS, aes(Treatment, Nf, fill=Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Soil Mositure Treatment", y = "Biological Nitrogen Fixation") +   theme(strip.background = element_rect(colour="black", fill="white",)) + scale_x_discrete(limits=c("4%","8%","16%"))+ scale_fill_grey())
                                                                          > ggarrange(p5, p6,p7, labels = c("a", "b", "c"))
                                                                          `geom_smooth()` using formula 'y ~ x'
                                                                          > (p5 <- ggplot(LabHarvest_VS, aes(Harvest, Nodules_W, fill = Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none") + labs(x = "Harvest", y = "Nodule biomass (grams)")+   facet_grid(. ~ Treatment)+theme(strip.background = element_rect(colour="black", fill="white",))+ scale_fill_grey())
                                                                          > (p6 <-ggplot(LabHarvest_VS, aes (x = Nodules_W, y = BG_DW, colour = Treatment,shape=Treatment))  +geom_point()   + theme.clean() + theme(strip.background = element_rect(colour="black", fill="white",))+theme(legend.position = "none")+labs(x = "Nodule biomass (grams)", y = "Belowground biomass (grams)")+geom_smooth(method=lm, aes(fill=Treatment))+ scale_fill_grey() + scale_color_grey())
                                                                          `geom_smooth()` using formula 'y ~ x'
                                                                          > (p7 <- ggplot(iso_VS, aes(Treatment, Nf, fill=Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Soil Moisture Treatment", y = "Biological Nitrogen Fixation") +   theme(strip.background = element_rect(colour="black", fill="white",)) + scale_x_discrete(limits=c("4%","8%","16%"))+ scale_fill_grey())
                                                                          > ggarrange(p5, p6,p7, labels = c("a", "b", "c"))
                                                                          `geom_smooth()` using formula 'y ~ x'
                                                                          > (p4 <- ggplot(stable_iso_area, aes(Treatment, delta13C, fill= Treatment)) + geom_boxplot( alpha = 0.8, colour = "#8B2323") + theme.clean() +  theme(axis.text.x = element_text(size = 12, angle = 0),legend.position="none")  + labs(x = "Soil Moisture Treatment", y = "Leaf 13C") 
                                                                             +   +   facet_wrap(.~Species)+theme(strip.background = element_rect(colour="black", fill="white",)) + scale_x_discrete(limits=c("4%","8%","16%"))+ scale_fill_grey())
                                                                         