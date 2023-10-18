pkg()

dt <- read_excel("enterovirus/VT_Database enterovirus 2022-2023.xlsx")

library(scales)

dt %>% 
  select(id,`sito prelievo`,  `specie NGS`,  `tipo NGS`, reads) %>% 
  filter(!is.na(reads)) %>% 
  mutate(`tipo NGS` = ifelse(is.na(`tipo NGS`), "ND",`tipo NGS` ), 
         `tipo NGS` = ifelse(`tipo NGS` == "ND",`specie NGS`,`tipo NGS`))%>%  
  #group_by(`tipo NGS`) %>% 
  group_by(id,`sito prelievo`, `tipo NGS` ) %>% 
  mutate(reads = sum(reads), 
         colorx = ifelse(`tipo NGS` == "E11", "blue",
                         ifelse(`tipo NGS` == "CVB5", "lightblue",   "gray"))) %>%   View()
  group_by(id) %>% 
  nest() %>%  
    
    mutate(tot = map(data, ~sum(.$reads,  na.rm = TRUE))) %>%  
  unnest(cols = c(data, tot)) %>% 
  mutate(reads_perc = round(100*(reads/tot),2)) %>%    
  select(-tot) %>% 
  ggplot()+
  aes(x = factor(id),
      y = reads)+
  geom_bar(aes(fill = colorx), stat = "identity", color ="black")+
  coord_flip()+
    facet_wrap(~`sito prelievo`, scales = "free")+
  theme(legend.position = "NULL")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))
  
    
dt %>% filter(`real-time PCR` == "P") %>% 
  select(id,`sito prelievo`,  `specie NGS`,  `tipo NGS`, reads) %>%   
  group_by( `tipo NGS` ) %>% 
  count() %>%  View()
    

dt %>% 
  select(id, `data prelievo`, stagione, `sito prelievo`, `real-time PCR`) %>%
  mutate(Date = floor_date(as_date(`data prelievo`), "month"), 
         Anno = year(`data prelievo`), 
         stagione = factor(stagione, levels = c( "I", "P", "E", "A"), 
                           labels = c("Inverno", "Primavera", "Estate", "Autunno")), 
         `sito prelievo` = factor(`sito prelievo`, labels= c("Sito B", "Sito C", "Sito V"))) %>%  
group_by(`sito prelievo`, Anno, stagione, `real-time PCR`) %>% 
  count() %>% 
  group_by(`sito prelievo`, Anno, stagione) %>% 
  pivot_wider(names_from = `real-time PCR`, values_from = n, values_fill = 0) %>%  
  mutate(perc = 100*(P/(N+P))) %>% 
  
  ggplot()+
  aes(x = stagione, 
      y = perc, 
    
      label = paste0(round(perc, 1), "%"))+
  #geom_point(size = 10, color = "gray")+
  geom_line(group = 1)+
  geom_label(color = "blue")+
  facet_grid( `sito prelievo` ~ Anno)+
  labs(y = "% positività", 
       title = "Andamento stagionale delle positività a Enterovirus in RT-PCR" )+
  
  theme_bw()+
  theme(axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_blank(), 
        axis.text.x = element_text(size=10),
        title = element_text(size = 15), 
        strip.text.x = element_text(size = 15, color = "red"),
        strip.text.y = element_text(size = 15, color = "black"))
 
  
  
  
  

    
  #pivot_wider(names_from = `tipo NGS`, values_from = reads ) %>% View()


