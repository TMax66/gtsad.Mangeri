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
                         ifelse(`tipo NGS` == "CVB5", "lightblue",   "gray"))) %>%    
  group_by(id) %>% 
  nest() %>%  
    
    mutate(tot = map(data, ~sum(.$reads,  na.rm = TRUE))) %>%  
  unnest(cols = c(data, tot)) %>% 
  mutate(reads_perc = round(100*(reads/tot),2)) %>%     
  select(-tot) -> dtplot
  
  
  
  
  ggplot(dtplot)+
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
    


pkg()

dt <- read_excel("enterovirus/VT_Dataset EV 2022-2023.xlsx")


dt %>% 
  select(id, `data prelievo`, stagione, `sito prelievo`, `real-time PCR`) %>% 
  mutate(Date = floor_date(as_date(`data prelievo`), "month"), 
         Anno = year(`data prelievo`),  
         stagione = factor(stagione, levels = c( "I", "P", "E", "A"), 
                           labels = c("Inverno", "Primavera", "Estate", "Autunno")), 
         `sito prelievo` = factor(`sito prelievo`, labels= c("Sito B", "Sito C", "Sito V"))) %>%   
group_by( Anno, stagione, `real-time PCR`) %>% 
 group_by(`sito prelievo`, Anno, stagione, `real-time PCR`) %>% 
  count() %>% 
  group_by( Anno, stagione) %>% 
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
  facet_grid(  ~ Anno)+
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
 
  
  
### by specie Sanger/NGS


 
  dt %>% 
  select(id, `data prelievo`, stagione,`real-time PCR`, `specie Sanger`) %>% 
  mutate(Date = floor_date(as_date(`data prelievo`), "month"), 
         Anno = year(`data prelievo`), 
         stagione = factor(stagione, levels = c( "I", "P", "E", "A"), 
                           labels = c("Inverno", "Primavera", "Estate", "Autunno"))) %>% 
  distinct(id, .keep_all = TRUE) %>%    
    mutate(`specie Sanger` = ifelse(is.na(`specie Sanger`) &
                                      `real-time PCR`== "P" , "Neg", 
                                    ifelse(`real-time PCR`== "N", "PCR-Neg",   `specie Sanger`))) %>%  
  group_by( Anno, stagione, `specie Sanger` ) %>% 
    count() %>% 
    pivot_wider(names_from = `specie Sanger`, values_from = n, values_fill = 0) %>% 
    adorn_totals(where = "col") %>%   
   # rename("PCR Neg" = `NA`) %>% 
    gt()
  

  dt %>% 
    select(id, `data prelievo`, stagione,`real-time PCR`, `specie NGS`) %>% 
    mutate(Date = floor_date(as_date(`data prelievo`), "month"), 
           Anno = year(`data prelievo`), 
           stagione = factor(stagione, levels = c( "I", "P", "E", "A"), 
                             labels = c("Inverno", "Primavera", "Estate", "Autunno"))) %>%  
    mutate(`specie NGS` = ifelse(is.na(`specie NGS`) &
                                      `real-time PCR`== "P" , "Neg", 
                                    ifelse(`real-time PCR`== "N", "PCR-Neg",   `specie NGS`))) %>%  
    filter( `real-time PCR` == "P") %>%   
    select(-`data prelievo`, -Date) %>%   
     
    group_by( id,Anno, stagione,`specie NGS` ) %>% 
    count() %>%  
    pivot_wider(names_from = `specie NGS`, values_from = n, values_fill = 0)  ->  dtwide_specie
    # adorn_totals(where = "row")
    # gt()
  profili_sp <- map2_df(dtwide_specie[, 5:9], names(dtwide_specie[, 5:9]), ~  replace(.x, .x >= 1, .y))
  profili_sp[,1:5]<-profili_sp[,1:5] != 0
  
  nomi_abb<-toupper(names(profili_sp)[1:5])
  
  
  X<-  apply(profili_sp[, 1:5], 1, function(x) nomi_abb[x])
  
  
  
  XX<-lapply(X, paste, collapse="-")
  
  dtwide_specie$profilo <- unlist(XX)
  
  dtwide_specie %>% 
    select(id, Anno,stagione,profilo) %>% 
    group_by(profilo) %>% 
    count() %>%  
    mutate(profilo = ifelse(profilo == "", "Neg", profilo))-> dtwsplot
  
  

dtwsplot %>% 
  ggplot()+
  aes(x = reorder(profilo,n), 
      y = n)+
  coord_flip()+
  geom_col()
    
  
  
dt %>% 
      select(id, `data prelievo`, stagione,`real-time PCR`, `tipo NGS`) %>% 
      mutate(Date = floor_date(as_date(`data prelievo`), "month"), 
             Anno = year(`data prelievo`), 
             stagione = factor(stagione, levels = c( "I", "P", "E", "A"), 
                               labels = c("Inverno", "Primavera", "Estate", "Autunno"))) %>%  
      mutate(`tipo NGS` = ifelse(is.na(`tipo NGS`) &
                                     `real-time PCR`== "P" , "Neg", 
                                   ifelse(`real-time PCR`== "N", "PCR-Neg",   `tipo NGS`))) %>% 
  filter( `real-time PCR` == "P") %>% 
      select(-`data prelievo`, -Date) %>%   
      group_by( id,Anno, stagione,`tipo NGS` ) %>% 
      count() %>% 
      pivot_wider(names_from = `tipo NGS`, values_from = n, values_fill = 0)  -> dtwide_tipo
    
    profili <- map2_df(dtwide_tipo[, 5:30], names(dtwide_tipo[, 5:30]), ~  replace(.x, .x >= 1, .y))
    profili[,1:26]<-profili[,1:26] != 0
    
    nomi_abb<-toupper(names(profili)[1:26])
    
    
    X<-  apply(profili[, 1:26], 1, function(x) nomi_abb[x])
    
    
    
    XX<-lapply(X, paste, collapse="-")
    
    dtwide_tipo$profilo <- unlist(XX)
    
    dtwide_tipo %>% 
      select(id, Anno,stagione,profilo) %>% 
      group_by( profilo) %>% 
      count() %>%  
      mutate(profilo = ifelse(profilo == "", "Neg", profilo)) -> dtwtplot
    

    dtwtplot %>% 
      ggplot()+
      aes(x = reorder(profilo,n), 
          y = n)+
      coord_flip()+
      geom_col()
    
    
     # adorn_totals(where = "row")
 
    
  #pivot_wider(names_from = `tipo NGS`, values_from = reads ) %>% View()


