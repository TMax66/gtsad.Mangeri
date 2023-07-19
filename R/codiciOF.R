dt <- read_excel("dati/df.xlsx")
dtlett <- read_excel("dati/datiletteratura.xlsx")




library(binom)
library(forestplot)

df <- dt %>% 
  mutate(category = casefold(category), 
         category = factor(category, levels = c("oysters", "mussels", "clams", "other")), 
         season = factor(season)) %>% 

  group_by(period, season, category, Nov) %>% 
  count() %>%  na.omit() %>% 
  pivot_wider(names_from = "Nov", values_from = "n", values_fill = 0) %>% 
  mutate(tested = neg+pos, 
         '%pos' = round(100*(pos/tested), 1)) %>% 
  select(-neg) %>% 
arrange(category)
#adorn_totals()





prev <- binom.bayes(
  x = df$pos, n = df$tested,
  type = "highest", conf.level = 0.95, tol = 1e-9, 
  prior.shape1 = 1, prior.shape2 = 1)

# binom.bayes(x = 0, n = 1, prior.shape1 = 0.00001, prior.shape2 = 0.00001)

prev <- cbind(df, prev[,6:8])

prev <- prev %>% 
  mutate(Prevalence = mean,
         liminf = lower,
         limsup = upper,
         across(where(is.double), round,2))

fplot <- function(data, periodo)
{
  data %>% ungroup() %>% 
    filter(period == periodo) %>%  
    #mutate(Prevalence = sprintf("%.2f", mean), .after = tested) %>% 
    mutate(CI = paste0("[",sprintf("%.2f", lower),", ", sprintf("%.2f", upper), "]")) %>%   
    
    forestplot(labeltext = c(  season, category,pos,
                               tested,  Prevalence, CI),
               graph.pos = 5,
               clip = c(0,1),
               boxsize = 0.2,
               ci.vertices = TRUE,
               ci.vertices.height = 0.05,
               xlab= expression(bold("Estimated prevalence with 95% CI")),
               title = paste("Bayesian Beta-binomial estimated posterior prevalence  of \n NoroVirus in shellfish sampling during", periodo, "period"),
               xlog = FALSE, 
               align = "llrrrc",
               colgap = unit(4, "mm"),
               
               # xticks = xticks,
               txt_gp = fpTxtGp(ticks=gpar(cex=1), 
                                xlab = gpar(cex=1))) %>% 
    fp_add_header(season = "Season", 
                  category = "Category",
                  pos = "No.positive", 
                  tested = "No.tested", 
                  Prevalence = "Prevalence",
                  CI = "95% CI" ) %>% 
    
    fp_add_lines(h_1 = gpar(lty = 1),
                 h_2 = gpar(lty = 1))
  
}


p1 <- fplot(prev, periodo = "cold1819")

p2 <- fplot(prev, periodo = "cold1920")

p3 <- fplot(prev, periodo = "mild19")

p4 <- fplot(prev, periodo = "mild20")


library(ggplotify)
library(patchwork)
 

p1 <- grid2grob(print(p1)) 

p2 <- grid2grob(print(p2))

p3 <- grid2grob(print(p3))

p4 <- grid2grob(print(p4))

wrap_elements(p1)/wrap_elements(p2)

wrap_elements(p3)/wrap_elements(p4)



# dati letteratura----

df <- dtlett %>% 
  rename(tested = totcampioni, pos = npositivi)

prev <- prev <- binom.bayes(
  x = df$pos, n = df$tested,
  type = "highest", conf.level = 0.95, tol = 1e-9)#, 
#  prior.shape1 = 1, prior.shape2 = 1)

prev <- cbind(df, prev[,6:8])

prev <- prev %>% 
  mutate(
         mean = mean,
         lower = lower,
         upper = upper,
         across(where(is.double), round, 2))


prev %>% ungroup() %>% 
  mutate(CI = paste0("[",sprintf("%.2f", lower),", ", sprintf("%.2f", upper), "]")) %>%    
  
  forestplot(labeltext = c(studio, anno, country, category, pos,
                             tested,  prevalence, CI),
             graph.pos = 7,
             clip = c(0,1),
             boxsize = 0.2,
             xticks=c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100),
             ci.vertices = TRUE,
             ci.vertices.height = 0.05,
             xlab= expression(bold("Estimated prevalence with 95% CI")),
             title = "Bayesian Beta-binomial estimated posterior prevalence  of \n NoroVirus in shellfish sampling from different Study",
             xlog = FALSE, 
             align = "llrrrc",
             colgap = unit(4, "mm"),
             txt_gp = fpTxtGp(ticks=gpar(cex=1), 
                              xlab = gpar(cex=1))) %>% 
  fp_add_header(studio = "Study", 
                anno = "Year",
                country = "Country", 
                category = "Category", 
                pos = "No.positive", 
                tested = "No.tested", 
                prevalence = "Prevalence",
                CI = "95% CI" ) %>% 
  
  fp_add_lines(h_1 = gpar(lty = 1),
               h_2 = gpar(lty = 1), 
               h_12 = gpar(lty = 1)) %>% 
  
  fp_append_row(mean  = 15.00,
                lower = 12.00,
                upper = 17.00,
                studio = expression(bold("Our Study")),
                anno = expression(bold("2018-2022")),
                country = expression(bold("Italy")),
                category = expression(bold("shellfish")),
                pos = expression(bold("126")),
                tested = expression(bold("861")),
                prevalence = expression(bold("15.00")),
                CI = expression(bold("[12.00, 17.00]")),
                position = "last",
                is.summary = FALSE) %>% 
  fp_set_style(box = c(rep("black", 11), "royalblue"))






dt %>% 
  group_by(Nov) %>% 
  count() %>% 
  pivot_wider(names_from = Nov, values_from = n)


binom.bayes(x = 126, n = 861)









#CANTINA----
# dt <- read_excel("dati/Database Norovirus.xlsx")
# 
# 
# dt$ID <- paste0(dt$year, dt$id, dt$sample_unit)
# 
# dt %>% 
#   mutate(NoV = GI+GII, 
#          Nov = ifelse(NoV > 0, "pos", "neg"),
#          nov = ifelse(NoV > 0, 1, 0),
#          
#          winter = ifelse(winter == 1, "winter", NA), 
#          autumn = ifelse(autumn == 1, "autumn", NA), 
#          spring = ifelse(spring == 1, "spring", NA), 
#          summer = ifelse(summer == 1, "summer", NA), 
#          
#          GI = ifelse(GI== 1, "GI", NA), 
#          GII = ifelse(GII == 1, "GII", NA), 
#          
#          `Macroarea_B-In`= ifelse(`Macroarea_B-In`== 1, "Bin", NA), 
#          `Macroarea_B-Out`= ifelse(`Macroarea_B-Out` == 1, "Bout", NA), 
#          Macroarea_Chamelea = ifelse(Macroarea_Chamelea == 1, "Chamelea", NA),
#          `Macroarea_Long-line` = ifelse(`Macroarea_Long-line`== 1, "Longline", NA), 
#          Macroarea_Sacca = ifelse(Macroarea_Sacca == 1, "Sacca", NA)
#          
#          ) %>%    
#   
#  unite(season, 11:14, sep = "") %>% 
#   mutate(season = str_remove_all(season, "NA"), 
#          season = ifelse(season == "", "summer", season)) %>% 
#   
#  unite(genogroup, 4:5, sep = "") %>%
#   mutate(genogroup = str_remove_all(genogroup, "NA"), 
#          genogroup = ifelse(genogroup== "", "Neg", genogroup)) %>% 
#    
#  unite(area, 11:15, sep = "" )  %>% 
#   mutate(area = str_remove_all(area, "NA"), 
#          area = ifelse(area == "", NA, area)) -> df
# 
# write.xlsx(df, file = "df.xlsx")


# df %>% 
#   group_by(season, Nov) %>% 
#   count()
# 
# df %>% 
#   group_by(year, season, category, Nov) %>% 
#   count() %>% 
#   pivot_wider(names_from = Nov, values_from = n, values_fill = 0) %>% 
#   mutate(prev = pos/(neg+pos)) %>%  View()





# library(rstanarm)
# library(see)
# library(bayestestR)

# fit1 <- stan_glm(nov ~ category , data = df, family = binomial)
# fit2 <- stan_glm(nov ~ category + year , data = df, family = binomial)
# fit3 <- stan_glm(nov ~ category + year + season, data = df, family = binomial)
# fit4 <- stan_glm(nov ~ category*season + year, data = df, family = binomial)
# fit5 <- stan_glm(nov ~ category*season*year, data = df, family = binomial)
# 
# 
# loo1 <- loo(fit1, k_threshold = 0.7)
# loo2 <- loo(fit2, k_threshold = 0.7)
# loo3 <- loo(fit3, k_threshold = 0.7)
# loo4 <- loo(fit4, k_threshold = 0.7)
# 
# 
# x <- loo_compare(loo1, loo2, loo3, loo4)



