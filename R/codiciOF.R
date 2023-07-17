dt <- read_excel("dati/df.xlsx")

library(binom)
library(forestplot)

df <- dt %>% 
  mutate(category = casefold(category), 
         category = factor(category, levels = c("oysters", "mussels", "clams", "other"))) %>% 

  group_by(period, season, category, Nov) %>% 
  count() %>%  na.omit() %>% 
  pivot_wider(names_from = "Nov", values_from = "n", values_fill = 0) %>% 
  mutate(tested = neg+pos) %>% 
arrange(category)
#adorn_totals()


prev <- binom.bayes(
  x = df$pos, n = df$tested,
  type = "highest", conf.level = 0.95, tol = 1e-9, 
  prior.shape1 = 0.8, prior.shape2 = 0.3)

binom.bayes(x = 0, n = 1, prior.shape1 = 0.00001, prior.shape2 = 0.00001)

prev <- cbind(df, prev[,6:8])

prev <- prev %>% 
  mutate(Prevalence = mean,
         liminf = lower,
         limsup = upper,
         across(where(is.double), round,2))


library(forestplot)
prev %>% ungroup() %>% 
  #mutate(Prevalence = sprintf("%.2f", mean), .after = tested) %>% 
  mutate(CI = paste0("[",sprintf("%.2f", lower),", ", sprintf("%.2f", upper), "]")) %>%  
  forestplot(labeltext = c( period, season, category,pos,
                            tested,  Prevalence, CI),
             clip = c(0,1),
             boxsize = 0.2,
             ci.vertices = TRUE,
             ci.vertices.height = 0.05,
             xlab= expression(bold("Estimated prevalence with 95% CI")),
             title = "Bayesian posterior estimated prevalence ( with 95% credibility interval) of NoroVirus",
             xlog = FALSE, 
             align = "llrrrc",
             colgap = unit(4, "mm"),
             # xticks = xticks,
             txt_gp = fpTxtGp(ticks=gpar(cex=1), 
                              xlab = gpar(cex=1))) %>% 
  fp_add_header(period  = "Year of Sampling",
                season = "Season of Sampling",
                category = "Species",
                pos = "No.positive", 
                tested = "No.tested", 
                Prevalence = "Prevalence",
                CI = "95% CI" ) %>% 
  
  fp_add_lines(h_1 = gpar(lty = 1),
               h_2 = gpar(lty = 1),
               h_9 = gpar(lty = 1)) %>%
  fp_set_zebra_style("#EFEFEF")
  
  fp_append_row(mean  = 0.39,
                lower = 0.33,
                upper = 0.45,
                anno = expression(bold("Overall Prevalence")),
                Pos = expression(bold("82")),
                tested = expression(bold("209")),
                Prevalence = expression(bold("0.39")),
                CI = expression(bold("[0.33, 0.45]")),
                position = "last",
                is.summary = FALSE) %>% 
  
  
  
  fp_set_style(box = c(rep("black", 5), "royalblue"))






























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



