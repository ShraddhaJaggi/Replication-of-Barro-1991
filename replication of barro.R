# DOWNLOADING VARIABLES

library(WDI)
library(dplyr)
library(tidyr)

# GDP

gdp <- WDI(country = "all",
           indicator = c(
             "NY.GDP.PCAP.KD",   # GDP per capita (constant USD)
             "SP.POP.TOTL"       # population
           ),
           start = 2000,
           end = 2023)

gdp_wide <- gdp %>%
  select(iso2c, country, year, NY.GDP.PCAP.KD, SP.POP.TOTL) %>%
  pivot_wider(names_from = year, values_from = NY.GDP.PCAP.KD)

gdp_wide <- gdp_wide %>%
  mutate(
    GDP2000 = `2000`,
    GDP2010 = `2010`,
    GDP2023 = `2023`,
    GDP2000SQ = GDP2000^2
  )

gdp_clean <- gdp %>%
  select(iso2c, country, year, NY.GDP.PCAP.KD) %>%
  pivot_wider(names_from = year, values_from = NY.GDP.PCAP.KD) %>%
  mutate(
    GDP2000 = `2000`,
    GDP2010 = `2010`,
    GDP2023 = `2023`,
    GROWTH_0023 = (log(GDP2023) - log(GDP2000)) / 23,
    GROWTH_1023 = (log(GDP2023) - log(GDP2010)) / 13
  )

gdp_clean <- gdp_clean %>%
  mutate(
    GDP2000 = `2000`,
    GDP2010 = `2010`,
    GDP2023 = `2023`
  )
names(gdp_clean)



#real investment

library(pwt10)
data("pwt10.0")

inv <- pwt10.0 %>%
  filter(year >= 2000 & year <= 2023) %>%
  group_by(isocode) %>%
  summarise(
    i_y = mean(csh_i, na.rm = TRUE)
  )

gc <- pwt10.0 %>%
  filter(year >= 2000 & year <= 2023) %>%
  group_by(isocode) %>%
  summarise(
    gc_y = mean(csh_g, na.rm = TRUE)
  )

inv_0023 <- pwt10.0 %>%
  filter(year >= 2000 & year <= 2023) %>%
  group_by(isocode) %>%
  summarise(
    i_y_0023 = mean(csh_i, na.rm = TRUE)
  )

gc_i <- pwt10.0 %>%
  filter(year >= 2000 & year <= 2023) %>%
  group_by(isocode) %>%
  summarise(
    gc_y = mean(csh_g, na.rm = TRUE),
    i_y  = mean(csh_i, na.rm = TRUE),
    gc_i = gc_y / i_y
  )

inv$isocode <- as.character(inv$isocode)
gc$isocode  <- as.character(gc$isocode)




#demographic variables

pop2000 <- gdp %>%
  filter(year == 2000) %>%
  select(iso2c, SP.POP.TOTL) %>%
  rename(POP2000 = SP.POP.TOTL)

demo <- WDI(country = "all",
            indicator = c(
              "SP.DYN.TFRT.IN",   # fertility
              "SH.DYN.MORT"       # child mortality
            ),
            start = 2000,
            end = 2023)

demo_avg <- demo %>%
  group_by(iso2c) %>%
  summarise(
    FERT = mean(SP.DYN.TFRT.IN, na.rm = TRUE),
    MORT04 = mean(SH.DYN.MORT, na.rm = TRUE),
    FERTNET = FERT * (1 - MORT04/1000)
  )

pop_growth <- gdp %>%
  group_by(iso2c) %>%
  summarise(
    GPOP0023 = (log(SP.POP.TOTL[year == 2023]) -
                  log(SP.POP.TOTL[year == 2000])) / 23
  )


# education

edu <- WDI(country = "all",
           indicator = c(
             "SE.SEC.ENRR",          # secondary enrolment
             "SE.PRM.ENRR",          # primary enrolment
             "SE.PRM.ENRL.TC.ZS",    # pupil- teacher ratio primary
             "SE.SEC.ENRL.TC.ZS",    # pupil- teacher ratio secondary
             "SE.ADT.LITR.ZS"),      # adult literacy rate
           start = 1990,
           end = 2023)

edu_avg <- edu %>%
  filter(year %in% c(1990, 2000, 2010)) %>%
  group_by(iso2c) %>%
  summarise(
    SEC90 = SE.SEC.ENRR[year == 1990][1],
    SEC00 = SE.SEC.ENRR[year == 2000][1],
    SEC10 = SE.SEC.ENRR[year == 2010][1],
    SEC_RATIO = SE.SEC.ENRL.TC.ZS[year == 2000][1],
    PRIM90 = SE.PRM.ENRR[year == 1990][1],
    PRIM00 = SE.PRM.ENRR[year == 2000][1],
    PRIM10 = SE.PRM.ENRR[year == 2010][1],
    PRIM_RATIO = SE.PRM.ENRL.TC.ZS[year == 2000][1],
    LIT00 = SE.ADT.LITR.ZS[year == 2000][1]
  )


#growth rate

growth <- gdp %>%
  select(iso2c, country, year, NY.GDP.PCAP.KD) %>%
  pivot_wider(names_from = year, values_from = NY.GDP.PCAP.KD) %>%
  mutate(
    GROWTH_0010 = (log(`2010`) - log(`2000`)) / 10
  )


# revolutions and coups per year and number of assassinations per year proxy

instability <- WDI(country = "all",
                   indicator = c(
                     "VC.IHR.PSRC.P5"
                   ),
                   start = 2000,
                   end = 2023)

instability_avg <- instability %>%
  group_by(iso2c) %>%
  summarise(
    VIOLENCE = mean(VC.IHR.PSRC.P5, na.rm = TRUE)
  )


# ppi 

ppi <- pwt10.0 %>%
  filter(year >= 2000 & year <= 2023) %>%
  group_by(isocode) %>%
  summarise(
    PPPI = mean(pl_i, na.rm = TRUE)
  )

ppi <- ppi %>%
  mutate(
    PPPI_DEV = PPPI - mean(PPPI, na.rm = TRUE)
  )

inv <- inv %>%
  mutate(isocode = as.character(isocode))

gc <- gc %>%
  mutate(isocode = as.character(isocode))

ppi <- ppi %>%
  mutate(isocode = as.character(isocode))

  


# final data

library(countrycode)

final_data <- gdp_clean %>%
  filter(!(iso2c %in% c("JG", "XK"))) %>%
  mutate(isocode = countrycode(iso2c, "iso2c", "iso3c")) %>%
  left_join(inv, by = "isocode") %>%
  left_join(gc, by = "isocode") %>%
  left_join(demo_avg, by = "iso2c") %>%
  left_join(pop_growth, by = "iso2c") %>%
  left_join(edu_avg, by = "iso2c") %>%
  left_join(pop2000, by = "iso2c") %>%
  left_join(instability_avg, by = "iso2c") %>%
  left_join(ppi, by = "isocode") %>%
  left_join(inv_0023, by = "isocode") %>%
  left_join(gc_i %>% select(isocode, gc_i), by = "isocode")


library(countrycode)

final_data <- final_data %>%
  mutate(
    region = countrycode(isocode, "iso3c", "region"),
    AFRICA = ifelse(region == "Sub-Saharan Africa", 1, 0),
    LATAM = ifelse(region == "Latin America & Caribbean", 1, 0)
  )

final_data$isocode <- as.character(final_data$isocode)



# REGRESSIONS

#correlation
cor(gdp_clean$GROWTH_0023, log(gdp_clean$GDP2000), use = "complete.obs")


#regression data

reg_data <- final_data %>%
  filter(
    GDP2000 > 0,
    !is.na(GROWTH_0023),
    !is.na(SEC90),
    !is.na(SEC00),
    !is.na(SEC10),
    !is.na(PRIM90),
    !is.na(PRIM00),
    !is.na(PRIM10)
  ) %>%
  mutate(
    logGDP = log(GDP2000),
    GDP2000SQ = GDP2000^2,
    sqrtpop2000 = sqrt(POP2000),
    sqrtGDP2000 = sqrt(GDP2000),
    rich_dummy = ifelse(GDP2000 > median(GDP2000, na.rm = TRUE), 1, 0)
  )



m1 <- lm(GROWTH_0023 ~ logGDP + SEC00 + PRIM00 + gc_y + VIOLENCE + PPPI_DEV, data = reg_data)
summary(m1)

m2 <- lm(GROWTH_0023 ~ logGDP + GDP2000SQ + SEC00 + PRIM00 + gc_y + VIOLENCE + PPPI_DEV, data = reg_data)
summary(m2)

m3 <- lm(GROWTH_1023 ~ logGDP + SEC00 + PRIM00 + gc_y + VIOLENCE + PPPI_DEV, data = reg_data)
summary(m3)

m4 <- lm(GROWTH_1023 ~ logGDP + I(log(`GDP2010`)) + SEC00 + PRIM00 
         + gc_y + VIOLENCE + PPPI_DEV, data = reg_data)
summary(m4)

m5 <- lm(GROWTH_0023 ~ logGDP + GDP2000SQ + SEC00 + PRIM00
         + gc_y + VIOLENCE + PPPI_DEV, data = reg_data %>% 
           filter(rich_dummy == 1))
summary(m5)

m6 <- lm(GROWTH_0023 ~ logGDP + SEC00 + PRIM00
         + gc_y + VIOLENCE + PPPI_DEV, data = reg_data, weights = sqrtGDP2000)
summary(m6)

m7 <- lm(GROWTH_0023 ~ logGDP + SEC00 + PRIM00
         + gc_y + VIOLENCE + PPPI_DEV, data = reg_data, weights = sqrtpop2000)
summary(m7)

m8 <- lm(GROWTH_0023 ~ logGDP + SEC00 + PRIM00 + SEC90 + PRIM90
         + gc_y + VIOLENCE + PPPI_DEV, data = reg_data, weights = sqrtpop2000)
summary(m8)

m9 <- lm(GROWTH_1023 ~ logGDP + SEC00 + PRIM00 + SEC10 + PRIM10
         + gc_y + VIOLENCE + PPPI_DEV, data = reg_data, weights = sqrtpop2000)
summary(m9)

m10 <- lm(GROWTH_0023 ~ logGDP + SEC00 + PRIM00 + PRIM_RATIO + gc_y 
          + VIOLENCE + PPPI_DEV, data = reg_data, weights = sqrtpop2000)
summary(m10) 

m11 <- lm(GROWTH_0023 ~ logGDP + SEC00 + PRIM00 + PRIM_RATIO + SEC_RATIO + gc_y 
          + VIOLENCE + PPPI_DEV, data = reg_data, weights = sqrtpop2000)
summary(m11) 

m12 <- lm(GROWTH_0023 ~ logGDP + SEC00 + LIT00 + gc_y 
          + VIOLENCE + PPPI_DEV, data = reg_data, weights = sqrtpop2000)
summary(m12) 

m13 <- lm(GROWTH_0023 ~ logGDP + SEC00 + PRIM00 + gc_y + VIOLENCE + 
            PPPI_DEV, data = reg_data, weights = sqrtpop2000)
summary(m13) 

m14 <- lm(GROWTH_0023 ~ logGDP + SEC00 + PRIM00 + gc_y + VIOLENCE + PPPI_DEV
          + AFRICA + LATAM , data = reg_data, weights = sqrtpop2000)
summary(m14) 

m15 <- lm(FERTNET ~ logGDP + SEC00 + PRIM00 + gc_y + VIOLENCE + PPPI_DEV, data = reg_data)
summary(m15)

m16 <- lm(FERT ~ logGDP + SEC00 + PRIM00 + gc_y + VIOLENCE + PPPI_DEV + MORT04, data = reg_data)
summary(m16)

m17 <- lm(FERTNET ~ logGDP + SEC00 + PRIM00 + gc_y + VIOLENCE + PPPI_DEV + MORT04, data = reg_data)
summary(m17)

m18 <- lm(GPOP0023 ~ logGDP + SEC00 + PRIM00 + gc_y + VIOLENCE + PPPI_DEV, data = reg_data)
summary(m18)

m19 <- lm(FERTNET ~ logGDP + SEC00 + PRIM00 + gc_y + VIOLENCE + PPPI_DEV + AFRICA + LATAM, data = reg_data)
summary(m19)

m20 <- lm(i_y ~ logGDP + SEC00 + PRIM00 + gc_y + VIOLENCE + PPPI_DEV + PPPI, data = reg_data)
summary(m20)

m21 <- lm(i_y ~ logGDP + SEC00 + PRIM00 + gc_y + VIOLENCE + PPPI_DEV + PPPI + AFRICA
          + LATAM, data = reg_data)
summary(m21)

m22 <- lm(GROWTH_0023 ~ logGDP + SEC00 + PRIM00 + gc_y + VIOLENCE + PPPI_DEV + i_y, data = reg_data)
summary(m22)

m23 <- lm(GROWTH_0023 ~ logGDP + SEC00 + PRIM00 + gc_y + VIOLENCE + PPPI_DEV + FERTNET, data = reg_data)
summary(m23)

m24 <- lm(GROWTH_0023 ~ logGDP + SEC00 + PRIM00 + gc_y + VIOLENCE + PPPI_DEV + gc_y, data = reg_data)
summary(m24)

m25 <- lm(GROWTH_0023 ~ logGDP + SEC00 + PRIM00 + gc_y + VIOLENCE + PPPI_DEV + i_y_0023 + gc_y, data = reg_data)
summary(m25)

m26 <- lm(GROWTH_0023 ~ logGDP + SEC00 + PRIM00 + gc_y + VIOLENCE + PPPI_DEV + gc_i, data = reg_data)
summary(m26)

m27 <- lm(GROWTH_0023 ~ logGDP + SEC00 + PRIM00 + gc_y + VIOLENCE + PPPI_DEV + i_y + FERTNET
          + AFRICA + LATAM, data = reg_data)
summary(m27)




# REGRESSION TABLES

# table 1.1

library(stargazer)

stargazer(m1, m2, m3, m4, m5, m6, m7, m8,
          type = "html",
          title = "Table 1: Regressions for Per Capita Growth",
          
          dep.var.labels.include = TRUE,
          
          add.lines = list(
            c("Weights", 
              "None","None","None","None",
              "Rich only","sqrt(GDP)","sqrt(POP)","None")
          ),
          
          omit.stat = c("f", "ser"),
          digits = 4,
          out = "table1.1.html")


#table 1.2

stargazer(m9, m10, m11, m12, m13, m14,
          type = "html",
          title = "Table 1: Regressions for Per Capita Growth",
          dep.var.labels.include = TRUE,
          
          column.labels = c("(9)", "(10)", "(11)", "(12)", "(13)", "(14)"),
          column.separate = rep(1, 6),   
          
          model.numbers = FALSE,         
          
          omit.stat = c("f", "ser"),
          digits = 4,
          out = "table1.2.html")


#checking for multicollinearity
cor(reg_data$SEC00, reg_data$PRIM00, use = "complete.obs")


# table 2

stargazer(m15, m16, m17, m18, m19, 
          type = "html",
          title = "Table 2: Regressions for Fertility",
         
          dep.var.labels.include = TRUE,
          
          column.labels = c("(15)", "(16)", "(17)", "(18)", "(19)"),
          column.separate = rep(1, 7),   
          
          model.numbers = FALSE,         
          
          omit.stat = c("f", "ser"),
          digits = 4,
          out = "table2.html")


# table 3


stargazer(m20, m21, 
          type = "html",
          title = "Table 3: Regressions for Investment",
          
          dep.var.labels.include = TRUE,
          
          column.labels = c("(20)", "(21)"),
          column.separate = rep(1, 2),   
          
          model.numbers = FALSE,         
          
          omit.stat = c("f", "ser"),
          digits = 4,
          out = "table3.html")



# table 4

stargazer(m22, m23, m24, m25, m26, m27, 
          type = "html",
          title = "Table 4: Interactions between Growth and Development",
          
          dep.var.labels.include = TRUE,
          
          column.labels = c("(22)", "(23)", "(24)", "(25)", "(26)", "(27)"),
          column.separate = rep(1, 6),   
          
          model.numbers = FALSE,         
          
          omit.stat = c("f", "ser"),
          digits = 4,
          out = "table4.html")




# FIGURES

# figure 1

plot_data <- gdp_clean %>%
  filter(
    !is.na(GDP2000),
    !is.na(GDP2023),
    GDP2000 > 0
  )

fig1 <- ggplot(gdp_clean %>%
                 filter(!is.na(GDP2000), !is.na(GDP2023), GDP2000 > 0),
               aes(x = log(GDP2000), y = GROWTH_0023)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Figure 1: Growth vs Initial Income (2000–2023)",
    x = "Log GDP per capita (2000)",
    y = "Growth rate (2000–2023)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,      # center
      size = 16,        # bigger size
      face = "bold"     # optional: makes it look nicer
    )
  )
fig1

ggsave("figure1.png",plot=fig1, width = 8, height = 6, dpi = 300)





#figure 2

res_sample <- reg_data %>%
  select(GROWTH_0023, logGDP, SEC00, PRIM00, gc_y, VIOLENCE, PPPI_DEV) %>%
  na.omit()

res_sample <- res_sample %>%
  mutate(
    res_growth = resid(
      lm(GROWTH_0023 ~ SEC00 + PRIM00 + gc_y + VIOLENCE + PPPI_DEV,
         data = res_sample)
    ),
    res_gdp = resid(
      lm(logGDP ~ SEC00 + PRIM00 + gc_y + VIOLENCE + PPPI_DEV,
         data = res_sample)
    )
  )

fig2 <- ggplot(res_sample,
               aes(x = res_gdp, y = res_growth)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Figure 2: Partial Relationship Between Growth and Initial Income",
    x = "Residual Log GDP per capita (2000)",
    y = "Residual Growth rate (2000–2023)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    )
  )
fig2

ggsave("figure2.png",plot=fig2, width = 8, height = 6, dpi = 300)

cor(res_sample$res_growth, res_sample$res_gdp)





#figure 3

edu_sample <- reg_data %>%
  select(GROWTH_0023, SEC00, PRIM00, logGDP, gc_y, VIOLENCE, PPPI_DEV) %>%
  na.omit()

edu_sample <- edu_sample %>%
  mutate(
    res_growth = resid(
      lm(GROWTH_0023 ~ logGDP + PRIM00 + gc_y + VIOLENCE + PPPI_DEV,
         data = edu_sample)
    ),
    res_sec = resid(
      lm(SEC00 ~ logGDP + PRIM00 + gc_y + VIOLENCE + PPPI_DEV,
         data = edu_sample)
    )
  )

fig3 <- ggplot(edu_sample,
               aes(x = res_sec, y = res_growth)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Figure 3: Partial Relationship Between Growth and School Enrollment",
    x = "Residual Secondary School Enrollment (2000)",
    y = "Residual Growth rate (2000–2023)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    )
  )

fig3

ggsave("figure3.png", plot = fig3, width = 8, height = 6, dpi = 300)




# figure 4

edu_sample2 <- reg_data %>%
  select(GROWTH_0023, SEC00, PRIM00, logGDP, gc_y, VIOLENCE, PPPI_DEV) %>%
  na.omit()

edu_sample2 <- edu_sample2 %>%
  mutate(
    # weighted education variable (Barro-style)
    edu_index = 0.0305 * SEC00 + 0.0250 * PRIM00,
    
    # residual growth
    res_growth = resid(
      lm(GROWTH_0023 ~ logGDP + gc_y + VIOLENCE + PPPI_DEV,
         data = edu_sample2)
    ),
    
    # residual education index
    res_edu = resid(
      lm(edu_index ~ logGDP + gc_y + VIOLENCE + PPPI_DEV,
         data = edu_sample2)
    )
  )

fig4 <- ggplot(edu_sample2,
               aes(x = res_edu, y = res_growth)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Figure 4: Growth vs Combined School Enrollment",
    x = "Residual Education Index (0.0305*SEC + 0.0250*PRIM)",
    y = "Residual Growth rate (2000–2023)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    )
  )

fig4

ggsave("figure4.png", plot = fig4, width = 8, height = 6, dpi = 300)




# figure 5

fert_data <- final_data %>%
  filter(
    !is.na(GDP2000),
    !is.na(FERTNET),
    GDP2000 > 0
  )

fig5 <- ggplot(fert_data,
               aes(x = log(GDP2000), y = FERTNET)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Figure 5: Net Fertility vs Initial Income",
    x = "Log GDP per capita (2000)",
    y = "Net Fertility Rate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    )
  )

fig5

ggsave("figure5.png", plot = fig5, width = 8, height = 6, dpi = 300)

cor(final_data$FERTNET, log(final_data$GDP2000), use = "complete.obs")




# figure 6

fert_edu_data <- final_data %>%
  filter(
    !is.na(FERTNET),
    !is.na(SEC00),
    !is.na(PRIM00)
  ) %>%
  mutate(
    edu_index = 3.01 * SEC00 + 1.56 * PRIM00
  )

fig6 <- ggplot(fert_edu_data,
               aes(x = edu_index, y = FERTNET)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Figure 6: Net Fertility vs School Enrollment",
    x = "Education Index (3.01*SEC + 1.56*PRIM)",
    y = "Net Fertility Rate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    )
  )

fig6

ggsave("figure6.png", plot = fig6, width = 8, height = 6, dpi = 300)




# figure 7

inv_data <- final_data %>%
  filter(
    !is.na(GDP2000),
    !is.na(i_y),
    GDP2000 > 0
  )

fig7 <- ggplot(inv_data,
               aes(x = log(GDP2000), y = i_y)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Figure 7: Investment Share vs Initial Income",
    x = "Log GDP per capita (2000)",
    y = "Investment Share (i/y)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    )
  )

fig7

ggsave("figure7.png", plot = fig7, width = 8, height = 6, dpi = 300)




# figure 8

inv_edu_data <- final_data %>%
  filter(
    !is.na(i_y),
    !is.na(SEC00),
    !is.na(PRIM00)
  ) %>%
  mutate(
    edu_index = 0.131 * SEC00 + 0.079 * PRIM00
  )

fig8 <- ggplot(inv_edu_data,
               aes(x = edu_index, y = i_y)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Figure 8: Investment Share vs School Enrollment",
    x = "Education Index (0.131*SEC + 0.079*PRIM)",
    y = "Investment Share (i/y)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    )
  )

fig8

ggsave("figure8.png", plot = fig8, width = 8, height = 6, dpi = 300)




# figure 9

gc_sample <- reg_data %>%
  select(GROWTH_0023, gc_y, logGDP, SEC00, PRIM00, VIOLENCE, PPPI_DEV) %>%
  na.omit()

gc_sample <- gc_sample %>%
  mutate(
    res_growth = resid(
      lm(GROWTH_0023 ~ logGDP + SEC00 + PRIM00 + VIOLENCE + PPPI_DEV,
         data = gc_sample)
    ),
    res_gc = resid(
      lm(gc_y ~ logGDP + SEC00 + PRIM00 + VIOLENCE + PPPI_DEV,
         data = gc_sample)
    )
  )

fig9 <- ggplot(gc_sample,
               aes(x = res_gc, y = res_growth)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Figure 9: Partial Relationship Between Growth and Government Consumption",
    x = "Residual Government Consumption (gc/y)",
    y = "Residual Growth rate (2000–2023)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    )
  )

fig9

ggsave("figure9.png", plot = fig9, width = 8, height = 6, dpi = 300)



# figure 10

ppi_data <- final_data %>%
  filter(
    !is.na(GDP2000),
    !is.na(PPPI),
    GDP2000 > 0
  )

fig10 <- ggplot(ppi_data,
                aes(x = log(GDP2000), y = PPPI)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Figure 10: PPP Investment Price vs Initial Income",
    x = "Log GDP per capita (2000)",
    y = "PPP Investment Price Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    )
  )

fig10

ggsave("figure10.png", plot = fig10, width = 8, height = 6, dpi = 300)




# figure 11

ppi_gdp <- pwt10.0 %>%
  filter(year >= 2000 & year <= 2023) %>%
  group_by(isocode) %>%
  summarise(
    PPPGDP = mean(pl_gdpo, na.rm = TRUE)
  )

ppi_gdp <- ppi_gdp %>%
  mutate(isocode = as.character(isocode))

final_data <- final_data %>%
  left_join(ppi_gdp, by = "isocode")

# figure 11

ppp_data <- final_data %>%
  filter(
    !is.na(GDP2000),
    !is.na(PPPGDP),
    GDP2000 > 0
  )

fig11 <- ggplot(ppp_data,
                aes(x = log(GDP2000), y = PPPGDP)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Figure 11: PPP GDP Price vs Initial Income",
    x = "Log GDP per capita (2000)",
    y = "PPP GDP Price Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    )
  )

fig11

ggsave("figure11.png", plot = fig11, width = 8, height = 6, dpi = 300)




# APPENDIX

summary_data <- final_data %>%
  select(
    GROWTH_0023, GROWTH_1023, GDP2000, i_y, gc_y,
    FERT, MORT04, FERTNET, GPOP0023, SEC90, SEC00, SEC10, SEC_RATIO, 
    PRIM90,PRIM00, PRIM10, PRIM_RATIO, LIT00, POP2000,
    VIOLENCE,PPPI, PPPI_DEV
  ) %>%
  mutate(across(everything(), as.numeric))

summary_data_df <- as.data.frame(summary_data)

stargazer(summary_data_df,
          type = "html",
          digits = 3,
          title = "Appendix: Summary Statistics",
          out = "summary_table.html")



