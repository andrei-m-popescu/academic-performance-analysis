library(WDI)
library(plm)
library(tidyverse)
library(lmtest)
library(sandwich)

countries <- c("RO", "FR", "DE", "IT", "ES", "NL", "PL", "HU", "CZ", "AT")

indicators <- c(
  gdp_pc = "NY.GDP.PCAP.KD",
  education = "SE.XPD.TOTL.GD.ZS",
  unemployment = "SL.UEM.TOTL.ZS",
  energy = "EG.USE.PCAP.KG.OE"
)

wb_data <- WDI(
  country = countries,
  indicator = indicators,
  start = 2010,
  end = 2020
)

head(wb_data)
str(wb_data)
summary(wb_data)
nrow(wb_data)

panel_df <- wb_data %>%
  select(country, year, gdp_pc, education, unemployment, energy) %>%
  arrange(country, year) %>%
  drop_na()

nrow(panel_df)
head(panel_df)

pdata <- pdata.frame(panel_df, index = c("country", "year"))
pdim(pdata)


m_pool <- plm(gdp_pc ~ education + unemployment + energy, data = pdata, model = "pooling")
m_fe   <- plm(gdp_pc ~ education + unemployment + energy, data = pdata, model = "within")
m_re   <- plm(gdp_pc ~ education + unemployment + energy, data = pdata, model = "random")

summary(m_pool)
summary(m_fe)
summary(m_re)


pFtest(m_fe, m_pool)

plmtest(m_pool, type = "bp")

phtest(m_fe, m_re)


library(lmtest)
library(sandwich)

coeftest(m_fe, vcov = vcovHC(m_fe, type = "HC1", cluster = "group"))
coeftest(m_re, vcov = vcovHC(m_re, type = "HC1", cluster = "group"))
