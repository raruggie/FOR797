library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(tidyverse)
library(tidymodels)
library(data.table)
library(randomForest)
library(VSURF)
library(ie2misc)
library(plotly)
library(caret)
library(splitTools)
library(hrbrthemes)
library(viridis)


load('Processed_Data/Master_Import_Data.Rdata')


dfPlot <- dfMaster[dfMaster$Watershed != "HBF"]
#dfPlot <- dfPlot[dfPlot$Watershed != "GOF"]


plt <- dfPlot %>%
  ggplot( aes(x=Watershed, y=FDOM_corrected_QSU, fill=Watershed)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=18),
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size = 14), 
    axis.text.x = element_text(angle = 45, size = 14)
  ) +
  #ylim(c(0,1)) +
  #coord_cartesian(ylim=c(0, 75)) +
  ggtitle("Fluorescent Dissolved Organic Matter (FDOM)") +
  labs(x="", y='FDOM (ppb quinine sulfate)')
  #labs(x="", y='Specific Conductivity (µS/cm)')
  #labs(x="", y='Temperature (°C)')

plt








