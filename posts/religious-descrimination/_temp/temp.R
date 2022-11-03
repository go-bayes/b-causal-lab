
```{r}
# read data
dat <- readRDS(pull_path)
# graph change in religion over time.


ids <- dat %>%
  dplyr::filter(
    (Wave == 2016 & YearMeasured == 1) |
      (Wave == 2017 & YearMeasured == 1) |
      (Wave == 2018 & YearMeasured == 1) |
      (Wave = 2019 & YearMeasured == 1)|
      (Wave = 2020 & YearMeasured == 1)|
      (Wave = 2021 & YearMeasured == 1)
  ) %>%
  group_by(Id, Wave) %>%
  select(Id, Wave)

length(unique(ids$Id))

dat$Spiritual.Identification


library(dplyr)
dt_timeline  <- dat %>%
  dplyr::filter((Wave == 2012 &  YearMeasured == 1) |
                  (Wave == 2013 &  YearMeasured == 1) |
                  (Wave == 2014 &  YearMeasured == 1) |
                  (Wave == 2015 &  YearMeasured == 1) |
                  (Wave == 2016 &  YearMeasured == 1) |
                  (Wave == 2017 &  YearMeasured == 1) |
                  (Wave == 2018 &  YearMeasured == 1) |
                  (Wave = 2019 & YearMeasured == 1)
  ) %>%
  droplevels() %>%
  mutate(ReligiousID = as.factor(round(Relid,0))) |>
  select(Perc.Religious.Discrim,
         TSCORE,
         ReligiousID,
         Spiritual.Identification,
         Relid,
         Id) %>%
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + TSCORE) %>%
  dplyr::filter(timeline > "2016-06-06") %>%
  # dplyr:::count(day = floor_date(timeline, "day"))%>%
  # dplyr::mutate(Attack_Condition = factor(
  #   ifelse(timeline < "2019-03-15", 0, 1),
  #   labels = c("Pre-Attack", "Post-attack")
  # )) %>%
  arrange(timeline)
```


```{r}
#| label: perc-dis-all
#| echo: false
ggplot(dt_timeline, aes(x = timeline, y = Perc.Religious.Discrim, color = ReligiousID)) +
  geom_jitter(alpha = .01, width = 1) +
  stat_smooth(method = "lm") +
  theme(legend.position = "bottom") +
  labs(
    title = "Perceived Religious Discrimination Years 2016 - 2022",
    subtlite = "N = 62,621 New Zealanders (NZAVS)",
    # subtitle = "Strong & apparently growing increase in acceptace after attacks",
    y = "Perceived Religious Discrimination (1-7)",
    x = "NZAVS Time 8 - 13 (2016-2022)"
  ) +
  scale_color_viridis_d(alpha = 1) + theme_classic()
```


```{r}
#| label: relid-all
#| echo: false
ggplot(dt_timeline, aes(x = timeline, y = Relid)) +
  geom_jitter(alpha = .01, width = 1) +
  stat_smooth(method = "lm") +
  theme(legend.position = "bottom") +
  labs(
    title = "Level of Religiousity Years 2016 - 2022",
    subtlite = "N = 62,621 New Zealanders (NZAVS)",
    # subtitle = "Strong & apparently growing increase in acceptace after attacks",
    y = "Level of Religiousity (0-7)",
    x = "NZAVS Time 8 - 13 (2016-2022)"
  )  + theme_classic()
```


```{r}
#| label: spirit-all
#| echo: false
ggplot(dt_timeline, aes(x = timeline, y = Spiritual.Identification)) +
  geom_jitter(alpha = .01, width = 1) +
  stat_smooth(method = "lm") +
  theme(legend.position = "bottom") +
  labs(
    title = "Level of Spiritual Identification Years 2016 - 2022",
    subtlite = "N = 62,621 New Zealanders (NZAVS)",
    # subtitle = "Strong & apparently growing increase in acceptace after attacks",
    y = "Level of Religiousity (0-7)",
    x = "NZAVS Time 8 - 13 (2016-2022)"
  ) + theme_classic()
```
