######################
# recreate Yamada chart

library(meta)
library(dplyr)
library(tibble)

dat <- tribble(
  ~study,              ~event.e, ~n.e, ~event.c, ~n.c,
  "Ramsden CE 2013",          32,  221,       24,  237,
  "Burr ML 1989",            132, 1018,      144, 1015,
  "Frantz ID 1989",           61, 4541,       54, 4516,
  "Leren P 1966",             42,  206,       74,  206,
  "MRC 1968",                 62,  199,       74,  194,
  "Rose GA 1965",             21,   54,       10,   26,
  "Dayton S 1969",            47,  424,       54,  422,
  "Vijayakumar M 2016",        0,   99,        0,   99,
  "Watts GF 1992",             2,   26,        5,   24
)

m1 <- metabin(
  event.e = event.e,
  n.e     = n.e,
  event.c = event.c,
  n.c     = n.c,
  studlab = study,
  data    = dat,
  sm      = "OR",
  method  = "MH",
  common  = TRUE,
  random  = TRUE,
  method.tau = "REML",
  method.random.ci    = FALSE,
  prediction = FALSE
)

forest(
  m1,
  leftcols  = "studlab",
  leftlabs  = "Study",
  rightcols = c("effect", "ci", "w.random"),
  rightlabs = c("OR", "95% CI", "Weight"),
  overall = TRUE,
  overall.hetstat = TRUE,
  smlab = "Odds ratio",
  xlab  = "Odds ratio",
  xlog  = TRUE,
  xlim  = c(0.1, 10),
  at    = c(0.1, 0.5, 1, 2, 10),
  digits = 2,
  digits.weight = 1,
  colgap.forest.left  = "10mm",
  colgap.forest.right = "8mm",
  addrows.below.overall = 3
)
grid::grid.text(
  "Yamada Figure 5\nSFA reduction trials on any coronary artery event",
  x = 0.5, y = 0.9,
  gp = grid::gpar(fontsize = 14, fontface = "bold")
)

### All cause mortality

dat <- tribble(
  ~study,              ~event.e, ~n.e, ~event.c, ~n.c,
  "Ramsden CE 2013",          35,  221,       28,  237,
  "Burr ML 1989",            111, 1018,      113, 1015,
  "Frantz ID 1989",           269, 4541,       248, 4516,
  "Leren P 1966",             41,  206,       55,  206,
  "MRC 1968",                 28,  199,       31,  194,
  "Rose GA 1965",             8,   54,       1,   26,
  "Dayton S 1969",            174,  424,       177,  422,
  "Vijayakumar M 2016",        0,   99,        2,   99,
  "Watts GF 1992",             1,   27,        3,   28
)

m1 <- metabin(
  event.e = event.e,
  n.e     = n.e,
  event.c = event.c,
  n.c     = n.c,
  studlab = study,
  data    = dat,
  sm      = "OR",
  method  = "MH",
  common  = TRUE,
  random  = TRUE,
  method.tau = "REML",
  method.random.ci    = FALSE,
  prediction = FALSE
)

forest(
  m1,
  leftcols  = "studlab",
  leftlabs  = "Study",
  rightcols = c("effect", "ci", "w.random"),
  rightlabs = c("OR", "95% CI", "Weight"),
  overall = TRUE,
  overall.hetstat = TRUE,
  smlab = "Odds ratio",
  xlab  = "Odds ratio",
  xlog  = TRUE,
  xlim  = c(0.01, 100),
  at    = c(0.01, 0.1, 1, 10, 100),
  digits = 2,
  digits.weight = 1,
  colgap.forest.left  = "10mm",
  colgap.forest.right = "8mm",
  addrows.below.overall = 3
)
grid::grid.text(
  "Yamada Figure 3\nSFA reduction trials on all-cause mortality",
  x = 0.5, y = 0.9,
  gp = grid::gpar(fontsize = 14, fontface = "bold")
)

######################
# recreate Steen chart

dat_pufa <- tribble(
  ~study,                 ~event.e, ~n.e, ~event.c, ~n.c,
  "DART 1989",                  35, 1018,       47, 1015,
  "Lyon Diet Heart 1994",        5,  302,       17,  303,
  "MRC 1968",                   25,  199,       25,  194,
  "Oslo Diet-Heart 1966",       24,  206,       31,  206,
  "Rose et al (corn oil) 1965",  7,   28,        5,   26,
  "Veterans Admin 1969",        13,  424,       21,  427
) %>%
  mutate(
    study = factor(
      study,
      levels = c(
        "DART 1989",
        "Lyon Diet Heart 1994",
        "MRC 1968",
        "Oslo Diet-Heart 1966",
        "Rose et al (corn oil) 1965",
        "Veterans Admin 1969"
      )
    )
  ) %>%
  arrange(study)

m_pufa <- metabin(
  event.e = event.e,
  n.e     = n.e,
  event.c = event.c,
  n.c     = n.c,
  studlab = study,
  data    = dat_pufa,
  sm      = "RR",
  method  = "MH",
  common  = FALSE,
  random  = TRUE,
  method.tau = "REML",   # DL gives more 'optimistic' precision
  method.random.ci    = FALSE,
  prediction = FALSE
)

summary(m_pufa)


forest(
  m_pufa,
  leftcols  = "studlab",
  leftlabs  = "Study",
  rightcols = c("effect", "ci", "w.random"),
  rightlabs = c("RR", "95% CI", "Weight"),
  overall = TRUE,
  overall.hetstat = TRUE,
  smlab = "Risk ratio",
  xlab  = "Risk ratio",
  xlog  = TRUE,
  xlim  = c(0.1, 10),
  at    = c(0.1, 0.5, 1, 2, 10),
  digits = 2,
  digits.weight = 1,
  colgap.forest.left  = "10mm",
  colgap.forest.right = "8mm",
  addrows.below.overall = 3,
  
)
grid::grid.text(
  "Steen Figure 3. Nonfatal MI\nsubgroup by PUFA replacement versus other macronutrients.",
  x = 0.5, y = 0.9,
  gp = grid::gpar(fontsize = 14, fontface = "bold")
)

#dev.off()
