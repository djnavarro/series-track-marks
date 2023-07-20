library(Rcpp)
library(ggplot2)
library(ggforce)
library(voronoise)
library(dplyr)

sys_id <- "04"
sys_name <- "new"
sourceCpp(here::here("source", paste0(sys_name, "_", sys_id, ".cpp")))

# parameters
seed <- 10
cat(seed, "\n")

set.seed(seed)

# fixed / default
scheme <- seed
layers <- 5
iter <- 50000
mesh <- .02
prefix <- paste0(sys_name, "_", sys_id, "_")
transparency <- "ff"
adjust <- function(x) {x}
brd <- 0
f <- 1
col_trans <- rank
bg <- "grey10"
pl <- "scico::grayC"
pal <- NULL
bg <- NULL
flip <- TRUE
ex <- 0
rd <- 0

fname <- paste0(prefix, seed, ".png")
fpath <- here::here("image", fname)

if(scheme == 1) {pl <- "grDevices::Purples";}
if(scheme == 2) {pal <- hcl.colors(100, palette = "PuRd"); bg <- "grey10";}
if(scheme == 3) {pl <- "grDevices::Oranges"; ex <- -.0004; rd <- .001;}
if(scheme == 4) {f <- 2; runif(1); pf <- jasmines::palette_named("blood"); pal <- pf(10); bg <- "black";}
if(scheme == 5) {pl <- "scico::bilbao"; bg <- "grey10";}
if(scheme == 6) {f <- 1; pal <- hcl.colors(256, palette = "Blues");}
if(scheme == 7) {pal <- scico::scico(4, palette = "grayC"); iter <- 100000} # <- slooooow
if(scheme == 8) {runif(1); pl <- "scico::bamako"; ex <- -.0004; rd <- .001; f <- .5}
if(scheme == 9) {pal <- scico::scico(4, palette = "lajolla"); iter <- 100000} # <- slooooow

if(is.null(pal)) {
  pal <- paletteer::paletteer_c(palette = pl, n = 256)
  pal <- adjust(pal)
}
filter_x <- c(-f, f)
filter_y <- c(-f, f)

cat("generating...\n")


# create data frame
df <- breaksheet(iter, layers)
df <- as.data.frame(df)
names(df) <- c("x","y","c")

# filter and transform
df <- df[-(1:100),]
if(!is.null(filter_x)) {
  x_ok <- df$x > filter_x[1] & df$x < filter_x[2]
  y_ok <- df$y > filter_y[1] & df$y < filter_y[2] 
  df <- df[x_ok & y_ok, ]
}
if(!is.null(col_trans)){
  df$c <- col_trans(df$c)
}
if(flip) {
  df$y <- -df$y
}

# scale the co-ordinates to the image size
px <- 5000
xrng <- max(df[,1]) - min(df[,1])
yrng <- max(df[,2]) - min(df[,2])
rng <- max(c(xrng, yrng))

# xdiff <- max(c(yrng - xrng, 0))/2
# ydiff <- max(c(xrng - yrng, 0))/2
# 
# df[,1] <- brd + (df[,1] - min(df[,1]) + xdiff) / rng * (px - 2*brd)
# df[,2] <- brd + (df[,2] - min(df[,2]) + ydiff) / rng * (px - 2*brd)


# create a vector of colours
ncol <- length(pal)
col_idx <- as.integer((df[,3] - min(df[,3])) / (max(df[,3]) - min(df[,3])) * (ncol - 1)) + 1L
pal <- gsub("FF$", transparency, pal)
df$col <- pal[col_idx]

#df <- df[col_idx < max(col_idx) * .9, ]

cat("rendering...\n")

sift <- function(grain = .025) {
  function(data) {
    data <- data %>% 
      group_by(group) %>%
      mutate(tilesize = (max(x) - min(x)) * (max(y) - min(y))) %>%
      ungroup()
    data$tilealpha <- .1
    data$tilealpha[data$tilesize < grain^2] <- 1
    return(data)
  }
}

if(is.null(bg)) bg <- pal[1]
p <- ggplot(
  data = df,
  mapping = aes(
    x = x, 
    y = y, 
    group = 1, 
    fill = col, 
    alpha = after_stat(tilealpha)
  )
) + 
  geom_voronoise(
    perturb = sift(), 
    max.radius = NULL, 
    radius = rd, 
    expand = ex
  ) +
  scale_fill_identity() + 
  scale_alpha_identity() + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void() + 
  theme(panel.background = element_rect(fill = bg, colour = bg)) + 
  coord_cartesian(
    xlim = filter_x * .9, 
    ylim = filter_y * .9
  )

ggsave(
  file = fpath,
  plot = p,
  width = 5000 / 300,
  height = 5000 / 300,
  dpi = 300
)

#cb <- cairobasic::CairoBasic$new(width = px, height = px, bg = bg, antialias = TRUE)
#cb$add_circles(x=df[,1], y = df[,2], r = 5, fill = col, colour = NA)
#cb$write_png(fpath)
