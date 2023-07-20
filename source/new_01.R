library(Rcpp)
library(ggplot2)
library(ggforce)

sys_id <- "01"
sys_name <- "new"
sourceCpp(here::here("source", paste0(sys_name, "_", sys_id, ".cpp")))

# parameters
seed <- 7

set.seed(seed)

# fixed / default
scheme <- seed
layers <- 5
iter <- 20000
prefix <- paste0(sys_name, "_", sys_id, "_")
transparency <- "ff"
adjust <- function(x) {x}
brd <- 0
f <- .5
col_trans <- rank
bg <- "grey10"
pl <- "scico::grayC"
pal <- NULL
bg <- NULL

fname <- paste0(prefix, seed, ".png")
fpath <- here::here("image", fname)

if(scheme == 1) {pl <- "grDevices::Purples"}
if(scheme == 2) {pl <- "grDevices::PuRd"}
if(scheme == 3) {pl <- "grDevices::Oranges"}
if(scheme == 4) {f <- 2; pf <- jasmines::palette_named("blood"); pal <- pf(256)}
if(scheme == 5) {f <- 3; pl <- "scico::bilbao"; bg <- "grey10"}
if(scheme == 6) {f <- 1; pl <- "grDevices::Blues"}
if(scheme == 7) {pal <- scico::scico(4, palette = "grayC"); iter <- 100000} # <- slooooow

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

df <- df[col_idx < max(col_idx) * .9, ]

cat("rendering...\n")

if(is.null(bg)) bg <- pal[1]
p <- ggplot(
  data = df,
  mapping = aes(x = x, y = y, group = 1, fill = col)
) + 
  geom_voronoi_tile(max.radius = .05) +
  scale_fill_identity() + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void() + 
  theme(panel.background = element_rect(fill = bg, colour = bg)) + 
  coord_cartesian(xlim = filter_x * .9, ylim = filter_y * .9)

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
