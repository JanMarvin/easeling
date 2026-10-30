ooxml_preset <- function(prst, w, h, adj) {
  ss <- min(w, h)
  a <- function(nm, def) {
    v <- if (nm %in% names(adj)) adj[[nm]] else NA_real_
    if (is.na(v)) def else v
  }
  poly <- function(x, y, closed = TRUE, nofill = FALSE, nostroke = FALSE, shade = 0) {
    list(x = x, y = y, closed = closed, nofill = nofill, nostroke = nostroke, shade = shade)
  }
  arc <- function(cx, cy, rx, ry, t0, t1, n = 48L) {
    t <- seq(t0, t1, length.out = n)
    list(x = cx + rx * cos(t), y = cy + ry * sin(t))
  }
  fit <- function(x, y) {
    list(
      x = (x - min(x)) / max(diff(range(x)), 1e-9) * w,
      y = (y - min(y)) / max(diff(range(y)), 1e-9) * h
    )
  }
  rect <- function(x0, y0, x1, y1, ...) poly(c(x0, x1, x1, x0), c(y0, y0, y1, y1), ...)
  bez <- function(p, n = 24L) {
    t <- seq(0, 1, length.out = n)
    list(
      x = (1 - t)^3 * p[1L] + 3 * (1 - t)^2 * t * p[3L] + 3 * (1 - t) * t^2 * p[5L] + t^3 * p[7L],
      y = (1 - t)^3 * p[2L] + 3 * (1 - t)^2 * t * p[4L] + 3 * (1 - t) * t^2 * p[6L] + t^3 * p[8L]
    )
  }
  right_arrow <- function(len, wid, a1, a2) {
    ssl <- min(len, wid)
    y1 <- wid / 2 - wid * a1 / 2
    y2 <- wid / 2 + wid * a1 / 2
    x1 <- len - ssl * a2
    list(x = c(0, x1, x1, len, x1, x1, 0), y = c(y1, y1, 0, wid / 2, wid, y2, y2))
  }
  star <- function(n, ratio) {
    k <- 0:(2 * n - 1)
    r <- ifelse(k %% 2L == 0L, 1, ratio)
    ang <- -pi / 2 + k * pi / n
    f <- fit(r * cos(ang), r * sin(ang))
    poly(f$x, f$y)
  }
  regular <- function(n) {
    ang <- -pi / 2 + (0:(n - 1L)) * 2 * pi / n
    f <- fit(cos(ang), sin(ang))
    poly(f$x, f$y)
  }
  rrect <- function(r, wedge = NULL) {
    q <- seq(0, pi / 2, length.out = 9L)
    top <- list(x = numeric(), y = numeric())
    right <- bottom <- left <- top
    ins <- function(side, e) {
      if (!is.null(wedge) && wedge$side == side) {
        e$x <- c(e$x, wedge$x)
        e$y <- c(e$y, wedge$y)
      }
      e
    }
    top <- ins("t", list(x = r, y = 0))
    tr <- list(x = w - r + r * sin(q), y = r - r * cos(q))
    right <- ins("r", list(x = w, y = r))
    br <- list(x = w - r + r * cos(q), y = h - r + r * sin(q))
    bottom <- ins("b", list(x = w - r, y = h))
    bl <- list(x = r - r * sin(q), y = h - r + r * cos(q))
    left <- ins("l", list(x = 0, y = h - r))
    tl <- list(x = r - r * cos(q), y = r - r * sin(q))
    poly(
      c(top$x, tr$x, right$x, br$x, bottom$x, bl$x, left$x, tl$x),
      c(top$y, tr$y, right$y, br$y, bottom$y, bl$y, left$y, tl$y)
    )
  }
  wedge_for <- function() {
    tx <- w / 2 + w * a("adj1", -20833) / 100000
    ty <- h / 2 + h * a("adj2", 62500) / 100000
    dx <- (tx - w / 2) / w
    dy <- (ty - h / 2) / h
    if (abs(dx) > abs(dy)) {
      if (dx < 0) {
        yb <- if (ty < h / 2) c(h / 6, h * 5 / 12) else c(h * 7 / 12, h * 5 / 6)
        list(side = "l", x = c(0, tx, 0), y = c(yb[2L], ty, yb[1L]))
      } else {
        yb <- if (ty < h / 2) c(h / 6, h * 5 / 12) else c(h * 7 / 12, h * 5 / 6)
        list(side = "r", x = c(w, tx, w), y = c(yb[1L], ty, yb[2L]))
      }
    } else {
      xb <- if (tx < w / 2) c(w / 6, w * 5 / 12) else c(w * 7 / 12, w * 5 / 6)
      if (dy < 0) {
        list(side = "t", x = c(xb[1L], tx, xb[2L]), y = c(0, ty, 0))
      } else {
        list(side = "b", x = c(xb[2L], tx, xb[1L]), y = c(h, ty, h))
      }
    }
  }
  deg <- function(v) v / 60000 * pi / 180

  switch(prst,
    rect = , flowChartProcess = , actionButtonBlank = list(rect(0, 0, w, h)),
    roundRect = list(rrect(ss * a("adj", 16667) / 100000)),
    trapezoid = {
      x2 <- ss * a("adj", 25000) / 100000
      list(poly(c(0, x2, w - x2, w), c(h, 0, 0, h)))
    },
    hexagon = {
      x1 <- ss * a("adj", 25000) / 100000
      list(poly(c(0, x1, w - x1, w, w - x1, x1), c(h / 2, 0, 0, h / 2, h, h)))
    },
    octagon = {
      x1 <- ss * a("adj", 29289) / 100000
      list(poly(
        c(0, x1, w - x1, w, w, w - x1, x1, 0),
        c(x1, 0, 0, x1, h - x1, h, h, h - x1)
      ))
    },
    pentagon = list(regular(5L)),
    heptagon = list(regular(7L)),
    decagon = list(regular(10L)),
    diamond = , flowChartDecision = list(poly(c(w / 2, w, w / 2, 0), c(0, h / 2, h, h / 2))),
    triangle = list(poly(c(w * a("adj", 50000) / 100000, w, 0), c(0, h, h))),
    rtTriangle = list(poly(c(0, 0, w), c(0, h, h))),
    parallelogram = {
      d <- ss * a("adj", 25000) / 100000
      list(poly(c(d, w, w - d, 0), c(0, 0, h, h)))
    },
    flowChartInputOutput = list(poly(c(w * 0.2, w, w * 0.8, 0), c(0, 0, h, h))),
    flowChartTerminator = {
      rx <- w * 1018 / 21600
      u <- arc(w - rx, h / 2, rx, h / 2, -pi / 2, pi / 2, 16L)
      v <- arc(rx, h / 2, rx, h / 2, pi / 2, 3 * pi / 2, 16L)
      list(poly(c(u$x, v$x), c(u$y, v$y)))
    },
    flowChartDocument = {
      b <- bez(c(w, h * 0.866, w / 2, h * 0.866, w / 2, h * 1.196, 0, h * 1.0086))
      s <- h / max(b$y)
      list(poly(c(0, w, b$x), c(0, 0, b$y * s)))
    },
    plus = {
      x1 <- ss * a("adj", 25000) / 100000
      list(poly(
        c(x1, w - x1, w - x1, w, w, w - x1, w - x1, x1, x1, 0, 0, x1),
        c(0, 0, x1, x1, h - x1, h - x1, h, h, h - x1, h - x1, x1, x1)
      ))
    },
    mathPlus = {
      t <- ss * a("adj1", 23520) / 200000
      ex <- w * 73490 / 200000
      ey <- h * 73490 / 200000
      cx <- w / 2
      cy <- h / 2
      list(poly(
        c(cx - t, cx + t, cx + t, cx + ex, cx + ex, cx + t, cx + t, cx - t, cx - t, cx - ex, cx - ex, cx - t),
        c(cy - ey, cy - ey, cy - t, cy - t, cy + t, cy + t, cy + ey, cy + ey, cy + t, cy + t, cy - t, cy - t)
      ))
    },
    star4 = list(star(4L, a("adj", 12500) / 50000)),
    star5 = list(star(5L, a("adj", 19098) / 50000)),
    star6 = list(star(6L, a("adj", 28868) / 50000)),
    star7 = list(star(7L, a("adj", 34601) / 50000)),
    star8 = list(star(8L, a("adj", 37500) / 50000)),
    star10 = list(star(10L, a("adj", 42533) / 50000)),
    star12 = list(star(12L, a("adj", 37500) / 50000)),
    star16 = list(star(16L, a("adj", 37500) / 50000)),
    star24 = list(star(24L, a("adj", 37500) / 50000)),
    star32 = list(star(32L, a("adj", 37500) / 50000)),
    rightArrow = {
      s <- right_arrow(w, h, a("adj1", 50000) / 100000, a("adj2", 50000) / 100000)
      list(poly(s$x, s$y))
    },
    leftArrow = {
      s <- right_arrow(w, h, a("adj1", 50000) / 100000, a("adj2", 50000) / 100000)
      list(poly(w - s$x, s$y))
    },
    downArrow = {
      s <- right_arrow(h, w, a("adj1", 50000) / 100000, a("adj2", 50000) / 100000)
      list(poly(s$y, s$x))
    },
    upArrow = {
      s <- right_arrow(h, w, a("adj1", 50000) / 100000, a("adj2", 50000) / 100000)
      list(poly(s$y, h - s$x))
    },
    leftRightArrow = {
      a1 <- a("adj1", 50000) / 100000
      x2 <- ss * a("adj2", 50000) / 100000
      y1 <- h / 2 - h * a1 / 2
      y2 <- h / 2 + h * a1 / 2
      list(poly(
        c(0, x2, x2, w - x2, w - x2, w, w - x2, w - x2, x2, x2),
        c(h / 2, 0, y1, y1, 0, h / 2, h, y2, y2, h)
      ))
    },
    upDownArrow = {
      a1 <- a("adj1", 50000) / 100000
      y2 <- ss * a("adj2", 50000) / 100000
      x1 <- w / 2 - w * a1 / 2
      x2 <- w / 2 + w * a1 / 2
      list(poly(
        c(w / 2, w, x2, x2, w, w / 2, 0, x1, x1, 0),
        c(0, y2, y2, h - y2, h - y2, h, h - y2, h - y2, y2, y2)
      ))
    },
    quadArrow = {
      hl <- ss * 0.225
      hh <- ss * 0.225
      sh <- ss * 0.1125
      cx <- w / 2
      cy <- h / 2
      list(poly(
        c(cx, cx + hh, cx + sh, cx + sh, w - hl, w - hl, w, w - hl, w - hl, cx + sh, cx + sh, cx + hh,
          cx, cx - hh, cx - sh, cx - sh, hl, hl, 0, hl, hl, cx - sh, cx - sh, cx - hh),
        c(0, hl, hl, cy - sh, cy - sh, cy - hh, cy, cy + hh, cy + sh, cy + sh, h - hl, h - hl,
          h, h - hl, h - hl, cy + sh, cy + sh, cy + hh, cy, cy - hh, cy - sh, cy - sh, hl, hl)
      ))
    },
    homePlate = {
      x1 <- w - ss * a("adj", 50000) / 100000
      list(poly(c(0, x1, w, x1, 0), c(0, 0, h / 2, h, h)))
    },
    chevron = {
      d <- ss * a("adj", 50000) / 100000
      list(poly(c(0, w - d, w, w - d, 0, d), c(0, 0, h / 2, h, h, h / 2)))
    },
    notchedRightArrow = {
      a1 <- a("adj1", 50000) / 100000
      x2 <- w - ss * a("adj2", 50000) / 100000
      y1 <- h / 2 - h * a1 / 2
      y2 <- h / 2 + h * a1 / 2
      list(poly(
        c(0, x2, x2, w, x2, x2, 0, h * a1 / 2),
        c(y1, y1, 0, h / 2, h, y2, y2, h / 2)
      ))
    },
    stripedRightArrow = {
      s <- right_arrow(w - ss * 0.2, h, a("adj1", 50000) / 100000, a("adj2", 50000) / 100000)
      y1 <- h / 2 - h * a("adj1", 50000) / 200000
      y2 <- h / 2 + h * a("adj1", 50000) / 200000
      list(
        rect(0, y1, ss / 32, y2),
        rect(ss / 16, y1, ss * 0.15, y2),
        poly(s$x + ss * 0.2, s$y)
      )
    },
    bentArrow = {
      sw <- ss * 0.25
      hh <- ss * 0.25
      hl <- ss * 0.25
      rr <- ss * 0.4375
      yo <- hh - sw / 2
      o <- arc(rr, yo + rr, rr, rr, pi, 3 * pi / 2, 12L)
      ri <- max(rr - sw, 0)
      i <- arc(sw + ri, yo + sw + ri, ri, ri, 3 * pi / 2, pi, 12L)
      list(poly(
        c(0, o$x, w - hl, w - hl, w, w - hl, w - hl, i$x, sw),
        c(h, o$y, yo, 0, hh, 2 * hh, yo + sw, i$y, h)
      ))
    },
    uturnArrow = {
      t <- ss * 0.25
      hh <- ss * 0.25
      hl <- ss * 0.25
      xr <- w - hh
      rcx <- (xr - t / 2) / 2
      cx <- (t / 2 + xr) / 2
      ry <- min(rcx, h * 0.4)
      cyc <- t / 2 + ry
      yb <- h - hl
      o <- arc(cx, cyc, rcx + t / 2, ry + t / 2, pi, 2 * pi, 32L)
      i <- arc(cx, cyc, max(rcx - t / 2, 0), max(ry - t / 2, 0), 2 * pi, pi, 32L)
      list(poly(
        c(0, o$x, xr + t / 2, xr + hh, xr, xr - hh, xr - t / 2, i$x, t),
        c(h, o$y, yb, yb, h, yb, yb, i$y, h)
      ))
    },
    curvedRightArrow = {
      t <- ss * 0.25
      hext <- ss * 0.25
      cx <- w * 0.6
      ry <- (h - hext) / 2 - t / 2
      cyc <- t / 2 + ry
      rx <- max(cx - t / 2, 1)
      u <- seq(0, pi, length.out = 40L)
      ox <- cx - (rx + t / 2) * sin(u)
      oy <- cyc - (ry + t / 2) * cos(u)
      ix <- cx - (rx - t / 2) * sin(rev(u))
      iy <- cyc - (ry - t / 2) * cos(rev(u))
      yi <- cyc + ry - t / 2
      yo <- cyc + ry + t / 2
      list(poly(c(ox, cx, w, cx, ix), c(oy, yo + hext, cyc + ry, yi - hext, iy)))
    },
    donut = {
      d <- ss * a("adj", 25000) / 100000
      o <- arc(w / 2, h / 2, w / 2, h / 2, 0, 2 * pi)
      i <- arc(w / 2, h / 2, w / 2 - d, h / 2 - d, 0, 2 * pi)
      list(poly(o$x, o$y), poly(i$x, i$y))
    },
    frame = {
      d <- ss * a("adj1", 12500) / 100000
      list(rect(0, 0, w, h), rect(d, d, w - d, h - d))
    },
    bevel = {
      d <- ss * a("adj", 12500) / 100000
      list(
        rect(d, d, w - d, h - d),
        poly(c(0, w, w - d, d), c(0, 0, d, d), shade = 0.2),
        poly(c(w, w, w - d, w - d), c(0, h, h - d, d), shade = -0.4),
        poly(c(w, 0, d, w - d), c(h, h, h - d, h - d), shade = -0.2),
        poly(c(0, d, d, 0), c(h, h - d, d, 0), shade = 0.4)
      )
    },
    foldedCorner = {
      d <- ss * a("adj", 16667) / 100000
      list(
        poly(c(0, w, w, w - d, w - d, 0), c(0, 0, h - d, h - d, h, h)),
        poly(c(w - d, w, w - d), c(h - d, h - d, h))
      )
    },
    cube = {
      d <- ss * a("adj", 25000) / 100000
      list(
        rect(0, d, w - d, h),
        poly(c(0, d, w, w - d), c(d, 0, 0, d), shade = 0.2),
        poly(c(w - d, w, w, w - d), c(d, 0, h - d, h), shade = -0.2)
      )
    },
    can = {
      ry <- ss * a("adj", 25000) / 200000
      top <- arc(w / 2, ry, w / 2, ry, 0, 2 * pi)
      up <- arc(w / 2, ry, w / 2, ry, pi, 2 * pi, 32L)
      low <- arc(w / 2, h - ry, w / 2, ry, 0, pi, 32L)
      list(
        poly(c(up$x, low$x), c(up$y, low$y), nostroke = FALSE),
        poly(top$x, top$y, shade = 0.2)
      )
    },
    plaque = {
      r <- ss * a("adj", 16667) / 100000
      c1 <- arc(0, 0, r, r, pi / 2, 0, 12L)
      c2 <- arc(w, 0, r, r, pi, pi / 2, 12L)
      c3 <- arc(w, h, r, r, -pi / 2, -pi, 12L)
      c4 <- arc(0, h, r, r, 0, -pi / 2, 12L)
      list(poly(c(c1$x, c2$x, c3$x, c4$x), c(c1$y, c2$y, c3$y, c4$y)))
    },
    smileyFace = {
      f <- arc(w / 2, h / 2, w / 2, h / 2, 0, 2 * pi)
      e1 <- arc(w * 0.3, h * 0.37, w * 0.06, h * 0.07, 0, 2 * pi, 24L)
      e2 <- arc(w * 0.7, h * 0.37, w * 0.06, h * 0.07, 0, 2 * pi, 24L)
      m <- seq(0, 1, length.out = 24L)
      my <- h * 0.62 + (h * 0.12 + h * (a("adj", 4653) / 100000 - 0.0465) * 2) * (1 - (2 * m - 1)^2)
      list(
        poly(f$x, f$y), poly(e1$x, e1$y, shade = -0.2), poly(e2$x, e2$y, shade = -0.2),
        poly(w * (0.25 + 0.5 * m), my, closed = FALSE, nofill = TRUE)
      )
    },
    noSmoking = {
      ri <- 1 - 2 * a("adj", 18750) / 100000 * ss / max(w, h)
      bh <- 0.2
      d <- c(cos(pi / 4), sin(pi / 4))
      nv <- c(-d[2L], d[1L])
      sector <- function(sgn) {
        so <- sqrt(1 - bh^2)
        si <- sqrt(max(ri^2 - bh^2, 0))
        p1 <- so * d * sgn + bh * nv * -sgn
        a1 <- atan2(p1[2L], p1[1L])
        a2 <- if (sgn > 0) atan2(-so * d[2L] + bh * nv[2L], -so * d[1L] + bh * nv[1L]) else
          atan2(so * d[2L] - bh * nv[2L], so * d[1L] - bh * nv[1L])
        if (a2 < a1) a2 <- a2 + 2 * pi
        pa <- seq(a1, a2, length.out = 32L)
        q1 <- si * d * sgn + bh * nv * -sgn
        b1 <- atan2(q1[2L], q1[1L])
        q2 <- if (sgn > 0) -si * d + bh * nv else si * d - bh * nv
        b2 <- atan2(q2[2L], q2[1L])
        if (b2 < b1) b2 <- b2 + 2 * pi
        pb <- seq(b2, b1, length.out = 32L)
        list(x = c(cos(pa), ri * cos(pb)), y = c(sin(pa), ri * sin(pb)))
      }
      s1 <- sector(1)
      s2 <- sector(-1)
      si <- sqrt(max(ri^2 - bh^2, 0))
      bx <- c(si * d[1L] + bh * nv[1L], si * d[1L] - bh * nv[1L], -si * d[1L] - bh * nv[1L], -si * d[1L] + bh * nv[1L])
      by <- c(si * d[2L] + bh * nv[2L], si * d[2L] - bh * nv[2L], -si * d[2L] - bh * nv[2L], -si * d[2L] + bh * nv[2L])
      tr <- function(p) poly(w / 2 + p$x * w / 2, h / 2 + p$y * h / 2)
      list(tr(s1), tr(s2), tr(list(x = bx, y = by)))
    },
    sun = {
      cx <- w / 2
      cy <- h / 2
      rb <- 0.3
      rings <- list()
      b <- arc(cx, cy, w * rb, h * rb, 0, 2 * pi)
      rings[[1L]] <- poly(b$x, b$y)
      th <- 12 * pi / 180
      for (k in 0:7) {
        ang <- k * pi / 4
        rr <- rb / cos(th)
        rings[[k + 2L]] <- poly(
          cx + c(rr * cos(ang - th) * w, 0.5 * w * cos(ang), rr * cos(ang + th) * w),
          cy + c(rr * sin(ang - th) * h, 0.5 * h * sin(ang), rr * sin(ang + th) * h)
        )
      }
      rings
    },
    moon = {
      o <- list(x = w + w * cos(seq(-pi / 2, -3 * pi / 2, length.out = 40L)),
                y = h / 2 + h / 2 * sin(seq(-pi / 2, -3 * pi / 2, length.out = 40L)))
      rin <- w * (1 - a("adj", 50000) / 100000)
      i <- list(x = w + rin * cos(seq(-3 * pi / 2, -pi / 2, length.out = 40L)),
                y = h / 2 + h / 2 * sin(seq(-3 * pi / 2, -pi / 2, length.out = 40L)))
      list(poly(c(o$x, i$x), c(o$y, i$y)))
    },
    heart = {
      t <- seq(0, 2 * pi, length.out = 90L)
      f <- fit(16 * sin(t)^3, -(13 * cos(t) - 5 * cos(2 * t) - 2 * cos(3 * t) - cos(4 * t)))
      list(poly(f$x, f$y))
    },
    lightningBolt = {
      px <- c(8472, 12860, 11050, 16577, 14767, 21600, 10012, 12222, 5022, 7602, 0) / 21600 * w
      py <- c(0, 6080, 6797, 12007, 12877, 21600, 14915, 13987, 9705, 8382, 3890) / 21600 * h
      list(poly(px, py))
    },
    teardrop = {
      ad <- a("adj", 100000) / 100000
      u <- arc(w / 2, h / 2, w / 2, h / 2, 0, 3 * pi / 2, 48L)
      list(poly(c(u$x, w / 2 + w / 2 * ad), c(u$y, h / 2 - h / 2 * ad)))
    },
    pie = {
      a1 <- deg(a("adj1", 0))
      a2 <- deg(a("adj2", 16200000))
      if (a2 <= a1) a2 <- a2 + 2 * pi
      u <- arc(w / 2, h / 2, w / 2, h / 2, a1, a2, 48L)
      list(poly(c(w / 2, u$x), c(h / 2, u$y)))
    },
    chord = {
      a1 <- deg(a("adj1", 2700000))
      a2 <- deg(a("adj2", 16200000))
      if (a2 <= a1) a2 <- a2 + 2 * pi
      u <- arc(w / 2, h / 2, w / 2, h / 2, a1, a2, 48L)
      list(poly(u$x, u$y))
    },
    arc = {
      a1 <- deg(a("adj1", 16200000))
      a2 <- deg(a("adj2", 0))
      if (a2 <= a1) a2 <- a2 + 2 * pi
      u <- arc(w / 2, h / 2, w / 2, h / 2, a1, a2, 48L)
      list(poly(c(w / 2, u$x), c(h / 2, u$y), nostroke = TRUE),
           poly(u$x, u$y, closed = FALSE, nofill = TRUE))
    },
    blockArc = {
      a1 <- deg(a("adj1", 10800000))
      a2 <- deg(a("adj2", 0))
      d <- ss * a("adj3", 25000) / 100000
      if (a2 <= a1) a2 <- a2 + 2 * pi
      o <- arc(w / 2, h / 2, w / 2, h / 2, a1, a2, 48L)
      i <- arc(w / 2, h / 2, w / 2 - d, h / 2 - d, a2, a1, 48L)
      list(poly(c(o$x, i$x), c(o$y, i$y)))
    },
    wedgeRectCallout = list(rrect(0, wedge_for())),
    wedgeRoundRectCallout = list(rrect(ss * a("adj3", 16667) / 100000, wedge_for())),
    cloud = , cloudCallout = {
      t <- seq(0, 2 * pi, length.out = 160L)
      r <- 0.86 + 0.14 * abs(sin(4 * t))
      c0 <- poly(w / 2 + r * cos(t) * w / 2, h / 2 + r * sin(t) * h / 2)
      if (prst == "cloud") {
        list(c0)
      } else {
        tx <- w / 2 + w * a("adj1", -20833) / 100000
        ty <- h / 2 + h * a("adj2", 62500) / 100000
        k <- c(0.55, 0.78, 0.94)
        sz <- c(0.07, 0.045, 0.03)
        rings <- list(c0)
        for (j in 1:3) {
          bx <- tx + (w / 2 - tx) * k[j]
          by <- ty + (h / 2 - ty) * k[j]
          cc <- arc(bx, by, w * sz[j], w * sz[j], 0, 2 * pi, 24L)
          rings[[j + 1L]] <- poly(cc$x, cc$y)
        }
        rings
      }
    },
    ribbon = list(poly(
      c(0, 0.2, 0.2, 0.8, 0.8, 1, 0.9, 1, 0.8, 0.8, 0.2, 0.2, 0, 0.1) * w,
      c(0.15, 0.15, 0, 0, 0.15, 0.15, 0.5, 0.85, 0.85, 0.7, 0.7, 0.85, 0.85, 0.5) * h
    )),
    wave = , doubleWave = {
      u <- seq(0, 1, length.out = 60L)
      amp <- h * a("adj1", 12500) / 100000
      k <- if (prst == "wave") 1 else 2
      s <- sin(2 * pi * k * u)
      list(poly(c(u * w, rev(u) * w), c(amp - amp * s, h - amp - rev(amp * s))))
    },
    gear6 = {
      t <- seq(0, 2 * pi, length.out = 360L)
      ph <- (t * 6 / (2 * pi) + 0.5) %% 1
      r <- ifelse(ph < 0.12, 0.8 + (0.12 - ph) / 0.12 * 0.0 + ph / 0.12 * 0.2,
        ifelse(ph < 0.38, 1, ifelse(ph < 0.5, 1 - (ph - 0.38) / 0.12 * 0.2, 0.8)))
      list(poly(w / 2 + r * cos(t) * w / 2, h / 2 + r * sin(t) * h / 2))
    },
    funnel = {
      top <- arc(w / 2, h * 0.07, w / 2, h * 0.07, 0, 2 * pi, 40L)
      bl <- arc(w / 2, h * 0.92, w * 0.08, h * 0.05, 0, pi, 20L)
      list(
        poly(c(0, w * 0.42, bl$x, w * 0.58, w), c(h * 0.07, h * 0.9, bl$y, h * 0.9, h * 0.07)),
        poly(top$x, top$y, nofill = TRUE)
      )
    },
    irregularSeal1 = {
      px <- c(10800, 14522, 14155, 18380, 16702, 21097, 17607, 21600, 16837, 18145, 14020, 13247, 10532, 8485,
              7715, 4762, 5667, 135, 3722, 0, 5627, 2450, 8625) / 21600 * w
      py <- c(5800, 0, 5325, 4457, 7315, 8137, 10475, 13290, 12942, 18095, 14457, 19737, 14935, 21600,
              15627, 17617, 13937, 14587, 11775, 8615, 7728, 3777, 5557) / 21600 * h
      list(poly(px, py))
    },
    verticalScroll = {
      r <- h * 0.08
      list(
        rect(w * 0.08, r, w * 0.92, h - r),
        rect(0, 0, w, 2 * r),
        rect(0, h - 2 * r, w, h)
      )
    },
    bracketPair = {
      r <- ss * a("adj", 16667) / 100000
      tl <- arc(r, r, r, r, 3 * pi / 2, pi, 9L)
      bl <- arc(r, h - r, r, r, pi, pi / 2, 9L)
      tr <- arc(w - r, r, r, r, 3 * pi / 2, 2 * pi, 9L)
      br <- arc(w - r, h - r, r, r, 0, pi / 2, 9L)
      body <- ooxml_preset("roundRect", w, h, c(adj = a("adj", 16667)))[[1L]]
      body$nostroke <- TRUE
      list(
        body,
        poly(c(tl$x, bl$x), c(tl$y, bl$y), closed = FALSE, nofill = TRUE),
        poly(c(tr$x, br$x), c(tr$y, br$y), closed = FALSE, nofill = TRUE)
      )
    },
    bracePair = {
      r <- ss * a("adj", 8333) / 100000
      lb <- function(side) {
        s <- if (side == "l") 1 else -1
        x0 <- if (side == "l") 0 else w
        q <- seq(0, 1, length.out = 8L)
        cur <- function(xa, ya, xc, yc, xb, yb) {
          list(x = (1 - q)^2 * xa + 2 * (1 - q) * q * xc + q^2 * xb, y = (1 - q)^2 * ya + 2 * (1 - q) * q * yc + q^2 * yb)
        }
        c1 <- cur(x0 + s * 2 * r, 0, x0 + s * r, 0, x0 + s * r, r)
        c2 <- cur(x0 + s * r, h / 2 - r, x0 + s * r, h / 2, x0, h / 2)
        c3 <- cur(x0, h / 2, x0 + s * r, h / 2, x0 + s * r, h / 2 + r)
        c4 <- cur(x0 + s * r, h - r, x0 + s * r, h, x0 + s * 2 * r, h)
        poly(c(c1$x, c2$x, c3$x, c4$x), c(c1$y, c2$y, c3$y, c4$y), closed = FALSE)
      }
      body <- ooxml_preset("roundRect", w, h, c(adj = a("adj", 8333)))[[1L]]
      body$nostroke <- TRUE
      l1 <- lb("l")
      l2 <- lb("r")
      l1$nofill <- TRUE
      l2$nofill <- TRUE
      list(body, l1, l2)
    },
    actionButtonHome = {
      list(
        rect(0, 0, w, h),
        poly(
          c(0.5, 0.68, 0.64, 0.64, 0.54, 0.54, 0.46, 0.46, 0.36, 0.36, 0.32) * w,
          c(0.3, 0.5, 0.5, 0.78, 0.78, 0.62, 0.62, 0.78, 0.78, 0.5, 0.5) * h
        )
      )
    },
    NULL
  )
}

ooxml_textrect <- function(prst, w, h, adj) {
  ss <- min(w, h)
  a <- function(nm, def) {
    v <- if (nm %in% names(adj)) adj[[nm]] else NA_real_
    if (is.na(v)) def else v
  }
  inset <- function(i, j = i) c(i, j, w - i, h - j)
  frac <- function(l, t, r, b) c(l * w, t * h, r * w, b * h)
  ell <- 0.14645
  switch(prst,
    ellipse = , smileyFace = , teardrop = , decagon = , donut = , noSmoking = , blockArc = frac(ell, ell, 1 - ell, 1 - ell),
    roundRect = , wedgeRoundRectCallout = inset(ss * a(if (prst == "roundRect") "adj" else "adj3", 16667) / 100000 * 0.29289),
    plaque = inset(ss * a("adj", 16667) / 100000 * 0.29289),
    diamond = , flowChartDecision = frac(0.25, 0.25, 0.75, 0.75),
    triangle = {
      ad <- a("adj", 50000) / 100000
      c(w * ad / 2, h / 2, w / 2 + w * ad / 2, h)
    },
    rtTriangle = frac(1 / 12, 0.5, 7 / 12, 1),
    parallelogram = {
      d <- ss * a("adj", 25000) / 100000
      c(d * 0.5, 0, w - d * 0.5, h)
    },
    trapezoid = {
      x2 <- ss * a("adj", 25000) / 100000
      c(x2 / 3, h / 3, w - x2 / 3, h)
    },
    hexagon = c(ss * a("adj", 25000) / 100000 * 0.5, h * 0.12, w - ss * a("adj", 25000) / 100000 * 0.5, h * 0.88),
    octagon = inset(ss * a("adj", 29289) / 100000 * 0.5),
    pentagon = frac(0.19, 0.2, 0.81, 0.81),
    heptagon = frac(0.16, 0.18, 0.84, 0.84),
    star4 = frac(0.3, 0.3, 0.7, 0.7),
    star5 = frac(0.2, 0.25, 0.8, 0.7),
    star6 = , star7 = , star8 = , star10 = , star12 = , star16 = , star24 = , star32 = {
      n <- as.numeric(sub("star", "", prst))
      half <- 0.5 * (if (n == 6) 0.577 else if (n == 7) 0.69 else if (n == 10) 0.85 else 0.75) * 0.7071
      frac(0.5 - half, 0.5 - half, 0.5 + half, 0.5 + half)
    },
    rightArrow = {
      a1 <- a("adj1", 50000) / 100000
      c(0, h / 2 - h * a1 / 2, w - ss * a("adj2", 50000) / 200000, h / 2 + h * a1 / 2)
    },
    leftArrow = {
      a1 <- a("adj1", 50000) / 100000
      c(ss * a("adj2", 50000) / 200000, h / 2 - h * a1 / 2, w, h / 2 + h * a1 / 2)
    },
    downArrow = {
      a1 <- a("adj1", 50000) / 100000
      c(w / 2 - w * a1 / 2, 0, w / 2 + w * a1 / 2, h - ss * a("adj2", 50000) / 200000)
    },
    upArrow = {
      a1 <- a("adj1", 50000) / 100000
      c(w / 2 - w * a1 / 2, ss * a("adj2", 50000) / 200000, w / 2 + w * a1 / 2, h)
    },
    leftRightArrow = {
      a1 <- a("adj1", 50000) / 100000
      x2 <- ss * a("adj2", 50000) / 200000
      c(x2, h / 2 - h * a1 / 2, w - x2, h / 2 + h * a1 / 2)
    },
    upDownArrow = {
      a1 <- a("adj1", 50000) / 100000
      y2 <- ss * a("adj2", 50000) / 200000
      c(w / 2 - w * a1 / 2, y2, w / 2 + w * a1 / 2, h - y2)
    },
    notchedRightArrow = {
      a1 <- a("adj1", 50000) / 100000
      c(h * a1 / 2, h / 2 - h * a1 / 2, w - ss * a("adj2", 50000) / 200000, h / 2 + h * a1 / 2)
    },
    homePlate = c(0, 0, w - ss * a("adj", 50000) / 200000, h),
    chevron = c(ss * a("adj", 50000) / 100000, 0, w - ss * a("adj", 50000) / 100000, h),
    cloud = , cloudCallout = frac(3163 / 21600, 3163 / 21600, 17337 / 21600, 16033 / 21600),
    plus = inset(ss * a("adj", 25000) / 100000),
    frame = inset(ss * a("adj1", 12500) / 100000),
    bevel = inset(ss * a("adj", 12500) / 100000),
    cube = c(0, ss * a("adj", 25000) / 100000, w - ss * a("adj", 25000) / 100000, h),
    can = c(0, ss * a("adj", 25000) / 100000, w, h - ss * a("adj", 25000) / 200000),
    foldedCorner = c(0, 0, w, h - ss * a("adj", 16667) / 100000),
    flowChartTerminator = frac(1018 / 21600, 3163 / 21600, 20582 / 21600, 18437 / 21600),
    flowChartInputOutput = frac(0.2, 0, 0.8, 1),
    flowChartDocument = frac(0, 0, 1, 17322 / 21600),
    heart = frac(0.22, 0.18, 0.78, 0.7),
    sun = frac(0.25, 0.25, 0.75, 0.75),
    irregularSeal1 = frac(4680 / 21600, 6182 / 21600, 16110 / 21600, 16200 / 21600),
    verticalScroll = frac(0.1, 0.16, 0.9, 0.84),
    ribbon = frac(0.2, 0, 0.8, 0.7),
    wave = , doubleWave = c(w * 0.1, 2 * h * a("adj1", 12500) / 100000, w * 0.9, h - 2 * h * a("adj1", 12500) / 100000),
    c(0, 0, w, h)
  )
}
