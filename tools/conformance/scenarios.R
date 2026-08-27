# Conformance battery: every device callback / GE feature, then packages.
# Each entry: name, code (function), w, h, expected_diff (renderer can't
# reproduce this by design - masks, clip paths, raster interpolation).
scenarios <- list(
  # -- device primitives, base graphics --
  list("prim_lines_legend", function() { plot(1:10,(1:10)^2,type="b",pch=19,main="T",xlab="x",ylab="y"); lines(1:10,100-(1:10)^2,lty=2,col=2,lwd=2); legend("top",c("a","b"),lty=1:2,col=1:2) }, 6, 4),
  list("prim_lty_zoo", function() { plot.new(); plot.window(c(0,1),c(0,8)); for (i in 0:6) abline(h=i, lty=i, lwd=2); abline(h=7, lty="1348", lwd=3) }, 5, 3),
  list("prim_join_cap", function() { plot.new(); plot.window(c(0,10),c(0,10)); lines(c(1,3,1),c(1,5,9),lwd=15,ljoin="mitre",lend="butt"); lines(c(5,7,5),c(1,5,9),lwd=15,ljoin="round",lend="round"); lines(c(8,9.5,8),c(1,5,9),lwd=15,ljoin="bevel",lend="square") }, 5, 4),
  list("prim_pch_zoo", function() { plot(rep(1:5,5), rep(1:5,each=5), pch=0:24, cex=2, col="navy", bg="gold") }, 5, 5),
  list("prim_rect_poly_alpha", function() { plot.new(); plot.window(c(0,1),c(0,1)); rect(.05,.05,.6,.6,col=adjustcolor("red",.5),border="darkred",lwd=3); polygon(c(.3,.9,.6),c(.3,.3,.9),col=adjustcolor("blue",.5),border=1) }, 4, 4),
  list("prim_circles_clip", function() { plot(c(0,1),c(0,1),type="n",xaxs="i",yaxs="i"); symbols(c(0,1,.5),c(0,1,.5),circles=c(.3,.3,.2),inches=FALSE,add=TRUE,bg=adjustcolor("skyblue",.8),fg="navy") }, 4, 4),
  list("prim_text_adj_rot", function() { plot.new(); plot.window(c(0,1),c(0,1)); text(.5,.9,"center",adj=.5); text(.5,.8,"left",adj=0); text(.5,.7,"right",adj=1); text(.2,.4,"rot45",srt=45); text(.8,.4,"rot90",srt=90); text(.5,.2,"big",cex=2); text(.5,.05,"bold-it",font=4) }, 4, 4),
  list("prim_plotmath_unicode", function() { plot.new(); plot.window(c(0,1),c(0,1)); text(.5,.8,expression(hat(beta)==frac(alpha^2,sqrt(x[i])))); text(.5,.4,"Ünïcødé 中文 ±µΩ") }, 4, 3),
  list("prim_polypath_winding", function() { plot.new(); plot.window(c(0,1),c(0,1)); polypath(c(.1,.9,.9,.1,NA,.3,.7,.7,.3),c(.1,.1,.9,.9,NA,.3,.3,.7,.7),col="gold",rule="evenodd",border="navy"); polypath(c(.4,.6,.6,.4)+.3,c(.4,.4,.6,.6),col=2,rule="winding") }, 4, 4),
  list("prim_raster", function() { plot(c(0,10),c(0,10),type="n"); image <- as.raster(matrix(hcl.colors(64),8)); rasterImage(image,1,1,4,4); rasterImage(image,5,5,8,8,angle=25); rasterImage(as.raster(matrix(c("#FF000080","#0000FF80"),1)),2,6,4,9) }, 5, 4),
  list("prim_image_contour", function() { image(volcano, col=hcl.colors(20)); contour(volcano, add=TRUE) }, 5, 4),
  list("prim_persp", function() { persp(volcano, theta=30, phi=25, col="lightblue", border="grey30") }, 5, 4),
  list("prim_mfrow_bg_clip", function() { par(mfrow=c(1,2), bg="ivory"); plot(1:5); plot(sin, -pi, pi); clip(-2,2,-.5,.5); abline(h=0, lwd=4, col=2) }, 6, 3),
  list("prim_hist_box_pie", function() { par(mfrow=c(1,3)); hist(mtcars$mpg,col="steelblue"); boxplot(mpg~cyl,mtcars,col=2:4); pie(c(3,2,5),col=hcl.colors(3)) }, 7, 3),
  # -- grid / GE features --
  list("grid_gradients", function() { grid::grid.rect(gp=grid::gpar(fill=grid::linearGradient(c("steelblue","white")))); grid::grid.circle(r=.25, gp=grid::gpar(fill=grid::radialGradient(c("gold","darkred")))) }, 4, 3),
  list("grid_path_holes", function() { grid::grid.path(x=c(.1,.9,.9,.1,.35,.65,.65,.35), y=c(.1,.1,.9,.9,.35,.35,.65,.65), id=rep(1:2,each=4), rule="evenodd", gp=grid::gpar(fill="seagreen", col="black", lwd=2)) }, 4, 4),
  list("grid_clippath", function() { grid::pushViewport(grid::viewport(clip=grid::circleGrob(r=.3))); grid::grid.rect(gp=grid::gpar(fill="red")); grid::popViewport() }, 3, 3),
  list("grid_mask_hard", function() { grid::pushViewport(grid::viewport(mask=grid::as.mask(grid::circleGrob(r=.3, gp=grid::gpar(fill="white", col=NA)), type="luminance"))); grid::grid.rect(gp=grid::gpar(fill="red")); grid::popViewport() }, 3, 3),
  list("grid_mask_soft", function() { suppressWarnings({grid::pushViewport(grid::viewport(mask=grid::as.mask(grid::circleGrob(r=.3, gp=grid::gpar(fill=grDevices::adjustcolor("white", .5), col=NA))))); grid::grid.rect(gp=grid::gpar(fill="red")); grid::popViewport()}) }, 3, 3, TRUE),
  list("grid_rot_text_just", function() { grid::grid.text("hjust-l", x=.5, just="left"); grid::grid.text("rot-30", y=.3, rot=30); grid::grid.text("multi\nline", y=.7) }, 4, 3),
  # -- packages --
  list("pkg_lattice_xyplot", function() print(lattice::xyplot(Sepal.Length~Petal.Length|Species, iris, pch=19, col.symbol="steelblue")), 6, 4),
  list("pkg_lattice_levelplot", function() print(lattice::levelplot(volcano, col.regions=hcl.colors(50))), 5, 4),
  list("pkg_ggplot_density", function() print(ggplot2::ggplot(mtcars, ggplot2::aes(mpg, fill=factor(gear)))+ggplot2::geom_density(alpha=.5)+ggplot2::ggtitle("d")), 6, 4),
  list("pkg_ggplot_facet_raster", function() print(ggplot2::ggplot(ggplot2::faithfuld, ggplot2::aes(waiting, eruptions, fill=density))+ggplot2::geom_raster()+ggplot2::facet_wrap(~eruptions>3)), 6, 4),
  list("pkg_ggplot_polar", function() print(ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), fill=factor(cyl)))+ggplot2::geom_bar()+ggplot2::coord_polar()), 4, 4),
  list("pkg_tinyplot_facet", function() { tinyplot::tinytheme("clean2"); tinyplot::plt(Sepal.Length~Petal.Length|Species, data=iris, facet="by", pch=19) }, 6, 4),
  list("pkg_survival", function() { fit <- survival::survfit(survival::Surv(time, status)~x, survival::aml); plot(fit, col=1:2, conf.int=TRUE, mark.time=TRUE) }, 5, 4),
  list("pkg_plotrix", function() { plotrix::pie3D(c(3,2,5), explode=.1); }, 4, 4),
  list("pkg_corrplot", function() corrplot::corrplot(cor(mtcars[,1:6]), method="ellipse"), 5, 5),
  list("pkg_sf_map", function() { nc <- sf::st_read(system.file("shape/nc.shp", package="sf"), quiet=TRUE); plot(nc["AREA"], main="nc") }, 6, 4)
)
