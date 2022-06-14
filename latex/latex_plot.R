nglue<-function(
    ...,
    .sep = "",
    .envir = parent.frame(), 
    .open = "{", 
    .close = "}", 
    .na = "NA", 
    .transformer = identity_transformer
){
  as.character(glue(
    ...,
    .sep=.sep,
    .envir=.envir,
    .open="@[",
    .close="]@",
    .na="NA",
    .transformer = .transformer
  ))
}


make_full_directory<-function(x){
  os_type<-.Platform$OS.type
  if(os_type=="windows")return(paste0(gsub("\\\\","/",normalizePath(getwd())),"/",x))
  paste0(normalizePath(getwd()),"\\",x)
}

make_pdf_page<-function(
    fn,
    pages=1,
    height="10cm",
    width="10cm",
    minipage=TRUE,
    valign="B"
){
  fname<-gsub("\\","/",tempfile(pattern = "pdf",tmpdir=make_full_directory("figure"),fileext=".pdf"),fixed=TRUE)
  pdf_subset(fn, pages = pages, output = fname)
  latex<-nglue(
    if(minipage){"\\begin{minipage}[t][@[height]@][t]{@[width]@}\n"}else(""),
    "\\includegraphics[height=@[height]@,width=@[width]@,valign=@[valign]@]{@[fname]@}\n",
    if(minipage){"\\end{minipage}\n"}else{""},
    .sep="\n",
    height=height,
    width=width,
    fname=fname
  )
  latex
}


make_plot<-function(
    plot_expr,
    height="2cm",
    width="3cm",
    envir=parent.frame(),
    minipage=TRUE,
    valign="B",
    dpi=7
){
  fname<-gsub("\\","/",tempfile(pattern = "plot",tmpdir=make_full_directory("figure"),fileext=".pdf"),fixed=TRUE)
  pdf(file=fname,width=dpi,height=dpi)
  eval(plot_expr,envir=envir)
  invisible(dev.off())
  latex<-nglue(
    if(minipage){"\\begin{minipage}[t][@[height]@][t]{@[width]@}\n"}else(""),
    "\\includegraphics[height=@[height]@,width=@[width]@,valign=@[valign]@]{@[fname]@}\n",
    if(minipage){"\\end{minipage}\n"}else{""},
    .sep="\n",
    height=height,
    width=width,
    fname=fname
  )
  latex
}

make_jpeg_plot<-function(
    plot_expr,
    height="2cm",
    width="3cm",
    envir=parent.frame(),
    minipage=TRUE,
    valign="B",
    pixels=2048
){
  fname<-gsub("\\","/",tempfile(pattern = "plot",tmpdir=make_full_directory("figure"),fileext=".jpeg"),fixed=TRUE)
  jpeg(file=fname,width=pixels,height=pixels)
  eval(plot_expr,envir=envir)
  invisible(dev.off())
  latex<-nglue(
    if(minipage){"\\begin{minipage}[t][@[height]@][t]{@[width]@}\n"}else(""),
    "\\includegraphics[height=@[height]@,width=@[width]@,valign=@[valign]@]{@[fname]@}\n",
    if(minipage){"\\end{minipage}\n"}else{""},
    .sep="\n",
    height=height,
    width=width,
    fname=fname
  )
  latex
}

make_png_plot<-function(
    plot_expr,
    height="2cm",
    width="3cm",
    envir=parent.frame(),
    minipage=TRUE,
    valign="B",
    pixels=2048
){
  fname<-gsub("\\","/",tempfile(pattern = "plot",tmpdir=make_full_directory("figure"),fileext=".png"),fixed=TRUE)
  png(file=fname,width=pixels,height=pixels)
  eval(plot_expr,envir=envir)
  invisible(dev.off())
  latex<-nglue(
    if(minipage){"\\begin{minipage}[t][@[height]@][t]{@[width]@}\n"}else(""),
    "\\includegraphics[height=@[height]@,width=@[width]@,valign=@[valign]@]{@[fname]@}\n",
    if(minipage){"\\end{minipage}\n"}else{""},
    .sep="\n",
    height=height,
    width=width,
    fname=fname
  )
  latex
}


make_pdf_page<-function(
    fn,
    pages=1,
    height="10cm",
    width="10cm",
    minipage=TRUE,
    valign="B"
){
  fname<-gsub("\\","/",tempfile(pattern = "pdf",tmpdir=make_full_directory("figure"),fileext=".pdf"),fixed=TRUE)
  pdf_subset(fn, pages = pages, output = fname)
  latex<-nglue(
    if(minipage){"\\begin{minipage}[t][@[height]@][t]{@[width]@}\n"}else(""),
    "\\includegraphics[height=@[height]@,width=@[width]@,valign=@[valign]@]{@[fname]@}\n",
    if(minipage){"\\end{minipage}\n"}else{""},
    .sep="\n",
    height=height,
    width=width,
    fname=fname
  )
  latex
}



