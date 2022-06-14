require(Hmisc)
require(glue)
require(scales)
require(highr)

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

latex_verbatim<-function(txt,sep="!")
{
  nglue("{\\tt \\verb@[sep]@@[txt]@@[sep]@}")
}

color_ramp<-function(x,from="white",to="red",p=0.5)
{
  cm<-colorRamp(c(from,to),alpha=TRUE)(rescale(x)^p)
  apply(cm,1,function(color_bytes)do.call(rgb,as.list(color_bytes/255)))
}

signed_ramp <- function(x,from="white",low="red",hi="green",p=0.5)
{
  gcr<-color_ramp(abs(x),from,hi,p)
  rcr<-color_ramp(abs(x),from,low,p)
  case_when(
    x>(1e-10)~gcr,
    x<(-1e-10)~rcr,
    TRUE~"#FFFFFFFF"
  )
}
cell_color<-function(t,c,model="[HTML]")
{
  paste0("\\cellcolor",model,"{",c,"}",t)
}

dot<-function(col,size="0.5ex",raise="0.2ex"){
  paste0(
    "\\raisebox{",raise,"}{\\tikz \\fill[",col,"] (0,0) circle (",size,");}"
  )
}

latex_verbatim<-function(txt,sep="!")
{
  nglue("{\\tt \\verb@[sep]@@[txt]@@[sep]@}")
}


latex_fun<-function(fun)
{
  hi_latex(str_c(
    as_label(enquo(fun)),
    " <- ",
    str_flatten(capture.output(print(fun,useSource=TRUE)),collapse="\n")
  )) %>%
    str_flatten(collapse ="\n") %>%
    {str_c(
      #"\\begin{kframe}\n",
      "\\begin{alltt}\n",
      .,
      "\\end{alltt}\n"
      #"\\end{kframe}\n"
    )}
}

make_spark<-function(x,width=20){
  if(length(x)<2)return("")
  sx<-round(rescale(seq_along(x)),digits=4)
  sy<-round(rescale(x),digits=4)
  imax<-which.max(sy)
  imin<-which.min(sy)
  res<-paste0(
    "\\setlength{\\sparkdotwidth}{2pt}",
    "\\setlength{\\sparklinethickness}{0.75pt}",
    "\\definecolor{sparklinecolor}{rgb}{0.2,0.2,1}",
    "\\renewcommand{\\sparklineheight}{1.75}",
    "\\begin{sparkline}{",width,"}",
    "\\sparkdot ",sx[imax]," ",sy[imax]," green!50 ",
    "\\sparkdot ",sx[imin]," ",sy[imin]," red!50 ",
    "\\spark ",paste(paste(sx,sy),collapse=" ")," / ",
    "\\end{sparkline}"
  )
  res
}


latex_table<-function(
    df,
    max_width="18cm",
    max_height="18cm",
    min_width="",
    min_height="",
    aln="l",
    haln="l",
    center=TRUE,
    tbl_names=FALSE,
    hdr_format="old",
    show_headers=TRUE,
    topline=TRUE,
    bottomline=TRUE,
    valign="B",
    title=NULL,
    aftertitle="\\hline",
    titleformat="|c|",
    frame=FALSE,
    rename_fun=identity,
    pre_line=rep("",nrow(df)),
    post_line=rep("",nrow(df))
)
{
  
  ltxtbl<-function(x,align,sep=" \\\\ ")nglue(
    "\\begin{tabular}[b]{@[align]@} ",
    "@[paste(x,collapse=sep)]@",
    "\\\\ ",
    "\\end{tabular} "
  )
  
  headers<-switch(
    hdr_format,
    asis=names(df),
    tbl=map2_chr(strsplit(rename_fun(names(df)),"_"),rep(aln,length.out=ncol(df)),ltxtbl),
    rawrot=nglue("\\multicolumn{1}{c}{\\rotatebox{90}{@[names(df)]@}}"),
    rot=nglue("\\multicolumn{1}{c}{\\rotatebox{90}{@[latexTranslate(rename_fun(names(df)))]@}}"),
    rawfunrot=nglue("\\multicolumn{1}{c}{\\rotatebox{90}{@[rename_fun(names(df))]@}}"),
    latexTranslate(rename_fun(names(df)))
  )
  header_line<-paste(headers,collapse=' & ')
  df_rows<-pmap_chr(df,paste,sep=' & ')
  tbl_rows<-str_c(pre_line,df_rows," \\\\ ",post_line," \n")
  table_block<-str_c(str_flatten(tbl_rows,collapse="")," \\\\")
  alns<-rep(aln,length.out=ncol(df))
  nglue(
    if(center)"\\begin{center}"else"",
    "\\begin{adjustbox}{",
    if(frame){"fbox=1pt 1pt 1pt,"}else{""},
    "tabular=@[str_flatten(alns)]@,",
    "valign=@[valign]@,",
    if(nchar(min_width)>0){"min width=@[min_width]@,"}else{""},
    if(nchar(min_height)>0){"min height=@[min_height]@,"}else{""},  
    "max width=@[max_width]@,",
    "max height=@[max_height]@",
    "}",
    #"\\\\ ",
    if(show_headers&topline){"\\\\ \\hline"}else{""},
    if(!is.null(title)){"\\multicolumn{@[ncol(df)]@}{@[titleformat]@}{@[title]@} \\\\ @[aftertitle]@ "}else{""},
    if(show_headers){"@[header_line]@ \\\\ \\hline "}else{""},
    table_block,
    if(bottomline){"\\hline"}else{""},
    "\\end{adjustbox}",
    if(center)"\\end{center}"else"",
    .sep=" "
  )
} 
