###########################################
# project: .r file of custom functions
# author: vvchoo
# date: feb 2021
###########################################

# name exports with dates
dater<-function(y,x){
  if(missing(y)){
    paste0("./figures/",tolower(format(Sys.Date(),"%b-%d")),"/",x)
  } else {
    paste0("./figures/",y,"/",tolower(format(Sys.Date(),"%b-%d")),"/",x)
  }
}

# need to have avenir lt 65 medium installed
windowsFonts(avenir=windowsFont("Avenir LT 65 Medium"))

# opposite of %in%
`%!in%`<-Negate(`%in%`)

# collapse NAs in rows
coalesce_by_column<-function(df) {
  return(dplyr::coalesce(!!!as.list(df)))
}

# reorder three ways in ggplot
reorder_within<-function(x,by,within,fun=mean,sep="_",...){
  new_x<-paste(x,within,sep=sep)
  stats::reorder(new_x,by,FUN=fun)
}

# scale for reordered plots
scale_x_reordered<-function(...,sep="_"){
  reg<-paste0(sep,".+$")
  ggplot2::scale_x_discrete(labels=function(x) gsub(reg,"",x),...)
}

# aiddata theme taken from matt
theme_aiddata<-function(base_size=12,base_family=""){ 
  theme(panel.background=element_rect(fill="white",colour="white",size=0.5,linetype="solid"),
        panel.grid.major=element_line(size=0.5,linetype='solid',colour="#F3F4F4"), 
        panel.grid.minor=element_line(size=0.25,linetype='solid',colour="#F3F4F4"),
        text=element_text(family="avenir",size=11),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank())
}

