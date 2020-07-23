#library(ez)
#library(tidyverse)
#library(afex)
#library(emmeans)
# apparently, I should never use library in a package
# but instead, devtools to add imports/depends to the description, and/or requireNamespace?!
# http://r-pkgs.had.co.nz/namespace.html#namespace

datex.create_data <- function()
{
  #' A Cat Function
  #'
  #' This function allows you to express your love of cats.
  #' @param love Do you love cats? Defaults to TRUE.
  #' @keywords cats
  #' @export
  #' @examples
  #' cat_function()
  datex.data.indep <- data.frame(id=1:1000,dv=sample(100,1000,replace=T),uv1=c(rep("a",500),rep("b",500)),uv2=rep(c(rep("c",125),rep("d",125),rep("e",125),rep("f",125)),2))
  datex.data.indep[sample(1000,10),"dv"] <- sample(100:500,10)
  #ezDesign(datex.data.indep,x=uv1,y=uv2)
  datex.data.indep
}

datex.center <- function(x)
{
  x<- x-median(x,na.rm=T)
  x
}

datex.is_outlier <- function(x) {
  return(x < quantile(x, 0.25,na.rm=T) - 1.5 * IQR(x,na.rm=T) | x > quantile(x, 0.75,na.rm=T) + 1.5 * IQR(x,na.rm=T))
}

datex.histo <- function(dat,dv,uv,bins,level)
{
  if (!missing(level)) {dat <- dat[dat[,uv]==level,]}
  p <- ggplot(dat,aes(x=!!sym(dv))) +
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   bins=bins,
                   colour="black", fill="gray") +
    geom_density(alpha=.2, fill="blue") #+
  #facet_grid(rows="Task")
  p
}

datex.boxplot <- function(dat,id,dv,uv,ylab,cloud=T,center=T)
{
  if (center)
  {
    dat <- dat %>% group_by(!!sym(uv)) %>% mutate(!!dv:=datex.center(!!sym(dv)))
  }
  dat <- datex.helper(dat,id,dv,uv)
  p <- dat %>%
  ggplot(aes(x=!!sym(uv),y=!!sym(dv),fill=!!sym(uv),label=label)) +
  geom_boxplot() +
  geom_text(nudge_x = 0.1,size=3,alpha=1) +
  ylab(ylab) +
  #facet_grid(cols=vars(!!sym(my_facet))) +
  theme(legend.position="none")
  if (cloud) {p <- p + geom_jitter(aes(y=point),shape=1,width=0.05,alpha=0.3,height=0)}
  p
}

datex.qqplot <- function(dat,dv,uv,level)
{
  p <- dat[dat[,uv]==level,] %>%
    ggplot(aes(sample=!!sym(dv))) +
    geom_qq() +
    geom_qq_line()
  p
}

# !!! all the descriptive stuff should be one function, really; or at least one for the summarise() which is always the same, and different
#     ones for the pre-aggregation; plus parameters for one or two uvs
datex.descriptives <- function(dat,dv,uv)
{
  dat %>% group_by(ID) %>% group_by(!!sym(uv)) %>% summarise(
    mean = mean(!!sym(dv),na.rm=T),
    median = median(!!sym(dv),na.rm=T),
    sd = sd(!!sym(dv),na.rm=T),
    n = length(!!sym(dv)),
    se = sd/sqrt(n)
  ) %>% select(c(uv,"n","mean","median","sd","se"))
}

datex.descriptives.between <- function(dat,dv,uv)
{
  by_PP.by_Exp <- dat %>% group_by(Exp,ID,!!sym(uv)) %>% summarise(
    avg.by_PP.by_Exp = mean(!!sym(dv),na.rm=T))
  by_PP.by_Exp %>% group_by(Exp,!!sym(uv)) %>% summarise(
    n = length(avg.by_PP.by_Exp),
    mean = mean(avg.by_PP.by_Exp,na.rm=T),
    median = median(avg.by_PP.by_Exp,na.rm=T),
    sd = sd(avg.by_PP.by_Exp,na.rm=T),
    se = sd/sqrt(n)
  ) %>% select(c(Exp,uv,"n","mean","median","sd","se"))
}

datex.descriptives.x2 <- function(dat,dv,uv1,uv2)
{
  if (uv1!=uv2)
  {
    dat %>% group_by(!!sym(uv1),!!sym(uv2)) %>% summarise(
      mean = mean(!!sym(dv),na.rm=T),
      sd = sd(!!sym(dv),na.rm=T),
      n = length(!!sym(dv)),
      se = sd/sqrt(n)
    ) %>% select(c(uv1,uv2,"n","mean","sd","se"))
  }
}

datex.barplot.between <- function(dat,dv,uv)
{
    # !!! should be in the pack; think long and hard about how to best implement this (plot functionality for different things)
  desc <- datex.descriptives.between(dat,dv,uv)
  desc %>% ggplot(aes(y=mean,x=!!sym(uv),fill=Exp)) +
    geom_bar(stat="identity",position=position_dodge()) +
    #theme(legend.position="none") +
    geom_errorbar(aes(ymin=mean-se,ymax=mean+se),position=position_dodge(.9),width=.2,color="black") # up and down
}

datex.barplot <- function(dat,dv,uv)
{
  desc <- datex.descriptives(dat,dv,uv)
  desc %>% ggplot(aes(y=mean,x=!!sym(uv),fill=!!sym(uv))) +
    geom_bar(stat="identity",position=position_dodge()) +
    theme(legend.position="none") +
    geom_errorbar(aes(ymin=mean-se,ymax=mean+se),position=position_dodge(.9),width=.2,color="black") # up and down
}

datex.anova <- function(dat,dv,within,between)
{
  # ??? afex reports corrected DF, that is why it is strange...
  # !!! type=2 only makes sense for backward compatibility with ezANOVA, I think !!!
  if (within!="none" & between=="none") {res <- aov_ez("ID",dv,dat,within=within,type=2,anova_table=list(es="pes",correction="none"))}
  if (within=="none" & between!="none") {res <- aov_ez("ID",dv,dat,between=between,type=2,anova_table=list(es="pes",correction="none"))}
  if (within!="none" & between!="none") {res <- aov_ez("ID",dv,dat,within=within,type=2,between=between,anova_table=list(es="pes",correction="none"))}
  res
}

datex.helper <- function(dat,id,dv,uv)
{
  dat %>% group_by(!!sym(uv)) %>% mutate(outlier=datex.is_outlier(!!sym(dv))) %>% # outliers
  mutate(label=ifelse(outlier==F,NA,!!sym(id))) %>% # outlier labels
  mutate(point=ifelse(outlier==T,as.numeric(NA),!!sym(dv))) # outlier points
}

datex.ridge <- function(dat,dv,uv,alpha_density=0.5,alpha_hist=0.2,scale=0.9,bins=10,facet,hist=FALSE,draw_baseline=T,col_density="black",col_hist="black",level,rug=FALSE)
{
  if (!missing(level)) {dat <- dat[dat[,uv]==level,]}
  # !!! point_color is questionable... hard to see for some colors, maybe just leave that black?
  p <- dat %>% ggplot(aes(x=!!sym(dv),y=!!sym(uv),fill=!!sym(uv),point_color=!!sym(uv)))
  if (hist==TRUE) {p <- p + geom_density_ridges(color=col_hist,draw_baseline=draw_baseline,stat="binline",rel_min_height=0.0,scale=scale,alpha=alpha_hist,bins=bins)}
  # ??? there HAS to be a better way, right ???
  if (rug==FALSE) {p <- p + geom_density_ridges(color=col_density,rel_min_height=0.0,scale=scale,jittered_points=F,alpha=alpha_density)}
  if (rug==TRUE) {p <- p + geom_density_ridges(color=col_density,rel_min_height=0.0,scale=scale,alpha=alpha_density,
        jittered_points = TRUE,
        point_shape = "|", point_size = 3, size = 0.25,
        position = position_points_jitter(height = 0)
  )}
  if (!missing(facet)) {p <- p + facet_grid(cols=vars(!!sym(facet)),scales="free")}
  p <- p + theme(legend.position="none")
  p
}

checks <- function()
{
  
  mod1 <- anova_out(dat %>% ezANOVA(wid=.(ID),dv="Errors",within=.(Task,Modality),detailed=T))
  mod2 <- anova_out(dat %>% filter(Modality=="manual") %>% ezANOVA(wid=.(ID),dv="Errors",within=.(Task),detailed=T))
  mod1[3]
  mod2[3]
  
}

#Sys.setenv(PATH = paste("C:/RTools/usr/bin", Sys.getenv("PATH"), sep=";"))
#dat <- read.csv("FP-2c.csv")
#dat$Exp <- "FP-2c"
#write.csv(dat,row.names = F,file="out.csv")
#dat$RTs <- dat$RTs*1000
