library(ez)
library(tidyverse)

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
  p <- ggplot(dat[dat[,uv]==level,],aes(x=!!sym(dv))) +
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

datex.descriptives <- function(dat,dv,uv)
{
  dat %>% group_by(!!sym(uv)) %>% summarise(
    mean = mean(!!sym(dv),na.rm=T),
    sd = sd(!!sym(dv),na.rm=T),
    n = length(!!sym(dv)),
    se = sd/sqrt(n)
  ) %>% select(c(uv,"n","mean","sd","se"))
}

datex.descriptives.x2 <- function(dat,dv,uv1,uv2)
{
  dat %>% group_by(!!sym(uv1),!!sym(uv2)) %>% summarise(
    mean = mean(!!sym(dv),na.rm=T),
    sd = sd(!!sym(dv),na.rm=T),
    n = length(!!sym(dv)),
    se = sd/sqrt(n)
  ) %>% select(c(uv1,uv2,"n","mean","sd","se"))
}

datex.barplot <- function(dat,dv,uv)
{
  desc <- datex.descriptives(dat,dv,uv)
  desc %>% ggplot(aes(y=mean,x=!!sym(uv),fill=!!sym(uv))) +
    geom_bar(stat="identity",position=position_dodge()) +
    theme(legend.position="none") +
    geom_errorbar(aes(ymin=mean-se,ymax=mean+se),position=position_dodge(.9),width=.2,color="black") # up and down
}

datex.helper <- function(dat,id,dv,uv)
{
  dat %>% group_by(!!sym(uv)) %>% mutate(outlier=datex.is_outlier(!!sym(dv))) %>% # outliers
  mutate(label=ifelse(outlier==F,NA,!!sym(id))) %>% # outlier labels
  mutate(point=ifelse(outlier==T,as.numeric(NA),!!sym(dv))) # outlier points
}

#Sys.setenv(PATH = paste("C:/RTools/usr/bin", Sys.getenv("PATH"), sep=";"))
#dat <- read.csv("mydata.csv")
# dat$RTs <- dat$RTs*1000
