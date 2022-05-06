#
# Authors:     AHG
# Maintainers: AHG
# Copyright:   2019, HRDAG, GPL v2 or later
# ============================================
# replication/x05_figures_tables/src/figs-tables.R

# Puts all table and figure generation in one place for replication purposes.

args <- list(multiplot ='src/multiplot.R',
             mse = 'input/mse-results.csv',
             pp = 'input/post-probs.csv',
             spp = 'input/summed-post-probs.csv.gz',
             unmerged = 'input/sv-lethals-anon.csv',
             merged = 'input/merged-records-anon.csv',
             strata = 'input/strata-list-es.csv',
             sums = 'input/sums.rds',
             pop = 'input/elsal-pop-data.csv',
             mapdata = 'input/SLV_adm1.rds',
             year_ests = 'output/year-est-table.txt',
             year_cor_spearman = 'output/year-cor-table-spearman.txt',
             dept_ests = 'output/bydept-table.txt',
             dept_detail = 'output/bydept-detail-table.txt',
             dept_cor_spearman = 'output/dept-cor-table-spearman.txt',
             overlap = 'output/overlap-table.txt',
             prior_compare = 'output/prior-comparison-fig.pdf',
             pp_viol = 'output/post-prob-violins-fig.pdf',
             pp_multi2 = 'output/post-prob-multi2-fig.pdf',
             pp_1981 = 'output/post-prob-1981-fig.pdf',
             year_fig_raw = 'output/lethals-by-yr-raw-fig.pdf',
             year_fig_est = 'output/lethals-by-yr-est-fig.pdf',
             dept_fig_raw = 'output/lethals-by-dept-raw-fig.pdf',
             dept_fig_est = 'output/lethals-by-dept-est-fig.pdf')


# setup

library(tidyverse)
library(sp)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(ggmosaic)
library(matrixStats)

source(args$multiplot)
options(scipen=999999)

# function 

make_pp_graph <- function(pp, nstar, output_pdf) {
  p <- ggplot(as.tibble(pp), aes(pp)) +
    geom_density() +
    geom_vline(xintercept=nstar) +
    xlim((nstar - 1), (max(pp) + 1)) +
    xlab('Estimated total killings and disappearances') +
    ylab("Density")
  
  ggsave(output_pdf, plot=p, width=7.5, height=4.5, units='in')
}

# population data 

# these data are from http://www.statoids.com/usv.html, 
# which compiles several other sources. In particular
# 1981 estimates are from The Statesman's Yearbook 
# 1988-89, ed. John Paxton. St. Martin's Press, New York.
# It notes that San Vicente's estimates are from 1980.

pop <- read_csv(args$pop) %>% 
  select(Department,`1981`) %>%
  rename(deptname=Department) %>%
  mutate(pop1981=round(`1981`/1000)) %>%
  select(deptname,pop1981)

print(pop)

# a magic number
nk.1992 <- 139 # total number of killings and disappearances reported in 1992.

# unmerged data
svdata <- as_tibble(read.csv(args$unmerged,header=TRUE,fill=TRUE,as.is=TRUE,sep='|'))
svdata$dept4 <- toupper(substr(gsub(' ','',svdata$dept,fixed=TRUE),1,4))
print(table(svdata$dept4,svdata$dataset))

# nk data (all observed cases by stratum)
nkdata <- read_delim(args$strata,delim='|')
nkdata$first4 <- toupper(substr(gsub(' ','',nkdata$stratum,fixed=TRUE),1,4))

nkdata.d <- filter(nkdata,stratum.type=='dept')
nkdata.d <- select(nkdata.d,first4,nk)
colnames(nkdata.d) <- c('dept','nk')

nkdata.y <- filter(nkdata,stratum.type=='year')
nkdata.y <- select(nkdata.y,first4,nk)
colnames(nkdata.y) <- c('year','nk')
nk.1991 <- as.numeric(nkdata.y$nk[nkdata.y$year==1991])
nk.9192 <- nk.1991+nk.1992
nkdata.y <- nkdata.y[-nrow(nkdata.y),]
nkdata.y <- rbind(nkdata.y,c('9192',nk.9192))

# posterior probabilities
pp <- read_csv(args$pp)

# mse ests
msedata <- read_csv(args$mse)

# mse ests by dept
mse.depts <- filter(msedata,deptsum==1)
mse.depts$dept <- toupper(substr(gsub(' ','',mse.depts$stratum,fixed=TRUE),1,4))
mse.depts <- select(mse.depts,stratum.type,dept,dgalog.025,dgalog.nhat,dgalog.975)
mse.depts <- left_join(nkdata.d,mse.depts,by='dept')

# mse ests by year
mse.years <- filter(msedata,yrsum==1)
pp <- select(pp,sname,starts_with('pp'))
mse.years <- left_join(mse.years,pp,by='sname') %>%
  mutate(year=substr(stratum,1,4)) %>%
  group_by(year) %>%
  summarise_at(vars(matches('nk'),starts_with('pp')),funs(sum)) %>%
  ungroup()

pp <- as.matrix(select(mse.years,starts_with('pp')))
x <- round(rowQuantiles(pp,probs=c(0.025,0.5,0.975)))
x <- as_tibble(x)
colnames(x) <- c('cilo','nhat','cihi')

mse.years <- bind_cols(select(mse.years,year,nk),x)
mse.years$year[mse.years$year=='9192'] <- '1991-92'

# geo data
md <- read_rds(args$mapdata)
md2 <- md
md <- unique(md@data[,5:6])
md$dept4 <- toupper(substr(gsub(' ','',md$NAME_1,fixed=TRUE),1,4))
md$deptname <- md$NAME_1
md$NAME_1 <- NULL 

# add deptcodes and numeric id's to svdata

svdata <- left_join(svdata,md,by='dept4')
svdata$deptcode <- svdata$ID_1
svdata$ID_1 <- NULL

# summarize by dept;
# join deptnames, deptcodes, pop info to dept data

print('making bydept')

bydept <- group_by(svdata,deptcode,dataset) %>%
  summarize(deaths=n()) %>%
  spread(dataset,deaths) %>%
  ungroup()

colnames(md) <- c('deptcode','dept','deptname')
bydept <- left_join(bydept,md,by='deptcode')
bydept <- left_join(bydept,pop)
colnames(bydept) <- c('deptcode','CDHES','Rescate','UNTRC','UNTRC2','dept','deptname','pop1981')
bydept <- left_join(bydept,nkdata.d,by='dept')
bydept <- left_join(mse.depts,bydept,by='dept')
bydept$nk <- bydept$nk.x
bydept$nk.x <- NULL
bydept$nk.y <- NULL

bydept <- select(bydept,deptcode,dept,deptname,
                 CDHES,Rescate,UNTRC,UNTRC2,
                 nk,dgalog.025,dgalog.nhat,dgalog.975,pop1981)

print('making per-cap columns')

for (ds in c('CDHES','Rescate','UNTRC','UNTRC2','nk','dgalog.nhat')) {
  nn2 <- paste(ds,'pc',sep='')
  bydept[[nn2]] <- bydept[[ds]]/bydept$pop1981
  nn3 <- paste(ds,'pc.ord',sep='')
  bydept[[nn3]] <- rank(bydept[[nn2]],na.last='keep')
  nn4 <- paste(ds,'pc.ord.top5',sep='')
  bydept[[nn4]] <- bydept[[nn3]]
  ndepts <- max(bydept[[nn4]],na.rm=TRUE)
  bydept[[nn4]][bydept[[nn4]] %in% c(1:(ndepts-5))] <- ndepts-5
  nn5 <- paste(ds,'pc.ord.top3',sep='')
  bydept[[nn5]] <- bydept[[nn3]]
  ndepts <- max(bydept[[nn5]],na.rm=TRUE)
  bydept[[nn5]][bydept[[nn5]] %in% c(1:(ndepts-3))] <- ndepts-3
}

bydept$burdenlo <- round(bydept$dgalog.025/bydept$pop1981,2)
bydept$burdenhi <- round(bydept$dgalog.975/bydept$pop1981,2)
bydept$format.ci <- paste(bydept$dgalog.nhat,' (',bydept$dgalog.025,', ',bydept$dgalog.975,')',sep='')
bydept$format.ci.pc <- paste(round(bydept$dgalog.nhatpc,2),' (',bydept$burdenlo,', ',bydept$burdenhi,')',sep='')

bydept$cap <- round(bydept$nk/bydept$dgalog.nhat,2)
bydept$caplo <- round(bydept$nk/bydept$dgalog.975,2)
bydept$caphi <- round(bydept$nk/bydept$dgalog.025,2)
bydept$format.cap <- paste(round(bydept$cap,2),' (',bydept$caplo,', ',bydept$caphi,')',sep='')

print(bydept)
write_csv(bydept,args$dept_detail)

bydept2 <- select(bydept,deptname,pop1981,nk,format.ci,format.ci.pc,format.cap)
print(bydept2)
write_csv(bydept2,args$dept_ests)

mapdata <- as.tibble(fortify(md2))
mapdata$deptcode <- as.integer(mapdata$id)
mapdata$id <- NULL

mapdata <- left_join(mapdata,bydept,by='deptcode')

# PLOTTING

# compare priors

x <- seq(0.0001,20,length.out=1000)
jeff <- -log(x)
jeff <- (jeff-min(jeff))/(max(jeff)-min(jeff)) # normalizes to (0,1)
jeff <- jeff/sum(jeff) # make sure probs sum to 1
logn <- dlnorm(x,meanlog=0.7,sdlog=0.6)
logn <- logn/sum(logn)

x.new <- c(x,x)
y.new <- c(jeff,logn)
z.new <- c(rep('Default',1000),rep('Lognormal',1000))
s2 <- tibble(x.new,y.new,z.new)
colnames(s2) <- c('x','y','Distribution')
s2$x <- s2$x+1

s3 <- ggplot(data=s2) + geom_line(aes(x=x,y=y,linetype=Distribution)) +
  xlab('Multiplier') + ylab('Prior probability') +
  scale_linetype_manual(values=c("Default"=2,"Lognormal"=1))

ggsave(args$prior_compare, plot=s3, width=7.5, height=4.5, units='in')

# plot data

svmap <- ggplot(data=mapdata)
svplot <- ggplot(data=svdata)

# lethal violence by year and dataset

print('making plot of reported lethal violence by year')

svplot.yr1 <- svplot+geom_line(mapping=aes(x=year,lty=dataset),stat='count') +
  scale_x_continuous(breaks=c(1980:1992),labels=as.character(c(1980:1992))) +
  labs(x='Year',y='Reported lethal violence',lty='Dataset',title='')

ggsave(args$year_fig_raw,plot=svplot.yr1,width=9,height=5,units='in')

mse.years$dataset <- "MSE Estimate"
mse.years$year[mse.years$year=='1991-92'] <- 1991
print(mse.years)
mse.years$year <- as.numeric(mse.years$year)
print(mse.years)

# lethal violence by year with MSE ests and CI

print('making plot of lethal violence by year, with MSE ests')

svplot.yr2 <- ggplot() +
  geom_ribbon(data=mse.years,mapping=aes(x=year,ymin=cilo,ymax=cihi),fill='light grey') +
  geom_line(data=mse.years,mapping=aes(x=year,y=nhat,linetype='MSE Estimate')) +
  geom_line(data=svdata,mapping=aes(x=year,linetype='Sum of observed'), stat='count') +
  geom_line(data=filter(svdata,dataset=="CDHES"),mapping=aes(x=year,linetype='CDHES'), stat='count') +
  geom_line(data=filter(svdata,dataset=="Rescate"),mapping=aes(x=year,linetype='Rescate'), stat='count') +
  geom_line(data=filter(svdata,dataset=="UN-TRC"),mapping=aes(x=year,linetype='UN-TRC'), stat='count') +
  geom_line(data=filter(svdata,dataset=="UN-TRC-2"),mapping=aes(x=year,linetype='UN-TRC-2'), stat='count') +
  labs(x='Year',y='Lethal violence', title='',linetype='Source') + 
  coord_cartesian(ylim=c(0,38000))+
  scale_x_continuous(limits=c(1980,1992),breaks=c(1980:1992)) +
  scale_linetype_discrete(limits=c('MSE Estimate','Sum of observed','CDHES','Rescate','UN-TRC','UN-TRC-2'))

ggsave(args$year_fig_est,plot=svplot.yr2,width=9,height=5,units='in')

# per capita lethal violence by department and dataset (log10)

print('log10 lethal violence by dept & dataset')

cdhes <- svmap+geom_polygon(aes(x=long,y=lat,group=group,fill=CDHESpc),color='black')+
  coord_map()+scale_fill_gradient2(trans="log10",limits=c(0.01,8.5),
                                   low="gray99",mid="gray50",high="gray1",na.value='white')+
  theme_void()+labs(fill='Log10 (reported) \nkillings per \nthousand \nresidents',title='CDHES data')+
  theme(plot.title = element_text(hjust = 0.5))

rescate <- svmap+geom_polygon(aes(x=long,y=lat,group=group,fill=Rescatepc),color='black')+
  coord_map()+scale_fill_gradient2(trans="log10",limits=c(0.01,8.5),
                                   low="gray99",mid="gray50",high="gray1",na.value='white')+
  theme_void()+labs(fill='Log10 (reported) \nkillings per \nthousand \nresidents',title='Rescate data')+
  theme(plot.title = element_text(hjust = 0.5))

untrc <- svmap+geom_polygon(aes(x=long,y=lat,group=group,fill=UNTRCpc),color='black')+
  coord_map()+scale_fill_gradient2(trans="log10",limits=c(0.01,8.5),
                                   low="gray99",mid="gray50",high="gray1",na.value='white')+
  theme_void()+labs(fill='Log10 (reported) \nkillings per \nthousand \nresidents',title='UNTRC data')+
  theme(plot.title = element_text(hjust = 0.5))

untrc2 <- svmap+geom_polygon(aes(x=long,y=lat,group=group,fill=UNTRC2pc),color='black')+
  coord_map()+scale_fill_gradient2(trans="log10",limits=c(0.01,8.5),
                                   low="gray99",mid="gray50",high="gray1",na.value='white')+
  theme_void()+labs(fill='Log10 (reported) \nkillings per \nthousand \nresidents',title='UNTRC-2 data')+
  theme(plot.title = element_text(hjust = 0.5))

pdf(file=args$dept_fig_raw,width=14,height=7)
multiplot(cdhes,rescate,untrc,untrc2,cols=2)
dev.off()

# map of lethal violence estimated prevalence by department with dots for uncertainty #

print('making map with uncertainty measures')

# summarize depts by lat, lon

deptll <- group_by(mapdata,deptcode)
deptll <- summarise(deptll,
                    meanlon=mean(long,na.rm=TRUE),
                    meanlat=mean(lat,na.rm=TRUE))

bydept <- left_join(bydept,deptll,by='deptcode')

# get max & min for whole map

minlat <- round(min(mapdata$lat,na.rm=TRUE),2)
minlon <- round(min(mapdata$long,na.rm=TRUE),2)
maxlat <- round(max(mapdata$lat,na.rm=TRUE),2)
maxlon <- round(max(mapdata$long,na.rm=TRUE),2)

latrange <- seq(minlat, maxlat,0.02)
lonrange <- seq(minlon, maxlon,0.02)

svgrid <- as_tibble(expand.grid(latrange,lonrange))
colnames(svgrid) <- c('lat','lon')
svgrid$deptcode <- NA

for (i in 1:14){
  assign(paste('tmp',i,sep=''),subset(mapdata,mapdata$deptcode %in% c(i,as.character(i))))
}

for (i in 1:nrow(svgrid)){
  print(i)
  lon <- svgrid$lon[i]
  lat <- svgrid$lat[i]
  for (d in c(1:14)){
    tmp <- get(paste('tmp',d,sep=''))
    chk <- point.in.polygon(lon,lat,tmp$long,tmp$lat)
    if (1 %in% chk){
      svgrid$deptcode[i] <- d
      break
    }
  }
}

svgrid$deptcode <- as.character(svgrid$deptcode)
bydept$deptcode <- as.character(bydept$deptcode)
svgrid <- left_join(svgrid,bydept,by='deptcode')
svgrid <- subset(svgrid,as.numeric(svgrid$deptcode) %in% c(1:14))
svgrid$lohi <- sample(c('lo','hi'),nrow(svgrid),replace=TRUE)
svgridlo <- subset(svgrid,svgrid$lohi=='lo')
svgridhi <- subset(svgrid,svgrid$lohi=='hi')

mse.ci <- svmap+
  geom_polygon(aes(x=long,y=lat,group=group,fill=dgalog.nhatpc),color='black',lwd=0.15)+
  coord_map()+
  scale_fill_gradient(limits=c(0,90),low="white",high="black", na.value='white')+
  geom_point(data=svgridlo,aes(x=lon,y=lat,color=burdenlo),show.legend=FALSE,alpha=0.7,size=0.6)+
  geom_point(data=svgridhi,aes(x=lon,y=lat,color=burdenhi),show.legend=FALSE,alpha=0.7,size=0.6)+
  scale_color_gradient(limits=c(0,90),low="white",high="black", na.value='white')+
  labs(fill='Estimated \nkillings per \nthousand \nresidents',title='',color='',
       caption='')+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust=0.5),
        axis.title = element_blank())+
  geom_polygon(aes(x=long,y=lat,group=group),fill=NA,color='black',lwd=0.25)

pdf(file=args$dept_fig_est,width=10,height=6)
plot(mse.ci)
dev.off()

# plots of posterior probabilities

#---multi-2-----
pp <- read_delim(args$spp, delim='|') %>%
  filter(sname == 'multi-2') %>%
  as.integer(.)
print(paste('multi-2: read ', length(pp), ' from postprobs'))

nstar <- read_rds(args$sums) %>%
  filter(sname == 'multi-2') %>%
  select(nstar) %>%
  as.integer(.)
print(paste('multi-2: nstar = ', nstar))
make_pp_graph(pp, nstar, args$pp_multi2)


#---1981-----
pp <- read_csv(args$pp) %>%
  filter(sname == 'year-1981') %>%
  as.integer(.)
print(paste0('1981: read ', length(pp), ' from postprobs'))

nstar <- read_csv(args$mse) %>%
  filter(sname == 'year-1981') %>%
  select(matches('x\\d{4}')) %>%
  rowSums(.)
print(paste0('1981 nstar = ', nstar))
make_pp_graph(pp, nstar, args$pp_1981)


#---violin graphs-----

spp <- read_delim(args$spp,delim='|')
sums <- read_rds(args$sums)
spp2 <- as.tibble(t(spp))
colnames(spp2) <- sums$sname
new.spp <- gather(spp2)
colnames(new.spp) <- c('str','val')
new.spp$val <- as.numeric(new.spp$val)

sums <- arrange(sums,desc(nstar)) %>%
  filter(nstar>=19046,nstrata>=10,sname!='pd3.pp')

new.spp <- filter(new.spp,str %in% sums$sname) %>%
  mutate(str=factor(str,levels=rev(sums$sname)))

p <- ggplot(data=new.spp) +
  geom_hline(yintercept=sums$dgalog.nhat[sums$sname=='multi-2'], color='grey') +
  geom_hline(yintercept=sums$dgalog.025[sums$sname=='multi-2'], color='grey',alpha=0.5) +
  geom_hline(yintercept=sums$dgalog.975[sums$sname=='multi-2'], color='grey',alpha=0.5) +
  geom_violin(aes(x=str,y=val),adjust=0.5) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  xlab('Stratifications') +
  ylab('Posterior distribution of estimate')

ggsave(args$pp_viol, plot=p, width=7.5, height=4.5, units='in')

# TABLES

print('making tables')

# overlap table
print('overlap table')
c1 <- c(rep(c(rep(0,8),rep(1,8)),1))
c2 <- c(rep(c(rep(0,4),rep(1,4)),2))
c3 <- c(rep(c(rep(0,2),rep(1,2)),4))
c4 <- c(rep(c(rep(0,1),rep(1,1)),8))
cs <- colSums(filter(msedata,stratum.type=='period2')[,2:16])
c5 <- c('0000',gsub('x','',names(cs)))
c6 <- c(NA,as.numeric(cs))
ol <- tibble(c1,c2,c3,c4,c5,c6)
colnames(ol) <- c('In CDHES',
                  'In Rescate',
                  'In UNTC',
                  'In UNTC-2',
                  'Overlap pattern',
                  'Killings with this overlap pattern')
write.table(ol,file=args$overlap,sep='|',row.names=FALSE,quote=FALSE)

# estimates by dept
print('dept table')
bydept$nkpc <- round(bydept$nkpc,2)
bd <- select(bydept,deptname,pop1981,nk,format.ci,format.ci.pc,format.cap)
write.table(bd[1:14,],file=args$dept_ests,sep='|',row.names=FALSE,quote=FALSE)


# cor (estimates by dept)
print('correlation by dept tables')
ctab <- select(bydept,CDHESpc,Rescatepc,UNTRCpc,UNTRC2pc,dgalog.nhatpc)

# ct <- round(cor(ctab,use='complete.obs'),2)
# ct <- ct*upper.tri(ct)
# diag(ct) <- 1
# ct[ct==0] <- '.'
# write.table(as_tibble(ct),file=args$dept_cor,sep='|',row.names=FALSE,quote=FALSE)

ct2 <- round(cor(ctab,use='complete.obs',method='spearman'),2)
ct2 <- ct2*upper.tri(ct2)
diag(ct2) <- 1
ct2[ct2==0] <- '.'
write.table(as_tibble(ct2),file=args$dept_cor_spearman,sep='|',row.names=FALSE,quote=FALSE)

# cor (estimates by year)
print('correlation tables, by year')
mse.years$year <- as.character(mse.years$year)
mse.years$year[mse.years$year=='1991'] <- '1991-1992'
x <- as.data.frame.matrix(table(svdata$year,svdata$dataset))
x$year <- rownames(x)
x$year[x$year %in% c(1991,1992)] <- '1991-1992'
x <- as.tibble(x) %>%
  filter(year>0) %>%
  group_by(year) %>%
  summarise(CDHES=sum(CDHES),
            Rescate=sum(Rescate),
            `UN-TRC`=sum(`UN-TRC`),
            `UN-TRC-2`=sum(`UN-TRC-2`)) %>%
  ungroup()
x <- left_join(x,mse.years,by='year')

ctab <- select(x,CDHES,Rescate,`UN-TRC`,`UN-TRC-2`,nhat)
ct2 <- round(cor(ctab,use='complete.obs',method='spearman'),2)
ct2 <- ct2*upper.tri(ct2)
diag(ct2) <- 1
ct2[ct2==0] <- '.'
write.table(as_tibble(ct2),file=args$year_cor_spearman,row.names=FALSE,sep='|',quote=FALSE)


# table of yearly estimates
print('table of estimates by year')

mse.years$format.ci <- paste(mse.years$nhat,' (',mse.years$cilo,', ',mse.years$cihi,')',sep='')
mse.years$cap.est <- round(mse.years$nk/mse.years$nhat,2)
mse.years$cap.lo <- round(mse.years$nk/mse.years$cihi,2)
mse.years$cap.hi <- round(mse.years$nk/mse.years$cilo,2)
mse.years$format.cap.ci <- paste(mse.years$cap.est,' (',mse.years$cap.lo,', ',mse.years$cap.hi,')',sep='')
mse.years <- select(mse.years,year,nk,format.ci,format.cap.ci) %>% arrange(year)
write.table(mse.years,file=args$year_ests,sep='|',row.names=FALSE,quote=FALSE)






             
             