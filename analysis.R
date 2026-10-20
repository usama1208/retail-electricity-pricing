# PG&E residential tariff analysis. Data are synthetic; totals cover the sample.
# Run with run.R, or set DATA_DIR and OUT_DIR before sourcing this file.
suppressPackageStartupMessages({library(data.table); library(fixest); library(ggplot2)})
if (!exists("DATA_DIR")) DATA_DIR <- "../data/ca_electricity_data"
if (!exists("OUT_DIR")) OUT_DIR <- getwd()
dir.create(file.path(OUT_DIR,"tables"),recursive=TRUE,showWarnings=FALSE)
options(scipen=8)
setDTthreads(2)
setFixest_nthreads(2)
export <- function(x,name) {fwrite(x,file.path(OUT_DIR,"tables",paste0(name,".csv"))); x}
fmt <- function(x,digits=2) formatC(x,format="f",digits=digits,big.mark=",")
usd <- function(x) paste0(ifelse(x<0,"−",""),"$",fmt(abs(x)))
message("Loading and validating CSVs")
h <- fread(file.path(DATA_DIR,"households.csv"),colClasses=c(tract="character"))
t <- fread(file.path(DATA_DIR,"tracts.csv"),colClasses=c(tract="character",county="character"))
w <- fread(file.path(DATA_DIR,"weather.csv"),colClasses=c(county="character"))
b <- fread(file.path(DATA_DIR,"bills.csv"))
original_unmatched <- sum(!h$tract %in% t$tract)
pad_changes <- 0L
for (x in list(h,t)) {
  original <- x$tract
  x[,tract:=sprintf("%06d",as.integer(trimws(tract)))]
  stopifnot(all(grepl("^[0-9]{6}$",x$tract)))
  pad_changes <- pad_changes+sum(original!=x$tract)
}
t[,county:=sprintf("%03d",as.integer(county))]; w[,county:=sprintf("%03d",as.integer(county))]
stopifnot(!anyDuplicated(h$ID),!anyDuplicated(t$tract),!anyDuplicated(w,by=c("county","year","month")),!anyDuplicated(b,by=c("ID","year","month")))
stopifnot(all(h$CARE %in% 0:1),all(t$PGE %in% 0:1),all(b$year %in% 2015:2023),all(b$month %in% 1:12),all(b[, .N,by=ID]$N==108),setequal(b$ID,h$ID),all(w$HDD>=0),all(w$CDD>=0),all(is.finite(b$kwh[!is.na(b$kwh)])),all(b$kwh[!is.na(b$kwh)]>0))
raw_missing <- sum(is.na(b$kwh))
if(all(t$PGE==as.integer(t$d>0))) t[,distance:=d] else if(all(t$PGE==as.integer(t$d<0))) t[,distance:=-d] else stop("Inconsistent utility/distance assignment")
h <- merge(h,t,by="tract",all.x=TRUE,sort=FALSE);stopifnot(!anyNA(h$PGE))
b[,time:=(year-2015L)*12L+month-1L];setorder(b,ID,time)
b[,connected:=!is.na(kwh)]
b[,median_kwh:=median(kwh,na.rm=TRUE),by=ID]
b[,candidate:=!is.na(kwh)&kwh/median_kwh>20]
b[,spike:=candidate & !shift(candidate,fill=FALSE) & !shift(candidate,type="lead",fill=FALSE) & (shift(connected,fill=FALSE)|shift(connected,type="lead",fill=FALSE)),by=ID]
stopifnot(!any(b$candidate & !b$spike))
spikes <- sum(b$spike); b[,kwh_clean:=fifelse(spike,NA_real_,kwh)]
med <- unique(b[,.(ID,median_kwh)])
h <- merge(h,med,by="ID",sort=FALSE);h[,high_usage:=median_kwh>3000]
b <- merge(b,h[,!"median_kwh"],by="ID",all.x=TRUE,sort=FALSE)
b <- merge(b,w,by=c("county","year","month"),all.x=TRUE,sort=FALSE)
stopifnot(!anyNA(b[,.(HDD,CDD,PGE,income,CARE)]))
b[,p:=fifelse(PGE==1,fifelse(year<2019,.25,.22),.19)]
b[,F:=fifelse(PGE==1,fifelse(year<2021,45,55),50)]
b[,price:=p*fifelse(CARE==1,2/3,1)]
b[,`:=`(post2019=PGE*(year>=2019),post2021=PGE*(year>=2021),HDD100=HDD/100,CDD100=CDD/100,log_kwh=log(kwh_clean))]
quality <- export(data.table(Measure=c("Households","Tracts","Bill rows","Closed-account months","Isolated spikes nulled","Persistent high-use households (median >3,000 kWh)","Tract strings changed by normalization","Unmatched households before normalization","Unmatched households after normalization","Tracts with reversed distance sign"),Count=c(nrow(h),nrow(t),nrow(b),raw_missing,spikes,sum(h$high_usage),pad_changes,original_unmatched,0,sum(t$PGE==as.integer(t$d<0)))),"data_quality")
# fixest supplies the within estimator. An explicit sandwich keeps the documented
# full fixed-effect degrees-of-freedom correction and normal intervals transparent.
regress <- function(d,names,outcome="log_kwh",cluster="county",fixed=TRUE,weights=NULL) {
  n <- nrow(d); nf <- 0L
  if(fixed) {
    f <- as.formula(paste(outcome,"~",paste(names,collapse="+"),"| ID + time"))
    fit <- feols(f,data=d,demeaned=TRUE,fixef.rm="none",fixef.tol=1e-10,fixef.iter=10000,notes=FALSE)
    X <- fit$X_demeaned; beta <- coef(fit); u <- residuals(fit)
    stopifnot(nobs(fit)==n)
    nf <- uniqueN(d$ID)+uniqueN(d$time)-1L
  } else {
    X <- as.matrix(d[,..names]); y <- d[[outcome]]
    if(!is.null(weights)) {X <- X*sqrt(weights);y <- y*sqrt(weights)}
    fit <- lm.fit(X,y); beta <- fit$coefficients;u <- fit$residuals
  }
  stopifnot(qr(X)$rank==ncol(X))
  bread <- solve(crossprod(X));g <- as.integer(factor(d[[cluster]]));G <- max(g)
  scores <- rowsum(X*as.numeric(u),g,reorder=TRUE)
  cov <- G/(G-1)*(n-1)/(n-ncol(X)-nf)*bread%*%crossprod(scores)%*%bread
  se <- sqrt(diag(cov))
  tab <- data.table(term=names,estimate=as.numeric(beta),SE=se,lower95=beta-1.96*se,upper95=beta+1.96*se,p_normal=2*pnorm(-abs(beta/se)))
  list(table=tab,beta=as.numeric(beta),cov=cov,n=n,clusters=G)
}
message("Estimating demand and robustness models")
main <- regress(b[!is.na(kwh_clean)&year<=2020],c("post2019","HDD100","CDD100"))
logchange <- log(.22/.25);elasticity <- main$beta[1]/logchange
ese <- main$table$SE[1]/abs(logchange);elo <- elasticity-1.96*ese;ehi <- elasticity+1.96*ese
models <- list()
model_row <- function(name,r,term="post2019",convert=TRUE) {
  idx <- which(r$table$term==term); v <- r$table[idx]; scale <- if(convert) logchange else 1
  est <- v$estimate/scale;se <- v$SE/abs(scale)
  data.table(Model=name,Estimate=est,SE=se,`95% low`=est-1.96*se,`95% high`=est+1.96*se,N=r$n,Clusters=r$clusters)
}
models[[1]] <- model_row("Primary elasticity: 2015–2020",main)
short <- regress(b[!is.na(kwh_clean)&year<=2019],c("post2019","HDD100","CDD100"));models[[2]] <- model_row("Elasticity: exclude 2020",short)
large <- regress(b[!is.na(kwh_clean)&year<=2020&!high_usage],c("post2019","HDD100","CDD100"));models[[3]] <- model_row("Elasticity: exclude high-use accounts",large)
stable <- b[year<=2020,.(n=sum(!is.na(kwh_clean))),by=ID][n==72,ID]
balanced <- regress(b[!is.na(kwh_clean)&year<=2020&ID %in% stable],c("post2019","HDD100","CDD100"));models[[4]] <- model_row("Elasticity: complete 72-month accounts",balanced)
full <- regress(b[!is.na(kwh_clean)],c("post2019","post2021","HDD100","CDD100"));models[[5]] <- model_row("Elasticity: 2015–2023 + 2021 shift",full)
placebo <- regress(b[!is.na(kwh_clean)&year>=2019],c("post2021","HDD100","CDD100"));models[[6]] <- model_row("2021 fixed-charge placebo (log points)",placebo,"post2021",FALSE)
connect <- regress(b,c("post2019","post2021","HDD100","CDD100"),outcome="connected");models[[7]] <- model_row("2021 connection check (fraction)",connect,"post2021",FALSE)
model_df <- export(rbindlist(models),"model_comparison")
baseline <- export(data.table(`Utility / period`=c("SCE, 2015–2023","PG&E, 2015–2018","PG&E, 2019–2020","PG&E, 2021–2023"),`Standard p ($/kWh)`=c(.19,.25,.22,.22),`CARE p ($/kWh)`=c(.19,.25,.22,.22)*2/3,`F ($/month)`=c(50,45,45,55)),"historical_tariffs")
message("Estimating event study and border validation")
years <- c(2015:2017,2019:2023);eventnames <- paste0("PGE_",years)
for(i in seq_along(years)) b[,(eventnames[i]):=PGE*(year==years[i])]
event <- regress(b[!is.na(kwh_clean)],c(eventnames,"HDD100","CDD100"))
ev <- cbind(data.table(Year=years),event$table[1:8]);export(ev,"event_study")
pre <- event$beta[1:3];wald <- as.numeric(t(pre)%*%solve(event$cov[1:3,1:3],pre));wp <- pchisq(wald,3,lower.tail=FALSE)
evc <- rbind(ev[,.(Year,estimate,lower95,upper95)],data.table(Year=2018,estimate=0,lower95=0,upper95=0));setorder(evc,Year)
annual <- b[,.(mean_kwh=mean(kwh_clean,na.rm=TRUE),connected=mean(connected)),by=.(year,PGE)];setorder(annual,year,PGE)
rdhh <- merge(b[!is.na(kwh_clean)&year==2018,.(log_kwh=mean(log_kwh),HDD100=mean(HDD100),CDD100=mean(CDD100)),by=ID],h,by="ID")
rdrows <- list()
for(bw in c(.1,.25,.5)) for(label in c("Income ($000s)","CARE share","2018 log usage")) {
  frame <- switch(label,"Income ($000s)"=t,"CARE share"=h,"2018 log usage"=rdhh)
  outcome <- switch(label,"Income ($000s)"="income","CARE share"="CARE","2018 log usage"="log_kwh")
  d <- copy(frame[abs(distance)<bw]);d[,`:=`(intercept=1,side_slope=distance*PGE)]
  names <- c("intercept","PGE","distance","side_slope");if(outcome=="log_kwh") names <- c(names,"HDD100","CDD100")
  rr <- regress(d,names,outcome,"tract",FALSE,1-abs(d$distance)/bw)$table[term=="PGE"]
  rdrows[[length(rdrows)+1]] <- data.table(`Bandwidth (degrees)`=bw,Outcome=label,`PG&E minus SCE`=rr$estimate,`SE (tract)`=rr$SE,`95% low`=rr$lower95,`95% high`=rr$upper95,N=nrow(d),Tracts=uniqueN(d$tract),`PG&E N`=sum(d$PGE),`SCE N`=sum(1-d$PGE))
}
rd <- export(rbindlist(rdrows),"border_validation")
rplot <- copy(rdhh[abs(distance)<.5]);rplot[,bin:=cut(distance,seq(-.5,.5,length.out=21))]
rb <- rplot[,.(distance=mean(distance),log_kwh=mean(log_kwh)),by=bin]
# Baseline simulation uses only connected 2023 accounts. Error-month imputation
# preserves fixed fees; closed-account months are never imputed or charged.
s <- copy(b[PGE==1 & year==2023 & connected]);s[,q0:=kwh_clean]
s[is.na(q0),q0:=s[,.(fill=median(kwh_clean,na.rm=TRUE)),by=ID][.SD,on="ID",fill]]
stopifnot(!anyNA(s$q0));imputed <- sum(s$spike)
s[,tier:=fcase(CARE==1,"CARE",income<75,"Non-CARE: income < $75k",income<125,"Non-CARE: $75k–<$125k",default="Non-CARE: income ≥ $125k")]
s[,weight:=fcase(CARE==1,0,income<75,.65,income<125,1,default=1.4)]
s[,bill0:=price*q0+F]
SMC <- .08;EMISSIONS <- 430*.45359237/1e6;SCC <- 190
carbon_cost <- EMISSIONS*SCC;private_cost <- SMC-carbon_cost
simulate <- function(e,pnew=.18,profit=FALSE,frame=s) {
  z <- copy(frame);z[,pnew:=pnew*fifelse(CARE==1,2/3,1)]
  ratio <- z$pnew/z$price;z[,q1:=q0*ratio^e]
  deltaq <- sum(z$q1-z$q0);target <- sum(z$bill0)+if(profit)private_cost*deltaq else 0
  vol <- sum(z$pnew*z$q1);care_fixed <- 6*sum(z$CARE)
  lambda <- (target-vol-care_fixed)/sum(z$weight)
  z[,Fnew:=fifelse(CARE==1,6,lambda*weight)]
  z[,bill1:=pnew*q1+Fnew]
  # Remove accumulated floating-point summation error without rounding fees.
  for(iteration in 1:3) {
    lambda <- lambda+(target-sum(z$bill1))/sum(z$weight)
    z[,Fnew:=fifelse(CARE==1,6,lambda*weight)]
    z[,bill1:=pnew*q1+Fnew]
  }
  z[,bill_change:=bill1-bill0]
  energy_cs <- if(abs(e+1)<1e-8) -z$q0*z$price*log(ratio) else z$q0*z$price*(1-ratio^(e+1))/(e+1)
  z[,CS:=energy_cs-(Fnew-F)]
  z[,welfare:=energy_cs+(pnew*q1-price*q0)-SMC*(q1-q0)]
  gap <- sum(z$bill1)-target
  if(abs(gap)>=1e-6) stop(sprintf("Revenue mismatch: gap=%.12f, target=%.6f, variable=%.6f, fixed=%.6f, lambda=%.6f",gap,target,vol,sum(z$Fnew),lambda))
  stopifnot(min(z$Fnew)>=0)
  list(z=z,result=list(elasticity=e,p=pnew,lambda=lambda,baseline_revenue=sum(z$bill0),new_revenue=sum(z$bill1),reconciliation=gap,delta_kwh=deltaq,kwh_pct=100*deltaq/sum(z$q0),CS=sum(z$CS),delta_tonnes=deltaq*EMISSIONS,climate_damage=deltaq*carbon_cost,welfare=sum(z$welfare),profit_change_proxy=sum(z$bill1-z$bill0)-private_cost*deltaq,volumetric_revenue=vol,care_fixed_revenue=care_fixed,weight_months=sum(z$weight)))
}
message("Calibrating revenue and profit variants")
a <- simulate(elasticity);z <- a$z;result <- a$result
ap <- simulate(elasticity,profit=TRUE);zp <- ap$z;rp <- ap$result
N <- uniqueN(s$ID);M <- nrow(s)
stopifnot(abs(sum(z$bill_change))<1e-6,abs(result$welfare-(result$CS+result$profit_change_proxy-result$climate_damage))<1e-6)
tariff <- z[,.(households=uniqueN(ID),connected_months=.N,p_new=first(pnew),F_new=first(Fnew)),by=tier]
tariff <- merge(tariff,zp[,.(F_profit_neutral_proxy=first(Fnew)),by=tier],by="tier");setorder(tariff,F_new);export(tariff,"proposed_tariff")
dist <- export(z[,.(months=.N,baseline_kwh=mean(q0),new_kwh=mean(q1),baseline_bill=mean(bill0),new_bill=mean(bill1),bill_change=mean(bill_change),net_CS=mean(CS),share_lower_bill=mean(bill_change<0)),by=tier][order(tier)],"distribution_by_tier")
care <- export(z[,.(months=.N,baseline_kwh=mean(q0),new_kwh=mean(q1),bill_change=mean(bill_change),net_CS=mean(CS)),by=CARE][order(CARE)],"distribution_by_care")
z[,income_band:=cut(income,c(-Inf,75,125,Inf),right=FALSE,labels=c("< $75k","$75k–<$125k","≥ $125k"))]
cross <- export(z[,.(months=.N,bill_change=mean(bill_change),net_CS=mean(CS)),by=.(income_band,CARE)][order(income_band,CARE)],"distribution_income_care")
impact <- export(data.table(Metric=c("Baseline consumption (kWh)","Proposed consumption (kWh)","Consumption change (kWh)","Consumption change (%)","Baseline revenue ($)","Proposed revenue ($)","Net consumer surplus change ($)","Net CS per observed household ($)","Net CS per connected month ($)","Baseline emissions proxy (metric t CO2e)","Proposed emissions proxy (metric t CO2e)","Emissions change (metric t CO2e)","Climate damage change (2020 $ proxy)","Social welfare change ($ benchmark)","Profit change ($ private-cost proxy)"),Value=c(sum(s$q0),sum(z$q1),result$delta_kwh,result$kwh_pct,result$baseline_revenue,result$new_revenue,result$CS,result$CS/N,result$CS/M,sum(s$q0)*EMISSIONS,sum(z$q1)*EMISSIONS,result$delta_tonnes,result$climate_damage,result$welfare,result$profit_change_proxy)),"aggregate_impacts")
scenarios <- data.table(Scenario=c("Less responsive: CI high","Central","More responsive: CI low","2019-only post period","Small cut","Larger cut","No demand response","Stress elasticity −0.5"),e=c(ehi,elasticity,elo,short$beta[1]/logchange,elasticity,elasticity,0,-.5),p=c(.18,.18,.18,.18,.20,.16,.18,.18))
sensitivity <- export(rbindlist(lapply(seq_len(nrow(scenarios)),function(i) {
  sc <- scenarios[i];a <- simulate(sc$e,sc$p);rr <- a$result
  data.table(Scenario=sc$Scenario,Elasticity=sc$e,`Standard p`=sc$p,`Recalibrated lambda`=rr$lambda,`kWh change (%)`=rr$kwh_pct,`CS ($)`=rr$CS,`CO2e change (t)`=rr$delta_tonnes,`Revenue gap if main F retained ($)`=sum(a$z$pnew*a$z$q1+z$Fnew)-sum(s$bill0))
})),"sensitivity")
round_gap <- sum(z$pnew*z$q1+round(z$Fnew,2))-sum(s$bill0)
# Plots are drawn by R/ggplot2 and embedded in the rendered R Markdown report.
theme_set(theme_minimal(base_size=12)+theme(panel.grid.minor=element_blank(),legend.position="bottom",plot.title=element_text(face="bold")))
annual[,Utility:=factor(PGE,levels=c(1,0),labels=c("PG&E","SCE"))]
plot_usage <- ggplot(annual,aes(year,mean_kwh,color=Utility))+geom_line(linewidth=.9)+geom_point()+scale_color_manual(values=c("#007c83","#cb6632"))+labs(title="Observed consumption while connected",x="Year",y="kWh per connected month")
plot_event <- ggplot(evc,aes(Year,estimate))+geom_hline(yintercept=0,color="grey60",linetype=2)+geom_errorbar(aes(ymin=lower95,ymax=upper95),width=.12,color="#007c83")+geom_line(color="#007c83")+geom_point(color="#007c83")+labs(title="Event study relative to 2018",x="Year",y="Log kWh difference")
plot_connection <- ggplot(annual,aes(year,connected*100,color=Utility))+geom_line(linewidth=.9)+geom_point()+scale_color_manual(values=c("#007c83","#cb6632"))+labs(title="Connection rates in the complete panel grid",x="Year",y="Connected (%)")
rb[,Utility:=ifelse(distance>0,"PG&E","SCE")]
plot_border <- ggplot(rb,aes(distance,log_kwh,color=Utility))+geom_line()+geom_point()+scale_color_manual(values=c("#007c83","#cb6632"))+labs(title="Usage near the border in 2018",x="Signed distance (degrees)",y="Mean log kWh")
message("Writing summary and run metadata")
summary <- c("# PG&E residential pricing reform","",sprintf("Reduce the standard marginal price from $0.22 to **$0.18/kWh** and retain CARE’s one-third discount, yielding **$0.12/kWh**. Set CARE’s fixed charge at **$6/month**. Non-CARE monthly fees are **%s**, **%s**, and **%s** for tract median incomes below $75,000, $75,000–below $125,000, and at least $125,000. These bands are policy assumptions. The reduction closes 28.6%% of the gap to the $0.08/kWh social marginal cost benchmark.",usd(result$lambda*.65),usd(result$lambda),usd(result$lambda*1.4)),"",sprintf("The 2019 PG&E price-cut comparison with SCE estimates elasticity at **%.3f** (95%% county-clustered normal interval **%.3f to %.3f**). Household and year-month effects and weather controls cover 2015–2020. Elasticity is the DiD coefficient divided by log(0.22/0.25). CARE is not used for causal identification. The pre-trend test has p = %.3f. **The fixed-charge placebo fails**: its estimate is %.4f log points (95%% interval %.4f–%.4f). A border CARE-enrollment imbalance further limits validation. Projections are conditional scenarios, not validated causal forecasts.",elasticity,elo,ehi,wp,placebo$beta[1],placebo$table$lower95[1],placebo$table$upper95[1]),"",sprintf("For **%s PG&E accounts and %s connected months observed in 2023**, the proposal:",fmt(N,0),fmt(M,0)),"",sprintf("- Preserves sample revenue at **%s**, using full-precision fees and financing CARE support within PG&E.",usd(result$baseline_revenue)),sprintf("- Raises consumption by **%s kWh (%.2f%%)**.",fmt(result$delta_kwh,0),result$kwh_pct),sprintf("- Raises net consumer surplus by **%s**, or **%s per connected month**.",usd(result$CS),usd(result$CS/M)),sprintf("- Adds **%s metric tonnes CO2e** and **%s** of climate damage under the historical EPA benchmark (2020 dollars).",fmt(result$delta_tonnes,1),usd(result$climate_damage)),"",sprintf("Average monthly bills change by **%s for CARE** and **%s for non-CARE** accounts. Low-use households can lose within each income band; tract income is an imperfect affordability proxy.",usd(care[CARE==1,bill_change]),usd(care[CARE==0,bill_change])),"",sprintf("**Equal revenue is not equal profit.** At the illustrative private-cost residual of $%.5f/kWh, sample profit falls %s. A separate profit-neutral calibration requires non-CARE fees of %s, %s, and %s, collecting %s more revenue. Actual profit neutrality requires private cost data; cent-rounded fees require reconciliation.",private_cost,usd(-result$profit_change_proxy),usd(rp$lambda*.65),usd(rp$lambda),usd(rp$lambda*1.4),usd(rp$new_revenue-rp$baseline_revenue)),"","The design improves consumption efficiency and CARE affordability while increasing near-term grid emissions. EV and heat-pump benefits are not estimated. Synthetic data, constant elasticity, unchanged connections, and an unknown population expansion factor limit interpretation. All totals refer to the sample. [Full report and sources](index.html).")
writeLines(summary,file.path(OUT_DIR,"RESULTS_SUMMARY.md"))
metadata <- list(engine="R",R=R.version.string,packages=sapply(c("data.table","fixest","ggplot2","rmarkdown","knitr"),function(p)as.character(packageVersion(p))),input_md5=as.list(tools::md5sum(list.files(DATA_DIR,pattern="csv$",full.names=TRUE))),primary=result,elasticity_CI=c(elo,ehi),profit_neutral_proxy=rp,pretrend_p=wp,rounding_revenue_gap=round_gap,simulation_households=N,simulation_months=M,imputed_spikes_2023=imputed,outlier_threshold_counts=setNames(lapply(c(10,20,50,100),function(v)sum(b$kwh/b$median_kwh>v,na.rm=TRUE)),c("10","20","50","100")))
jsonlite::write_json(metadata,file.path(OUT_DIR,"run_metadata.json"),pretty=TRUE,auto_unbox=TRUE,digits=16)
writeLines(capture.output(sessionInfo()),file.path(OUT_DIR,"sessionInfo.txt"))
