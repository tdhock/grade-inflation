library(data.table)
library(ggplot2)
html.file.vec <- Sys.glob("data/*.htm")
dist.dt.list <- list()
for(html.file in html.file.vec){
  meta <- nc::capture_first_vec(
    html.file, "/", semester=".*?", year="[0-9]+", as.integer)
  dist.dt.list[[html.file]] <- data.table(meta, htmltab::htmltab(html.file))
}
dist.dt <- do.call(rbind, dist.dt.list)
nc::capture_first_df(dist.dt, Class=list(
  "CS ", level=".", function(x)paste0(x, "xx")))
dist.dt[, category := ifelse(year <= 19, "18+19", "20+21")]
dist.tall <- nc::capture_melt_single(
  dist.dt,
  grade="^[A-FW]$",
  value.name="students.chr")
dist.tall[, students := as.integer(students.chr)]
dist.totals <- dist.tall[, .(
  total.students=sum(students)
), by=.(year, level, grade)]
dist.totals[, percent.students := 100*total.students/sum(total.students), by=.(year, level)]

gg <- ggplot()+
  geom_bar(aes(
    grade, percent.students),
    stat="identity",
    data=dist.totals)+
  geom_label(aes(
    grade, percent.students, label=round(percent.students)),
    stat="identity",
    alpha=0.75,
    data=dist.totals)+
  facet_grid(level ~ year)
png("figure-before-after-level-percent.png", width=12, height=8, units="in", res=200)
print(gg)
dev.off()

dist.totals[, next.year := year+1]
diff.dt <- dist.totals[
  dist.totals, on=.(next.year=year, grade, level), nomatch=0L]
diff.dt[, show.years := paste0(next.year,"-",year)]
diff.dt[, diff.percent := i.percent.students-percent.students]
gg <- ggplot()+
  geom_bar(aes(
    grade, diff.percent),
    stat="identity",
    data=diff.dt)+
  geom_label(aes(
    grade, diff.percent, label=round(diff.percent,1)),
    stat="identity",
    alpha=0.75,
    data=diff.dt)+
  facet_grid(level ~ show.years)
png("figure-before-after-level-percent-diff.png", width=12, height=8, units="in", res=200)
print(gg)
dev.off()
