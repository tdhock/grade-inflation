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
dist.dt[, category := ifelse(year <= 19, "18+19", "20+21")]
dist.tall <- nc::capture_melt_single(
  dist.dt,
  grade="^[A-F]$",
  value.name="students.chr")
dist.tall[, students := as.integer(students.chr)]
dist.totals <- dist.tall[, .(
  total.students=sum(students)
), by=.(year, grade)]
dist.totals[, prop.students := total.students/sum(total.students), by=year]

gg <- ggplot()+
  geom_bar(aes(
    grade, prop.students),
    stat="identity",
    data=dist.totals)+
  facet_grid(. ~ year)
png("figure-before-after-prop.png", width=6, height=4, units="in", res=200)
print(gg)
dev.off()

dist.totals[, next.year := year+1]
diff.dt <- dist.totals[dist.totals, on=.(next.year=year, grade), nomatch=0L]
diff.dt[, show.years := paste0(next.year,"-",year)]
diff.dt[, diff.prop := i.prop.students-prop.students]

gg <- ggplot()+
  geom_bar(aes(
    grade, diff.prop),
    stat="identity",
    data=diff.dt)+
  facet_grid(. ~ show.years)
png("figure-before-after-prop-diff.png", width=6, height=4, units="in", res=200)
print(gg)
dev.off()
