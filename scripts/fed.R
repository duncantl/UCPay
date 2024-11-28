library(UCPay)
c = ucpay(title = "ACADEMIC COORD", year = 2023)
a = ucpay(title = "ACADEMIC ADMINISTRATOR", year = 2023)
fed = rbind(c, a)


fed2 = fed[fed$LastName != "*****",]
fed2$name = paste(fed2$LastName, fed2$FirstName, sep = ", ")


library(PRM)
prm = mkPRMCon()

info = lapply(fed2$name, function(nm) try(prmQueryName(nm, curl = prm)))
names(info) = fed2$name

nm = sapply(info, function(x) x$total)


names(info)[nm == 0]
info = info[nm > 0]
nm = sapply(info, function(x) x$total)

who = names(info)[nm > 1]

orNA = function(x) if(length(x)) x else NA
i = sapply(info[nm > 1], function(x) orNA(grep("academic", sapply(x$records, `[[`, "primaryTitle"), ignore.case = TRUE)))
i["HUGHES, JON"] = 1

info[names(i)] = mapply(function(x, i) { x$records = x$records[i]; x}, info[names(i)], i, SIMPLIFY = FALSE)

# Check
stopifnot(all( sapply(info, function(x) length(x$records)) == 1))

ids = lapply(info, function(x) x$records[[1]]$loginId)


w = sapply(ids, is.null)

roles = lapply(ids, function(id) if(is.null(id)) NULL else try(getPRMInfo(id, curl = prm, asDf = TRUE)))
roles2 = lapply(ids, function(id) if(is.null(id)) NULL else try(getPRMInfo(id, curl = prm, asDf = FALSE)))

roles3 = lapply(roles2, function(x) x$roles[ sapply(x$roles, function(x) x$activeStatusCode) != "INACTIVE" ])
roles4 = roles3[ sapply(roles3, length) > 0 ]

roles5 = lapply(roles4, function(x) unlist(sapply(x, `[[`, "title")))
ok = sapply(roles5, function(x) any(grepl("^academic", x, ignore.case = TRUE)))
table(ok)

m = match( names(roles5)[ok], fed2$name)

fed2$active = FALSE
fed2$active[m] = TRUE

tmp = fed2[ order(fed2$active, decreasing = TRUE), ]

tmp$Title = unlist(tmp$Title)

writexl::write_xlsx(tmp[, c("name", "Title", "active")], "~/Federation_Nov24.xlsx")

#sapply(roles[!w], )
