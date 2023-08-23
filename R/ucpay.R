#
# I had a reason to search this database to find people with a particular title
# so I could find those people in our PRM system.
# Specifically, given the name of somebody, we could find their record in the UC wage database,
# find the title there and then use that as the title search term to find all people with that title
# and then take their names and find them in PRM and then find their actual UCPath  title codes.
#
#
#

library(RCurl)
library(RJSONIO)

if(FALSE) {
    library(RHTMLForms)
    library(XML)    
    u = "https://ucannualwage.ucop.edu/wage/"
    doc = htmlParse(getURLContent(u))
    docName(doc) = u
    f = getHTMLFormDescription(doc)
    names(f)
    names(f[[1]])
    names(f[[1]]$elements)

    locs = f[[1]]$elements$location$options
    CampusNames = unclass(locs)
    dput()
    #
    # fun = createFunction(f$sform)
}

now =
function()    
    as.numeric(Sys.time())*1000

# From meta data computations above
CampusNames = c(ALL = "ALL", ASUCLA = "ASUCLA", Berkeley = "Berkeley", Davis = "Davis", 
Hastings = "Hastings College Of Law", Irvine = "Irvine", `Los Angeles` = "Los Angeles", 
Merced = "Merced", Riverside = "Riverside", `San Diego` = "San Diego", 
`San Francisco` = "San Francisco", `Santa Barbara` = "Santa Barbara", 
`Santa Cruz` = "Santa Cruz", UCOP = "UCOP")

ucpay = 
function(name = NA, title = NA, location = "Davis",
         year = 2022, maxNum = NA, nrows = 1000, nd = now(), ...)
{
    location = match.arg(location, names(CampusNames))

    u = "https://ucannualwage.ucop.edu/wage/search.action"
    params = list(`_search` = "false", nd = as.character(nd), rows = nrows, page = 1, 
                  sidx = "EAW_LST_NAM", sord = "asc", year = as.character(year), location = location[], 
                  firstname = "", lastname = "", title = "", startSal = "", 
                  endSal = "")

    if(!is.na(name)) {
        if(length(name) == 1)
            name = c(name, "")
        
        params[ c("lastName", "firstname") ] = name
    }


    nr = 0
    ans = list()
    while(TRUE) {

        js = httpPOST(u, postfields = paste(names(params), params, sep = "=", collapse = "&"), ...)
        js = gsub("'", '"', js)
        tmp = fromJSON(js)

        # page through the results.
#        browser()
        ans = c(ans, tmp$rows)
        nr = as.integer(tmp$records)
        if(length(ans) >= nr || (!is.na(maxNum) && length(ans) >= maxNum))
            break

        params$page = params$page + 1L
    }

    ans
}
