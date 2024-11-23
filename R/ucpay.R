#
# I had a reason to search this database to find people with a particular title
# so I could find those people in our PRM system.
# Specifically, given the name of somebody, we could find their record in the UC wage database,
# find the title there and then use that as the title search term to find all people with that title
# and then take their names and find them in PRM and then find their actual UCPath  title codes.
#
#
#

if(FALSE) {

    z = ucpay(title = "PROF OF CLIN-SFT-VM")
    z = ucpay("Temple Lang")
    z = ucpay(maxNum = 3000)
}


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
         year = 2022, maxNum = NA, nrows = 1000, nd = now(),
         useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:132.0) Gecko/20100101 Firefox/132.0",
         ...)
{
    location = match.arg(location, names(CampusNames))

#    u = "https://ucannualwage.ucop.edu/wage/search.action"
    u = "https://ucannualwage.ucop.edu/wage/search.do"    
    params = list(`_search` = "false", nd = gsub("\\.[0-9]+$", "", as.character(nd)), rows = nrows, page = 1, 
                  sidx = "EAW_LST_NAM", sord = "asc", year = as.character(year), location = unname(CampusNames[location]), 
                  firstname = "", lastname = "", title = "", startSal = "", 
                  endSal = "")

    if(!is.na(name)) {
        if(length(name) == 1)
            name = c(name, "")
        
        params[ c("lastname", "firstname") ] = name
    }

    if(!is.na(title))
        params$title = title

    ans = list()
    # page through the results.    
    while(TRUE) {

        js = httpPOST(u, postfields = paste(names(params), params, sep = "=", collapse = "&"), ..., useragent = useragent)
        js = gsub("'", '"', js)
        tmp = fromJSON(js)

        ans = c(ans, tmp$rows)
        nr = as.integer(tmp$records)
        if(length(ans) >= nr || (!is.na(maxNum) && length(ans) >= maxNum))
            break

        params$page = params$page + 1L
    }

    mkDF(ans)
}


mkDF =
function(ans)
{

    if(length(ans) == 0)
       return(NULL) # or a df with no rows but the correct columns.
    
    vars = c("Index", "Year", "Location", "FirstName", "LastName", "Title", "GrossPay", "RegularPay", "OvertimePay", "OtherPay")
    if(length(ans) == 1) 
        df = as.data.frame(as.list(ans[[1]]$cell))
    else 
        df = as.data.frame(t(sapply(ans, function(x) x$cell)))

   

    names(df) = vars
    numVars = c("Year", "GrossPay", "RegularPay", "OvertimePay", "OtherPay")
    df[numVars] = lapply(df[numVars], as.numeric)

#NO Gross    df$TotalPay = Reduce(`+`, df[ grep("Pay", names(df))])
    df
}
