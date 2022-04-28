library(lubridate)

#oracle date
dmy("31-Jan-2020")
dmy("29-Feb-2020")
dmy("31-Mar-2020")
dmy("30-Apr-2020")
dmy("31-May-2020")
dmy("30-Jun-2020")
dmy("31-Jul-2020")
dmy("31-Aug-2020")
dmy("30-Sep-2020")
dmy("31-Oct-2020")
dmy("30-Nov-2020")
dmy("31-Dec-2020")

#iso8601
ymd("2022-04-27")
ymd_hms("2022-04-27T19:45:22+00:00")
ymd_hms("2022-04-27T19:45:22Z")
ymd_hms("20220427T194522Z")
ymd_hms("2022-04-27 19:45:22")

#quarter
yq("2021 Q1")
yq("2021 Q2")
yq("2021 Q3")
yq("2021 Q4")

yq("2021 Q3") - days(1)

#now
now()
today()
now("UTC")
dt = now("UTC");dt
dte = today("UTC");dte

#get set
update(dt, year=1900)

#round
rollforward(dt)
rollforward(dte)

#date diff
dt + minutes(30)
dte + minutes(30)
dte + days(10)

#physical duration
dt + dminutes(30)
dte + dminutes(30)
dte + ddays(10)



