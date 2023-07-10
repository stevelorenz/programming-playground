import static java.util.Calendar.*

TimeZone.default = TimeZone.getTimeZone('GMT')
def date = new Date(0)
def dateMap = [y:date[YEAR] - 1900, m:date[MONTH], d:date[DAY_OF_MONTH]]
def out = "Year $dateMap.y Month $dateMap.m Day $dateMap.d"
assert out == 'Year 70 Month 0 Day 1'

def tz = TimeZone.getTimeZone('GMT')
def format = 'd MMM YYYY HH:mm:SS z'
out = "Date is ${date.format(format, tz)} !"
assert out == 'Date is 1 Jan 1970 00:00:00 GMT !'

def sql = """
SELECT FROM MyTable
    WHERE Year = $dateMap.y
"""
assert sql == '''
SELECT FROM MyTable
    WHERE Year = 70
'''
