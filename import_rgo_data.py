import sqlite3 as sqlite
import datetime
from os import path
import filecmp

"""
#https://solarscience.msfc.nasa.gov/greenwch.shtml

Project global variables here:
Columns Quantity
1-4     Year
5-6     Month
7-8     Day of month
9-12    Time in thousandths of day (.500 = 1200 UT)
13-20   Greenwich sunspot group # through 1976; NOAA/USAF grp # after 1976

21-22   00 1874-1981
23-24   Greenwich Group type

21      Suffix to group number 1982 to present
22-24   NOAA Group type (A=Alpha, B=Beta, D=Delta, G=Gamma) 1982 to present

25      Single space
26-29   Observed umbral area in millionths of solar disk 1874 through 1981

26-29      0 from 1982 to present (umbral area unavailable from NOAA)

30      Single space
31-34   Observed whole spot area in mill. of sol. disk
35      Single space
36-39   Corrected (for foreshortening) umbral area in millionths of solar
        hemisphere, 1874 through 1981

36-39       0 from 1982 to present (umbral area unavailable from NOAA)
40      Single space
41-44   Corrected whole spot area in millionths of solar hemisphere
45      Single space
46-50   Distance from center of solar disk in disk radii
51      Single space
52-56   Position angle from heliographic north (0=north, 90=east limb)
57      Single space
58-62   Carrington Longitude in degrees
63      Single space
64-68   Latitude, negative to the South
69      Single space
70-74   Central  meridian distance, negative to the East.
"""
project_path = 'C:/Iakovos_RGO_data/'
to_import_folder = path.join(project_path, 'to_import')
print to_import_folder
imported_folder = path.join(project_path, 'imported')
database_name = 'RGO.sqlite'
database = sqlite.connect(path.join(project_path, database_name))
cursor = database.cursor()
table_name = "sunspots"

class DataLine(object): 
    __slots__ = ['date','time','csgcgt','noaa','oua','owsa','cua','cwsa','dcsd','pahn','cld','lns','cmd'] 
    """ 
    Represents the data contained in a dataline. 
    Includes the date, time, and RGO sunspot data. 
    """ 
    def __init__(self, line, filename): 
        super(DataLine, self).__init__() 
        indices = ((0,4), (4,6), (6,8), (8,12), (12,20), (20,21), (21,24), (25,29), (30,34), (35,39), (40,44), (45,50), (51,56), (57,62), (63,68), (69,74)) 
        v = [line[x1:x2] for (x1, x2) in indices] 
        year, month, day, time, csgcgt, suf, noaa, oua, owsa, cua, cwsa, dcsd, pahn, cld, lns, cmd = v 
        self.date = '%s-%02d-%02d' %(year.strip(), int(month), int(day)) 
        self.time = time.strip() 
        self.csgcgt = csgcgt.strip() 
        self.noaa = noaa.strip() 
        self.oua = oua.strip() 
        self.owsa = owsa.strip() 
        self.cua = cua.strip() 
        self.cwsa = cwsa.strip() 
        self.dcsd = dcsd.strip() 
        self.pahn = pahn.strip() 
        self.cld = cld.strip() 
        self.lns = lns.strip() 
        self.cmd = cmd.strip() 
                   
    def insert_into_db(self, database):
        sql = "insert into %s values ('%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s');" % (table_name, self.date, self.time, self.csgcgt, self.noaa, self.oua, self.owsa, self.cua, self.cwsa, self.dcsd, self.pahn, self.cld, self.lns, self.cmd)
        database.execute(sql)


def setup_db():
    try:
        sql = "create table %s (date text, time text, csgcgt text, noaa text, oua text, owsa text, cua text, cwsa text, dcsd text, pahn text, cld text, lns text, cmd text)" % (table_name)
        database.execute(sql)
        database.commit()
        print "... table created..."
    except:
        print "... table already exists..."


def import_file(filename):
    s = 0; f = 0
    print "... importing %s..." %(filename)
    print path.join(to_import_folder, filename)
    with open(path.join(to_import_folder, filename)) as data_file:
        for line in data_file:
            try:
                data_point = DataLine(line.rstrip('\n\r'), filename)
                data_point.insert_into_db(database)
                s += 1
            except:
                f += 1
                print "... %s " %(line)
    
    s += 1
    database.commit()
    return (s, f)


"""
Get unique (new) files from the to_import_folder 
Try setting up the database in case not previously created.
Import each file by iterating over lines, creating a DataLine object for each line,
and calling the object to insert itself into the database.
"""
files = filecmp.dircmp(to_import_folder, imported_folder).left_only
print files
setup_db()

success = 0
failures = 0

start_time = datetime.datetime.now()
for f in files:
    (suc, fail) = import_file(f)
    success += suc
    failures += fail

end_time = datetime.datetime.now()
time_to_import = end_time - start_time
print "Time to import = %d seconds" % (time_to_import.seconds)
print "Success = %d, Failures = %d, ratio = %.7f" %(success, failures, float(failures)/ (float(success + failures)+1))

