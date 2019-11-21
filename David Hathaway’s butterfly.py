
#http://solarscience.msfc.nasa.gov/greenwch/bflydata.txt
# by Gary Richardson

class Butterfly:
	def __init__(self, inFileName, tempFileName, outFileName):
		# The tempFIleName and outFileName are prefex only.

		self.inFileName = inFileName
		self.tempFile1Name = "%s1.tmp" % (tempFileName,)
		self.tempFile2Name =  "%s2.tmp" % (tempFileName,)
		self.tempFile3Name =  "%s3.tmp" % (tempFileName,)
		self.outFile1Name = "%s1.dat" % (outFileName,)
		self.outFile2Name =  "%s2.dat" % (outFileName,)
		self.outFile3Name =  "%s3.dat" % (outFileName,)

	def scan(self, th2, th3):
		infile = file(self.inFileName)
		tempfile1 = file(self.tempFile1Name, 'w')
		tempfile2 = file(self.tempFile2Name, 'w')
		tempfile3 = file(self.tempFile3Name, 'w')
		try:
			deltaLat = 180.0 / 49
			while 1:
				L = infile.next()
				if not L or (len(L) < 3):
					break
				carnum = int(L.strip())
				areas = []
				for j in range(5):
					L = infile.next()
					L = L[:-2]				# get rid of trailing comma and newline
					L = L.replace(' ', '').split(',')
					areas.extend(L)
				latitude = -90.0
				for n in range(50):
					a = int(areas[n])
					if a > 0:
						latitude = (n * deltaLat) - 90.0
						if a > th3:
							tempfile3.write('%6d %8.2f %d\n' % (carnum, latitude, a))
						elif a > th2:
							tempfile2.write('%6d %8.2f %d\n' % (carnum, latitude, a))
						else:
							tempfile1.write('%6d %8.2f %d\n' % (carnum, latitude, a))
		except StopIteration:
			pass
		infile.close()
		tempfile1.close()
		tempfile2.close()
		tempfile3.close()

	def generatePlotData(self, tempFileName, outFileName, boxHeight, boxWidthSF):
		tempfile = file(tempFileName)
		outfile = file(outFileName, 'w')
		boxHalfHeight = boxHeight / 2.0
		try:
			while 1:
				tmp = tempfile.next()
				carnum, latitude, a = tmp.split()
				carnum = int(carnum)
				latitude = float(latitude)
				dx = int(a) * boxWidthSF
				x, y = carnum, latitude
				xlow, xhigh = carnum - dx, carnum + dx
				ylow, yhigh = latitude - boxHalfHeight, latitude + boxHalfHeight
				if carnum > 300:
						tmp = '%6d  %8.3f  %8.2f %8.2f %8.2f %8.2f\n' % (x, y, xlow, xhigh, ylow, yhigh)	
				#tmp = '%6d  %8.3f  %8.2f %8.2f %8.2f %8.2f\n' % (x, y, xlow, xhigh, ylow, yhigh)				
				#if carnum >= 2000:                                                                
				outfile.write(tmp)
		except StopIteration:
			pass
		tempfile.close()
		outfile.close()
		
b = Butterfly("bflyData.txt", "bflyTemp", "bflyPlot")
th2 = 100							# 0.1 % ?
th3 = 400							# 1.0 %?
boxWidthSF = 0.001			# ??
boxHeight = 180.0 / 50.0
b.scan(th2, th3 )
b.generatePlotData(b.tempFile1Name, b.outFile1Name, boxHeight, boxWidthSF)
b.generatePlotData(b.tempFile2Name, b.outFile2Name, boxHeight, boxWidthSF)
b.generatePlotData(b.tempFile3Name, b.outFile3Name, boxHeight, boxWidthSF)

