

set ylabel "Latitude"
set xlabel "Carrington Number"
set yrange [-90:90]
set style fill solid
set key top left
plot "bflyPlot1.dat" u 1:2:3:4:5:6 w boxxyerrorbars lc rgbcolor  "grey100" ,\
 "bflyPlot2.dat" u 1:2:3:4:5:6 w boxxyerrorbars lc rgbcolor  "red" ,\
 "bflyPlot3.dat" u 1:2:3:4:5:6 w boxxyerrorbars lc rgbcolor  "yellow"