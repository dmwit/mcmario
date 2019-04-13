file = "doubles-no-div.dat"
set terminal x11
plot file using 1:2 title "A", \
     file using 1:3 title "B", \
     file using 1:4 title "C", \
     file using 1:5 title "D", \
     file using 1:6 title "E", \
     file using 1:7 title "F"
pause -1
