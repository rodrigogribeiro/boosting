#!/bin/bash

progs="escritas leituras"

maximos=(200 60)
minimos=(0.1 0.1)
index=0
if [ "$1" == "l" ]; then
	for prog in $progs
	do
	max=${maximos[$index]}
	min=${minimos[$index]}
	echo "Barras - $prog - $max"
	echo "set terminal png size 720,540 enhanced font 'Verdana,10'
		  set output '$prog.png'
		  set title \"SET\"
		  set datafile missing \"-\"
		  set xtics nomirror rotate by -45
		  set key noenhanced
		  set logscale y
		  set yrange [$min:$max]
		  set ylabel \"Time (sec)\"
		  set xlabel \"cores\"
		  #set style data linespoints
		  set style line 1 lc rgb '#0060ad' lt 1 lw 2 pt 7 ps 1.5   # --- blue
		  set style line 2 lc rgb '#dd181f' lt 1 lw 2 pt 5 ps 1.5   # --- red
		  plot 'data/$prog.dat' using 2:xtic(1) title columnheader(2) with linespoints ls 1, for [i=3:3] '' using i title columnheader(i) with linespoints ls 2
		  " | gnuplot
	index=$index+1
	done
else
	if [ "$1" == "b" ]; then
		for prog in $progs
		do
		max=${maximos[$index]}
		min=${minimos[$index]}
		echo "Barras - $prog - $max"
		echo "set terminal png size 640,480 enhanced font 'Verdana,10'
			  set output '$prog.png'
			  set title \"$prog\"
			  #set auto x
			  set logscale y
			  set yrange [$min:$max]
			  set ylabel \"Time (sec)\"
			  set xlabel \"cores\"
			  set style data histogram 
			  set style histogram cluster gap 1
			  set style fill solid border -1
			  set boxwidth 0.9
			  set xtic rotate by -45 scale 0 font \",8\"
			  #set bmargin 5
			  plot 'data/$prog.dat' using 2:xtic(1) ti col, '' u 3 ti col
			  " | gnuplot
		index=$index+1
		done
	else
		echo "Erro, opção invalida: "
	fi
fi
