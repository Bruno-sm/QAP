SRC=src
DATA=data
RES=res
REPETITIONS=1
TABUITER=1000


all-results: $(RES) greedy local-search-bf local-search-bn local-search-vnd stm-tabu-search 

.DEFAULT: $(RES) compile
	racket $(SRC)/qap.rkt -r $(REPETITIONS) --csv-output --$@ $(DATA)/*.dat > $(RES)/$@.csv
	xsv select Case,values_mean,values_deviation,cpu_ms $(RES)/$@.csv > $(RES)/$@-abridged.csv
	echo "deviation_mean,cpu_ms_mean" > $(RES)/$@-nutshell.csv
	xsv select values_deviation,cpu_ms $(RES)/$@.csv | tail -n `xsv count $(RES)/$@.csv` | awk -F , 'BEGIN{n='`xsv count $(RES)/$@.csv`'}{val_sum+=$$1; cpu_sum+=$$2}END{print val_sum/n","cpu_sum/n}' >> $(RES)/$@-nutshell.csv

$(RES):
	mkdir $(RES)

compile: $(SRC)/qap.rkt
	raco make $(SRC)/qap.rkt
