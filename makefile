GHC=ghc
LATEX=latex
FLAGS=--make

all: haskword report

haskword: Haskword.hs
	${GHC} ${FLAGS} Haskword.hs

report: report.tex
	${LATEX} report
	${LATEX} report
	dvipdf report.dvi

clean: 
	rm -rf Haskword report.pdf *.hi *.o *.aux *.out *.blg *.log *.dvi
