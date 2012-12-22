figures  := LC transline cpb\
 extjc jc supersplit butterfly wolverine \
 blochspheretoymodel linecuts4pan \
 hetvsphtcnt quadratures quadratureshipower lnrvsnonlnr \
 expschematic expsample expvrabimap \
 ghz0 ghz1 ghz2 ghz3 ghz4 ghz5 \
 tiecirct mmpurcell transcouple transintransline \
 nodissip thermal rau

pfigures := dispersion wavefunctions dispersionng

.PHONY: pdf eps default clean mostlyclean merged.tex figinput.tex
default: pdf

expsample-conv-uncrop.pdf: figexpsample.eps
	ps2pdf.exe $< $@	


pdfiles := $(figures:=-conv.pdf) $(pfigures:=-conv.pdf)
epsfiles := $(figures:=-conv.eps) $(pfigures:=-conv.eps)
epsgzfiles := $(figures:=-conv.eps.bb) $(figures:=-conv.eps.gz) \
	$(pfigures:=-conv.eps.bb) $(pfigures:=-conv.eps.gz)

intermediates:=$(figures:%=fig%.ps) $(figures:%=fig%.aux) \
$(figures:%=fig%.log) $(figures:%=%-conv-uncrop.pdf) \
$(figures:%=fig%.dvi) $(figures:%=fig%.eps) \
$(figures:%=%-conv-nobb.eps) $(pfigures:=-conv-uncrop.pdf) \
$(pfigures:=-conv-uncrop.log) $(pfigures:=-conv-uncrop.aux) \
texpreamble.fmt texpreamble.log merged.ps merged.dvi merged.aux \
merged.log merged.tex merged.sv?

.DELETE_ON_ERROR: intermediates

view: merged.pdf
	cygstart $<

comp: merged-pso.pdf
	cygstart $<

.SECONDARY: $(intermediates)

merged: merged.pdf;
pdf: $(pdfiles) ;
eps: $(epsfiles) ;
egz: $(epsgzfiles) ;

merged.tex:
	echo > $@  '\documentclass{article}'
	echo >> $@ '\usepackage{graphicx}'
	echo >> $@ '\usepackage[external,subgroups,table]{svn-multi}'
	echo >> $@ '\renewcommand{\svntable}{\begin{tabular}{lrll}\hline}'
	echo >> $@ '\renewcommand{\svntablehead}{Name&Rev&Author&Time\\\hline}'
	echo >> $@ '\svnid{$$Id: Makefile 472 2010-06-19 16:31:34Z lsb $$}'
	echo >> $@ '\svnexternal[pgf]{$(pfigures:%={raw/pgf%.tex})}'
	echo >> $@ '\svnexternal[pfrag]{$(figures:%={raw/fig%.tex})}'
	echo >> $@ '\svnexternal[eps]{$(figures:%={raw/%.eps})}'
	echo >> $@ '\usepackage[active,graphics,tightpage]{preview}'
	echo >> $@ '\setlength\PreviewBorder{0 pt}'
	echo >> $@ '\begin{document}'
	echo >> $@ '\begin{preview}\tableofrevisions\end{preview}'
	for f in $(pdfiles); do echo >> $@ "\includegraphics{$$f}"; done
	echo >> $@ "\end{document}"

SHELL := /bin/bash

figinput.tex:
	echo > $@ '\newcommand{\incfigurefile}{figures/merged.pdf}'
	z=2; for f in $(pdfiles); do echo >> $@ "\\expandafter\\newcommand\\csname $${f}page\\endcsname{$$((z++))}"; done

merged.pdf: merged.tex figinput.tex $(pdfiles)
	pdflatex -quiet -draftmode merged
	svn-multi merged
	pdflatex -quiet -draftmode merged
	pdflatex -quiet merged

merged-pso.pdf: merged.pdf
	scp merged.pdf vm:figs
	ssh vm 'cd figs; python ~/pdfsizeopt/pdfsizeopt.py \
	--use-pngout=true --use-jbig2=true --do-unify-fonts=false \
	--use-multivalent=false merged.pdf'
	# read -p 'ready?'
	scp vm:figs/merged.pso.pdf merged-pso.pdf
	echo >> figinput.tex '\renewcommand{\incfigurefile}{figures/merged-pso.pdf}'
	
mostlyclean:
	-rm -f $(intermediates)

texpreamble.fmt: ../macros.tex figuremacros.tex
	pdftex -quiet -job-name=texpreamble -ini "&latex" \
"\newcommand{\maybedump}{\dump} \input{figuremacros.tex}"

fig%.tex: raw/fig%.tex
	tail -n +2 $< > $@

fig%.dvi: fig%.tex raw/%.eps texpreamble.fmt
	latex -quiet "&texpreamble" $<

fig%.ps: fig%.dvi raw/%.eps
	dvips -q -P pdf -E -e0 fig$*

fig%.eps: fig%.ps
	epstool --quiet --copy -b $< $@	

%-conv-uncrop.pdf: fig%.eps
	ps2pdf.exe -dAutoFilterColorImages\#false  -dColorImageFilter\#/FlateEncode $< $@	
#	ps2pdf $< $@
#	arg. ghostscript no support /PassThroughJPEGImages. Project since 2000
#       Distiller?

%-conv-uncrop.pdf: raw/pgf%.tex
	pdflatex -quiet $< -job-name=$*-conv-uncrop

%-conv.pdf: %-conv-uncrop.pdf
	pdfcrop $< $@

%-conv-nobb.eps: %-conv.pdf
	pdftops -level3 -eps $< $@

%-conv.eps: %-conv-nobb.eps
	sed '2s/^/%/' < $< > $@

%-conv.eps.bb: %-conv.eps
	grep '^%%BoundingBox' $< > $@

%-conv.eps.gz: %-conv.eps
	gzip -f $<

clean: mostlyclean
	-rm $(pdfiles) $(epsfiles) $(epsgzfiles) merged.pdf merged-pso.pdf figinput.tex

