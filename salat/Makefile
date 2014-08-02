TDIR = test/suite
TEXDIR = src
SCRATCHDIR = scratchpad
BNCHDIR = test/bench
SALATDIR = src/salat

ALL = $(TEXDIR)/Text/LaTeX.hs \
      $(TEXDIR)/Text/LaTeX/Base.hs \
      $(TEXDIR)/Text/LaTeX/Base/Class.hs \
      $(TEXDIR)/Text/LaTeX/Base/Commands.hs \
      $(TEXDIR)/Text/LaTeX/Base/Parser.hs \
      $(TEXDIR)/Text/LaTeX/Base/Render.hs \
      $(TEXDIR)/Text/LaTeX/Base/Syntax.hs \
      $(TEXDIR)/Text/LaTeX/Base/Types.hs \
      $(TEXDIR)/Text/LaTeX/Base/ConduitParser2.hs \
      $(TEXDIR)/Text/LaTeX/Base/Warnings.hs \
      $(TEXDIR)/Text/LaTeX/Base/Writer.hs 

SALL =	$(SALATDIR)/salat.hs \
	$(SALATDIR)/Rules.hs 
	# $(SALATDIR)/ConduitParser3.hs

test:		$(TDIR)/tparser
smoke:		$(TDIR)/smoke
samples:	$(TDIR)/samples
run:		test samples
		$(TDIR)/tparser 10000
		$(TDIR)/samples

SRCDIR = src/Text/LaTeX/Base

GHC = ghc
FLGS = -D_TEST -Wall --make -rtsopts -O2
INC  = -i./src 
SALATINC = $(INC) -i./src/salat

runsam:	samples
	$(TDIR)/samples

parse:	$(TDIR)/parse

$(TDIR)/math:	$(TDIR)/math.hs $(ALL)
		$(GHC) $(FLGS) $(INC) $@

$(TDIR)/tparser:	$(TDIR)/tparser.hs $(ALL)
			$(GHC) $(FLGS) $(INC) $@

$(TDIR)/samples:	$(TDIR)/samples.hs $(ALL)
			$(GHC) $(FLGS) $(INC) $(SALATINC) $@

$(TDIR)/smoke:	$(TDIR)/smoke.hs $(ALL)
		$(GHC) $(FLGS) $(INC) $@

$(TDIR)/parse:	$(TDIR)/parse.hs $(ALL)
		$(GHC) $(FLGS) $(INC) $@

salat:	$(SALATDIR)/salat

$(SALATDIR)/salat:	$(ALL) $(SALL)
			$(GHC) $(FLGS) $(SALATINC) $@
			strip $@

install:	salat
		sudo cp $(SALATDIR)/salat /usr/local/bin/


clean:
	rm -f $(TDIR)/*.hi
	rm -f $(TDIR)/*.o
	rm -f $(SCRATCHDIR)/*.hi
	rm -f $(SCRATCHDIR)/*.o
	rm -f $(SCRATCHDIR)/salat5
	rm -f $(SRCDIR)/*.hi
	rm -f $(SRCDIR)/*.o
	rm -f $(SALATDIR)/*.hi
	rm -f $(SALATDIR)/*.o
	rm -f $(SALATDIR)/salat
	rm -f test/bin/*.o
	rm -f test/bin/*.hi
	rm -f test/suite/*.hi
	rm -f test/suite/*.o
	rm -f test/suite/samples
	rm -f test/suite/smoke
	rm -f test/suite/tparser
	rm -f test/tex/out/*.tex

build: $(tests) $(SUBSRC)
