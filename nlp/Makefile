TDIR = test/suite
SMOKEDIR = test/smoke
COMDIR = src/common
BNCHDIR = src/test/bench

tests = $(TDIR)/tstRake

SRCDIR = src
SUTDIR = src/NLP/RAKE

SUBSRC = $(SUTDIR)/Text.hs \
	 $(SUTDIR)/Stopwords.hs

GHC = ghc
FLGS = -DTEST -Wall -rtsopts -prof -auto-all -caf-all -Ltest/suite --make 
INC  = -i./src -i$(SUTDIR) -itest/suite

smoke:	$(SMOKEDIR)/test1 \
	$(SMOKEDIR)/test2 \
	$(SMOKEDIR)/test3 \
	$(SMOKEDIR)/try

suite:	$(TDIR)/tstRake

mem:	$(TDIR)/mem1

$(TDIR)/tstRake:	$(TDIR)/tstRake.hs $(SUBSRC)
			$(GHC) $(FLGS) $(INC) $@

$(TDIR)/mem1:		$(TDIR)/mem1.hs $(SUBSRC)
			$(GHC) $(FLGS) $(INC) $@ 

$(SMOKEDIR)/try:	$(SMOKEDIR)/try.hs $(SUBSRC)
			$(GHC) $(FLGS) $(INC) $@

$(SMOKEDIR)/test1:	$(SMOKEDIR)/test1.hs $(SUBSRC)
			$(GHC) $(FLGS) $(INC) $@

$(SMOKEDIR)/test2:	$(SMOKEDIR)/test2.hs $(SUBSRC)
			$(GHC) $(FLGS) $(INC) $@

$(SMOKEDIR)/test3:	$(SMOKEDIR)/test3.hs $(SUBSRC)
			$(GHC) $(FLGS) $(INC) $@

run:	$(suite)
	$(TDIR)/tstRake

clean:
	rm -f $(TDIR)/*.hi
	rm -f $(TDIR)/*.o
	rm -f $(TDIR)/tstRake 
	rm -f $(TDIR)/mem1
	rm -f $(SMOKEDIR)/*.hi
	rm -f $(SMOKEDIR)/*.o
	rm -f $(SUTDIR)/*.hi
	rm -f $(SUTDIR)/*.o
	rm -f test/bin/*.o
	rm -f test/bin/*.hi
	rm -f test/smoke/test1
	rm -f test/smoke/test2
	rm -f test/smoke/test3
	rm -f test/smoke/try
	rm -f *.hp 
	rm -f *.aux 
	rm -f *.prof 
	rm -f *.ps 

build: $(tests) $(SUBSRC)
