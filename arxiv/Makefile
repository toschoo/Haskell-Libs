TDIR = test/suite
SMOKEDIR = test/smoke
COMDIR = src/common
BNCHDIR = src/test/bench

tests = $(TDIR)/tstArxiv

SRCDIR = src
SUTDIR = src/Network/Api

SUBSRC = $(SUTDIR)/Arxiv.hs 

GHC = ghc
FLGS = -DTEST -Wall --make
INC  = -i./src -i$(SUTDIR) -itest/suite

smoke:	$(SMOKEDIR)/query1 \
	$(SMOKEDIR)/result1 \
	$(SMOKEDIR)/arxiv

$(SMOKEDIR)/op:	$(SMOKEDIR)/op.hs $(SUBSRC)
		$(GHC) $(FLGS) $(INC) $@

suite:	$(TDIR)/tstArxiv

$(TDIR)/tstArxiv:	$(TDIR)/tstArxiv.hs $(SUBSRC)
			$(GHC) $(FLGS) $(INC) $@

$(SMOKEDIR)/arxiv:	$(SMOKEDIR)/arxiv.hs $(SUBSRC)
			$(GHC) $(FLGS) $(INC) $@

$(SMOKEDIR)/query1:	$(SMOKEDIR)/query1.hs $(SUBSRC)
			$(GHC) $(FLGS) $(INC) $@

$(SMOKEDIR)/result1:	$(SMOKEDIR)/result1.hs $(SUBSRC)
			$(GHC) $(FLGS) $(INC) $@

run:	$(suite)
	$(TDIR)/tstArxiv

clean:
	rm -f $(TDIR)/*.hi
	rm -f $(TDIR)/*.o
	rm -f $(TDIR)/tstArxiv
	rm -f $(SMOKEDIR)/*.hi
	rm -f $(SMOKEDIR)/*.o
	rm -f $(SUTDIR)/*.hi
	rm -f $(SUTDIR)/*.o
	rm -f test/bin/*.o
	rm -f test/bin/*.hi
	rm -f test/smoke/query1
	rm -f test/smoke/arxiv
	rm -f test/smoke/result1
	rm -f tmp/*.pdf

build: $(tests) $(SUBSRC)
