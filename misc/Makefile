TDIR = test/suite
SMOKEDIR = test/smoke
COMDIR = src/common
BNCHDIR = src/test/bench

tests = $(TDIR)/tstAlTypes

SRCDIR = src
SUTDIR = src/Algebra

SUBSRC = $(SUTDIR)/Types.hs 

GHC = ghc
FLGS = -DTEST -Wall -rtsopts -prof -auto-all -caf-all -Ltest/suite --make 
INC  = -i./src -i$(SUTDIR) -itest/suite

smoke:	$(SMOKEDIR)/query1 \
	$(SMOKEDIR)/query2 \
	$(SMOKEDIR)/result1 \
	$(SMOKEDIR)/arxiv

$(SMOKEDIR)/op:	$(SMOKEDIR)/op.hs $(SUBSRC)
		$(GHC) $(FLGS) $(INC) $@

suite:	$(TDIR)/tstAlTypes

mem:	$(TDIR)/mem1

$(TDIR)/tstAlTypes:	$(TDIR)/tstAlTypes.hs $(SUBSRC)
			$(GHC) $(FLGS) $(INC) $@

# $(TDIR)/mem1:		$(TDIR)/mem1.hs $(SUBSRC)
#			$(GHC) $(FLGS) $(INC) test/suite/phelp.o $@ 

$(TDIR)/mem1:		$(TDIR)/mem1.hs $(SUBSRC)
			$(GHC) $(FLGS) $(INC) $@ 

$(SMOKEDIR)/arxiv:	$(SMOKEDIR)/arxiv.hs $(SUBSRC)
			$(GHC) $(FLGS) $(INC) $@

$(SMOKEDIR)/query1:	$(SMOKEDIR)/query1.hs $(SUBSRC)
			$(GHC) $(FLGS) $(INC) $@

$(SMOKEDIR)/query2:	$(SMOKEDIR)/query2.hs $(SUBSRC)
			$(GHC) $(FLGS) $(INC) $@

$(SMOKEDIR)/result1:	$(SMOKEDIR)/result1.hs $(SUBSRC)
			$(GHC) $(FLGS) $(INC) $@

run:	$(suite) $(SUBSRC)
	$(TDIR)/tstAlTypes

clean:
	rm -f $(TDIR)/*.hi
	rm -f $(TDIR)/*.o
	rm -f $(TDIR)/tstAlTypes
	rm -f $(TDIR)/mem1
	rm -f $(SMOKEDIR)/*.hi
	rm -f $(SMOKEDIR)/*.o
	rm -f $(SUTDIR)/*.hi
	rm -f $(SUTDIR)/*.o
	rm -f test/bin/*.o
	rm -f test/bin/*.hi
	rm -f test/smoke/result1
	rm -f tmp/*.pdf
	rm -f *.hp 
	rm -f *.aux 
	rm -f *.prof 
	rm -f *.ps 

build: $(tests) $(SUBSRC)
