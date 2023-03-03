TDIR = test/suite
SMOKEDIR = test/smoke
COMDIR = src/common
BNCHDIR = src/test/bench

SRCDIR = src
SUTDIR = src/Algorithms

SUBSRC = $(SUTDIR)/Types.hs 

GHC = ghc
FLGS = -DTEST -Wall -rtsopts -Ltest/suite --make # -prof -auto-all -caf-all -
INC  = -i./src -i$(SUTDIR) -itest/suite -i$(SMOKEDIR)

smoke:	$(SMOKEDIR)/qknight \
        $(SMOKEDIR)/dijkstra1

qknight: $(SMOKEDIR)/qknight

suite:	$(TDIR)/tstAlTypes

$(TDIR)/tstAlTypes:	$(TDIR)/tstAlTypes.hs $(SUBSRC)
			$(GHC) $(FLGS) $(INC) $@

$(SMOKEDIR)/qknight:	$(SMOKEDIR)/qknight.hs $(SMOKEDIR)/KnightAndQueen.hs $(SUTDIR)/Dijkstra.hs
			$(GHC) $(FLGS) $(INC) $@

$(SMOKEDIR)/dijkstra1:	$(SMOKEDIR)/dijkstra1.hs $(SUTDIR)/Dijkstra.hs
			$(GHC) $(FLGS) $(INC) $@

$(TDIR)/Spec:	$(TDIR)/Spec.hs
		$(GHC) $(FLGS) $(INC) $@

run:	$(suite) $(SUBSRC)
	$(TDIR)/tstAlTypes

clean:
	rm -f $(TDIR)/*.hi
	rm -f $(TDIR)/*.o
	rm -f $(SMOKEDIR)/qknight
	rm -f $(SMOKEDIR)/dijkstra1
	rm -f $(SMOKEDIR)/*.hi
	rm -f $(SMOKEDIR)/*.o
	rm -f $(TDIR)/Spec
	rm -f $(SUTDIR)/*.hi
	rm -f $(SUTDIR)/*.o
	rm -f test/bin/*.o
	rm -f test/bin/*.hi
	rm -f tmp/*.pdf
	rm -f *.hp 
	rm -f *.aux 
	rm -f *.prof 
	rm -f *.ps 

build: $(tests) $(SUBSRC)
