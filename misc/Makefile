TDIR = test/suite
SMOKEDIR = test/smoke
BNCHDIR = src/test/bench

SRCDIR = src
SUTDIR = src/Algorithms

GHC = ghc
FLGS = -DTEST -Wall -rtsopts -Ltest/suite --make
BFLGS = -DTEST -Wall -prof -auto-all -caf-all -rtsopts -Ltest/suite --make
INC  = -i./src -i$(SUTDIR) -itest/suite -i$(SMOKEDIR)

smoke:	$(SMOKEDIR)/qknight \
        $(SMOKEDIR)/dijkstra1

qknight: $(SMOKEDIR)/qknight
dijkstra1: $(SMOKEDIR)/dijkstra1

run:	$(TDIR)/Spec.hs
	runhaskell $(INC) $(TDIR)/Spec.hs

doc:	$(SUTDIR)/Dijkstra.hs
	haddock -h -o ./doc $(SUTDIR)/Dijkstra.hs

$(SMOKEDIR)/qknight:	$(SMOKEDIR)/qknight.hs $(SMOKEDIR)/KnightAndQueen.hs $(SUTDIR)/Dijkstra.hs
			$(GHC) $(FLGS) $(INC) $@

$(SMOKEDIR)/dijkstra1:	$(SMOKEDIR)/dijkstra1.hs $(SUTDIR)/Dijkstra.hs
			$(GHC) $(FLGS) $(INC) $@

$(TDIR)/Spec:	$(TDIR)/Spec.hs
		$(GHC) $(FLGS) $(INC) $@

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
