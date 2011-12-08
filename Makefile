TDIR = src/test/suite
SMOKEDIR = src/test/smoke
COMDIR = src/common

tests = $(TDIR)/tstIOMap 

SRCDIR = src
SUTDIR = src/Data
COMDIR = src/common

SUBSRC = $(SUTDIR)/IOMap.hs \
         $(COMDIR)/Graph.hs \
         $(COMDIR)/Visual.hs 

GHC = ghc
FLGS = -DTEST -Wall --make
INC  = -i./src -i./src/common -i./src/Data 

smoke:	$(SMOKEDIR)/op 

$(SMOKEDIR)/op:	$(SMOKEDIR)/op.hs $(SUBSRC)
		$(GHC) $(FLGS) $(INC) $@

suite:	$(TDIR)/tstIOMap 

$(TDIR)/tstIOMap:	$(TDIR)/tstIOMap.hs $(SUBSRC)
			$(GHC) $(FLGS) $(INC) $@

run:	$(suite)
	$(TDIR)/tstIOMap

clean:
	rm -f $(TDIR)/*.hi
	rm -f $(TDIR)/*.o
	rm -f $(TDIR)/tstIOMap
	rm -f $(SMOKEDIR)/*.hi
	rm -f $(SMOKEDIR)/*.o
	rm -f $(SMOKEDIR)/op
	rm -f $(SUTDIR)/*.hi
	rm -f $(SUTDIR)/*.o
	rm -f $(COMDIR)/*.hi
	rm -f $(COMDIR)/*.o
	rm -f test/bin/*.o
	rm -f test/bin/*.hi

build: $(tests) $(SUBSRC)
