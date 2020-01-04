include Make.inc

OUTLIB = libredealloc.a

all: $(OUTLIB)

$(OUTLIB): re_de_alloc.o \
	re_de_alloc_sp.o re_de_alloc_dp.o re_de_alloc_qp.o \
	re_de_alloc_isp.o re_de_alloc_idp.o re_de_alloc_iqp.o \
	re_de_alloc_csp.o re_de_alloc_cdp.o re_de_alloc_cqp.o \
	re_de_alloc_char.o re_de_alloc_logic.o \
	kinds.o
	ar rvcs $@ $^

#########
# KINDS #
#########
kinds.o: kinds.F90
	$(FC) $(FCFLAGS) $(FCWFLAGS) -c $<

#######################
# MEMORY REALLOCATION #
#######################
re_de_alloc.o: re_de_alloc.F90 \
	re_de_alloc_sp.o re_de_alloc_dp.o re_de_alloc_qp.o \
	re_de_alloc_isp.o re_de_alloc_idp.o re_de_alloc_iqp.o \
	re_de_alloc_csp.o re_de_alloc_cdp.o re_de_alloc_cqp.o \
	re_de_alloc_char.o re_de_alloc_logic.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -c $<

re_de_alloc_sp.o: re_de_alloc_sp.F90 kinds.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -c $<

re_de_alloc_dp.o: re_de_alloc_dp.F90 kinds.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -c $<

re_de_alloc_qp.o: re_de_alloc_qp.F90 kinds.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -c $<

re_de_alloc_isp.o: re_de_alloc_isp.F90 kinds.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -c $<

re_de_alloc_idp.o: re_de_alloc_idp.F90 kinds.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -c $<

re_de_alloc_iqp.o: re_de_alloc_iqp.F90 kinds.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -c $<

re_de_alloc_csp.o: re_de_alloc_csp.F90 kinds.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -c $<

re_de_alloc_cdp.o: re_de_alloc_cdp.F90 kinds.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -c $<

re_de_alloc_cqp.o: re_de_alloc_cqp.F90 kinds.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -c $<

re_de_alloc_char.o: re_de_alloc_char.F90
	$(FC) $(FCFLAGS) $(FCWFLAGS) -c $<

re_de_alloc_logic.o: re_de_alloc_logic.F90
	$(FC) $(FCFLAGS) $(FCWFLAGS) -c $<

.PHONY: test buildtest runtest clean distclean

test: libredealloc.a
	(cd test; make test)

buildtest: libredealloc.a
	(cd test; make buildtest)

runtest: libredealloc.a
	(cd test; make runtest)

clean:
	rm -f *.o *.mod
	(cd test; make clean)

distclean: clean
	rm -f $(OUTLIB) 
	(cd test; make distclean)
