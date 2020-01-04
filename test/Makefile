FC = gfortran
FCFLAGS = -O3 -g -fcheck=all -fbacktrace \
          -fmax-errors=3 -fimplicit-none -D__DEBUG_MSG
FCWFLAGS = -Wall -Wextra -Wpedantic -Wsurprising

ifneq ($(wildcard ../Make.inc),)
   include ../Make.inc
endif
 
LIBS = -Wl,-Bstatic -L../ -lredealloc -Wl,-Bdynamic
INCLUDE = -I../

all: test

test: runtest_re_de_alloc

runtest: ../libredealloc.a

buildtest: ../libredealloc.a

../libredealloc.a: 
	(cd ../; make libredealloc.a)

runtest_re_de_alloc: runtest_re_de_alloc_isp \
	           runtest_re_de_alloc_idp \
	           runtest_re_de_alloc_iqp \
	           runtest_re_de_alloc_sp \
	           runtest_re_de_alloc_dp \
	           runtest_re_de_alloc_qp \
	           runtest_re_de_alloc_csp \
	           runtest_re_de_alloc_cdp \
	           runtest_re_de_alloc_cqp \
	           runtest_re_de_alloc_char \
	           runtest_re_de_alloc_logic

buildtest_re_de_alloc: test_re_de_alloc_isp.x \
                  test_re_de_alloc_idp.x \
                  test_re_de_alloc_iqp.x \
                  test_re_de_alloc_sp.x \
                  test_re_de_alloc_dp.x \
                  test_re_de_alloc_qp.x \
                  test_re_de_alloc_csp.x \
                  test_re_de_alloc_cdp.x \
                  test_re_de_alloc_cqp.x \
                  test_re_de_alloc_char.x \
                  test_re_de_alloc_logic.x

runtest_re_de_alloc_isp: test_re_de_alloc_isp.x
	@(./test_re_de_alloc_isp.x)
runtest_re_de_alloc_idp: test_re_de_alloc_idp.x
	@(./test_re_de_alloc_idp.x)
runtest_re_de_alloc_iqp: test_re_de_alloc_iqp.x
	@(./test_re_de_alloc_iqp.x)
runtest_re_de_alloc_sp: test_re_de_alloc_sp.x
	@(./test_re_de_alloc_sp.x)
runtest_re_de_alloc_dp: test_re_de_alloc_dp.x
	@(./test_re_de_alloc_dp.x)
runtest_re_de_alloc_qp: test_re_de_alloc_qp.x
	@(./test_re_de_alloc_qp.x)
runtest_re_de_alloc_csp: test_re_de_alloc_csp.x
	@(./test_re_de_alloc_csp.x)
runtest_re_de_alloc_cdp: test_re_de_alloc_cdp.x
	@(./test_re_de_alloc_cdp.x)
runtest_re_de_alloc_cqp: test_re_de_alloc_cqp.x
	@(./test_re_de_alloc_cqp.x)
runtest_re_de_alloc_char: test_re_de_alloc_char.x
	@(./test_re_de_alloc_char.x)
runtest_re_de_alloc_logic: test_re_de_alloc_logic.x
	@(./test_re_de_alloc_logic.x)

test_re_de_alloc_isp.x: test_re_de_alloc_isp.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -o $@ $^ $(LIBS)
test_re_de_alloc_isp.o: test_re_de_alloc_isp.F90 ../libredealloc.a
	$(FC) $(FCFLAGS) $(FCWFLAGS) $(INCLUDE) -c $<
test_re_de_alloc_idp.x: test_re_de_alloc_idp.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -o $@ $^ $(LIBS)
test_re_de_alloc_idp.o: test_re_de_alloc_idp.F90 ../libredealloc.a
	$(FC) $(FCFLAGS) $(FCWFLAGS) $(INCLUDE) -c $<
test_re_de_alloc_iqp.x: test_re_de_alloc_iqp.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -o $@ $^ $(LIBS)
test_re_de_alloc_iqp.o: test_re_de_alloc_iqp.F90 ../libredealloc.a
	$(FC) $(FCFLAGS) $(FCWFLAGS) $(INCLUDE) -c $<
test_re_de_alloc_sp.x: test_re_de_alloc_sp.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -o $@ $^ $(LIBS)
test_re_de_alloc_sp.o: test_re_de_alloc_sp.F90 ../libredealloc.a
	$(FC) $(FCFLAGS) $(FCWFLAGS) $(INCLUDE) -Wno-compare-reals -c $<
test_re_de_alloc_dp.x: test_re_de_alloc_dp.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -o $@ $^ $(LIBS)
test_re_de_alloc_dp.o: test_re_de_alloc_dp.F90 ../libredealloc.a
	$(FC) $(FCFLAGS) $(FCWFLAGS) $(INCLUDE) -Wno-compare-reals -c $<
test_re_de_alloc_qp.x: test_re_de_alloc_qp.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -o $@ $^ $(LIBS)
test_re_de_alloc_qp.o: test_re_de_alloc_qp.F90 ../libredealloc.a
	$(FC) $(FCFLAGS) $(FCWFLAGS) $(INCLUDE) -Wno-compare-reals -c $<
test_re_de_alloc_csp.x: test_re_de_alloc_csp.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -o $@ $^ $(LIBS)
test_re_de_alloc_csp.o: test_re_de_alloc_csp.F90 ../libredealloc.a
	$(FC) $(FCFLAGS) $(FCWFLAGS) $(INCLUDE) -Wno-compare-reals -c $<
test_re_de_alloc_cdp.x: test_re_de_alloc_cdp.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -o $@ $^ $(LIBS)
test_re_de_alloc_cdp.o: test_re_de_alloc_cdp.F90 ../libredealloc.a
	$(FC) $(FCFLAGS) $(FCWFLAGS) $(INCLUDE) -Wno-compare-reals -c $<
test_re_de_alloc_cqp.x: test_re_de_alloc_cqp.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -o $@ $^ $(LIBS)
test_re_de_alloc_cqp.o: test_re_de_alloc_cqp.F90 ../libredealloc.a
	$(FC) $(FCFLAGS) $(FCWFLAGS) $(INCLUDE) -Wno-compare-reals -c $<
test_re_de_alloc_char.x: test_re_de_alloc_char.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -o $@ $^ $(LIBS)
test_re_de_alloc_char.o: test_re_de_alloc_char.F90 ../libredealloc.a
	$(FC) $(FCFLAGS) $(FCWFLAGS) $(INCLUDE) -Wno-compare-reals -c $<
test_re_de_alloc_logic.x: test_re_de_alloc_logic.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -o $@ $^ $(LIBS)
test_re_de_alloc_logic.o: test_re_de_alloc_logic.F90 ../libredealloc.a
	$(FC) $(FCFLAGS) $(FCWFLAGS) $(INCLUDE) -Wno-compare-reals -c $<

.PHONY: test clean distclean

clean:
	rm -f *.o *.mod *.MOD

distclean: clean
	rm -f *.x
