LIBNAME=leaps
LD=ld

OBJS=   leaps.o leapshdr.o

leap.so:$(OBJS)
	@$(LD) $(SHLIBLDFLAGS) -o $(LIBNAME).so $(OBJS)

clean:
	@rm -f *.o *.so

