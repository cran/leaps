LIBNAME=leaps
LD=ld

OBJS=   leaps.o leapshdr.o

leaps.so: $(OBJS)
	@$(LD) $(SHLIBLDFLAGS) -o $(LIBNAME).so $(OBJS)

clean:
	@rm -f *.o *.so

