all: list.out

%.out: %.hs
	ghc -dynamic $< -o $@

.PHONY: clean
clean:
	@find ./ -name "*.o" -print -delete
	@find ./ -name "*.out" -print -delete
	@find ./ -name "*.hi" -print -delete
