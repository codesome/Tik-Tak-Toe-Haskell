install:
	ghc GameMain.hs TikTakToe.hs

play: install
	./GameMain

clean:
	rm -f *.o *.hi GameMain

.PHONY = clean