
build:
	ghc CountingMonoids.hs -O2 -o countingmonoids

first:
	./countingmonoids first & sleep 120 ; kill $!

second:
	./countingmonoids second & sleep 120 ; kill $!