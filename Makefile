repl:
	cd ./Cluzzy
	clash --interactive ./Cluzzy/Testbenches.hs
	cd -

docs:
	cabal haddock
	weasyprint ./dist/doc/html/Cluzzy/Fuzzifier.html ./Docs/Fuzzifier.pdf
	weasyprint ./dist/doc/html/Cluzzy/Config.html ./Docs/Config.pdf
	weasyprint ./dist/doc/html/Cluzzy/Testbenches-FuzzifierTestbench.html ./Docs/FuzzifierTestbench.pdf
