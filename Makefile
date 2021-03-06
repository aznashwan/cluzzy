repl:
	cd ./Cluzzy
	clash --interactive ./Cluzzy/Testbenches.hs
	cd -

docs:
	cabal haddock
	weasyprint ./dist/doc/html/Cluzzy/Fuzzifier.html ./Docs/Fuzzifier.pdf
	weasyprint ./dist/doc/html/Cluzzy/Config.html ./Docs/Config.pdf
	weasyprint ./dist/doc/html/Cluzzy/Testbenches-FuzzifierTestbench.html ./Docs/FuzzifierTestbench.pdf
	weasyprint ./dist/doc/html/Cluzzy/Testbenches-FLCTestbench.html ./Docs/FLCTestbench.pdf
	weasyprint ./dist/doc/html/Cluzzy/FuzzySet.html ./Docs/FuzzySet.pdf
	weasyprint ./dist/doc/html/Cluzzy/Rules.html ./Docs/Rules.pdf
	weasyprint ./dist/doc/html/Cluzzy/Controller.html ./Docs/Controller.pdf
