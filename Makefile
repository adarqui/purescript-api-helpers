build:
	pulp -w build

build-psa:
	pulp -w build --stash --censor-lib --censor-codes=ImplicitImport,UnusedExplicitImport,HidingImport,ImplicitQualifiedImport

tests:
	pulp -w test

bower:
	bower install

curtag:
	git push origin :v0.11.0
	git push origin v0.11.0
