all:
	rm -f biblioteczka.zip dokumentacja.zip
	rm -rf biblioteczka
	mkdir biblioteczka/
#	-cp -rv * biblioteczka/ 
#	rm -rf biblioteczka/cabal-dev biblioteczka/dist biblioteczka/biblioteczka biblioteczka/makefile biblioteczka/static/tmp biblioteczka/config/client_session_key.aes biblioteczka/*sqlite3 biblioteczka/incoming
	cp -rv *.hs Handler/ Settings/ Model/ biblioteczka.cabal static/ config/ INSTALL.txt LICENSE messages/ templates/ tests/ biblioteczka/
	rm -rf biblioteczka/static/tmp

	make keter
	cp biblioteczka.keter biblioteczka/biblioteczka.keter.tgz

	find biblioteczka/ -exec touch {} \;

	7z a -mx=9 biblioteczka.zip biblioteczka
	7z a -mx=9 dokumentacja.zip bd2012-tworz-baze.sql INSTALL.txt

clean:
	cabal clean
	rm -rf biblioteczka/
	rm -f biblioteczka.zip dokumentacja.zip

keter:
	cabal-dev configure -f-dev -f-library-only -f-heroku
	cabal-dev build
	strip dist/build/biblioteczka/biblioteczka
	rm -f biblioteczka.keter
	rm -rf static/tmp
	tar zcfv biblioteczka.keter  --exclude="*.bin" dist/build/biblioteczka/biblioteczka static config incoming/

nginx:
	make keter
	sudo cp biblioteczka.keter /opt/keter/incoming/
