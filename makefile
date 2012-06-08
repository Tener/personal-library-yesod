clean:
	cabal clean
keter:
	cabal-dev configure -f-dev -f-library-only -f-heroku
	cabal-dev build
	strip dist/build/personallibrary/personallibrary
	rm -f personallibrary.keter
	rm -rf static/tmp
	tar zcfv personallibrary.keter  --exclude="*.bin" dist/build/personallibrary/personallibrary static config incoming/

nginx:
	make keter
	sudo cp personallibrary.keter /opt/keter/incoming/
