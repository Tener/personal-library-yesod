clean:
	cabal clean
keter:
	cabal configure -f-dev -f-library-only -f-heroku
	cabal build
	strip dist/build/personallibrary/personallibrary
	rm -f personallibrary.keter
	rm -rf static/tmp
	tar zcfv personallibrary.keter  --exclude="*.bin" dist/build/personallibrary/personallibrary static config

nginx:
	make keter
	sudo cp personallibrary.keter /opt/keter/incoming/

heroku-oneshot:
	git pull
	cabal-dev install -fheroku --enable-split-objs --disable-documentation --disable-library-profiling --disable-executable-profiling
	strip dist/build/personallibrary/personallibrary
	git checkout -b deploy
	git add -f dist/build/personallibrary/personallibrary
	git commit -m "binary"
	git push -f heroku deploy:master
	git checkout master
	git branch -D deploy
