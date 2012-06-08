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

heroku-stage1:
	git pull
	cabal-dev install -fheroku --ghc-options=-split-objs
	strip dist/build/personallibrary/personallibrary
	scp dist/build/personallibrary/personallibrary raptor.local:/home/tener/dokumenty/projekty/personallibrary/personallibrary-freshbuild

heroku-stage2:
	git checkout -b deploy
	mv personallibrary-freshbuild personallibraryHeroku
	git add zapisyOazaHeroku
	git commit -m "binary"
	git push -f heroku deploy:master
	git checkout master
	git branch -D deploy
