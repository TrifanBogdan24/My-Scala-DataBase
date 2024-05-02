ARCHVE_NAME='Tema 2 Scala'

archive:
	rm -f *.zip ~/Downloads/$(ARCHVE_NAME).zip
	zip -r $(ARCHVE_NAME).zip src/main/scala/* build.sbt ID.txt
	cp $(ARCHVE_NAME).zip ~/Downloads/

zip: archive

test:
	sbt test
