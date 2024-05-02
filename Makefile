ARCHVE_NAME='PP_Tema_2_Trifan_Bogdan_Cristian_322CDa'

archive:
	rm -f $(ARHIVE_NAME).zip ~/Downloads/$(ARCHVE_NAME).zip
	zip -r $(ARCHVE_NAME).zip src/main/scala/* build.sbt ID.txt
	cp $(ARCHVE_NAME).zip ~/Downloads/

zip: archive
