my_rcsdoc = rcs_doc

doc_DATA = itclnh_iwd.pdf

clean-local:
	-rm -rf $(my_rcsdoc)
	rm -rf Doxyfile doxygen itclnh_iwd.{aux,pdf,idx,blg,bbl,log,out,toc}

itclnh_iwd.pdf: itclnh_iwd.tex doxygen/refman.pdf
	TEXINPUTS=$(srcdir):$(TEXINPUTS) TEXFORMATS=$(srcdir):$(TEXFORMATS) latex -output-format=pdf itclnh_iwd.tex

doxygen/refman.pdf: doxygen
	make -C doxygen refman.pdf

doxygen:
	cat $(top_srcdir)/Doxyfile | sed 's:^OUTPUT_DIRECTORY .*:OUTPUT_DIRECTORY = $(PWD)/rcs_doc:g' > Doxyfile
	cd $(top_srcdir) && doxygen $(PWD)/Doxyfile &>/dev/null
	ln -s rcs_doc/latex doxygen

install-data-hook:
	cp -ax $(my_rcsdoc)/html $(DESTDIR)$(docdir)/

uninstall-local:
	rm -rf $(DESTDIR)$(docdir)/html
