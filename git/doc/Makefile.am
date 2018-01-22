my_rcsdoc = ../rcs_doc

doc_DATA = itclnh_iwd.pdf

clean-local:
	-rm -rf $(my_rcsdoc)
	rm -rf doxygen itclnh_iwd.{aux,pdf,idx,blg,bbl,log,out,toc}

itclnh_iwd.pdf: itclnh_iwd.tex doxygen/refman.pdf
	latex -output-format=pdf itclnh_iwd.tex

doxygen/refman.pdf: doxygen
	make -C doxygen

doxygen:
	cd .. && doxygen Doxyfile &>/dev/null
	ln -s ../rcs_doc/latex doxygen

install-data-hook:
	cp -ax $(my_rcsdoc)/html $(DESTDIR)$(docdir)/

uninstall-local:
	rm -rf $(DESTDIR)$(docdir)/html
