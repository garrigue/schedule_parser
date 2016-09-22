#
# Create a program from the xml output of researchr
#
# To get the xml, enter edit mode, go to General Data / Data Export
# and click on ACM DL Mobile app XML

program.pdf: program.tex schedule.tex
	pdflatex -8bit program
	pdflatex -8bit program  # for cross-references

schedule.tex: acmdlxml.xml schedule_parser
	./schedule_parser acmdlxml.xml > $@

program2-nup.pdf: program2.pdf
	pdfnup $<

program2.pdf: program2.tex schedule2.tex
	pdflatex -8bit program2

schedule2.tex: acmdlxml.xml schedule_parser2
	./schedule_parser2 acmdlxml.xml > $@

# The location for xml-light below is if you have installed ocaml through opam,
# otherwise you may have to change it
XML_LIGHT_DIR= `opam config var xml-light:lib`
schedule_parser: schedule_parser.ml
	ocamlc -I $(XML_LIGHT_DIR) xml-light.cma str.cma $< -o $@

schedule_parser2: schedule_parser2.ml
	ocamlc -I $(XML_LIGHT_DIR) xml-light.cma str.cma $< -o $@

# If you do not have Nicolas Cannasse's xml-light, but are using opam,
# the line below should be sufficient.
.PHONY: xml-light
xml-light:
	opam install xml-light

.PHONY: clean
clean:
	rm -f *~ program.pdf schedule.tex schedule2.tex *.cm* schedule_parser \
	  schedule_parser2
