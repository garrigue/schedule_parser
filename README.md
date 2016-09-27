# schedule_parser
Tool to produce programme from [researchr](http://conf.researchr.org/) data.

In order to use this tool for your conference, you should just download `acmdlxml.xml`
from researchr/Generic Data/Data Export, and edit `schedule_parser.ml` to describe
the different tracks.

Dependencies:
* [`ocaml`](http://ocaml.org): used 4.02, but any sufficiently recent version should be fine
* [`xml-light`](https://github.com/ncannasse/xml-light): the `Makefile` assumes it was installed through [opam](https://opam.ocaml.org/), in case it was installed by another mean you should set `XML_LIGHT_DIR` to the installation directory
