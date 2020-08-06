default: alldoc

include Makefile

# Headers & License
LICENSE_CEA_IMAG = \
	$(PINSEC_LICENSE_CEA_IMAG:%=$(PINSEC_DIR)/%) \
	$(BINSEC_LICENSE_CEA_IMAG:%=$(BINSEC_DIR)/%) 
LICENSE_IMAG = \
	$(PINSEC_LICENSE_IMAG:%=$(PINSEC_DIR)/%) \
	$(BINSEC_LICENSE_IMAG:%=$(BINSEC_DIR)/%) 
LICENSE_CEA = \
	$(SRC_BUILD_FILES) \
	$(PINSEC_LICENSE_CEA:%=$(PINSEC_DIR)/%) \
	$(BINSEC_LICENSE_CEA:%=$(BINSEC_DIR)/%)
LICENSE_EXTERNAL_CHLIPALA = $(BINSEC_LICENSE_EXTERNAL_CHLIPALA:%=$(BINSEC_DIR)/%)
LICENSE_EXTERNAL = $(PINSEC_EXTERNAL) $(BINSEC_EXTERNAL)


define mk_license
	$(PP) $(1)
	for f in $(1); do \
		$(HEADACHE) -r -c $(HEADACHE_CONFIG) -h $(2) $$f; \
	done
endef

.PHONY: headers
ifneq ($(HEADACHE), no)
headers:
	$(call mk_license, $(LICENSE_IMAG), headers/IMAG_LGPL)
	$(call mk_license, $(LICENSE_CEA_IMAG), headers/CEA_IMAG_LGPL)
	$(call mk_license, $(LICENSE_CEA), headers/CEA_LGPL)
	$(call mk_license, $(LICENSE_EXTERNAL_CHLIPALA), headers/BSD_CHLIPALA)
endif


DATE=$(shell date +"%Y%m%d")
RELEASE_CODE_NAME = "Astute_Asterix"
VERSION=$(shell cat VERSION)
NAME = binsec-$(VERSION)-$(DATE)
SYMLINK = $(NAME)
PINSEC_FILES = $(PINSEC_DISTRIB_FILES:%=$(SYMLINK)/$(PINSEC_DIR)/%)
BINSEC_FILES = $(BINSEC_DISTRIB_FILES:%=$(SYMLINK)/$(BINSEC_DIR)/%)
SRC_BUILD_FILES = configure configure.ac \
	Piqi.mk Caml.mk Config.mk.in Makefile \
	install-sh CHANGES VERSION
SRC_OTHER_FILES = LICENSE README
OPAM_DIR = opam
DISTRIB_FILES = \
	$(SRC_BUILD_FILES:%=$(SYMLINK)/%) \
	$(SRC_OTHER_FILES:%=$(SYMLINK)/%) \
	$(PINSEC_FILES) $(BINSEC_FILES) \
	$(SYMLINK)/$(OPAM_DIR)
TARBALL = $(NAME).tgz


tarball: clean-tarball
	autoconf
	$(PP) "Making tarball $(TARBALL)"
	$(RM) $(SYMLINK)
	ln -sf . $(SYMLINK)
	@$(TAR) $(TARBALL) $(DISTRIB_FILES)
	unlink $(SYMLINK)

clean-tarball:
	-rm -f *.tgz
	-rm -f $(TARBALL)

##
# Documentation
##

.SUFFIXES: .md .html

DOC_SRCDIR = doc_src
# ADD YOUR FILES HERE
MD_FILES = tools

DOC_DESTDIR ?= docgen
HTML_FILES = $(MD_FILES:%=$(DOC_DESTDIR)/%.html)


.PHONY: docdir
docdir:
	$(MKDIR_P) $(DOC_DESTDIR)

.PHONY: html htmlmd
htmlmd : $(HTML_FILES)
html : docdir 

HTML_DISTRIB_DIR = $(DOC_DESTDIR)/distrib
API_DOC_DIR = $(DOC_DESTDIR)/apiref
BINSEC_DOC_DIR = $(BINSEC_DIR)/binsec.docdir
ISED ?= sed -i


.phony: api_base 

api_base:
	env USE_OCAMLBUILD=no $(MAKE) -C $(BINSEC_DIR) depend
	env USE_OCAMLBUILD=no $(MAKE) -C $(BINSEC_DIR) camldoc
	$(MKDIR_P) $(API_DOC_DIR)

html_distrib:
	$(MKDIR_P) $(HTML_DISTRIB_DIR)
	$(CP) $(TARBALL) $(HTML_DISTRIB_DIR)

$(API_DOC_DIR)/%.html: $(BINSEC_DOC_DIR)/%.html 
	$(PP) "awk $@"
	$(AWK) -f scripts/poshtml.awk $< > $@


html_files: api_base $(addprefix $(API_DOC_DIR)/, $(notdir $(wildcard $(BINSEC_DOC_DIR)/*html)))

apidoc: html_files


alldoc: html apidoc distrib md5
	env USE_OCAMLBUILD=no $(MAKE) -C $(BINSEC_DIR) clean

# The utility is called md5 under Mac OS X / BSD 
MD5 ?= md5sum 
md5: distrib
	$(MD5) $(TARBALL) > $(HTML_DISTRIB_DIR)/$(NAME).md5

INTERFACE_CHECKER=scripts/check_interface

$(INTERFACE_CHECKER): $(INTERFACE_CHECKER:%=%.ml)
	$(CAMLBYT) -o $@ $^

check: veryclean $(INTERFACE_CHECKER)
	$(INTERFACE_CHECKER)

