# test-chicken not included, because I had problems installing r7rs for chicken
all: test-gauche test-kawa test-guile test-chibi
.PHONY: test-gauche test-kawa test-guile test-chibi test-chicken

test-gauche:
	gosh -I . srfi-234-test.scm

test-kawa:
	cp srfi/234.sld srfi/234.scm
	kawa srfi-234-test.scm
	rm srfi/234.scm

test-guile:
	cp srfi/234.sld srfi/srfi-234.scm
	guile -L . srfi-234-test.scm
	rm srfi/srfi-234.scm

test-chibi:
	chibi-scheme srfi-234-test.scm

test-chicken:
	csc -R r7rs -X r7rs -sJ -o srfi/srfi-234.so srfi/234.sld
	csi -s srfi-234-test.scm
	rm srfi/srfi-234.so
	rm srfi-234.import.scm
