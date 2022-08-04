.PHONY: test-gauche test-kawa test-chibi test-chicken

test-gauche:
	gosh -I . toposort-test.scm

test-kawa:
	cp toposort.sld toposort.scm
	kawa toposort-test.scm
	rm toposort.scm

test-chibi:
	chibi-scheme toposort-test.scm

test-chicken:
	csc -R r7rs -X r7rs -sJ -o toposort.so toposort.sld
	csi -I . -R r7rs -s toposort-test.scm
	rm toposort.so
	rm toposort.import.scm
