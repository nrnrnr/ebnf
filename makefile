%.icn:	%.nw
	notangle '-L$$line!@__%-1L%N' $^ | sed 's/!@__/ /g' > $@

%.u1:	%.icn
	icont -c $^

all: lib ebnf

ebnf: ebnf.icx

lib: ebnflex.u1 ebnfint.u1
	cp ebnflex.u? ebnfint.u? /lib/icon


ebnf.icx: check.u1 ebnflex.u1 emitc.u1 emiticn.u1 gtoks.u1 images.u1 \
        ll1.u1 main.u1 readgram.u1
	icont -o $@ $^


