%.icn:	%.nw
	notangle '"-L$$line %-1L \"%F\"%N"' $^ > $@

%.u1:	%.icn
	icont -c $^

all: lib ebnf

lib: ebnflex.u1 ebnfint.u1
	cp ebnflex.u? ebnfint.u? /lib/icon


ebnf:	check.u1 ebnflex.u1 emitc.u1 emiticn.u1 flatten.u1 gtoks.u1 images.u1 \
        ll1.u1 main.u1 readgram.u1 write.u1
	icont -o $@ $^


