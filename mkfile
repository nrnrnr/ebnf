SOURCE=`echo *[!t].nw`
U1=`echo $SOURCE | sed 's/.nw/.u1/g'`
ICN=`echo $SOURCE | sed 's/.nw/.icn/g'`

%.u1: %.icn
	icont -c $prereq

%.icn: %.nw
	notangle -L'#line %-1L "%F"%N' $prereq | cpif $target

%.sml: %.g ebnf
	./ebnf -ml $stem.g > $target

%.html: %.nw
	noweave -filter l2h -autodefs icon -index -html $prereq > $target

all:V: ebnf ebnflex.sml
test:V: ebnflex.sml testll.sml

ebnf: $U1
	icont -o $target $prereq

lump:V: ebnf.icn

ebnf.icn: $ICN ebnfint.icn
	ilump $prereq > $target

ebnf-dist.icn: ebnf.icn
	cp $prereq $target

clean:V:
	rm -f *~ *.icn y.tab.[ch] y.output
	rm -f *.u1 *.u2 $ICN

clobber:V: clean
	rm -f ebnf ebnflex

ebnflex.sml: emitml.nw
	notangle -Rmlfuns.t emitml.nw > ebnflex.sml
