# gpp options. Like -H except there is no escape character
g = -U "<\#" ">" "\B" "|" ">" "<" ">" "\#" ""

# bibtex bibliography
bib =  manual.bib

# directory in which to put output documents
out = out

all: manual.pdf manual.epub manual.html

manual.pdf: manual.md
	gpp $(g) manual.md | \
        pandoc --bibliography $(bib) \
               --toc -N -s -o $(out)/manual.pdf

manual.epub: manual.md
	gpp $(g) manual.md | \
	pandoc --bibliography $(bib) \
               --toc -N -s -o $(out)/manual.epub

manual.docx: manual.md
	gpp $(g) manual.md | \
	pandoc --bibliography $(bib) \
               --toc -N -s -o $(out)/manual.docx

manual.html: manual.md
	gpp $(g) manual.md | \
	pandoc --bibliography $(bib) \
               --toc -N -s -o $(out)/manual.html

clean:
	rm out/*
