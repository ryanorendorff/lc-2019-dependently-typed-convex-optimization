DOC=DependentConvexTalk

${DOC}.pdf: ${DOC}.tex
	xelatex -shell-escape ${DOC}.tex
	xelatex -shell-escape ${DOC}.tex

${DOC}.tex: ${DOC}.lhs
	lhs2TeX ${DOC}.lhs > ${DOC}.tex
