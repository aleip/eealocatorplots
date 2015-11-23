# Attention! This kills all word documents!!
taskkill.exe /IM WINWORD.EXE

# Convert to word
/cygdrive/c/Program\ Files/RStudio/bin/pandoc/pandoc ${1}.md --to docx --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --output $1.docx --highlight-style tango 

# Open word
test -e ${1}.docx && /cygdrive/c/Program\ Files\ \(x86\)/Microsoft\ Office/Office14/WINWORD.EXE ${1}.docx &
