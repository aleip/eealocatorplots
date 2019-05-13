
A) This is a list of the steps to deal with the inventory data and to produce plots, etc:

1.- Unzip 'D:\dev\ghginventory\2019\eealocator\CRF_MMR_20190508.zip'

2.- Open 'eealocator_csv.bash' and change 'subyear' and 'submission'

3.- Run this bash program:
	cd /d/dev/ghginventory/eealocatorplots
	./eealocator_csv.bash

4.- Open 'D:\dev\ghginventory\eealocatorplots\curplot.r' and change 'cursubm' and 'invyear'. Save and NOT run

5.- Open 'D:\dev\ghginventory\eealocatorplots\eealocatorplots.r' and run it! (Until Step 9, included)






B) Steps to produce NIR-CAPRI comparison plots:

0.- Some files and R scripts used/produced in step A should be in GoogleDrive: eealocatorplots.jrc@gmail.com (ask to Adrian/Xavi for the password)

1.- Open 'capinv.r'. In 2019 it was in '\\s-jrciprap246p.jrc.it\dev\CAPRImodel\Trunk_as_in_repository\gams', but this might change... ask to Adrian!

2.- Run it after changing some variables, if needed (e.g. cursubm, curdir).
(There is an error in L500 due to the "ReporteRs", which is no longer available. But this part is no necessary for now)






C) Steps to produce the Chapter 5 of the report:

1.- Open 'nircat.bash' in a text editor and change submission date (L51)

2.- Open 'nir0header.Rmd', change 'cursubm' and save it

3.- Run it:
	cd /d/dev/ghginventory/eealocatorplots
	./nircat.bash

4.- Apply a macro to the Word document generated (to change format, etc):
	
	i) File > Options > Add-ins > Manage > Templates > Go
	ii) Select niragritemplate.dotm and OK. 'Enable Content' if requested
	iii) View > Macros > formatNIR > Run

5.- Some tables/figures need to be copy/pasted by hand (ask Gema/Adrion where to find them)

























