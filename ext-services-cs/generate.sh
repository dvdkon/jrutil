#!/bin/sh

cd "$(dirname $0)"

echo Generating KadrWsCiselniky...
rm KadrWsCiselniky/*.cs 2>/dev/null
dotnet-svcutil \
	--outputDir KadrWsCiselniky \
	--projectFile ext-services-cs.csproj \
	--sync \
	https://provoz.spravazeleznic.cz/kadrws/ciselniky.asmx?wsdl

echo Generating CzPttXml...
xscgen \
	--namespace=CzPttXml \
	--nullable \
	--collectionType=System.Array \
	--collectionSettersMode=Public \
	--netCore \
	-o CzPttXml \
	../jrutil/src/CzPtt/czptt.xsd

