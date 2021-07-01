PROPERTIES = \
	cabalPackage=alertmanager-openapi \
	cabalVersion=0.0.1.0 \
	configType=AlertmanagerClientConfig \
	useKatip=false

# This trick for comma-separated lists is take from:
# https://stackoverflow.com/questions/7525589/create-comma-separated-lists-in-gnu-make
#
# Stack Overflow user content is licensed under CC-BY-SA 4.0. The license
# can be found at:
# https://creativecommons.org/licenses/by-sa/4.0/

null  :=
space := $(null) #
comma := ,

generate:
	openapi-generator generate \
		-i alertmanager/api/v2/openapi.yaml \
		--additional-properties $(subst $(space),$(comma),$(strip $(PROPERTIES))) \
		--ignore-file-override alertmanager-openapi/.openapi-generator-ignore \
		-g haskell-http-client -o alertmanager-openapi/

