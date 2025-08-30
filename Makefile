all: build

.PHONY: build
build:
	dub build --parallel --build-mode=singleFile --compiler=ldc2 --debug debug

.PHONY: clean
clean:
	dub clean
	-rm -f gidgen

# Make sure you have updated the source/relver.d so your release build has correct version.
# Release build with debuginfo.
.PHONY: release
release:
	dub build -b release-debug --parallel --build-mode=singleFile --compiler=ldc2 --d-version=RELEASE_BUILD
