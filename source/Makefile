all: build

odhug: Odhug.hs
ghc --make mxnet.hs

preview: Odhug
./Odhug preview

build: 
./Odhug build

clean:
rm -rf _site _cache odhug odhig.hi

upload: build
rsync -aP -e _site  