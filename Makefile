lint:
	 nixfmt --check $(shell find . -name '*.nix')

reformat:
	 nixfmt $(shell find . -name '*.nix')

%:
	home-manager $@