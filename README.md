# My Dotfiles

Set up [nix]:

    sh <(curl -L https://nixos.org/nix/install)

Setup home-manager:

    # if you need a channel
    nix-channel --add https://github.com/nix-community/home-manager/archive/release-20.09.tar.gz home-manager
    nix-channel --update

    # install home-manager
    nix-shell '<home-manager>' -A install


[home-manager]: https://github.com/nix-community/home-manager
[nix]: https://nixos.org/download.html
