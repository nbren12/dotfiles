{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = builtins.getEnv "USER";
  home.homeDirectory = builtins.getEnv "HOME";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";

  home.packages = with pkgs; [ hugo nixfmt google-cloud-sdk openssh rsync ];

  home.sessionVariables = { EDITOR = "vim"; };

  programs.git = {
    enable = true;
    userName = "Noah D. Brenowitz";
    userEmail = "nbren12@gmail.com";
    aliases = {
      a = "add";
      co = "checkout";
      br = "branch";
      ds = "diff --stat";
      ls =
        "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative";
      lss = ''
        log --pretty=format:"%C(yellow)%h%Cred%d\ %Creset%s%Cblue\ [%cn]" --decorate'';
      ll = ''
        log --pretty=format:"%C(yellow)%h%Cred%d\ %Creset%s%Cblue\ [%cn]" --decorate --numstat'';
      ln = "log --name-status";
      li = "log --stat";
      lsf = "ls-files";
      cp = "cherry-pick";
      st = "status -s";
      cl = "clone";
      ci = "commit";
      cm = "commit --amend";
      diff = "diff --word-diff";
      dc = "diff --cached";
      ttc = "clean -x -d";
    };
    ignores =
      [ "*.aux" "*.bbl" "*.bcf" "*.blg" "*.pdf" "*.gz" "*.fdb_latexmk" ];
  };

  programs.vim = { enable = true; };

  programs.tmux = {
    enable = true;
    extraConfig = builtins.readFile ~/dotfiles/.tmux.conf;
  };

  programs.direnv = { enable = true; };

  programs.fish = {
    enable = true;
    interactiveShellInit = ''
      source ~/dotfiles/.dotfiles/shell/aliases.sh
      fish_vi_key_bindings

      # key bindings
      bind -M insert \cg 'git status'
    '';

  };
  home.file.".config/fish/conf.d/prompt.fish".source = ~/dotfiles/.config/fish/conf.d/prompt.fish;

  programs.fzf.enable = true;
  programs.fzf.enableFishIntegration = true;

  programs.vscode = { enable = true; };
}
