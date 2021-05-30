{
  ssh = "ssh -Y";
  cp = "cp -i";
  mv = "mv -i";
  ls = "ls -F";
  less = "less -r";

  # cmake;
  rmcmake = "rm -rf CMakeFiles CMakeCache.txt";

  # Directory movement
  p = "pushd";
  o = "popd";
  d = "dirs -v";
  ".." = "cd ..";
  "..." = "cd ../..";

  # weather
  weather = "curl wttr.in";

  # jupyter
  jlab = "jupyter lab --no-browser --port $JUPYTER_PORT";

  # GCS vms
  vmstart = "gcloud compute instances start --zone us-central1-a noah-vm";
  vmstop = "gcloud compute instances stop --zone us-central1-a noah-vm";
  vmssh = "gcloud compute ssh --zone us-central1-a noahb@noah-vm";
  vmls = "gcloud compute instances list";

  # kubectl
  k = "kubectl";
  hs = "home-manager switch";

  # dates
  today = "date +%F";
}
