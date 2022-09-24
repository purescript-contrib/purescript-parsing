let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220921/packages.dhall
        sha256:169bd823a71ae033eaf4f77776e184f12c656163feae14e7f649a48932ca6ac0

in  upstream
  with minibench.version = "77329a816a4be933cd3c51cc8517608d5affb450"
