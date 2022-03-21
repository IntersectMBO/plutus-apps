FROM agocorona/ubuntu-nix

RUN echo "substituters  = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/" >> /etc/nix/nix.conf \
    && echo "trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" >> /etc/nix/nix.conf \
    && cd /home/user && git clone https://github.com/input-output-hk/plutus-apps \
    && cd /home/user/plutus-apps                ##### for a particular branch: && git checkout 41149926c108c71831cfe8d244c83b0ee4bf5c8a \
    && cd /home/user/plutus-apps && /nix/store/*nix-2.7.0/bin/nix-shell  

CMD cd /home/user/plutus-apps && /nix/store/*nix-2.7.0/bin/nix-shell
